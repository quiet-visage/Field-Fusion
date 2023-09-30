#include "msdf.hh"
#include "freetype/freetype.h"
#include "freetype/ftimage.h"
#include "msdferror.hh"
#include "msdfmap.hh"
#include "serializer.hh"
#include "shaders.hh"
#include <GL/glew.h>
#include <cassert>
#include <cstdint>
#include <iostream>
#include <memory>
#include <numeric>
#include <string>

namespace msdf {
namespace internal {
const extern GLfloat _MAT4_ZERO_INIT[4][4];
bool compile_shader(const char *source, GLenum type, GLuint *shader, const char *version);
MapItem *map_get_or_add(Context &ctx, Font &font, Atlas &atlas, int32_t key);
inline int is_control(int32_t code) { return (code <= 31) || (code >= 128 && code <= 159); };
struct use_free {
    void operator()(void *x) { free(x); }
};

} // namespace internal

struct IndexEntry {
    float offset_x;
    float offset_y;
    float size_x;
    float size_y;
    float bearing_x;
    float bearing_y;
    float glyph_width;
    float glyph_height;
};

void ortho(float left, float right, float bottom, float top, float nearVal, float farVal, float dest[][4]) {
    GLfloat rl, tb, fn;

    memcpy(dest, internal::_MAT4_ZERO_INIT, sizeof(internal::_MAT4_ZERO_INIT));

    rl = 1.0f / (right - left);
    tb = 1.0f / (top - bottom);
    fn = -1.0f / (farVal - nearVal);

    dest[0][0] = 2.0f * rl;
    dest[1][1] = 2.0f * tb;
    dest[2][2] = 2.0f * fn;
    dest[3][0] = -(right + left) * rl;
    dest[3][1] = -(top + bottom) * tb;
    dest[3][2] = (farVal + nearVal) * fn;
    dest[3][3] = 1.0f;
}

[[nodiscard]] std::variant<Context, std::exception> Context::create(const char *version) noexcept {
    Context result{};
    FT_Error error = FT_Init_FreeType(&result._ft_library);

    if (error != 0)
        return FtInitializationError();

    glGetIntegerv(GL_MAX_TEXTURE_SIZE, &result._max_texture_size);

    unsigned vertex_shader, geometry_shader, fragment_shader;
    if (!internal::compile_shader(msdf_vertex, GL_VERTEX_SHADER, &vertex_shader, version))
        return MsdfVertexShaderCompileError();
    if (!internal::compile_shader(msdf_fragment, GL_FRAGMENT_SHADER, &fragment_shader, version))
        return MsdfFragmentShaderCompileError();
    if (!(result._gen_shader = glCreateProgram()))
        return ShaderLinkageError();

    glAttachShader(result._gen_shader, vertex_shader);
    glAttachShader(result._gen_shader, fragment_shader);
    glLinkProgram(result._gen_shader);
    glDeleteShader(vertex_shader);
    glDeleteShader(fragment_shader);

    int status;
    glGetProgramiv(result._gen_shader, GL_LINK_STATUS, &status);
    if (!status) {
        std::cerr << "Failed to link shader program" << std::endl;
        return ShaderLinkageError();
    }

    result._uniforms.atlas_projection = glGetUniformLocation(result._gen_shader, "projection");
    result._uniforms.texture_offset = glGetUniformLocation(result._gen_shader, "offset");
    result._uniforms.glyph_height = glGetUniformLocation(result._gen_shader, "glyph_height");
    result._uniforms.translate = glGetUniformLocation(result._gen_shader, "translate");
    result._uniforms.scale = glGetUniformLocation(result._gen_shader, "scale");
    result._uniforms.range = glGetUniformLocation(result._gen_shader, "range");
    result._uniforms.meta_offset = glGetUniformLocation(result._gen_shader, "meta_offset");
    result._uniforms.point_offset = glGetUniformLocation(result._gen_shader, "point_offset");
    result._uniforms.metadata = glGetUniformLocation(result._gen_shader, "metadata");
    result._uniforms.point_data = glGetUniformLocation(result._gen_shader, "point_data");

    if (!internal::compile_shader(font_vertex, GL_VERTEX_SHADER, &vertex_shader, version))
        return MsdfVertexShaderCompileError();
    if (!internal::compile_shader(font_geometry, GL_GEOMETRY_SHADER, &geometry_shader, version))
        return FontGeometryShaderCompileError();
    if (!internal::compile_shader(font_fragment, GL_FRAGMENT_SHADER, &fragment_shader, version))
        return FontFragmentShaderCompileError();
    if (!(result._render_shader = glCreateProgram()))
        return ShaderLinkageError();

    glAttachShader(result._render_shader, vertex_shader);
    glAttachShader(result._render_shader, geometry_shader);
    glAttachShader(result._render_shader, fragment_shader);
    glLinkProgram(result._render_shader);
    glDeleteShader(vertex_shader);
    glDeleteShader(geometry_shader);
    glDeleteShader(fragment_shader);

    glGetProgramiv(result._render_shader, GL_LINK_STATUS, &status);
    if (!status) {
        glDeleteProgram(result._gen_shader);
        return ShaderLinkageError();
    }

    result._uniforms.window_projection = glGetUniformLocation(result._render_shader, "projection");
    result._uniforms.font_atlas_projection = glGetUniformLocation(result._render_shader, "font_projection");
    result._uniforms.index = glGetUniformLocation(result._render_shader, "font_index");
    result._uniforms.atlas = glGetUniformLocation(result._render_shader, "font_atlas");
    result._uniforms.padding = glGetUniformLocation(result._render_shader, "padding");
    result._uniforms.dpi = glGetUniformLocation(result._render_shader, "dpi");
    result._uniforms.units_per_em = glGetUniformLocation(result._render_shader, "units_per_em");
    result._dpi[0] = 72.0;
    result._dpi[1] = 72.0;

    glGenVertexArrays(1, &result._bbox_vao);
    glGenBuffers(1, &result._bbox_vbo);
    glBindBuffer(GL_ARRAY_BUFFER, result._bbox_vbo);
    glBufferData(GL_ARRAY_BUFFER, 12 * sizeof(float), 0, GL_STREAM_READ);
    glBindBuffer(GL_ARRAY_BUFFER, 0);

    return result;
}

[[nodiscard]] std::variant<Font, std::exception> Font::create(const Context &ctx, const char *font_name,
                                                              const float scale, const float range) noexcept {
    FT_Face face;
    Font result{font_name, scale, range};

    if (FT_New_Face(ctx._ft_library, result._font_name, 0, &face))
        return FtFaceInitializationError();

    FT_Select_Charmap(face, ft_encoding_unicode);

    result._face = face;
    result._vertical_advance = (float)(result._face->ascender - result._face->descender);

    glGenBuffers(1, &result._meta_input_buffer);
    glGenBuffers(1, &result._point_input_buffer);
    glGenTextures(1, &result._meta_input_texture);
    glGenTextures(1, &result._point_input_texture);

    return result;
}

[[nodiscard]] Atlas Atlas::create(Context &ctx, int texture_width, int padding) noexcept {
    Atlas result{0};
    result._texture_width = texture_width ? texture_width : ctx._max_texture_size;

    result._nglyphs = 0;
    result._nallocated = 0;
    result._offset_x = 1;
    result._offset_y = 1;
    result._y_increment = 0;
    result._texture_height = 0;
    result._padding = padding;

    glGenBuffers(1, &result._index_buffer);
    glGenTextures(1, &result._index_texture);

    glGenTextures(1, &result._atlas_texture);
    glGenFramebuffers(1, &result._atlas_framebuffer);

    result._refcount = 0;

    return result;
}

int Font::generate_glyphs(const Context &ctx, Atlas &atlas, const std::vector<int> &keys) noexcept {
    GLint original_viewport[4];
    glGetIntegerv(GL_VIEWPORT, original_viewport);

    int retval = -2;
    bool range = false;
    int nrender = keys.size();

    if (nrender <= 0)
        return -1;

    /* Calculate the amount of memory needed on the GPU.*/
    std::unique_ptr<size_t[]> meta_sizes(new size_t[nrender]());
    std::unique_ptr<size_t[]> point_sizes(new size_t[nrender]());
    // std::unique_ptr<IndexEntry []>atlas_index (new IndexEntry[nrender]);

    /* We will start with a square texture. */
    int new_texture_height = atlas._texture_height ? atlas._texture_height : 1;
    int new_index_size = atlas._nallocated ? atlas._nallocated : 1;

    /* Amount of new memory needed for the index. */
    std::unique_ptr<IndexEntry[]> atlas_index(new IndexEntry[nrender]());

    size_t meta_size_sum = 0, point_size_sum = 0;
    for (size_t i = 0; (int)i < (int)nrender; ++i) { // MARK
        int code = keys[i];
        glyph_buffer_size(_face, code, &meta_sizes[i], &point_sizes[i]);

        meta_size_sum += meta_sizes[i];
        point_size_sum += point_sizes[i];
    }

    /* Allocate the calculated amount. */
    std::unique_ptr<char[], internal::use_free> point_data((char *)calloc(point_size_sum, 1));
    std::unique_ptr<char[], internal::use_free> metadata((char *)calloc(meta_size_sum, 1));

    /* Serialize the glyphs into RAM. */
    char *meta_ptr = metadata.get();
    char *point_ptr = point_data.get();
    for (size_t i = 0; (int)i < (int)nrender; ++i) { // nrender = 1
        float buffer_width, buffer_height;

        int code = keys[i];
        serialize_glyph(_face, code, meta_ptr, (GLfloat *)point_ptr);

        MapItem *m = _character_index.insert(code);
        m->advance[0] = (float)_face->glyph->metrics.horiAdvance;
        m->advance[1] = (float)_face->glyph->metrics.vertAdvance;

        buffer_width = _face->glyph->metrics.width / SERIALIZER_SCALE + _range;
        buffer_height = _face->glyph->metrics.height / SERIALIZER_SCALE + _range;
        buffer_width *= _scale;
        buffer_height *= _scale;

        meta_ptr += meta_sizes[i];
        point_ptr += point_sizes[i];

        if (atlas._offset_x + buffer_width > atlas._texture_width) {
            atlas._offset_y += (atlas._y_increment + atlas._padding);
            atlas._offset_x = 1;
            atlas._y_increment = 0;
        }
        atlas._y_increment =
            (size_t)buffer_height > atlas._y_increment ? (size_t)buffer_height : atlas._y_increment;

        atlas_index[i].offset_x = atlas._offset_x;
        atlas_index[i].offset_y = atlas._offset_y;
        atlas_index[i].size_x = buffer_width;
        atlas_index[i].size_y = buffer_height;
        atlas_index[i].bearing_x = _face->glyph->metrics.horiBearingX;
        atlas_index[i].bearing_y = _face->glyph->metrics.horiBearingY;
        atlas_index[i].glyph_width = _face->glyph->metrics.width;
        atlas_index[i].glyph_height = _face->glyph->metrics.height;

        atlas._offset_x += (size_t)buffer_width + atlas._padding;

        while ((atlas._offset_y + buffer_height) > new_texture_height) {
            new_texture_height *= 2;
        }
        if (new_texture_height > ctx._max_texture_size) {
            std::cerr << "exceeded max texture size" << std::endl;
            ;
            return -1;
        }
        while ((int)(atlas._nglyphs + i) >= new_index_size) {
            new_index_size *= 2;
        }
    }

    /* Allocate and fill the buffers on GPU. */
    glBindBuffer(GL_ARRAY_BUFFER, _meta_input_buffer);
    glBufferData(GL_ARRAY_BUFFER, meta_size_sum, metadata.get(), GL_DYNAMIC_READ);

    glBindBuffer(GL_ARRAY_BUFFER, _point_input_buffer);
    glBufferData(GL_ARRAY_BUFFER, point_size_sum, point_data.get(), GL_DYNAMIC_READ);

    if ((int)atlas._nallocated == new_index_size) {
        glBindBuffer(GL_ARRAY_BUFFER, atlas._index_buffer);
    } else {
        GLuint new_buffer;
        glGenBuffers(1, &new_buffer);
        glBindBuffer(GL_ARRAY_BUFFER, new_buffer);
        glBufferData(GL_ARRAY_BUFFER, sizeof(IndexEntry) * new_index_size, 0, GL_DYNAMIC_READ);
        if (glGetError() == GL_OUT_OF_MEMORY) {
            glDeleteBuffers(1, &new_buffer);
            glBindBuffer(GL_ARRAY_BUFFER, 0);
            std::cerr << "out of gpu mem" << std::endl;
            ;
            return -1;
        }
        if (atlas._nglyphs) {
            glBindBuffer(GL_COPY_READ_BUFFER, atlas._index_buffer);
            glCopyBufferSubData(GL_COPY_READ_BUFFER, GL_ARRAY_BUFFER, 0, 0,
                                atlas._nglyphs * sizeof(IndexEntry));
            glBindBuffer(GL_COPY_READ_BUFFER, 0);
        }
        atlas._nallocated = new_index_size;
        glDeleteBuffers(1, &atlas._index_buffer);
        atlas._index_buffer = new_buffer;
    }
    const size_t index_size = nrender * sizeof(IndexEntry);
    glBufferSubData(GL_ARRAY_BUFFER, sizeof(IndexEntry) * atlas._nglyphs, index_size, atlas_index.get());

    glBindBuffer(GL_ARRAY_BUFFER, 0);

    /* Link sampler textures to the buffers. */
    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_BUFFER, _meta_input_texture);
    glTexBuffer(GL_TEXTURE_BUFFER, GL_R8UI, _meta_input_buffer);
    glBindTexture(GL_TEXTURE_BUFFER, 0);

    glActiveTexture(GL_TEXTURE1);
    glBindTexture(GL_TEXTURE_BUFFER, _point_input_texture);
    glTexBuffer(GL_TEXTURE_BUFFER, GL_R32F, _point_input_buffer);
    glBindTexture(GL_TEXTURE_BUFFER, 0);

    glActiveTexture(GL_TEXTURE2);
    glBindTexture(GL_TEXTURE_BUFFER, atlas._index_texture);
    glTexBuffer(GL_TEXTURE_BUFFER, GL_R32F, atlas._index_buffer);
    glBindTexture(GL_TEXTURE_BUFFER, 0);

    glActiveTexture(GL_TEXTURE0);

    /* Generate the atlas texture and bind it as the framebuffer. */
    if (atlas._texture_height == new_texture_height) {
        /* No need to extend the texture. */
        glBindFramebuffer(GL_DRAW_FRAMEBUFFER, atlas._atlas_framebuffer);
        glBindTexture(GL_TEXTURE_2D, atlas._atlas_texture);
        glViewport(0, 0, atlas._texture_width, atlas._texture_height);
    } else {
        GLuint new_texture;
        GLuint new_framebuffer;
        glGenTextures(1, &new_texture);
        glGenFramebuffers(1, &new_framebuffer);
        glBindFramebuffer(GL_DRAW_FRAMEBUFFER, new_framebuffer);

        glBindTexture(GL_TEXTURE_2D, new_texture);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

        glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA32F, atlas._texture_width, new_texture_height, 0, GL_RGBA,
                     GL_FLOAT, NULL);

        if (glGetError() == GL_OUT_OF_MEMORY) {
            /* Buffer size too big, are you trying to type Klingon? */
            glBindFramebuffer(GL_FRAMEBUFFER, 0);
            glDeleteFramebuffers(1, &new_framebuffer);
            glDeleteTextures(1, &new_texture);
            std::cerr << "out of gpu mem" << std::endl;
            return -1;
        }

        glFramebufferTexture2D(GL_DRAW_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, new_texture, 0);
        glViewport(0, 0, atlas._texture_width, new_texture_height);
        glClearColor(0.0, 0.0, 0.0, 1.0);
        glClear(GL_COLOR_BUFFER_BIT);

        if (atlas._texture_height) {
            /* Old texture had data -> copy. */
            glBindFramebuffer(GL_READ_FRAMEBUFFER, atlas._atlas_framebuffer);
            glBlitFramebuffer(0, 0, atlas._texture_width, atlas._texture_height, 0, 0, atlas._texture_width,
                              atlas._texture_height, GL_COLOR_BUFFER_BIT, GL_NEAREST);
            glBindFramebuffer(GL_READ_FRAMEBUFFER, 0);
        }

        glBindFramebuffer(GL_READ_FRAMEBUFFER, 0);
        atlas._texture_height = new_texture_height;
        glDeleteTextures(1, &atlas._atlas_texture);
        atlas._atlas_texture = new_texture;
        glDeleteFramebuffers(1, &atlas._atlas_framebuffer);
        atlas._atlas_framebuffer = new_framebuffer;
    }
    glBindTexture(GL_TEXTURE_2D, 0);

    GLfloat framebuffer_projection[4][4];
    ortho(0, (GLfloat)atlas._texture_width, 0, (GLfloat)atlas._texture_height, -1.0, 1.0,
          framebuffer_projection);
    ortho(-(GLfloat)atlas._texture_width, (GLfloat)atlas._texture_width, -(GLfloat)atlas._texture_height,
          (GLfloat)atlas._texture_height, -1.0, 1.0, atlas._projection);

    glUseProgram(ctx._gen_shader);
    glUniform1i(ctx._uniforms.metadata, 0);
    glUniform1i(ctx._uniforms.point_data, 1);

    glUniformMatrix4fv(ctx._uniforms.atlas_projection, 1, GL_FALSE, (GLfloat *)framebuffer_projection);

    glUniform2f(ctx._uniforms.scale, _scale, _scale);
    glUniform1f(ctx._uniforms.range, _range);
    glUniform1i(ctx._uniforms.meta_offset, 0);
    glUniform1i(ctx._uniforms.point_offset, 0);

    if (glCheckFramebufferStatus(GL_FRAMEBUFFER) != GL_FRAMEBUFFER_COMPLETE)
        fprintf(stderr, "msdfgl: framebuffer incomplete: %x\n", glCheckFramebufferStatus(GL_FRAMEBUFFER));

    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_BUFFER, _meta_input_texture);

    glActiveTexture(GL_TEXTURE1);
    glBindTexture(GL_TEXTURE_BUFFER, _point_input_texture);

    glBindVertexArray(ctx._bbox_vao);
    glBindBuffer(GL_ARRAY_BUFFER, ctx._bbox_vbo);

    glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 2 * sizeof(GLfloat), 0);
    glEnableVertexAttribArray(0);

    int meta_offset = 0;
    int point_offset = 0;
    for (int i = 0; i < nrender; ++i) {
        IndexEntry g = atlas_index[i];
        float w = g.size_x;
        float h = g.size_y;
        GLfloat bounding_box[] = {0, 0, w, 0, 0, h, 0, h, w, 0, w, h};
        glBufferSubData(GL_ARRAY_BUFFER, 0, sizeof(bounding_box), bounding_box);

        glUniform2f(ctx._uniforms.translate, -g.bearing_x / SERIALIZER_SCALE + _range / 2.0f,
                    (g.glyph_height - g.bearing_y) / SERIALIZER_SCALE + _range / 2.0f);

        glUniform2f(ctx._uniforms.texture_offset, g.offset_x, g.offset_y);
        glUniform1i(ctx._uniforms.meta_offset, meta_offset);
        glUniform1i(ctx._uniforms.point_offset, point_offset / (2 * sizeof(GLfloat)));
        glUniform1f(ctx._uniforms.glyph_height, g.size_y);

        /* No need for draw call if there are no contours */
        if (((unsigned char *)metadata.get())[meta_offset])
            glDrawArrays(GL_TRIANGLES, 0, 6);

        meta_offset += meta_sizes[i];
        point_offset += point_sizes[i];
    }

    glDisableVertexAttribArray(0);

    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_BUFFER, 0);

    glActiveTexture(GL_TEXTURE1);
    glBindTexture(GL_TEXTURE_BUFFER, 0);

    glUseProgram(0);

    glBindFramebuffer(GL_FRAMEBUFFER, 0);

    atlas._nglyphs += nrender;
    retval = nrender;

    glViewport(original_viewport[0], original_viewport[1], original_viewport[2], original_viewport[3]);

    return retval;
}
int Font::generate_ascii(const Context &ctx, Atlas &atlas) noexcept {
    std::vector<int> keys(0xff);
    std::iota(keys.begin(), keys.end(), 0);
    return generate_glyphs(ctx, atlas, keys);
}
void Font::render(const Context &ctx, Atlas &atlas, std::vector<Glyph> glyphs,
                  const float *projection) noexcept {
    for (int i = 0; i < glyphs.size(); ++i) {
        MapItem *e = _character_index.get(glyphs.at(i).codepoint);
        glyphs.at(i).codepoint = e->codepoint_index;
    }

    GLuint glyph_buffer;
    GLuint vao;
    glGenBuffers(1, &glyph_buffer);
    glGenVertexArrays(1, &vao);
    glBindVertexArray(vao);
    glBindBuffer(GL_ARRAY_BUFFER, glyph_buffer);
    glBufferData(GL_ARRAY_BUFFER, glyphs.size() * sizeof(Glyph), &glyphs[0], GL_DYNAMIC_DRAW);

    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, sizeof(Glyph), (void *)offsetof(Glyph, x));

    glEnableVertexAttribArray(1);
    glVertexAttribIPointer(1, 4, GL_UNSIGNED_BYTE, sizeof(Glyph), (void *)offsetof(Glyph, color));

    glEnableVertexAttribArray(2);
    glVertexAttribIPointer(2, 1, GL_INT, sizeof(Glyph), (void *)offsetof(Glyph, codepoint));

    glEnableVertexAttribArray(3);
    glVertexAttribPointer(3, 1, GL_FLOAT, GL_FALSE, sizeof(Glyph), (void *)offsetof(Glyph, size));

    glEnableVertexAttribArray(4);
    glVertexAttribPointer(4, 1, GL_FLOAT, GL_FALSE, sizeof(Glyph), (void *)offsetof(Glyph, offset));

    glEnableVertexAttribArray(5);
    glVertexAttribPointer(5, 1, GL_FLOAT, GL_FALSE, sizeof(Glyph), (void *)offsetof(Glyph, skew));

    glEnableVertexAttribArray(6);
    glVertexAttribPointer(6, 1, GL_FLOAT, GL_FALSE, sizeof(Glyph), (void *)offsetof(Glyph, strength));

    glUseProgram(ctx._render_shader);

    /* Bind atlas texture and index buffer. */
    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, atlas._atlas_texture);
    glUniform1i(ctx._uniforms.atlas, 0);

    glActiveTexture(GL_TEXTURE1);
    glBindTexture(GL_TEXTURE_BUFFER, atlas._index_texture);
    glUniform1i(ctx._uniforms.index, 1);

    glUniformMatrix4fv(ctx._uniforms.font_atlas_projection, 1, GL_FALSE, (GLfloat *)atlas._projection);

    glUniformMatrix4fv(ctx._uniforms.window_projection, 1, GL_FALSE, projection);
    glUniform1f(ctx._uniforms.padding, (GLfloat)(_range / 2.0 * SERIALIZER_SCALE));
    glUniform1f(ctx._uniforms.units_per_em, (GLfloat)_face->units_per_EM);
    glUniform2fv(ctx._uniforms.dpi, 1, ctx._dpi);

    /* Render the glyphs. */
    glDrawArrays(GL_POINTS, 0, glyphs.size());

    /* Clean up. */
    glActiveTexture(GL_TEXTURE1);
    glBindTexture(GL_TEXTURE_BUFFER, 0);

    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, 0);

    glUseProgram(0);

    glDisableVertexAttribArray(0);
    glDisableVertexAttribArray(1);
    glDisableVertexAttribArray(2);
    glDisableVertexAttribArray(3);
    glDisableVertexAttribArray(4);
    glDisableVertexAttribArray(5);
    glDisableVertexAttribArray(6);

    glBindBuffer(GL_ARRAY_BUFFER, 0);
    glBindVertexArray(0);
    glDeleteBuffers(1, &glyph_buffer);
    glDeleteVertexArrays(1, &vao);
}
[[nodiscard]] std::vector<Glyph> Font::print_unicode(Context &ctx, Atlas &atlas,
                                                     const std::u32string_view unicode_string, const float x,
                                                     const float y, const long color, const float size,
                                                     const bool enable_kerning, const float offset,
                                                     const float skew, const float strength) noexcept {
    std::vector<Glyph> result;
    float x0 = x;
    for (size_t i = 0; i < unicode_string.size(); i++) {
        const auto &character = unicode_string.at(i);
        result.push_back({});
        auto &element = result.at(result.size() - 1);
        element.x = x0;
        element.y = y;
        element.color = color;
        element.codepoint = unicode_string.at(i);
        element.size = size;
        element.offset = offset;
        element.skew = skew;
        element.strength = strength;

        auto idx = internal::map_get_or_add(ctx, *this, atlas, character);

        FT_Vector kerning{0};
        const bool should_get_kerning = enable_kerning and FT_HAS_KERNING(_face) and (i > 0);
        if (should_get_kerning) {
            const auto &previous_character = unicode_string.at(i - 1);
            FT_Get_Kerning(_face, FT_Get_Char_Index(_face, previous_character),
                           FT_Get_Char_Index(_face, character), FT_KERNING_UNSCALED, &kerning);
        }

        x0 += (idx->advance[0] + kerning.x) * (size * ctx._dpi[0] / 72.0f) / _face->units_per_EM;
    }
    return result;
}
} // namespace msdf

namespace msdf::internal {
const GLfloat _MAT4_ZERO_INIT[4][4] = {
    {0.0f, 0.0f, 0.0f, 0.0f}, {0.0f, 0.0f, 0.0f, 0.0f}, {0.0f, 0.0f, 0.0f, 0.0f}, {0.0f, 0.0f, 0.0f, 0.0f}};
bool compile_shader(const char *source, GLenum type, GLuint *shader, const char *version) {
    /* Default to version */
    if (!version)
        version = "330 core";

    *shader = glCreateShader(type);
    if (!*shader) {
        fprintf(stderr, "failed to create shader\n");
    }

    const char *src[] = {"#version ", version, "\n", source};

    glShaderSource(*shader, 4, src, NULL);
    glCompileShader(*shader);

    GLint status;
    glGetShaderiv(*shader, GL_COMPILE_STATUS, &status);
    if (!status) {
        char log[1024];
        GLsizei len;
        glGetShaderInfoLog(*shader, 1024, &len, log);
        fprintf(stderr, "Error: compiling: %*s\n", len, log);
        return false;
    }

    return true;
}

MapItem *map_get_or_add(Context &ctx, Font &font, Atlas &atlas, int32_t key) {
    if (not font._character_index.in(key)) {
        font.generate_glyphs(ctx, atlas, {key});
    }
    return font._character_index.get(key);
}
} // namespace msdf::internal
