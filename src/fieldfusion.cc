#include "fieldfusion.hh"

#include <GL/glew.h>

#include <cassert>
#include <cstdint>
#include <memory>
#include <numeric>

#include "fferror.hh"
#include "ffmap.hh"
#include "ffserializer.hh"
#include "ffshaders.hh"
#include "freetype/freetype.h"
#include "freetype/ftimage.h"

namespace ff {
namespace internal {
const extern GLfloat kmat4_zero_init[4][4];
bool CompileShader(const char *source, GLenum type, GLuint *shader, const char *version);
struct UseFree {
    void operator()(void *x) { free(x); }
};

}  // namespace internal

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

void Ortho(float left, float right, float bottom, float top, float nearVal, float farVal, float dest[][4]) {
    GLfloat rl, tb, fn;

    memcpy(dest, internal::kmat4_zero_init, sizeof(internal::kmat4_zero_init));

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

[[nodiscard]] Result<void> FieldFusion::Init(const char *version) noexcept {
    FT_Error error = FT_Init_FreeType(&ft_library_);
    if (error != 0) return Error::FtInitializationFail;

    glGetIntegerv(GL_MAX_TEXTURE_SIZE, &max_texture_size_);

    unsigned vertex_shader, geometry_shader, fragment_shader;
    if (!internal::CompileShader(kmsdf_vertex, GL_VERTEX_SHADER, &vertex_shader, version))
        return Error::MsdfVertexShaderCompileFail;
    if (!internal::CompileShader(kmsdf_fragment, GL_FRAGMENT_SHADER, &fragment_shader, version))
        return Error::MsdfFragmentShaderCompileFail;
    if (!(gen_shader_ = glCreateProgram())) return Error::ShaderLinkageFail;

    glAttachShader(gen_shader_, vertex_shader);
    glAttachShader(gen_shader_, fragment_shader);
    glLinkProgram(gen_shader_);
    glDeleteShader(vertex_shader);
    glDeleteShader(fragment_shader);

    int status;
    glGetProgramiv(gen_shader_, GL_LINK_STATUS, &status);
    if (!status) return Error::ShaderLinkageFail;

    uniforms_.atlas_projection = glGetUniformLocation(gen_shader_, "projection");
    uniforms_.texture_offset = glGetUniformLocation(gen_shader_, "offset");
    uniforms_.glyph_height = glGetUniformLocation(gen_shader_, "glyph_height");
    uniforms_.translate = glGetUniformLocation(gen_shader_, "translate");
    uniforms_.scale = glGetUniformLocation(gen_shader_, "scale");
    uniforms_.range = glGetUniformLocation(gen_shader_, "range");
    uniforms_.meta_offset = glGetUniformLocation(gen_shader_, "meta_offset");
    uniforms_.point_offset = glGetUniformLocation(gen_shader_, "point_offset");
    uniforms_.metadata = glGetUniformLocation(gen_shader_, "metadata");
    uniforms_.point_data = glGetUniformLocation(gen_shader_, "point_data");

    if (!internal::CompileShader(kfont_vertex, GL_VERTEX_SHADER, &vertex_shader, version))
        return Error::MsdfVertexShaderCompileFail;
    if (!internal::CompileShader(kfont_geometry, GL_GEOMETRY_SHADER, &geometry_shader, version))
        return Error::FontGeometryShaderCompileFail;
    if (!internal::CompileShader(kfont_fragment, GL_FRAGMENT_SHADER, &fragment_shader, version))
        return Error::FontFragmentShaderCompileFail;
    if (!(render_shader_ = glCreateProgram())) return Error::ShaderLinkageFail;

    glAttachShader(render_shader_, vertex_shader);
    glAttachShader(render_shader_, geometry_shader);
    glAttachShader(render_shader_, fragment_shader);
    glLinkProgram(render_shader_);
    glDeleteShader(vertex_shader);
    glDeleteShader(geometry_shader);
    glDeleteShader(fragment_shader);

    glGetProgramiv(render_shader_, GL_LINK_STATUS, &status);
    if (!status) {
        glDeleteProgram(gen_shader_);
        return Error::ShaderLinkageFail;
    }

    uniforms_.window_projection = glGetUniformLocation(render_shader_, "projection");
    uniforms_.font_atlas_projection = glGetUniformLocation(render_shader_, "font_projection");
    uniforms_.index = glGetUniformLocation(render_shader_, "font_index");
    uniforms_.atlas = glGetUniformLocation(render_shader_, "font_atlas");
    uniforms_.padding = glGetUniformLocation(render_shader_, "padding");
    uniforms_.dpi = glGetUniformLocation(render_shader_, "dpi");
    uniforms_.units_per_em = glGetUniformLocation(render_shader_, "units_per_em");
    dpi_[0] = 72.0;
    dpi_[1] = 72.0;

    glGenVertexArrays(1, &bbox_vao_);
    glGenBuffers(1, &bbox_vbo_);
    glBindBuffer(GL_ARRAY_BUFFER, bbox_vbo_);
    glBufferData(GL_ARRAY_BUFFER, 12 * sizeof(float), 0, GL_STREAM_READ);
    glBindBuffer(GL_ARRAY_BUFFER, 0);

    return {};
}

[[nodiscard]] Result<size_t> FieldFusion::NewFont(Atlas &a, const char *path, const float scale,
                                                  const float range) noexcept {
    fonts_.push_back({});
    size_t handle = fonts_.size() - 1;
    auto &font = fonts_.at(handle);
    font.font_path = path;
    font.scale = scale;
    font.range = range;

    if (FT_New_Face(ft_library_, path, 0, &font.face)) return Error::FtFaceInitializationFail;
    FT_Select_Charmap(font.face, ft_encoding_unicode);
    font.vertical_advance = (float)(font.face->ascender - font.face->descender);

    glGenBuffers(1, &font.meta_input_buffer);
    glGenBuffers(1, &font.point_input_buffer);
    glGenTextures(1, &font.meta_input_texture);
    glGenTextures(1, &font.point_input_texture);

    auto success = GenAscii(a, font);
    if (not success) {
        return success.error_;
    }

    return handle;
}

[[nodiscard]] Atlas FieldFusion::NewAtlas(const int texture_width, const int padding) noexcept {
    Atlas a;
    a.texture_width = texture_width;
    a.padding = padding;
    glGenBuffers(1, &a.index_buffer);
    glGenTextures(1, &a.index_texture);
    glGenTextures(1, &a.atlas_texture);
    glGenFramebuffers(1, &a.atlas_framebuffer);
    return a;
}

[[nodiscard]] Result<void> FieldFusion::GenAscii(Atlas &a, Font &f) noexcept {
    std::vector<int32_t> codepoints(0xff);
    std::iota(codepoints.begin(), codepoints.end(), 0);
    return GenGlyphs(a, f, codepoints);
}

[[nodiscard]] Result<void> FieldFusion::GenGlyphs(Atlas &a, Font &f,
                                                  const std::vector<int32_t> &codepoints) noexcept {
    GLint original_viewport[4];
    glGetIntegerv(GL_VIEWPORT, original_viewport);
    int nrender = codepoints.size();

    if (nrender <= 0) return {};

    /* Calculate the amount of memory needed on the GPU.*/
    std::unique_ptr<size_t[]> meta_sizes(new size_t[nrender]());
    std::unique_ptr<size_t[]> point_sizes(new size_t[nrender]());

    /* We will start with a square texture. */
    int new_texture_height = a.texture_height ? a.texture_height : 1;
    int new_index_size = a.nallocated ? a.nallocated : 1;

    /* Amount of new memory needed for the index. */
    std::unique_ptr<IndexEntry[]> atlas_index(new IndexEntry[nrender]());

    size_t meta_size_sum = 0, point_size_sum = 0;
    for (size_t i = 0; (int)i < (int)nrender; ++i) {  // MARK
        int code = codepoints[i];
        auto res = GlyphBufferSize(f.face, code, &meta_sizes[i], &point_sizes[i]);
        if (not res) return res.error_;

        meta_size_sum += meta_sizes[i];
        point_size_sum += point_sizes[i];
    }

    /* Allocate the calculated amount. */
    std::unique_ptr<char[], internal::UseFree> point_data((char *)calloc(point_size_sum, 1));
    std::unique_ptr<char[], internal::UseFree> metadata((char *)calloc(meta_size_sum, 1));

    /* Serialize the glyphs into RAM. */
    char *meta_ptr = metadata.get();
    char *point_ptr = point_data.get();
    for (size_t i = 0; (int)i < (int)nrender; ++i) {
        float buffer_width, buffer_height;

        int code = codepoints[i];
        auto res = SerializeGlyph(f.face, code, meta_ptr, (GLfloat *)point_ptr);
        if (not res) return res.error_;

        auto m = f.character_index.insert(code);
        m.get().advance[0] = (float)f.face->glyph->metrics.horiAdvance;
        m.get().advance[1] = (float)f.face->glyph->metrics.vertAdvance;

        buffer_width = f.face->glyph->metrics.width / kserializer_scale + f.range;
        buffer_height = f.face->glyph->metrics.height / kserializer_scale + f.range;
        buffer_width *= f.scale;
        buffer_height *= f.scale;

        meta_ptr += meta_sizes[i];
        point_ptr += point_sizes[i];

        if (a.offset_x + buffer_width > a.texture_width) {
            a.offset_y += (a.y_increment + a.padding);
            a.offset_x = 1;
            a.y_increment = 0;
        }
        a.y_increment = (size_t)buffer_height > a.y_increment ? (size_t)buffer_height : a.y_increment;

        atlas_index[i].offset_x = a.offset_x;
        atlas_index[i].offset_y = a.offset_y;
        atlas_index[i].size_x = buffer_width;
        atlas_index[i].size_y = buffer_height;
        atlas_index[i].bearing_x = f.face->glyph->metrics.horiBearingX;
        atlas_index[i].bearing_y = f.face->glyph->metrics.horiBearingY;
        atlas_index[i].glyph_width = f.face->glyph->metrics.width;
        atlas_index[i].glyph_height = f.face->glyph->metrics.height;

        a.offset_x += (size_t)buffer_width + a.padding;

        while ((a.offset_y + buffer_height) > new_texture_height) {
            new_texture_height *= 2;
        }
        if (new_texture_height > max_texture_size_) return Error::ExceededMaxTextureSize;

        while ((int)(a.nglyphs + i) >= new_index_size) {
            new_index_size *= 2;
        }
    }

    /* Allocate and fill the buffers on GPU. */
    glBindBuffer(GL_ARRAY_BUFFER, f.meta_input_buffer);
    glBufferData(GL_ARRAY_BUFFER, meta_size_sum, metadata.get(), GL_DYNAMIC_READ);

    glBindBuffer(GL_ARRAY_BUFFER, f.point_input_buffer);
    glBufferData(GL_ARRAY_BUFFER, point_size_sum, point_data.get(), GL_DYNAMIC_READ);

    if ((int)a.nallocated == new_index_size) {
        glBindBuffer(GL_ARRAY_BUFFER, a.index_buffer);
    } else {
        GLuint new_buffer;
        glGenBuffers(1, &new_buffer);
        glBindBuffer(GL_ARRAY_BUFFER, new_buffer);
        glBufferData(GL_ARRAY_BUFFER, sizeof(IndexEntry) * new_index_size, 0, GL_DYNAMIC_READ);
        if (glGetError() == GL_OUT_OF_MEMORY) {
            glDeleteBuffers(1, &new_buffer);
            glBindBuffer(GL_ARRAY_BUFFER, 0);
            return Error::OutOfGpuMemory;
        }
        if (a.nglyphs) {
            glBindBuffer(GL_COPY_READ_BUFFER, a.index_buffer);
            glCopyBufferSubData(GL_COPY_READ_BUFFER, GL_ARRAY_BUFFER, 0, 0, a.nglyphs * sizeof(IndexEntry));
            glBindBuffer(GL_COPY_READ_BUFFER, 0);
        }
        a.nallocated = new_index_size;
        glDeleteBuffers(1, &a.index_buffer);
        a.index_buffer = new_buffer;
    }
    const size_t index_size = nrender * sizeof(IndexEntry);
    glBufferSubData(GL_ARRAY_BUFFER, sizeof(IndexEntry) * a.nglyphs, index_size, atlas_index.get());

    glBindBuffer(GL_ARRAY_BUFFER, 0);

    /* Link sampler textures to the buffers. */
    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_BUFFER, f.meta_input_texture);
    glTexBuffer(GL_TEXTURE_BUFFER, GL_R8UI, f.meta_input_buffer);
    glBindTexture(GL_TEXTURE_BUFFER, 0);

    glActiveTexture(GL_TEXTURE1);
    glBindTexture(GL_TEXTURE_BUFFER, f.point_input_texture);
    glTexBuffer(GL_TEXTURE_BUFFER, GL_R32F, f.point_input_buffer);
    glBindTexture(GL_TEXTURE_BUFFER, 0);

    glActiveTexture(GL_TEXTURE2);
    glBindTexture(GL_TEXTURE_BUFFER, a.index_texture);
    glTexBuffer(GL_TEXTURE_BUFFER, GL_R32F, a.index_buffer);
    glBindTexture(GL_TEXTURE_BUFFER, 0);

    glActiveTexture(GL_TEXTURE0);

    /* Generate the atlas texture and bind it as the framebuffer. */
    if (a.texture_height == new_texture_height) {
        /* No need to extend the texture. */
        glBindFramebuffer(GL_DRAW_FRAMEBUFFER, a.atlas_framebuffer);
        glBindTexture(GL_TEXTURE_2D, a.atlas_texture);
        glViewport(0, 0, a.texture_width, a.texture_height);
    } else {
        GLuint new_texture;
        GLuint new_framebuffer;
        glGenTextures(1, &new_texture);
        glGenFramebuffers(1, &new_framebuffer);
        glBindFramebuffer(GL_DRAW_FRAMEBUFFER, new_framebuffer);

        glBindTexture(GL_TEXTURE_2D, new_texture);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

        glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA32F, a.texture_width, new_texture_height, 0, GL_RGBA, GL_FLOAT,
                     NULL);

        if (glGetError() == GL_OUT_OF_MEMORY) {
            /* Buffer size too big, are you trying to type Klingon? */
            glBindFramebuffer(GL_FRAMEBUFFER, 0);
            glDeleteFramebuffers(1, &new_framebuffer);
            glDeleteTextures(1, &new_texture);
            return Error::OutOfGpuMemory;
        }

        glFramebufferTexture2D(GL_DRAW_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, new_texture, 0);
        glViewport(0, 0, a.texture_width, new_texture_height);
        glClearColor(0.0, 0.0, 0.0, 1.0);
        glClear(GL_COLOR_BUFFER_BIT);

        if (a.texture_height) {
            /* Old texture had data -> copy. */
            glBindFramebuffer(GL_READ_FRAMEBUFFER, a.atlas_framebuffer);
            glBlitFramebuffer(0, 0, a.texture_width, a.texture_height, 0, 0, a.texture_width,
                              a.texture_height, GL_COLOR_BUFFER_BIT, GL_NEAREST);
            glBindFramebuffer(GL_READ_FRAMEBUFFER, 0);
        }

        glBindFramebuffer(GL_READ_FRAMEBUFFER, 0);
        a.texture_height = new_texture_height;
        glDeleteTextures(1, &a.atlas_texture);
        a.atlas_texture = new_texture;
        glDeleteFramebuffers(1, &a.atlas_framebuffer);
        a.atlas_framebuffer = new_framebuffer;
    }
    glBindTexture(GL_TEXTURE_2D, 0);

    GLfloat framebuffer_projection[4][4];
    Ortho(0, (GLfloat)a.texture_width, 0, (GLfloat)a.texture_height, -1.0, 1.0, framebuffer_projection);
    Ortho(-(GLfloat)a.texture_width, (GLfloat)a.texture_width, -(GLfloat)a.texture_height,
          (GLfloat)a.texture_height, -1.0, 1.0, a.projection);

    glUseProgram(gen_shader_);
    glUniform1i(uniforms_.metadata, 0);
    glUniform1i(uniforms_.point_data, 1);

    glUniformMatrix4fv(uniforms_.atlas_projection, 1, GL_FALSE, (GLfloat *)framebuffer_projection);

    glUniform2f(uniforms_.scale, f.scale, f.scale);
    glUniform1f(uniforms_.range, f.range);
    glUniform1i(uniforms_.meta_offset, 0);
    glUniform1i(uniforms_.point_offset, 0);

    if (glCheckFramebufferStatus(GL_FRAMEBUFFER) != GL_FRAMEBUFFER_COMPLETE)
        fprintf(stderr, "msdfgl: framebuffer incomplete: %x\n", glCheckFramebufferStatus(GL_FRAMEBUFFER));

    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_BUFFER, f.meta_input_texture);

    glActiveTexture(GL_TEXTURE1);
    glBindTexture(GL_TEXTURE_BUFFER, f.point_input_texture);

    glBindVertexArray(bbox_vao_);
    glBindBuffer(GL_ARRAY_BUFFER, bbox_vbo_);

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

        glUniform2f(uniforms_.translate, -g.bearing_x / kserializer_scale + f.range / 2.0f,
                    (g.glyph_height - g.bearing_y) / kserializer_scale + f.range / 2.0f);

        glUniform2f(uniforms_.texture_offset, g.offset_x, g.offset_y);
        glUniform1i(uniforms_.meta_offset, meta_offset);
        glUniform1i(uniforms_.point_offset, point_offset / (2 * sizeof(GLfloat)));
        glUniform1f(uniforms_.glyph_height, g.size_y);

        /* No need for draw call if there are no contours */
        if (((unsigned char *)metadata.get())[meta_offset]) glDrawArrays(GL_TRIANGLES, 0, 6);

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

    a.nglyphs += nrender;

    glViewport(original_viewport[0], original_viewport[1], original_viewport[2], original_viewport[3]);

    return {};
}

[[nodiscard]] Result<void> FieldFusion::Draw(Atlas &a, Font &f, Glyphs glyphs,
                                             const float *projection) noexcept {
#pragma omp parallel for
    for (int i = 0; i < glyphs.size(); ++i) {
        auto e = f.character_index.at(glyphs.at(i).codepoint);
        if (not e) return Error::GlyphGenerationFail;
        glyphs.at(i).codepoint = e.value().get().codepoint_index;
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

    glUseProgram(render_shader_);

    /* Bind atlas texture and index buffer. */
    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, a.atlas_texture);
    glUniform1i(uniforms_.atlas, 0);

    glActiveTexture(GL_TEXTURE1);
    glBindTexture(GL_TEXTURE_BUFFER, a.index_texture);
    glUniform1i(uniforms_.index, 1);

    glUniformMatrix4fv(uniforms_.font_atlas_projection, 1, GL_FALSE, (GLfloat *)a.projection);

    glUniformMatrix4fv(uniforms_.window_projection, 1, GL_FALSE, projection);
    glUniform1f(uniforms_.padding, (GLfloat)(f.range / 2.0 * kserializer_scale));
    glUniform1f(uniforms_.units_per_em, (GLfloat)f.face->units_per_EM);
    glUniform2fv(uniforms_.dpi, 1, dpi_);

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
    return {};
}

[[nodiscard]] Result<Glyphs> FieldFusion::PrintUnicode(Atlas &a, Font &f, const std::u32string_view buffer,
                                                       const float x, const float y, const long color,
                                                       const float size, const bool enable_kerning,
                                                       const bool print_vertically, const float offset,
                                                       const float skew, const float strength) noexcept {
    std::vector<Glyph> result;
    float x0 = x;
    float y0 = y;
    for (size_t i = 0; i < buffer.size(); i++) {
        const auto &codepoint = (int32_t)buffer.at(i);
        result.push_back({});
        auto &element = result.at(result.size() - 1);
        element.x = x0;
        element.y = y0;
        element.color = color;
        element.codepoint = codepoint;
        element.size = size;
        element.offset = offset;
        element.skew = skew;
        element.strength = strength;

        auto idx = f.character_index.at(codepoint);
        if (not idx.has_value()) {
            auto gen_status = GenGlyphs(a, f, std::vector<int32_t>{codepoint});
            if (not gen_status) return gen_status.error_;
            idx = f.character_index.at(codepoint);
            assert(idx.has_value());
        }

        FT_Vector kerning{0};
        const bool should_get_kerning = enable_kerning and FT_HAS_KERNING(f.face) and (i > 0);
        if (should_get_kerning) {
            const auto &previous_character = buffer.at(i - 1);
            FT_Get_Kerning(f.face, FT_Get_Char_Index(f.face, previous_character),
                           FT_Get_Char_Index(f.face, codepoint), FT_KERNING_UNSCALED, &kerning);
        }

        if (not print_vertically)
            x0 +=
                (idx.value().get().advance[0] + kerning.x) * (size * dpi_[0] / 72.0f) / f.face->units_per_EM;
        else
            y0 +=
                (idx.value().get().advance[1] + kerning.y) * (size * dpi_[0] / 72.0f) / f.face->units_per_EM;
    }
    return result;
}
void FieldFusion::Destroy() noexcept {
    for (auto &font : fonts_) FT_Done_Face(font.face);
    FT_Done_FreeType(ft_library_);
}
}  // namespace ff

namespace ff::internal {
const GLfloat kmat4_zero_init[4][4] = {
    {0.0f, 0.0f, 0.0f, 0.0f}, {0.0f, 0.0f, 0.0f, 0.0f}, {0.0f, 0.0f, 0.0f, 0.0f}, {0.0f, 0.0f, 0.0f, 0.0f}};
bool CompileShader(const char *source, GLenum type, GLuint *shader, const char *version) {
    /* Default to version */
    if (!version) version = "330 core";

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
}  // namespace ff::internal
