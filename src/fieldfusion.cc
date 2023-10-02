#include "fieldfusion.hh"

#include <GL/glew.h>

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

const extern GLfloat _MAT4_ZERO_INIT[4][4];

bool compile_shader(const char *source, GLenum type, GLuint *shader, const char *version);
Result<MapItem *> map_get_or_add(Context &ctx, Font &font, Atlas &atlas, int32_t codepoint);
inline int is_control(int32_t code) { return (code <= 31) || (code >= 128 && code <= 159); };
struct use_free {
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

[[nodiscard]] Result<Context> Context::create(const char *version) noexcept {
    Context result{};
    FT_Error error = FT_Init_FreeType(&result.ft_library_);

    if (error != 0) return Error::FtInitializationFail;

    glGetIntegerv(GL_MAX_TEXTURE_SIZE, &result.max_texture_size_);

    unsigned vertex_shader, geometry_shader, fragment_shader;
    if (!internal::compile_shader(msdf_vertex, GL_VERTEX_SHADER, &vertex_shader, version))
        return Error::MsdfVertexShaderCompileFail;
    if (!internal::compile_shader(msdf_fragment, GL_FRAGMENT_SHADER, &fragment_shader, version))
        return Error::MsdfFragmentShaderCompileFail;
    if (!(result.gen_shader_ = glCreateProgram())) return Error::ShaderLinkageFail;

    glAttachShader(result.gen_shader_, vertex_shader);
    glAttachShader(result.gen_shader_, fragment_shader);
    glLinkProgram(result.gen_shader_);
    glDeleteShader(vertex_shader);
    glDeleteShader(fragment_shader);

    int status;
    glGetProgramiv(result.gen_shader_, GL_LINK_STATUS, &status);
    if (!status) return Error::ShaderLinkageFail;

    result.uniforms_.atlas_projection = glGetUniformLocation(result.gen_shader_, "projection");
    result.uniforms_.texture_offset = glGetUniformLocation(result.gen_shader_, "offset");
    result.uniforms_.glyph_height = glGetUniformLocation(result.gen_shader_, "glyph_height");
    result.uniforms_.translate = glGetUniformLocation(result.gen_shader_, "translate");
    result.uniforms_.scale = glGetUniformLocation(result.gen_shader_, "scale");
    result.uniforms_.range = glGetUniformLocation(result.gen_shader_, "range");
    result.uniforms_.meta_offset = glGetUniformLocation(result.gen_shader_, "meta_offset");
    result.uniforms_.point_offset = glGetUniformLocation(result.gen_shader_, "point_offset");
    result.uniforms_.metadata = glGetUniformLocation(result.gen_shader_, "metadata");
    result.uniforms_.point_data = glGetUniformLocation(result.gen_shader_, "point_data");

    if (!internal::compile_shader(font_vertex, GL_VERTEX_SHADER, &vertex_shader, version))
        return Error::MsdfVertexShaderCompileFail;
    if (!internal::compile_shader(font_geometry, GL_GEOMETRY_SHADER, &geometry_shader, version))
        return Error::FontGeometryShaderCompileFail;
    if (!internal::compile_shader(font_fragment, GL_FRAGMENT_SHADER, &fragment_shader, version))
        return Error::FontFragmentShaderCompileFail;
    if (!(result.render_shader_ = glCreateProgram())) return Error::ShaderLinkageFail;

    glAttachShader(result.render_shader_, vertex_shader);
    glAttachShader(result.render_shader_, geometry_shader);
    glAttachShader(result.render_shader_, fragment_shader);
    glLinkProgram(result.render_shader_);
    glDeleteShader(vertex_shader);
    glDeleteShader(geometry_shader);
    glDeleteShader(fragment_shader);

    glGetProgramiv(result.render_shader_, GL_LINK_STATUS, &status);
    if (!status) {
        glDeleteProgram(result.gen_shader_);
        return Error::ShaderLinkageFail;
    }

    result.uniforms_.window_projection = glGetUniformLocation(result.render_shader_, "projection");
    result.uniforms_.font_atlas_projection = glGetUniformLocation(result.render_shader_, "font_projection");
    result.uniforms_.index = glGetUniformLocation(result.render_shader_, "font_index");
    result.uniforms_.atlas = glGetUniformLocation(result.render_shader_, "font_atlas");
    result.uniforms_.padding = glGetUniformLocation(result.render_shader_, "padding");
    result.uniforms_.dpi = glGetUniformLocation(result.render_shader_, "dpi");
    result.uniforms_.units_per_em = glGetUniformLocation(result.render_shader_, "units_per_em");
    result.dpi_[0] = 72.0;
    result.dpi_[1] = 72.0;

    glGenVertexArrays(1, &result.bbox_vao_);
    glGenBuffers(1, &result.bbox_vbo_);
    glBindBuffer(GL_ARRAY_BUFFER, result.bbox_vbo_);
    glBufferData(GL_ARRAY_BUFFER, 12 * sizeof(float), 0, GL_STREAM_READ);
    glBindBuffer(GL_ARRAY_BUFFER, 0);

    return result;
}

[[nodiscard]] Result<void> Font::init_face(const Context &ctx) noexcept {
    if (FT_New_Face(ctx.ft_library_, font_path_, 0, &face_)) return Error::FtFaceInitializationFail;
    FT_Select_Charmap(face_, ft_encoding_unicode);
    vertical_advance_ = (float)(face_->ascender - face_->descender);
    glGenBuffers(1, &meta_input_buffer_);
    glGenBuffers(1, &point_input_buffer_);
    glGenTextures(1, &meta_input_texture_);
    glGenTextures(1, &point_input_texture_);
    return {};
}

void Font::init_textures() noexcept {
    glGenBuffers(1, &meta_input_buffer_);
    glGenBuffers(1, &point_input_buffer_);
    glGenTextures(1, &meta_input_texture_);
    glGenTextures(1, &point_input_texture_);
}

void Atlas::init_textures() noexcept {
    glGenBuffers(1, &index_buffer_);
    glGenTextures(1, &index_texture_);
    glGenTextures(1, &atlas_texture_);
    glGenFramebuffers(1, &atlas_framebuffer_);
}

Result<void> Font::generate_glyphs(const Context &ctx, Atlas &atlas,
                                   const std::vector<int> &codepoint) noexcept {
    GLint original_viewport[4];
    glGetIntegerv(GL_VIEWPORT, original_viewport);
    int nrender = codepoint.size();

    if (nrender <= 0) return {};

    /* Calculate the amount of memory needed on the GPU.*/
    std::unique_ptr<size_t[]> meta_sizes(new size_t[nrender]());
    std::unique_ptr<size_t[]> point_sizes(new size_t[nrender]());

    /* We will start with a square texture. */
    int new_texture_height = atlas.texture_height_ ? atlas.texture_height_ : 1;
    int new_index_size = atlas.nallocated_ ? atlas.nallocated_ : 1;

    /* Amount of new memory needed for the index. */
    std::unique_ptr<IndexEntry[]> atlas_index(new IndexEntry[nrender]());

    size_t meta_size_sum = 0, point_size_sum = 0;
    for (size_t i = 0; (int)i < (int)nrender; ++i) {  // MARK
        int code = codepoint[i];
        auto res = glyph_buffer_size(face_, code, &meta_sizes[i], &point_sizes[i]);
        if (not res) return res.error_;

        meta_size_sum += meta_sizes[i];
        point_size_sum += point_sizes[i];
    }

    /* Allocate the calculated amount. */
    std::unique_ptr<char[], internal::use_free> point_data((char *)calloc(point_size_sum, 1));
    std::unique_ptr<char[], internal::use_free> metadata((char *)calloc(meta_size_sum, 1));

    /* Serialize the glyphs into RAM. */
    char *meta_ptr = metadata.get();
    char *point_ptr = point_data.get();
    for (size_t i = 0; (int)i < (int)nrender; ++i) {
        float buffer_width, buffer_height;

        int code = codepoint[i];
        auto res = serialize_glyph(face_, code, meta_ptr, (GLfloat *)point_ptr);
        if (not res) return res.error_;

        MapItem *m = character_index_.insert(code);
        m->advance[0] = (float)face_->glyph->metrics.horiAdvance;
        m->advance[1] = (float)face_->glyph->metrics.vertAdvance;

        buffer_width = face_->glyph->metrics.width / kserializer_scale + range_;
        buffer_height = face_->glyph->metrics.height / kserializer_scale + range_;
        buffer_width *= scale_;
        buffer_height *= scale_;

        meta_ptr += meta_sizes[i];
        point_ptr += point_sizes[i];

        if (atlas.offset_x_ + buffer_width > atlas.texture_width_) {
            atlas.offset_y_ += (atlas.y_increment_ + atlas.padding_);
            atlas.offset_x_ = 1;
            atlas.y_increment_ = 0;
        }
        atlas.y_increment_ =
            (size_t)buffer_height > atlas.y_increment_ ? (size_t)buffer_height : atlas.y_increment_;

        atlas_index[i].offset_x = atlas.offset_x_;
        atlas_index[i].offset_y = atlas.offset_y_;
        atlas_index[i].size_x = buffer_width;
        atlas_index[i].size_y = buffer_height;
        atlas_index[i].bearing_x = face_->glyph->metrics.horiBearingX;
        atlas_index[i].bearing_y = face_->glyph->metrics.horiBearingY;
        atlas_index[i].glyph_width = face_->glyph->metrics.width;
        atlas_index[i].glyph_height = face_->glyph->metrics.height;

        atlas.offset_x_ += (size_t)buffer_width + atlas.padding_;

        while ((atlas.offset_y_ + buffer_height) > new_texture_height) {
            new_texture_height *= 2;
        }
        if (new_texture_height > ctx.max_texture_size_) return Error::ExceededMaxTextureSize;

        while ((int)(atlas.nglyphs_ + i) >= new_index_size) {
            new_index_size *= 2;
        }
    }

    /* Allocate and fill the buffers on GPU. */
    glBindBuffer(GL_ARRAY_BUFFER, meta_input_buffer_);
    glBufferData(GL_ARRAY_BUFFER, meta_size_sum, metadata.get(), GL_DYNAMIC_READ);

    glBindBuffer(GL_ARRAY_BUFFER, point_input_buffer_);
    glBufferData(GL_ARRAY_BUFFER, point_size_sum, point_data.get(), GL_DYNAMIC_READ);

    if ((int)atlas.nallocated_ == new_index_size) {
        glBindBuffer(GL_ARRAY_BUFFER, atlas.index_buffer_);
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
        if (atlas.nglyphs_) {
            glBindBuffer(GL_COPY_READ_BUFFER, atlas.index_buffer_);
            glCopyBufferSubData(GL_COPY_READ_BUFFER, GL_ARRAY_BUFFER, 0, 0,
                                atlas.nglyphs_ * sizeof(IndexEntry));
            glBindBuffer(GL_COPY_READ_BUFFER, 0);
        }
        atlas.nallocated_ = new_index_size;
        glDeleteBuffers(1, &atlas.index_buffer_);
        atlas.index_buffer_ = new_buffer;
    }
    const size_t index_size = nrender * sizeof(IndexEntry);
    glBufferSubData(GL_ARRAY_BUFFER, sizeof(IndexEntry) * atlas.nglyphs_, index_size, atlas_index.get());

    glBindBuffer(GL_ARRAY_BUFFER, 0);

    /* Link sampler textures to the buffers. */
    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_BUFFER, meta_input_texture_);
    glTexBuffer(GL_TEXTURE_BUFFER, GL_R8UI, meta_input_buffer_);
    glBindTexture(GL_TEXTURE_BUFFER, 0);

    glActiveTexture(GL_TEXTURE1);
    glBindTexture(GL_TEXTURE_BUFFER, point_input_texture_);
    glTexBuffer(GL_TEXTURE_BUFFER, GL_R32F, point_input_buffer_);
    glBindTexture(GL_TEXTURE_BUFFER, 0);

    glActiveTexture(GL_TEXTURE2);
    glBindTexture(GL_TEXTURE_BUFFER, atlas.index_texture_);
    glTexBuffer(GL_TEXTURE_BUFFER, GL_R32F, atlas.index_buffer_);
    glBindTexture(GL_TEXTURE_BUFFER, 0);

    glActiveTexture(GL_TEXTURE0);

    /* Generate the atlas texture and bind it as the framebuffer. */
    if (atlas.texture_height_ == new_texture_height) {
        /* No need to extend the texture. */
        glBindFramebuffer(GL_DRAW_FRAMEBUFFER, atlas.atlas_framebuffer_);
        glBindTexture(GL_TEXTURE_2D, atlas.atlas_texture_);
        glViewport(0, 0, atlas.texture_width_, atlas.texture_height_);
    } else {
        GLuint new_texture;
        GLuint new_framebuffer;
        glGenTextures(1, &new_texture);
        glGenFramebuffers(1, &new_framebuffer);
        glBindFramebuffer(GL_DRAW_FRAMEBUFFER, new_framebuffer);

        glBindTexture(GL_TEXTURE_2D, new_texture);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

        glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA32F, atlas.texture_width_, new_texture_height, 0, GL_RGBA,
                     GL_FLOAT, NULL);

        if (glGetError() == GL_OUT_OF_MEMORY) {
            /* Buffer size too big, are you trying to type Klingon? */
            glBindFramebuffer(GL_FRAMEBUFFER, 0);
            glDeleteFramebuffers(1, &new_framebuffer);
            glDeleteTextures(1, &new_texture);
            return Error::OutOfGpuMemory;
        }

        glFramebufferTexture2D(GL_DRAW_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, new_texture, 0);
        glViewport(0, 0, atlas.texture_width_, new_texture_height);
        glClearColor(0.0, 0.0, 0.0, 1.0);
        glClear(GL_COLOR_BUFFER_BIT);

        if (atlas.texture_height_) {
            /* Old texture had data -> copy. */
            glBindFramebuffer(GL_READ_FRAMEBUFFER, atlas.atlas_framebuffer_);
            glBlitFramebuffer(0, 0, atlas.texture_width_, atlas.texture_height_, 0, 0, atlas.texture_width_,
                              atlas.texture_height_, GL_COLOR_BUFFER_BIT, GL_NEAREST);
            glBindFramebuffer(GL_READ_FRAMEBUFFER, 0);
        }

        glBindFramebuffer(GL_READ_FRAMEBUFFER, 0);
        atlas.texture_height_ = new_texture_height;
        glDeleteTextures(1, &atlas.atlas_texture_);
        atlas.atlas_texture_ = new_texture;
        glDeleteFramebuffers(1, &atlas.atlas_framebuffer_);
        atlas.atlas_framebuffer_ = new_framebuffer;
    }
    glBindTexture(GL_TEXTURE_2D, 0);

    GLfloat framebuffer_projection[4][4];
    ortho(0, (GLfloat)atlas.texture_width_, 0, (GLfloat)atlas.texture_height_, -1.0, 1.0,
          framebuffer_projection);
    ortho(-(GLfloat)atlas.texture_width_, (GLfloat)atlas.texture_width_, -(GLfloat)atlas.texture_height_,
          (GLfloat)atlas.texture_height_, -1.0, 1.0, atlas.projection_);

    glUseProgram(ctx.gen_shader_);
    glUniform1i(ctx.uniforms_.metadata, 0);
    glUniform1i(ctx.uniforms_.point_data, 1);

    glUniformMatrix4fv(ctx.uniforms_.atlas_projection, 1, GL_FALSE, (GLfloat *)framebuffer_projection);

    glUniform2f(ctx.uniforms_.scale, scale_, scale_);
    glUniform1f(ctx.uniforms_.range, range_);
    glUniform1i(ctx.uniforms_.meta_offset, 0);
    glUniform1i(ctx.uniforms_.point_offset, 0);

    if (glCheckFramebufferStatus(GL_FRAMEBUFFER) != GL_FRAMEBUFFER_COMPLETE)
        fprintf(stderr, "msdfgl: framebuffer incomplete: %x\n", glCheckFramebufferStatus(GL_FRAMEBUFFER));

    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_BUFFER, meta_input_texture_);

    glActiveTexture(GL_TEXTURE1);
    glBindTexture(GL_TEXTURE_BUFFER, point_input_texture_);

    glBindVertexArray(ctx.bbox_vao_);
    glBindBuffer(GL_ARRAY_BUFFER, ctx.bbox_vbo_);

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

        glUniform2f(ctx.uniforms_.translate, -g.bearing_x / kserializer_scale + range_ / 2.0f,
                    (g.glyph_height - g.bearing_y) / kserializer_scale + range_ / 2.0f);

        glUniform2f(ctx.uniforms_.texture_offset, g.offset_x, g.offset_y);
        glUniform1i(ctx.uniforms_.meta_offset, meta_offset);
        glUniform1i(ctx.uniforms_.point_offset, point_offset / (2 * sizeof(GLfloat)));
        glUniform1f(ctx.uniforms_.glyph_height, g.size_y);

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

    atlas.nglyphs_ += nrender;

    glViewport(original_viewport[0], original_viewport[1], original_viewport[2], original_viewport[3]);

    return {};
}

Result<void> Font::generate_ascii(const Context &ctx, Atlas &atlas) noexcept {
    std::vector<int> codepoint(0xff);
    std::iota(codepoint.begin(), codepoint.end(), 0);
    return generate_glyphs(ctx, atlas, codepoint);
}

void Font::draw(const Context &ctx, Atlas &atlas, std::vector<Glyph> glyphs,
                const float *projection) noexcept {
    for (int i = 0; i < glyphs.size(); ++i) {
        MapItem *e = character_index_.get(glyphs.at(i).codepoint);
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

    glUseProgram(ctx.render_shader_);

    /* Bind atlas texture and index buffer. */
    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, atlas.atlas_texture_);
    glUniform1i(ctx.uniforms_.atlas, 0);

    glActiveTexture(GL_TEXTURE1);
    glBindTexture(GL_TEXTURE_BUFFER, atlas.index_texture_);
    glUniform1i(ctx.uniforms_.index, 1);

    glUniformMatrix4fv(ctx.uniforms_.font_atlas_projection, 1, GL_FALSE, (GLfloat *)atlas.projection_);

    glUniformMatrix4fv(ctx.uniforms_.window_projection, 1, GL_FALSE, projection);
    glUniform1f(ctx.uniforms_.padding, (GLfloat)(range_ / 2.0 * kserializer_scale));
    glUniform1f(ctx.uniforms_.units_per_em, (GLfloat)face_->units_per_EM);
    glUniform2fv(ctx.uniforms_.dpi, 1, ctx.dpi_);

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

[[nodiscard]] Result<Glyphs> Font::print_unicode(Context &ctx, Atlas &atlas,
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
        if (not idx) return idx.error_;

        FT_Vector kerning{0};
        const bool should_get_kerning = enable_kerning and FT_HAS_KERNING(face_) and (i > 0);
        if (should_get_kerning) {
            const auto &previous_character = unicode_string.at(i - 1);
            FT_Get_Kerning(face_, FT_Get_Char_Index(face_, previous_character),
                           FT_Get_Char_Index(face_, character), FT_KERNING_UNSCALED, &kerning);
        }

        x0 += (idx.value()->advance[0] + kerning.x) * (size * ctx.dpi_[0] / 72.0f) / face_->units_per_EM;
    }
    return result;
}
[[nodiscard]] Result<Glyphs> Font::print_unicode_vertically(Context &ctx, Atlas &atlas,
                                                            const std::u32string_view unicode_string,
                                                            const float x, const float y, const long color,
                                                            const float size, const bool enable_kerning,
                                                            const float offset, const float skew,
                                                            const float strength) noexcept {
    std::vector<Glyph> result;
    float y0 = y;
    for (size_t i = 0; i < unicode_string.size(); i++) {
        const auto &character = unicode_string.at(i);
        result.push_back({});
        auto &element = result.at(result.size() - 1);
        element.x = x;
        element.y = y0;
        element.color = color;
        element.codepoint = unicode_string.at(i);
        element.size = size;
        element.offset = offset;
        element.skew = skew;
        element.strength = strength;

        auto idx = internal::map_get_or_add(ctx, *this, atlas, character);
        if (not idx) return idx.error_;

        FT_Vector kerning{0};
        const bool should_get_kerning = enable_kerning and FT_HAS_KERNING(face_) and (i > 0);
        if (should_get_kerning) {
            const auto &previous_character = unicode_string.at(i - 1);
            FT_Get_Kerning(face_, FT_Get_Char_Index(face_, previous_character),
                           FT_Get_Char_Index(face_, character), FT_KERNING_UNSCALED, &kerning);
        }

        y0 += (idx.value()->advance[1] + kerning.y) * (size * ctx.dpi_[1] / 72.0f) / face_->units_per_EM;
    }
    return result;
}
}  // namespace ff

namespace ff::internal {
const GLfloat _MAT4_ZERO_INIT[4][4] = {
    {0.0f, 0.0f, 0.0f, 0.0f}, {0.0f, 0.0f, 0.0f, 0.0f}, {0.0f, 0.0f, 0.0f, 0.0f}, {0.0f, 0.0f, 0.0f, 0.0f}};
bool compile_shader(const char *source, GLenum type, GLuint *shader, const char *version) {
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

Result<MapItem *> map_get_or_add(Context &ctx, Font &font, Atlas &atlas, int32_t codepoint) {
    auto ret = font.character_index_.get(codepoint);
    if (not font.character_index_.in(codepoint)) {
        auto result = font.generate_glyphs(ctx, atlas, {codepoint});
        if (not result) return result.error_;
        ret = font.character_index_.get(codepoint);
        if (ret == nullptr) return Error::GlyphGenerationFail;
    }
    return ret;
}
}  // namespace ff::internal
