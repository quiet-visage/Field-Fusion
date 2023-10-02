#pragma once
#include <ft2build.h>

#include <string>

#include "ffresult.hh"
#include "freetype/ftmodapi.h"
#include FT_FREETYPE_H

#include "fferror.hh"
#include "ffmap.hh"

namespace ff {
struct Glyph {
    /**
     * X and Y coordinates in in the projection coordinates.
     */
    float x;
    float y;

    /**
     * The color of the character in 0xRRGGBBAA format.
     */
    long color;

    /**
     * Unicode code point of the character.
     */
    int32_t codepoint;

    /**
     * Font size to use for rendering of this character.
     */
    float size;

    /**
     * Y offset (for e.g. subscripts and superscripts).
     */
    float offset;

    /**
     * The amount of "lean" on the character. Positive leans to the right,
     * negative leans to the left. Skew can create /italics/ effect without
     * loading a separate font atlas.
     */
    float skew;

    /**
     * The "boldness" of the character. 0.5 is normal strength, lower is thinner
     * and higher is thicker. Strength can create *bold* effect without loading
     * a separate font atlas.
     */
    float strength;
};
using Glyphs = std::vector<Glyph>;
struct Context;
struct Atlas {
    int refcount_; /* Amount of fonts using this atlas */
    int implicit_; /* Set to 1 if the atlas was created automatically and not by
                      user */

    float projection_[4][4];

    /**
     * 2D RGBA atlas texture containing all MSDF-glyph bitmaps.
     */
    uint atlas_texture_;
    uint atlas_framebuffer_;

    /**
     * 1D buffer containing glyph position information per character in the
     * atlas texture.
     */
    uint index_texture_;
    uint index_buffer_;

    /**
     * Amount of glyphs currently rendered on the textures.
     */
    size_t nglyphs_{0};

    /**
     * The current size of the buffer index texture.
     */
    size_t nallocated_{0};

    int texture_width_;
    /**
     * The amount of allocated texture height.
     */
    int texture_height_{0};

    /**
     * The location in the atlas where the next bitmap would be rendered.
     */
    size_t offset_y_{1};
    size_t offset_x_{1};
    size_t y_increment_{0};

    /**
     * Amount of pixels to leave blank between MSDF bitmaps.
     */
    int padding_;
    [[nodiscard]] Atlas(int texture_width, int padding = 2)
        : texture_width_(texture_width), padding_(padding){};
    void init_textures() noexcept;
};

struct Font {
    const char *font_path_;
    float scale_;
    float range_;

    float vertical_advance_;

    Map character_index_;

    /**
     * FreeType Face handle.
     */
    FT_Face face_;

    /**
     * Texture buffer objects for serialized FreeType data input.
     */
    uint meta_input_buffer_;
    uint point_input_buffer_;
    uint meta_input_texture_;
    uint point_input_texture_;

    [[nodiscard]] Font(const char *font_path, const float scale = 4.0f, const float range = 2.0f) noexcept
        : font_path_(font_path), scale_(scale), range_(range){};
    [[nodiscard]] Result<void> init_face(const Context &ctx) noexcept;
    void init_textures() noexcept;
    void draw(const Context &ctx, Atlas &atlas, Glyphs glyphs, const float *projection) noexcept;
    Result<void> generate_glyphs(const Context &ctx, Atlas &atlas,
                                 const std::vector<int> &codepoints) noexcept;
    Result<void> generate_ascii(const Context &ctx, Atlas &atlas) noexcept;
    [[nodiscard]] Result<Glyphs> print_unicode(Context &ctx, Atlas &atlas,
                                               const std::u32string_view unicode_string, const float x,
                                               const float y, const long color, const float size,
                                               const bool enable_kerning = true, const float offset = 0.0f,
                                               const float skew = 0.0f, const float strength = 0.5f) noexcept;
    [[nodiscard]] Result<Glyphs> print_unicode_vertically(Context &ctx, Atlas &atlas,
                                                          const std::u32string_view unicode_string,
                                                          const float x, const float y, const long color,
                                                          const float size, const bool enable_kerning = true,
                                                          const float offset = 0.0f, const float skew = 0.0f,
                                                          const float strength = 0.5f) noexcept;
    void inline destroy() noexcept { FT_Done_Face(face_); }
};

struct Context {
    struct Uniforms {
        int window_projection;
        int font_atlas_projection;
        int index;
        int atlas;
        int padding;
        int offset;
        int dpi;
        int units_per_em;
        int atlas_projection;
        int texture_offset;
        int translate;
        int scale;
        int range;
        int glyph_height;
        int meta_offset;
        int point_offset;
        int metadata;
        int point_data;
    };

    FT_Library ft_library_;

    float dpi_[2];
    uint gen_shader_;
    uint render_shader_;
    Uniforms uniforms_;
    uint bbox_vao_;
    uint bbox_vbo_;
    int max_texture_size_;

    [[nodiscard]] static Result<Context> create(const char *version) noexcept;
    void inline destroy() noexcept { FT_Done_FreeType(ft_library_); };
};

void ortho(float left, float right, float bottom, float top, float nearVal, float farVal, float dest[][4]);
}  // namespace ff
