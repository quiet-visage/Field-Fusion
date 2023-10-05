#pragma once
#include <ft2build.h>

#include <string>

#include "ffresult.hh"
#include "freetype/ftmodapi.h"
#include FT_FREETYPE_H

#include "ffatlas.hh"
#include "fferror.hh"
#include "fffont.hh"
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

struct FieldFusion {
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
    std::vector<Font> fonts_;

    [[nodiscard]] Result<void> init(const char *version) noexcept;
    [[nodiscard]] Result<size_t> new_font(Atlas &, const char *path, const float scale = 4.0f,
                                          const float range = 2.0f) noexcept;
    Atlas new_atlas(const int texture_width, const int padding = 2) noexcept;
    [[nodiscard]] Result<void> generate_ascii(Atlas &, Font &) noexcept;
    [[nodiscard]] Result<void> generate_glyph(Atlas &, Font &,
                                              const std::vector<int32_t> &codepoints) noexcept;
    [[nodiscard]] Result<void> generate_glyph(Atlas &, Font &, int32_t codepoint) noexcept;
    [[nodiscard]] Result<void> draw(Atlas &, Font &, Glyphs, const float *projection) noexcept;
    [[nodiscard]] Result<Glyphs> print_unicode(Atlas &, Font &, const std::u32string_view buffer,
                                               const float x, const float y, const long color,
                                               const float size, const bool enable_kerning = true,
                                               const bool print_vertically = false, const float offset = 0.0f,
                                               const float skew = 0.0f,
                                               const float strength = 0.50f) noexcept;
    void destroy() noexcept;
};

void ortho(float left, float right, float bottom, float top, float nearVal, float farVal, float dest[][4]);
}  // namespace ff
