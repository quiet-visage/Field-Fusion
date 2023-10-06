#pragma once
#include <ft2build.h>

#include <string>

#include "ffresult.hh"
#include "freetype/ftmodapi.h"
#include FT_FREETYPE_H

#include "fferror.hh"
#include "ffmap.hh"

namespace ff {
using Handle = size_t;
struct Font {
    const char *font_path;
    float scale;
    float range;
    float vertical_advance;

    Map character_index;

    /**
     * FreeType Face handle.
     */
    FT_Face face;

    /**
     * Texture buffer objects for serialized FreeType data input.
     */
    uint meta_input_buffer;
    uint point_input_buffer;
    uint meta_input_texture;
    uint point_input_texture;
};
struct Atlas {
    int refcount; /* Amount of fonts using this atlas */
    int implicit; /* Set to 1 if the atlas was created automatically and not by
                      user */

    float projection[4][4];

    /**
     * 2D RGBA atlas texture containing all MSDF-glyph bitmaps.
     */
    unsigned atlas_texture;
    unsigned atlas_framebuffer;

    /**
     * 1D buffer containing glyph position information per character in the
     * atlas texture.
     */
    unsigned index_texture;
    unsigned index_buffer;

    /**
     * Amount of glyphs currently rendered on the textures.
     */
    size_t nglyphs{0};

    /**
     * The current size of the buffer index texture.
     */
    size_t nallocated{0};

    int texture_width;
    /**
     * The amount of allocated texture height.
     */
    int texture_height{0};

    /**
     * The location in the atlas where the next bitmap would be rendered.
     */
    size_t offset_y{1};
    size_t offset_x{1};
    size_t y_increment{0};

    /**
     * Amount of pixels to leave blank between MSDF bitmaps.
     */
    int padding;
};

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
struct FontTexturePack {
    Font font;
    Atlas atlas;
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
    std::vector<FontTexturePack> fonts_;

    [[nodiscard]] Result<void> Init(const char *version) noexcept;
    [[nodiscard]] Result<Handle> NewFont(const char *path, const float scale = 4.0f, const float range = 2.0f,
                                         const int texture_width = 1024,
                                         const int texture_padding = 2) noexcept;
    [[nodiscard]] Result<void> RemoveFont(const Handle) noexcept;
    [[nodiscard]] Result<void> GenAscii(FontTexturePack &) noexcept;
    [[nodiscard]] Result<void> GenGlyphs(FontTexturePack &, const std::vector<int32_t> &codepoints) noexcept;
    [[nodiscard]] Result<void> Draw(FontTexturePack &, Glyphs, const float *projection) noexcept;
    [[nodiscard]] Result<Glyphs> PrintUnicode(FontTexturePack &, const std::u32string_view buffer,
                                              const float x, const float y, const long color,
                                              const float size, const bool enable_kerning = true,
                                              const bool print_vertically = false, const float offset = 0.0f,
                                              const float skew = 0.0f, const float strength = 0.50f) noexcept;
    void Destroy() noexcept;
};

void Ortho(float left, float right, float bottom, float top, float nearVal, float farVal, float dest[][4]);
inline void GlyphsCat(Glyphs &to, Glyphs &from) { to.insert(to.end(), from.begin(), from.end()); }
}  // namespace ff
