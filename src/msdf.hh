#pragma once
#include "freetype/ftmodapi.h"
#include <ft2build.h>
#include <string>
#include <string_view>
#include FT_FREETYPE_H
#include "msdferror.hh"
#include "msdfmap.hh"
#include <iostream>
#include <memory>
#include <variant>

namespace msdf {

struct Context;
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
    FT_ULong codepoint;

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

struct Atlas {
    int _refcount; /* Amount of fonts using this atlas */
    int _implicit; /* Set to 1 if the atlas was created automatically and not by
                      user */

    float _projection[4][4];

    /**
     * 2D RGBA atlas texture containing all MSDF-glyph bitmaps.
     */
    uint _atlas_texture;
    uint _atlas_framebuffer;

    /**
     * 1D buffer containing glyph position information per character in the
     * atlas texture.
     */
    uint _index_texture;
    uint _index_buffer;

    /**
     * Amount of glyphs currently rendered on the textures.
     */
    size_t _nglyphs;

    /**
     * The current size of the buffer index texture.
     */
    size_t _nallocated;

    int _texture_width;
    /**
     * The amount of allocated texture height.
     */
    int _texture_height;

    /**
     * The location in the atlas where the next bitmap would be rendered.
     */
    size_t _offset_y;
    size_t _offset_x;
    size_t _y_increment;

    /**
     * Amount of pixels to leave blank between MSDF bitmaps.
     */
    int _padding;

    [[nodiscard]] static Atlas create(Context &ctx, int texture_width, int padding) noexcept;
};

struct Font {
    const char *_font_name;
    const float _scale;
    const float _range;

    float _vertical_advance;

    Map _character_index;

    /**
     * FreeType Face handle.
     */
    FT_Face _face;

    /**
     * Texture buffer objects for serialized FreeType data input.
     */
    uint _meta_input_buffer;
    uint _point_input_buffer;
    uint _meta_input_texture;
    uint _point_input_texture;

    Font() = delete;

    [[nodiscard]] static std::variant<Font, std::exception> create(const Context &ctx, const char *font_name,
                                                                   const float scale = 4.0f,
                                                                   const float range = 2.0f) noexcept;
    void render(const Context &ctx, Atlas &atlas, std::vector<Glyph> glyphs,
                const float *projection) noexcept;
    int generate_glyphs(const Context &ctx, Atlas &atlas, const std::vector<int> &keys) noexcept;
    int generate_ascii(const Context &ctx, Atlas &atlas) noexcept;
    [[nodiscard]] std::vector<Glyph>
    print_unicode(Context &ctx, Atlas &atlas, const std::u32string_view unicode_string, const float x,
                  const float y, const long color, const float size, const bool enable_kerning = true,
                  const float offset = 0.0f, const float skew = 0.0f, const float strength = 0.5f) noexcept;
    void inline destroy() { FT_Done_Face(_face); }

  protected:
    Font(const char *font_name, const float scale, const float range)
        : _font_name(font_name), _scale(scale), _range(range){};
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

    FT_Library _ft_library;

    float _dpi[2];
    uint _gen_shader;
    uint _render_shader;
    Uniforms _uniforms;
    uint _bbox_vao;
    uint _bbox_vbo;
    int _max_texture_size;

    Context() = delete;
    [[nodiscard]] static std::variant<Context, std::exception> create(const char *version) noexcept;
    void inline destroy() { FT_Done_FreeType(_ft_library); };
};

void ortho(float left, float right, float bottom, float top, float nearVal, float farVal, float dest[][4]);
inline bool is_error(std::variant<Context, std::exception> &a) {
    return std::holds_alternative<std::exception>(a);
}
inline bool is_error(std::variant<Font, std::exception> &a) {
    return std::holds_alternative<std::exception>(a);
}
inline Context &unwrap(std::variant<Context, std::exception> &a) { return std::get<Context>(a); };
inline Font &unwrap(std::variant<Font, std::exception> &a) { return std::get<Font>(a); };
} // namespace msdf
