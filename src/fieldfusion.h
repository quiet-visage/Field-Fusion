#pragma once

#ifdef __cplusplus
extern "C" {
#endif

#include <ft2build.h>
#include FT_FREETYPE_H

#ifndef FIELDFUSION_DONT_INCLUDE_GLAD
#include <glad.h>
#endif

#include <stdbool.h>
#include <uchar.h>

#include "assert.h"
#include "freetype/ftoutln.h"

typedef unsigned long int ulong;
typedef unsigned short int ushort;
typedef unsigned int uint;

typedef int ff_font_handle_t;

struct ff_map_item {
    FT_ULong codepoint;
    int codepoint_index;
    float advance[2];
};

struct ht_codepoint_entry {
    bool not_empty;
    int key;
    struct ff_map_item value;
    struct ht_codepoint_entry *next;
};

struct ht_codepoint_map {
    struct ht_codepoint_entry *entries;
};

struct ff_map {
    struct ff_map_item extended_ascii_[0xff];
    struct ht_codepoint_map codepoint_map;
};

struct ff_font {
    const char *font_path;
    float scale;
    float range;
    float vertical_advance;

    struct ff_map character_index;

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

struct ff_atlas {
    int refcount; /* Amount of fonts using this atlas */
    int implicit; /* Set to 1 if the atlas was created automatically
                     and not by user */

    float projection[4][4];

    /**
     * 2D RGBA atlas texture containing all MSDF-glyph bitmaps.
     */
    unsigned atlas_texture;
    unsigned atlas_framebuffer;

    /**
     * 1D buffer containing glyph position information per character
     * in the atlas texture.
     */
    unsigned index_texture;
    unsigned index_buffer;

    /**
     * Amount of glyphs currently rendered on the textures.
     */
    size_t nglyphs;

    /**
     * The current size of the buffer index texture.
     */
    size_t nallocated;

    int texture_width;
    /**
     * The amount of allocated texture height.
     */
    int texture_height;

    /**
     * The location in the atlas where the next bitmap would be
     * rendered.
     */
    size_t offset_y;
    size_t offset_x;
    size_t y_increment;

    /**
     * Amount of pixels to leave blank between MSDF bitmaps.
     */
    int padding;
};

struct ff_position {
    float x;
    float y;
};

struct ff_characteristics {
    /**
     * Y offset (for e.g. subscripts and superscripts).
     */
    float offset;

    /**
     * The amount of "lean" on the character. Positive leans to the
     * right, negative leans to the left. Skew can create /italics/
     * effect without loading a separate font atlas.
     */
    float skew;

    /**
     * The "boldness" of the character. 0.5 is normal strength, lower
     * is thinner and higher is thicker. Strength can create *bold*
     * effect without loading a separate font atlas.
     */
    float strength;
};

struct ff_glyph {
    /**
     * X and Y coordinates in in the projection coordinates.
     */
    struct ff_position position;

    /**
     * The color of the character in 0xRRGGBBAA format.
     */
    uint32_t color;

    /**
     * Unicode code point of the character.
     */
    char32_t codepoint;

    /**
     * Font size to use for rendering of this character.
     */
    float size;
    struct ff_characteristics characteristics;
};

struct ff_font_texture_pack {
    struct ff_font font;
    struct ff_atlas atlas;
};

struct ht_fpack_entry {
    bool not_empty;
    int key;
    struct ff_font_texture_pack value;
    struct ht_fpack_entry *next;
};

struct ht_fpack_map {
    struct ht_fpack_entry *entries;
};

struct ff_dimensions {
    float width;
    float height;
};

struct ff_typography {
    ff_font_handle_t font;
    float size;
    uint32_t color;
};

struct ff_glyphs_vector {
    struct ff_glyph *data;
    ulong size;
    ulong capacity;
};

struct ff_font_config {
    float scale;
    float range;
    int texture_width;
    int texture_padding;
};

enum ff_print_options {
    ff_print_options_enable_kerning = 0x2,
    ff_print_options_print_vertically = 0x4,
};

struct ff_utf8_str {
    char *data;
    ulong size;
};

struct ff_utf32_str {
    char32_t *data;
    ulong size;
};

struct ff_print_params {
    struct ff_typography typography;
    int print_flags;
    struct ff_characteristics characteristics;
    bool draw_spaces;
};

struct ff_ortho_params {
    float scr_left;
    float scr_right;
    float scr_bottom;
    float scr_top;
    float near;
    float far;
};

void ff_initialize(const char *version);
void ff_terminate();
struct ff_font_config ff_default_font_config(void);
ff_font_handle_t ff_new_font(const char *path,
                             const struct ff_font_config config);
void ff_remove_font(const ff_font_handle_t handle);
int ff_gen_glyphs(const ff_font_handle_t, const char32_t *codepoints,
                  const ulong codepoints_count);
void ff_draw(const ff_font_handle_t, const struct ff_glyph *glyphs,
             const ulong glyphs_count, const float *projection);
struct ff_characteristics ff_get_default_characteristics();
int ff_get_default_print_flags();
int ff_utf8_to_utf32(char32_t *dest, const char *src,
                     const ulong count);
int ff_utf32_to_utf8(char *dest, const char32_t *src,
                     const ulong count);
void ff_print_utf8(struct ff_glyphs_vector *vec,
                   const struct ff_utf8_str str,
                   const struct ff_print_params params,
                   const struct ff_position);
void ff_print_utf32(struct ff_glyphs_vector *vec,
                    const struct ff_utf32_str str,
                    const struct ff_print_params params,
                    const struct ff_position);
struct ff_dimensions ff_measure(const ff_font_handle_t,
                                const char32_t *str,
                                const ulong str_count,
                                const float size,
                                const bool with_kerning);

void ff_get_ortho_projection(const struct ff_ortho_params params,
                             float dest[][4]);
struct ff_glyphs_vector ff_glyphs_vector_create();
void ff_glyphs_vector_destroy(struct ff_glyphs_vector *v);
void ff_glyphs_vector_push(struct ff_glyphs_vector *v,
                           struct ff_glyph glyph);
void ff_glyphs_vector_clear(struct ff_glyphs_vector *v);
void ff_glyphs_vector_cat(struct ff_glyphs_vector *dest,
                          struct ff_glyphs_vector *src);

#ifdef __cplusplus
}
#endif
