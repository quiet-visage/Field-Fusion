#pragma once

#ifdef __cplusplus
extern "C" {
#endif

#include <ft2build.h>

#include FT_FREETYPE_H

#ifndef FIELDFUSION_DONT_INCLUDE_GLAD
#include <glad.h>
#endif
#include <GL/gl.h>
#include <stdbool.h>
#include <uchar.h>

#include "assert.h"
#include "freetype/ftoutln.h"

typedef unsigned long int ulong;
typedef unsigned short int ushort;
typedef unsigned int uint;

typedef int ff_font_handle_t;

typedef struct {
    FT_ULong codepoint;
    int codepoint_index;
    float advance[2];
} ff_map_item_t;

typedef struct ht_codepoint_next_entry_t {
    bool not_empty;
    int key;
    ff_map_item_t value;
    struct ht_codepoint_next_entry_t *next;
} ht_codepoint_entry_t;

typedef struct {
    ht_codepoint_entry_t *entries;
} ht_codepoint_map_t;

typedef struct {
    ff_map_item_t extended_ascii_[0xff];
    ht_codepoint_map_t codepoint_map;
} ff_map_t;

typedef struct {
    const char *font_path;
    float scale;
    float range;
    float vertical_advance;

    ff_map_t character_index;

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
} ff_font_t;

typedef struct {
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
} ff_atlas_t;

typedef struct {
    float x;
    float y;
} ff_position_t;

typedef struct {
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
} ff_characteristics_t;

typedef struct {
    /**
     * X and Y coordinates in in the projection coordinates.
     */
    ff_position_t position;

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
    ff_characteristics_t characteristics;
} ff_glyph_t;

typedef struct {
    ff_font_t font;
    ff_atlas_t atlas;
} ff_font_texture_pack_t;

typedef struct ht_fpack_next_entry_t {
    bool not_empty;
    int key;
    ff_font_texture_pack_t value;
    struct ht_fpack_next_entry_t *next;
} ht_fpack_entry_t;

typedef struct {
    ht_fpack_entry_t *entries;
} ht_fpack_map_t;

typedef struct {
    float width;
    float height;
} ff_dimensions_t;

typedef struct {
    ff_font_handle_t font;
    float size;
    uint32_t color;
} ff_typography_t;

typedef struct {
    ff_glyph_t *data;
    ulong size;
    ulong capacity;
} ff_glyphs_vector_t;

typedef struct {
    float scale;
    float range;
    int texture_width;
    int texture_padding;
} ff_font_config_t;

typedef enum {
    ff_print_options_enable_kerning = 0x2,
    ff_print_options_print_vertically = 0x4,
} ff_print_options_t;

typedef struct {
    char *data;
    ulong size;
} ff_utf8_str_t;

typedef struct {
    char32_t *data;
    ulong size;
} ff_utf32_str_t;

typedef struct {
    ff_typography_t typography;
    int print_flags;
    ff_characteristics_t characteristics;
} print_params_t;

typedef struct {
    float scr_left;
    float scr_right;
    float scr_bottom;
    float scr_top;
    float near;
    float far;
} ortho_projection_params_t;

void ff_initialize(const char *version);
void ff_terminate();
ff_font_config_t ff_default_font_config(void);
ff_font_handle_t ff_new_font(const char *path,
                             ff_font_config_t config);
void ff_remove_font(ff_font_handle_t handle);
int ff_gen_glyphs(ff_font_handle_t, char32_t *codepoints,
                  ulong codepoints_count);
void ff_draw(ff_font_handle_t, ff_glyph_t *glyphs, ulong glyphs_count,
             float *projection);
ff_characteristics_t ff_get_default_characteristics();
int ff_get_default_print_flags();
int ff_utf8_to_utf32(char32_t *dest, const char *src, ulong count);
void ff_print_utf8(ff_glyphs_vector_t *vec, ff_utf8_str_t str,
                   print_params_t params, ff_position_t);
void ff_print_utf32(ff_glyphs_vector_t *vec, ff_utf32_str_t str,
                    print_params_t params, ff_position_t);
ff_dimensions_t ff_measure(const ff_font_handle_t, char32_t *str,
                           ulong str_count, float size,
                           bool with_kerning);

void ff_get_ortho_projection(ortho_projection_params_t params,
                             float dest[][4]);
ff_glyphs_vector_t ff_glyphs_vector_new();
ff_glyph_t *ff_glyphs_vector_get(ff_glyphs_vector_t *v, ulong index);
void ff_glyphs_vector_free(ff_glyphs_vector_t *v);
void ff_glyphs_vector_push(ff_glyphs_vector_t *v, ff_glyph_t glyph);
void ff_glyphs_vector_cat(ff_glyphs_vector_t *dest,
                          ff_glyphs_vector_t *src);

#ifdef __cplusplus
}
#endif
