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

#include "../src/code_map.h"
#include "assert.h"

typedef int ff_font_id_t;
typedef int c32_t;

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
} ff_pos_t;

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
} ff_attrs_t;

typedef struct {
    /**
     * X and Y coordinates in in the projection coordinates.
     */
    ff_pos_t position;

    /**
     * The color of the character in 0xRRGGBBAA format.
     */
    uint32_t color;

    /**
     * Unicode code point of the character.
     */
    c32_t codepoint;

    /**
     * Font size to use for rendering of this character.
     */
    float size;
    ff_attrs_t attrs;
} ff_glyph_t;

typedef struct {
    ff_font_t font;
    ff_atlas_t atlas;
} ff_font_texture_pack_t;

typedef struct ht_fpack_entry {
    bool not_empty;
    int key;
    ff_font_texture_pack_t value;
    struct ht_fpack_entry *next;
} ht_fpack_entry_t;

typedef struct {
    struct ht_fpack_entry *entries;
} ht_fpack_map_t;

typedef struct {
    float width;
    float height;
} ff_dimensions_t;

typedef struct {
    ff_font_id_t font;
    float size;
    uint32_t color;
} ff_typo_t;

typedef struct {
    ff_glyph_t *data;
    size_t len;
    size_t cap;
} ff_glyph_vec_t;

typedef struct {
    float scale;
    float range;
    int texture_width;
    int texture_padding;
} ff_font_config_t;

typedef enum {
    ff_flag_default = 0x1,
    ff_flag_enable_kerning = 0x1,
    ff_flag_print_vertically = 0x2,
    ff_flag_draw_space = 0x4,
    ff_flag_draw_tabs = 0x8,
} ff_print_flag_e;

void ff_initialize(const char *sl_version);
void ff_terminate();
ff_font_config_t ff_default_font_config(void);
ff_font_id_t ff_new_load_font_from_memory(const unsigned char *bytes,
                                          size_t size,
                                          ff_font_config_t config);
ff_font_id_t ff_load_font(const char *path, ff_font_config_t config);
void ff_unload_font(ff_font_id_t font);
int ff_gen_glyphs(ff_font_id_t font, const c32_t *codepoints,
                  ulong codepoints_len);
void ff_draw(ff_font_id_t font, const ff_glyph_t *glyphs,
             ulong glyphs_len, const float *projection);
ff_attrs_t ff_get_default_attributes();
size_t ff_utf8_to_utf32(c32_t *dest, const char *src, ulong src_len);
size_t ff_utf32_to_utf8(char *dest, const c32_t *src, ulong src_len);
ff_dimensions_t ff_print_utf8(ff_glyph_t *glyphs, size_t *out_len,
                              const char *str, size_t str_len,
                              ff_typo_t typo, float x, float y,
                              ff_print_flag_e flags,
                              ff_attrs_t *optional_attrs);
ff_dimensions_t ff_print_utf32(ff_glyph_t *glyphs, size_t *out_len,
                               const c32_t *str, size_t str_len,
                               ff_typo_t typo, float x, float y,
                               ff_print_flag_e flags,
                               ff_attrs_t *optional_attrs);
ff_dimensions_t ff_print_utf32_vec(ff_glyph_vec_t *v,
                                   const c32_t *str, size_t str_len,
                                   ff_typo_t typo, float x, float y,
                                   ff_print_flag_e flags,
                                   ff_attrs_t *optional_attrs);
ff_dimensions_t ff_print_utf8_vec(ff_glyph_vec_t *v, const char *str,
                                  size_t str_len, ff_typo_t typo,
                                  float x, float y,
                                  ff_print_flag_e flags,
                                  ff_attrs_t *optional_attrs);
ff_dimensions_t ff_measure_utf32(const c32_t *str, size_t str_len,
                                 ff_font_id_t font, float size,
                                 bool with_kerning);
ff_dimensions_t ff_measure_utf8(const char *str, size_t str_len,
                                ff_font_id_t font, float size,
                                bool with_kerning);
void ff_get_ortho_projection(float left, float right, float bottom,
                             float top, float near, float far,
                             float dest[][4]);
void ff_set_glyphs_pos(ff_glyph_t *glyphs, size_t count, float x,
                       float y);

ff_glyph_vec_t ff_glyph_vec_create();
void ff_glyph_vec_destroy(ff_glyph_vec_t *v);
void ff_glyph_vec_push(ff_glyph_vec_t *v, ff_glyph_t glyph);
void ff_glyph_vec_clear(ff_glyph_vec_t *v);
void ff_glyph_vec_cat(ff_glyph_vec_t *dest, ff_glyph_vec_t *src);
void ff_glyphs_vec_prealloc(ff_glyph_vec_t *v, size_t n_elements);

#ifdef __cplusplus
}
#endif
