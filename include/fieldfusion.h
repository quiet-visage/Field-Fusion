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

#define FF_DEFAULT_FONT_CONFIG                               \
    (FF_Font_Config) {                                       \
        .scale = 2.0f, .range = 2.2f, .texture_width = 1024, \
        .texture_padding = 4                                 \
    }

typedef int FF_Font_Id;
typedef int C32;

typedef struct {
    float x;
    float y;
} FF_Pos;

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
} FF_Attr;

typedef struct {
    /**
     * X and Y coordinates in in the projection coordinates.
     */
    FF_Pos position;

    /**
     * The color of the character in 0xRRGGBBAA format.
     */
    uint32_t color;

    /**
     * Unicode code point of the character.
     */
    C32 codepoint;

    /**
     * Font size to use for rendering of this character.
     */
    float size;
    FF_Attr attrs;
} FF_Glyph;

typedef struct {
    float width;
    float height;
} FF_Dimensions;

typedef struct {
    FF_Font_Id font;
    float size;
    uint32_t color;
} FF_Typo;

typedef struct {
    FF_Glyph *data;
    size_t len;
    size_t cap;
} FF_Glyph_Vec;

typedef struct {
    float scale;
    float range;
    int texture_width;
    int texture_padding;
} FF_Font_Config;

typedef enum {
    FF_FLAG_DEFAULT = 0x1,
    FF_FLAG_ENABLE_KERNING = 0x1,
    FF_FLAG_PRINT_VERTICALLY = 0x2,
    FF_FLAG_DRAW_SPACE = 0x4,
    FF_FLAG_DRAW_TABS = 0x8,
} FF_Print_Flag;

typedef struct {
    FF_Typo typo;
    FF_Print_Flag flags;
    FF_Attr attributes;
} FF_Style;

void ff_initialize(const char *sl_version);
void ff_terminate();
FF_Font_Id ff_new_load_font_from_memory(const unsigned char *bytes,
                                        size_t size,
                                        FF_Font_Config config);
FF_Font_Id ff_load_font(const char *path, FF_Font_Config config);
void ff_unload_font(FF_Font_Id font);
int ff_gen_glyphs(FF_Font_Id font, const C32 *codepoints,
                  ulong codepoints_len);
void ff_draw(const FF_Glyph *glyphs, ulong glyphs_len,
             const float *projection, FF_Style style);
void ff_draw_str32(const C32 *str, size_t len, float x, float y,
                   float *projection, FF_Style style);
void ff_draw_str8(const char *str, size_t len, float x, float y,
                  float *projection, FF_Style style);
FF_Attr ff_get_default_attributes();
size_t ff_utf8_to_utf32(C32 *dest, const char *src, ulong src_len);
size_t ff_utf32_to_utf8(char *dest, const C32 *src, ulong src_len);
FF_Dimensions ff_print_utf8(FF_Glyph *glyphs, size_t *out_len,
                            const char *str, size_t str_len, float x,
                            float y, FF_Style style);
FF_Dimensions ff_print_utf32(FF_Glyph *glyphs, size_t *out_len,
                             const C32 *str, size_t str_len, float x,
                             float y, FF_Style style);
FF_Dimensions ff_print_utf32_vec(FF_Glyph_Vec *v, const C32 *str,
                                 size_t str_len, float x, float y,
                                 FF_Style style);
FF_Dimensions ff_print_utf8_vec(FF_Glyph_Vec *v, const char *str,
                                size_t str_len, float x, float y,
                                FF_Style style);
FF_Dimensions ff_measure_utf32(const C32 *str, size_t str_len,
                               FF_Style style);
FF_Dimensions ff_measure_utf8(const char *str, size_t str_len,
                              FF_Style style);
void ff_get_ortho_projection(float left, float right, float bottom,
                             float top, float near, float far,
                             float dest[][4]);
void ff_set_glyphs_pos(FF_Glyph *glyphs, size_t count, float x,
                       float y);
FF_Glyph_Vec ff_glyph_vec_create();
void ff_glyph_vec_destroy(FF_Glyph_Vec *v);
void ff_glyph_vec_push(FF_Glyph_Vec *v, FF_Glyph glyph);
void ff_glyph_vec_clear(FF_Glyph_Vec *v);
void ff_glyph_vec_cat(FF_Glyph_Vec *dest, FF_Glyph_Vec *src);
void ff_glyphs_vec_prealloc(FF_Glyph_Vec *v, size_t n_elements);

#ifdef __cplusplus
}
#endif
