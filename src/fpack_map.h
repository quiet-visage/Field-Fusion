#pragma once

#include <freetype/freetype.h>
#include <stdbool.h>

#include "code_map.h"

typedef struct {
    const char *font_path;
    float scale;
    float range;
    float vertical_advance;

    FF_Map character_index;

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
} FF_Font;

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
} FF_Atlas;

typedef struct {
    FF_Font font;
    FF_Atlas atlas;
} FF_Font_Texture_Pack;

typedef struct FF_Fpack_Entry_Struct {
    bool not_empty;
    int key;
    FF_Font_Texture_Pack value;
    struct FF_Fpack_Entry_Struct *next;
} FF_Fpack_Entry;

typedef struct {
    FF_Fpack_Entry *entries;
} FF_Fpack_Map;

FF_Fpack_Map fpack_map_create();
FF_Font_Texture_Pack *fpack_map_get(FF_Fpack_Map *hashtable, int key);
void fpack_map_free(FF_Fpack_Map *ht);
void fpack_map_entry_free(FF_Fpack_Entry entry);
void fpack_map_set(FF_Fpack_Map *hashtable, int key,
                   FF_Font_Texture_Pack value);
struct FF_Fpack_Entry_Struct fpack_map_entry_new(
    int key, const FF_Font_Texture_Pack value);
