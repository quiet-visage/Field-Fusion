#pragma once

#include <ft2build.h>
#include FT_FREETYPE_H

extern const float g_serializer_scale;

int ff_serializer_serialize_glyph(FT_Face face, int code,
                                  char *meta_buffer,
                                  float *point_buffer);
void glyph_buffer_size(FT_Face face, int code, size_t *meta_size,
                       size_t *point_size);
