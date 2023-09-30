#pragma once
#include <ft2build.h>
#include FT_FREETYPE_H

namespace msdf {
#define SERIALIZER_SCALE 64.0f
int glyph_buffer_size(FT_Face face, int code, size_t *meta_size, size_t *point_size);
int serialize_glyph(FT_Face face, int code, char *meta_buffer, float *point_buffer);
} // namespace msdf
