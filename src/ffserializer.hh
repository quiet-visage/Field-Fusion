#pragma once
#include <ft2build.h>

#include "ffresult.hh"
#include FT_FREETYPE_H

namespace ff {
constexpr const float kserializer_scale = 64;
static_assert(kserializer_scale >= 8);
Result<void> GlyphBufferSize(FT_Face face, int code, size_t *meta_size, size_t *point_size) noexcept;
Result<void> SerializeGlyph(FT_Face face, int code, char *meta_buffer, float *point_buffer) noexcept;
}  // namespace ff
