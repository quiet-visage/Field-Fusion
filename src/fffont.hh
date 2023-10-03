#pragma once

#include "ffmap.hh"
#include "freetype/ftmodapi.h"
#include FT_FREETYPE_H

namespace ff {
struct Font {
    const char *font_path_;
    float scale_;
    float range_;
    float vertical_advance_;

    Map character_index_;

    /**
     * FreeType Face handle.
     */
    FT_Face face_;

    /**
     * Texture buffer objects for serialized FreeType data input.
     */
    uint meta_input_buffer_;
    uint point_input_buffer_;
    uint meta_input_texture_;
    uint point_input_texture_;

    [[nodiscard]] Font(const char *font_path, const float scale = 4.0f, const float range = 2.0f) noexcept
        : font_path_(font_path), scale_(scale), range_(range){};
    void inline destroy() noexcept { FT_Done_Face(face_); }
};
}  // namespace ff
