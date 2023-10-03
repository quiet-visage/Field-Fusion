#pragma once

#include <cstddef>

namespace ff {
struct Atlas {
    int refcount_; /* Amount of fonts using this atlas */
    int implicit_; /* Set to 1 if the atlas was created automatically and not by
                      user */

    float projection_[4][4];

    /**
     * 2D RGBA atlas texture containing all MSDF-glyph bitmaps.
     */
    unsigned atlas_texture_;
    unsigned atlas_framebuffer_;

    /**
     * 1D buffer containing glyph position information per character in the
     * atlas texture.
     */
    unsigned index_texture_;
    unsigned index_buffer_;

    /**
     * Amount of glyphs currently rendered on the textures.
     */
    size_t nglyphs_{0};

    /**
     * The current size of the buffer index texture.
     */
    size_t nallocated_{0};

    int texture_width_;
    /**
     * The amount of allocated texture height.
     */
    int texture_height_{0};

    /**
     * The location in the atlas where the next bitmap would be rendered.
     */
    size_t offset_y_{1};
    size_t offset_x_{1};
    size_t y_increment_{0};

    /**
     * Amount of pixels to leave blank between MSDF bitmaps.
     */
    int padding_;
    [[nodiscard]] Atlas(int texture_width, int padding = 2)
        : texture_width_(texture_width), padding_(padding){};
};
}  // namespace ff