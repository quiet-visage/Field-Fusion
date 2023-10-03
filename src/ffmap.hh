#pragma once
#include <ft2build.h>

#include <functional>
#include <vector>

#include "ffresult.hh"
#include FT_FREETYPE_H
#include <optional>

namespace ff {
constexpr const size_t kdynamic_map_initial_size = 0xff;
struct MapItem {
    FT_ULong codepoint;
    int codepoint_index;
    float advance[2];
};
struct Map {
    using ItemRef = std::reference_wrapper<MapItem>;
    std::vector<MapItem> dynamic_map_;
    Map() { dynamic_map_.reserve(kdynamic_map_initial_size); }
    std::optional<ItemRef> at(FT_ULong codepoint) noexcept;
    ItemRef insert(const FT_ULong codepoint) noexcept;
};
}  // namespace ff
