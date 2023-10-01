#pragma once
#include <ft2build.h>

#include <vector>
#include FT_FREETYPE_H

namespace ff {
constexpr const size_t kdynamic_map_initial_size = 0xff;
struct MapItem {
    FT_ULong codepoint;
    int codepoint_index;
    float advance[2];
};
struct Map {
    std::vector<MapItem> dynamic_map_;
    Map() { dynamic_map_.reserve(kdynamic_map_initial_size); }
    MapItem *get(FT_ULong codepoint);
    MapItem *insert(FT_ULong codepoint);
    inline bool in(FT_ULong codepoint) { return get(codepoint) != &*dynamic_map_.end(); }
};
}  // namespace ff
