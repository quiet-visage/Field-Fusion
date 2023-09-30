#pragma once
#include <ft2build.h>
#include <vector>
#include FT_FREETYPE_H

namespace msdf {
#define MSDFGL_MAP_DYNAMIC_INITIAL_SIZE 256
struct MapItem {
    FT_ULong codepoint;
    int codepoint_index;
    float advance[2];
};
struct Map {
    std::vector<MapItem> dynamic_map;
    MapItem *get(FT_ULong codepoint);
    MapItem *insert(FT_ULong codepoint);
    inline bool in(FT_ULong codepoint) { return get(codepoint) != &*dynamic_map.end(); }
};
} // namespace msdf
