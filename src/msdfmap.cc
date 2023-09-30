#include "msdfmap.hh"
#include <algorithm>

namespace msdf {
MapItem *Map::get(FT_ULong codepoint) {
    if (dynamic_map.empty())
        return NULL;
    const auto cmp = [codepoint](const auto &a) { return codepoint == a.codepoint; };
    auto result_it = std::find_if(dynamic_map.begin(), dynamic_map.end(), cmp);
    return &*result_it;
}

MapItem *Map::insert(FT_ULong codepoint) {
    MapItem new_item{0};
    new_item.codepoint = codepoint;
    auto res = dynamic_map.insert(dynamic_map.end(), new_item);
    res->codepoint_index = dynamic_map.size() - 1;
    return &*res;
}
} // namespace msdf
