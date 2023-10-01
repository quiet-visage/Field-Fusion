#include "ffmap.hh"

#include <algorithm>

namespace ff {
MapItem *Map::get(FT_ULong codepoint) {
    if (dynamic_map_.empty()) return NULL;
    const auto cmp = [codepoint](const auto &a) { return codepoint == a.codepoint; };
    auto result_it = std::find_if(dynamic_map_.begin(), dynamic_map_.end(), cmp);
    return &*result_it;
}

MapItem *Map::insert(FT_ULong codepoint) {
    MapItem new_item{0};
    new_item.codepoint = codepoint;
    auto res = dynamic_map_.insert(dynamic_map_.end(), new_item);
    res->codepoint_index = dynamic_map_.size() - 1;
    return &*res;
}
}  // namespace ff
