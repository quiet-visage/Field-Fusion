#include "ffmap.hh"

#include <algorithm>

namespace ff {
std::optional<Map::ItemRef> Map::at(FT_ULong codepoint) noexcept {
    if (dynamic_map_.empty()) return {};
    const auto cmp = [codepoint](const auto &a) { return codepoint == a.codepoint; };
    const auto result_it = std::find_if(dynamic_map_.begin(), dynamic_map_.end(), cmp);
    if (result_it == dynamic_map_.end()) return {};
    const auto index = std::distance(dynamic_map_.begin(), result_it);
    return {std::ref(dynamic_map_.at(index))};
}

Map::ItemRef Map::insert(const FT_ULong codepoint) noexcept {
    MapItem new_item{0};
    new_item.codepoint = codepoint;
    auto res = dynamic_map_.insert(dynamic_map_.end(), new_item);
    res->codepoint_index = dynamic_map_.size() - 1;
    return std::ref(dynamic_map_.at(dynamic_map_.size() - 1));
}
}  // namespace ff
