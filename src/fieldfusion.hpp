#pragma once

#include <ft2build.h>

#include <array>
#include <cstdint>
#include <exception>
#include <string>
#include <unordered_map>
#include <vector>
#include FT_FREETYPE_H

#ifndef FIELDFUSION_DONT_INCLUDE_GLAD
#include <glad.h>
#endif
#include <GL/gl.h>

#include <cmath>

#include "freetype/ftoutln.h"

using ff_font_handle_t = size_t;

constexpr const size_t kdynamic_map_initial_size = 0xff;
struct ff_map_item_t {
    FT_ULong codepoint;
    int codepoint_index;
    float advance[2];
};

struct ff_map_t {
    std::array<ff_map_item_t, 0xff> extended_ascii_;
    std::unordered_map<char32_t, ff_map_item_t> codepoint_map_;

    ff_map_item_t *at(char32_t codepoint) noexcept;
    ff_map_item_t *insert(const char32_t codepoint) noexcept;
};

struct ff_font_t {
    const char *font_path;
    float scale;
    float range;
    float vertical_advance;

    ff_map_t character_index;

    /**
     * FreeType Face handle.
     */
    FT_Face face;

    /**
     * Texture buffer objects for serialized FreeType data input.
     */
    uint meta_input_buffer;
    uint point_input_buffer;
    uint meta_input_texture;
    uint point_input_texture;
};

struct ff_atlas_t {
    int refcount; /* Amount of fonts using this atlas */
    int implicit; /* Set to 1 if the atlas was created automatically and not by
                      user */

    float projection[4][4];

    /**
     * 2D RGBA atlas texture containing all MSDF-glyph bitmaps.
     */
    unsigned atlas_texture;
    unsigned atlas_framebuffer;

    /**
     * 1D buffer containing glyph position information per character in the
     * atlas texture.
     */
    unsigned index_texture;
    unsigned index_buffer;

    /**
     * Amount of glyphs currently rendered on the textures.
     */
    size_t nglyphs{0};

    /**
     * The current size of the buffer index texture.
     */
    size_t nallocated{0};

    int texture_width;
    /**
     * The amount of allocated texture height.
     */
    int texture_height{0};

    /**
     * The location in the atlas where the next bitmap would be rendered.
     */
    size_t offset_y{1};
    size_t offset_x{1};
    size_t y_increment{0};

    /**
     * Amount of pixels to leave blank between MSDF bitmaps.
     */
    int padding;
};

struct ff_position_t {
    float x;
    float y;
};

struct ff_glyph_t {
    struct Characteristics {
        /**
         * Y offset (for e.g. subscripts and superscripts).
         */
        float offset;

        /**
         * The amount of "lean" on the character. Positive leans to the right,
         * negative leans to the left. Skew can create /italics/ effect without
         * loading a separate font atlas.
         */
        float skew;

        /**
         * The "boldness" of the character. 0.5 is normal strength, lower is thinner
         * and higher is thicker. Strength can create *bold* effect without loading
         * a separate font atlas.
         */
        float strength;
    };
    /**
     * X and Y coordinates in in the projection coordinates.
     */
    ff_position_t position;

    /**
     * The color of the character in 0xRRGGBBAA format.
     */
    uint32_t color;

    /**
     * Unicode code point of the character.
     */
    char32_t codepoint;

    /**
     * Font size to use for rendering of this character.
     */
    float size;
    Characteristics characteristics;
};

struct ff_font_texture_pack_t {
    ff_font_t font;
    ff_atlas_t atlas;
};

struct ff_dimensions_t {
    float width;
    float height;
};

struct ff_typography_t {
    ff_font_handle_t font;
    float size;
    uint32_t color;
};

using ff_glyphs_t = std::vector<ff_glyph_t>;

enum ff_print_options_t : int {
    ff_print_options_enable_kerning = 0x2,
    ff_print_options_print_vertically = 0x4,
};

void ff_initialize(const char *version) noexcept;
ff_font_handle_t ff_new_font(const char *path, const float scale = 4.0f,
                       const float range = 2.0f, const int texture_width = 1024,
                       const int texture_padding = 2) noexcept;
void ff_remove_font(const ff_font_handle_t) noexcept;
void ff_gen_glyphs(const ff_font_handle_t, const std::vector<char32_t> &codepoints) noexcept;
void ff_draw(const ff_font_handle_t, const ff_glyphs_t &, const float *projection) noexcept;
[[nodiscard]] ff_glyphs_t ff_print_unicode(
    const ff_typography_t, const std::u32string_view buffer, const ff_position_t position,
    const int print_options = ff_print_options_enable_kerning,
    const ff_glyph_t::Characteristics = {0.0f, 0.0f, 0.5f}) noexcept;
[[nodiscard]] ff_dimensions_t ff_measure(const ff_font_handle_t,
                                     const std::u32string_view &buffer, const float size,
                                     const bool with_kerning = true);
void ff_terminate() noexcept;

void ff_ortho(float left, float right, float bottom, float top, float nearVal,
             float farVal, float dest[][4]);
inline void ff_glyphs_cat(ff_glyphs_t &to, ff_glyphs_t &from) {
    to.insert(to.end(), from.begin(), from.end());
}

#ifdef FIELDFUSION_IMPLEMENTATION
#include <algorithm>
#include <cassert>
#include <memory>
#include <numeric>

static const char *kfont_fragment = R"SHADER(
precision highp float;
in vec2 text_pos;
in vec4 text_color;
in float strength;
out vec4 color;

uniform sampler2D font_atlas;
uniform mat4 font_projection;

float median(float r, float g, float b) {
    return max(min(r, g), min(max(r, g), b));
}
float pxRange = 4.0;

float srgb_from_linear(float value) {
  return value<= 0.0031308f
       ? value* 12.92f
       : pow (value, 1.0f/2.4f) * 1.055f - 0.055f;
}
float Luma(vec3 color) { return dot(color, vec3(0.2126, 0.7152, 0.0722)); }
void main() {
    vec2 coords = (font_projection * vec4(text_pos, 0.0, 1.0)).xy;

    /* Invert the strength so that 1.0 becomes bold and 0.0 becomes thin */
    float threshold = 1.0 - strength;

    vec2 msdfUnit = pxRange/vec2(textureSize(font_atlas, 0));
    vec3 s = texture(font_atlas, coords).rgb;
    float sigDist = median(s.r, s.g, s.b) - threshold;
    sigDist *= dot(msdfUnit, 0.5/fwidth(coords));
    float opacity = clamp(sigDist + 0.5, 0.0, 1.0);

    vec3 v = text_color.rgb;
    v = mix(vec4(0.0, 0.0, 0.0, 0.0), text_color, opacity).rgb;
    v.r = srgb_from_linear(v.r); 
    v.g = srgb_from_linear(v.g); 
    v.b = srgb_from_linear(v.b); 

    float uSaturation = 4.0f;
    float luma = Luma(v);
    v = mix(vec3(luma), v, uSaturation);

    color = vec4(v, opacity);

})SHADER";

static const char *kfont_geometry = R"SHADER(
layout (points) in;
layout (triangle_strip, max_vertices = 4) out;

in VS_OUT {
    int glyph;
    vec4 color;
    float size;
    float y_offset;
    float skewness;
    float strength;
} gs_in[];

out vec2 text_pos;
out vec4 text_color;
out float strength;

uniform mat4 projection;
uniform float padding;
uniform float units_per_em;
uniform vec2 dpi;

precision mediump samplerBuffer;
uniform samplerBuffer font_index;


void main() {
    text_color = gs_in[0].color;
    strength = gs_in[0].strength;

    vec4 font_size = vec4(gs_in[0].size * dpi / 72.0 / units_per_em, 1.0, 1.0);

    int _offset = 8 * gs_in[0].glyph;
    vec2 text_offset = vec2(texelFetch(font_index, _offset + 0).r,
                            texelFetch(font_index, _offset + 1).r);
    vec2 glyph_texture_width = vec2(texelFetch(font_index, _offset + 2).r, 0.0 );
    vec2 glyph_texture_height = vec2(0.0, texelFetch(font_index, _offset + 3).r);

    vec4 bearing = vec4(texelFetch(font_index, _offset + 4).r,
                        -texelFetch(font_index, _offset + 5).r, 0.0, 0.0) * font_size;

    vec4 glyph_width = vec4(texelFetch(font_index, _offset + 6).r, 0.0, 0.0, 0.0) * font_size;
    vec4 glyph_height = vec4(0.0, texelFetch(font_index, _offset + 7).r, 0.0, 0.0) * font_size;

    vec4 padding_x = vec4(padding, 0.0, 0.0, 0.0) * font_size;
    vec4 padding_y = vec4(0.0, padding, 0.0, 0.0) * font_size;
    float skewness = gs_in[0].skewness;

    vec4 p = gl_in[0].gl_Position + vec4(0.0, gs_in[0].y_offset, 0.0, 0.0);
    vec4 _p = p;


    // BL
    _p = p + bearing + glyph_height - padding_x + padding_y;
    _p.x += skewness * (p.y - _p.y);
    gl_Position = projection * _p;
    text_pos = text_offset + glyph_texture_height;
    EmitVertex();

    // BR
    _p = p + bearing + glyph_height + glyph_width + padding_x + padding_y;
    _p.x += skewness * (p.y - _p.y);
    gl_Position = projection * _p;
    text_pos = text_offset + glyph_texture_width + glyph_texture_height;
    EmitVertex();

    // TL
    _p = p + bearing - padding_x - padding_y;
    _p.x += skewness * (p.y - _p.y);
    gl_Position = projection * _p;
    text_pos = text_offset;
    EmitVertex();

    // TR
    _p = p + bearing + glyph_width + padding_x - padding_y;
    _p.x += skewness * (p.y - _p.y);
    gl_Position = projection * _p;
    text_pos = text_offset + glyph_texture_width;
    EmitVertex();

    EndPrimitive();
}
   
)SHADER";

static const char *kmsdf_vertex = R"SHADER(
layout (location = 0) in vec2 vertex;

precision mediump float;
uniform mat4 projection;
uniform vec2 offset;

void main() {
    gl_Position = projection * vec4(vertex.xy + offset, 1.0, 1.0);
}
)SHADER";

static const char *kfont_vertex = R"SHADER(
layout (location = 0) in vec2 vertex;
layout (location = 1) in uvec4 glyph_color;
layout (location = 2) in int glyph_index;
layout (location = 3) in float size;
layout (location = 4) in float y_offset;
layout (location = 5) in float skewness;
layout (location = 6) in float strength;

uniform mat4 projection;

out VS_OUT {
    int glyph;
    vec4 color;
    float size;
    float y_offset;
    float skewness;
    float strength;

} vs_out;

void main() {
    gl_Position = vec4(vertex.xy, 0.0, 1.0);
    vs_out.glyph = glyph_index;
    uvec4 c = glyph_color;
    vs_out.color = vec4(float(c.a) / 255.0, float(c.b) / 255.0,
                        float(c.g) / 255.0, float(c.r) / 255.0);
    vs_out.size = size;
    vs_out.y_offset = y_offset;
    vs_out.skewness = skewness;
    vs_out.strength = strength;
}
)SHADER";

static const char *kmsdf_fragment = R"SHADER(
#define IDX_CURR 0
#define IDX_SHAPE 1
#define IDX_INNER 2
#define IDX_OUTER 3
#define IDX_RED 0
#define IDX_GREEN 1
#define IDX_BLUE 2
#define IDX_NEGATIVE 0
#define IDX_POSITIVE 1
#define IDX_MAX_INNER 0
#define IDX_MAX_OUTER 1


precision mediump float;

precision mediump samplerBuffer;
precision mediump usamplerBuffer;
uniform usamplerBuffer metadata;
uniform samplerBuffer point_data;

#define meta_at(i) texelFetch(metadata, int(i)).r
#define point_at(i) vec2(texelFetch(point_data, 2 * int(i)).r, \
                         texelFetch(point_data, 2 * int(i) + 1).r)

uniform vec2 offset;

uniform vec2 translate;
uniform vec2 scale;
uniform float range;
uniform int meta_offset;
uniform int point_offset;
uniform float glyph_height;

out vec4 color;

const float PI = 3.1415926535897932384626433832795;
const float INFINITY = 3.402823466e+38;

const uint BLACK = 0u;
const uint RED = 1u;
const uint GREEN = 2u;
const uint BLUE = 4u;
const uint YELLOW = RED | GREEN;
const uint MAGENTA = BLUE | RED;
const uint CYAN = BLUE | GREEN;
const uint WHITE = RED | GREEN | BLUE;

struct segment {
    vec3 min_true;
    vec2 mins[2];
    int nearest_points;
    int nearest_npoints;
};

struct workspace {
    segment segments[4 * 3];

    vec3 maximums[2];
    vec3 min_absolute;
};

workspace ws;

vec3 signed_distance_linear(vec2 p0, vec2 p1, vec2 origin);
vec3 signed_distance_quad(vec2 p0, vec2 p1, vec2 p2, vec2 origin);
void add_segment_true_distance(int segment_index, int npoints, int points, vec3 d);
vec3 get_pixel_distance(vec2);

vec2 orthonormal(vec2 v) {float len = length(v); return vec2(v.y / len, -v.x / len);}
float cross_(vec2 a, vec2 b) { return a.x * b.y - a.y * b.x; }
float median(vec3 d) {return max(min(d.r, d.g), min(max(d.r, d.g), d.b));}
void add_segment_pseudo_distance(int segment_index, vec2 d);
vec2 distance_to_pseudo_distance(int npoints, int points, vec3 d, vec2 p);
bool point_facing_edge(int prev_npoints, int prev_points, int cur_npoints, int cur_points,
                       int next_npoints, int next_points, vec2 p, float param);
void add_segment(int prev_npoints, int prev_points, int cur_npoints, int cur_points,
                 int next_npoints, int next_points, uint color, vec2 point);
void set_contour_edge(int winding, vec2 point);
float compute_distance(int segment_index, vec2 point);


bool less(vec2 a, vec2 b) {
    return abs(a.x) < abs(b.x) || (abs(a.x) == abs(b.x) && a.y < b.y);
}


void main() {
    vec2 coords = gl_FragCoord.xy - offset;

    vec2 p = ((coords + 0.49) / scale) - vec2(translate.x, -translate.y);
    p.y  = (glyph_height / scale.y) - p.y;

    ws.maximums[0].r = -INFINITY;
    ws.maximums[1].r = -INFINITY;
    ws.maximums[0].g = -INFINITY;
    ws.maximums[1].g = -INFINITY;
    ws.maximums[0].b = -INFINITY;
    ws.maximums[1].b = -INFINITY;
    ws.min_absolute.r = -INFINITY;
    ws.min_absolute.g = -INFINITY;
    ws.min_absolute.b = -INFINITY;

    for (int i = 0; i < (4 * 3); ++i) {
        ws.segments[i].mins[0].x = -INFINITY;
        ws.segments[i].mins[1].x = -INFINITY;
        ws.segments[i].min_true.x = -INFINITY;
        ws.segments[i].nearest_points = -1;
    }
    int point_index = point_offset;
    int meta_index = meta_offset;


    uint ncontours = meta_at(meta_index++);

    for (uint _i = 0u; _i < ncontours; ++_i) {
        int winding = int(meta_at(meta_index++)) - 1;
        uint nsegments = meta_at(meta_index++);

        uint s_color = meta_at(meta_index);
        uint s_npoints = meta_at(meta_index + 1);

        /** TODO: Move the following checks to the preprocessor, no need to do
                  them for every fragment. */
        /* Ignore empty contours. */
        if (nsegments == 0u) {
            continue;
        }

        /* Ignore contours with just one linear segment, some fonts seem to have them. */
        if (nsegments == 1u && s_npoints == 2u) {
            point_index += 2;
            meta_index += 2;
            continue;
        }

        /* Ignore contours with just two linear segments, some fonts seem to have them. */
        if (nsegments == 2u && s_npoints == 2u && meta_at(meta_index + 3) == 2u) {
            point_index += 4;
            meta_index += 4;
            continue;
        }

        int cur_points = point_index;
        uint cur_color = meta_at(meta_index + 2 * (int(nsegments) - 1));
        uint cur_npoints = meta_at(meta_index + 2 * (int(nsegments) - 1) + 1);


        uint prev_npoints = nsegments >= 2u ?
            meta_at(meta_index + 2 * (int(nsegments) - 2) + 1) : s_npoints;
        int prev_points = point_index;

        for (uint _i = 0u; _i < nsegments - 1u; ++_i) {
            uint npoints = meta_at(meta_index + 2 * int(_i) + 1);
            cur_points += (int(npoints) - 1);
        }

        for (uint _i = 0u; (_i < (nsegments - 2u)) && nsegments >= 2u; ++_i) {
            uint npoints = meta_at(meta_index + 2 * int(_i) + 1);
            prev_points += (int(npoints) - 1);
        }

        for (uint _i = 0u; _i < nsegments; ++_i) {

            add_segment(int(prev_npoints), prev_points, int(cur_npoints), cur_points,
                        int(s_npoints), point_index, cur_color, p);

            prev_points = cur_points;
            prev_npoints = cur_npoints;

            cur_points = point_index;
            cur_npoints = s_npoints;
            cur_color = s_color;

            s_color = meta_at(meta_index++ + 2);
            point_index += (int(s_npoints) - 1);
            s_npoints = meta_at(meta_index++ + 2);
        }
        point_index += 1;

        set_contour_edge(winding, p);
    }

    vec3 d = get_pixel_distance(p);

    color = vec4(d / range + 0.5, 1.0);

    // For testing
    // color = median(color.rgb) > 0.5 ? vec4(1.0, 1.0, 1.0, 1.0) : vec4(0.0, 0.0, 0.0, 1.0);
}

void merge_segment(int s, int other) {
    if (less(ws.segments[other].min_true.xy, ws.segments[s].min_true.xy)) {
        ws.segments[s].min_true = ws.segments[other].min_true;

        ws.segments[s].nearest_npoints = ws.segments[other].nearest_npoints;
        ws.segments[s].nearest_points = ws.segments[other].nearest_points;
    }
    if (less(ws.segments[other].mins[IDX_NEGATIVE], ws.segments[s].mins[IDX_NEGATIVE]))
        ws.segments[s].mins[IDX_NEGATIVE] = ws.segments[other].mins[IDX_NEGATIVE];
    if (less(ws.segments[other].mins[IDX_POSITIVE], ws.segments[s].mins[IDX_POSITIVE])) {
        ws.segments[s].mins[IDX_POSITIVE] = ws.segments[other].mins[IDX_POSITIVE];
    }
}

void merge_multi_segment(int e, int other) {
    merge_segment(e * 3 + IDX_RED, other * 3 + IDX_RED);
    merge_segment(e * 3 + IDX_GREEN, other * 3 + IDX_GREEN);
    merge_segment(e * 3 + IDX_BLUE, other * 3 + IDX_BLUE);
}

void add_segment(int prev_npoints, int prev_points, int cur_npoints, int cur_points,
                 int next_npoints, int next_points, uint s_color, vec2 point) {

    vec3 d;
    if (cur_npoints == 2)
        d = signed_distance_linear(point_at(cur_points),
                                   point_at(cur_points + 1),
                                   point);
    else
        d = signed_distance_quad(point_at(cur_points),
                                 point_at(cur_points + 1),
                                 point_at(cur_points + 2),
                                 point);

    if ((s_color & RED) > 0u)
        add_segment_true_distance(IDX_CURR * 3 + IDX_RED, cur_npoints, cur_points, d);
    if ((s_color & GREEN) > 0u)
        add_segment_true_distance(IDX_CURR * 3 + IDX_GREEN, cur_npoints, cur_points, d);
    if ((s_color & BLUE) > 0u)
        add_segment_true_distance(IDX_CURR * 3 + IDX_BLUE, cur_npoints, cur_points, d);

    if (point_facing_edge(prev_npoints, prev_points, cur_npoints, cur_points,
                          next_npoints, next_points, point, d.z)) {

        vec2 pd = distance_to_pseudo_distance(cur_npoints, cur_points, d, point);
        if ((s_color & RED) > 0u)
            add_segment_pseudo_distance(IDX_CURR * 3 + IDX_RED, pd);
        if ((s_color & GREEN) > 0u)
            add_segment_pseudo_distance(IDX_CURR * 3 + IDX_GREEN, pd);
        if ((s_color & BLUE) > 0u)
            add_segment_pseudo_distance(IDX_CURR * 3 + IDX_BLUE, pd);
    }
}

vec3 get_distance(int segment_index, vec2 point) {
    vec3 d;
    d.r = compute_distance(segment_index * 3 + IDX_RED, point);
    d.g = compute_distance(segment_index * 3 + IDX_GREEN, point);
    d.b = compute_distance(segment_index * 3 + IDX_BLUE, point);
    return d;
}

void set_contour_edge(int winding, vec2 point) {

    vec3 d = get_distance(IDX_CURR, point);

    merge_multi_segment(IDX_SHAPE, IDX_CURR);
    if (winding > 0 && median(d) >= 0.0)
        merge_multi_segment(IDX_INNER, IDX_CURR);
    if (winding < 0 && median(d) <= 0.0)
        merge_multi_segment(IDX_OUTER, IDX_CURR);

    int i = winding < 0 ? IDX_MAX_INNER : IDX_MAX_OUTER;

    ws.maximums[i] = (median(d) > median(ws.maximums[i])) ? d : ws.maximums[i];
    ws.min_absolute = (abs(median(d)) < abs(median(ws.min_absolute))) ? d : ws.min_absolute;
}

vec2 segment_direction(int points, int npoints, float param) {
    return mix(point_at(points + 1) - point_at(points),
               point_at(points + npoints - 1) - point_at(points + npoints - 2),
               param);
}

vec2 segment_point(int points, int npoints, float param) {
    return mix(mix(point_at(points), point_at(points + 1), param),
               mix(point_at(points + npoints - 2), point_at(points + npoints - 1), param),
               param);
}


vec2 distance_to_pseudo_distance(int npoints, int points, vec3 d, vec2 p) {
    if (d.z >= 0.0 && d.z <= 1.0)
        return d.xy;

    vec2 dir = normalize(segment_direction(points, npoints, d.z < 0.0 ? 0.0 : 1.0));
    vec2 aq = p - segment_point(points, npoints, d.z < 0.0 ? 0.0 : 1.0);
    float ts = dot(aq, dir);
    if (d.z < 0.0 ? ts < 0.0 : ts > 0.0) {
        float pseudo_distance = cross_(aq, dir);
        if (abs(pseudo_distance) <= abs(d.x)) {
            d.x = pseudo_distance;
            d.y = 0.0;
        }
    }
    return d.xy;
}

void add_segment_true_distance(int segment_index, int npoints, int points, vec3 d) {
    bool is_less = less(d.xy, ws.segments[segment_index].min_true.xy);
    ws.segments[segment_index].min_true =
        is_less ? d : ws.segments[segment_index].min_true;

    ws.segments[segment_index].nearest_points =
        is_less ? points : ws.segments[segment_index].nearest_points;
    ws.segments[segment_index].nearest_npoints =
        is_less ? npoints : ws.segments[segment_index].nearest_npoints;
}


void add_segment_pseudo_distance(int segment_index, vec2 d) {
    int i = d.x < 0.0 ? IDX_NEGATIVE : IDX_POSITIVE;
    vec2 _d = ws.segments[segment_index].mins[i];
    ws.segments[segment_index].mins[i] = less(d, _d) ? d : _d;
}

bool point_facing_edge(int prev_npoints, int prev_points, int cur_npoints, int cur_points,
                       int next_npoints, int next_points, vec2 p, float param) {

    if (param >= 0.0 && param <= 1.0)
        return true;

    vec2 prev_edge_dir = -normalize(segment_direction(prev_points, prev_npoints, 1.0));
    vec2 edge_dir =
        normalize(segment_direction(cur_points, cur_npoints, param < 0.0 ? 0.0 : 1.0)) *
        (param < 0.0 ? 1.0 : -1.0);
    vec2 next_edge_dir = normalize(segment_direction(next_points, next_npoints, 0.0));
    vec2 point_dir = p - segment_point(cur_points, cur_npoints, param < 0.0 ? 0.0 : 1.0);
    return dot(point_dir, edge_dir) >=
           dot(point_dir, param < 0.0 ? prev_edge_dir : next_edge_dir);
}

float compute_distance(int segment_index, vec2 point) {

    int i = ws.segments[segment_index].min_true.xy.x < 0.0 ? IDX_NEGATIVE : IDX_POSITIVE;
    float min_distance = ws.segments[segment_index].mins[i].x;

    if (ws.segments[segment_index].nearest_points == -1) return min_distance;
    vec2 d = distance_to_pseudo_distance(ws.segments[segment_index].nearest_npoints,
                                         ws.segments[segment_index].nearest_points,
                                         ws.segments[segment_index].min_true, point);
    min_distance = abs(d.x) < abs(min_distance) ? d.x : min_distance;

    return min_distance;
}

vec3 signed_distance_linear(vec2 p0, vec2 p1, vec2 origin) {
    vec2 aq = origin - p0;
    vec2 ab = p1 - p0;
    float param = dot(aq, ab) / dot(ab, ab);
    vec2 eq = (param > .5 ? p1 : p0) - origin;
    float endpoint_distance = length(eq);
    if (param > 0.0 && param < 1.0) {
        float ortho_distance = dot(orthonormal(ab), aq);
        if (abs(ortho_distance) < endpoint_distance)
            return vec3(ortho_distance, 0, param);
    }
    return vec3(sign(cross_(aq, ab)) *endpoint_distance,
                abs(dot(normalize(ab), normalize(eq))),
                param);
}

vec3 signed_distance_quad(vec2 p0, vec2 p1, vec2 p2, vec2 origin) {
    vec2 qa = p0 - origin;
    vec2 ab = p1 - p0;
    vec2 br = p2 - p1 - ab;
    float a = dot(br, br);
    float b = 3.0 * dot(ab, br);
    float c = 2.0 * dot(ab, ab) + dot(qa, br);
    float d = dot(qa, ab);
    float coeffs[3];
    float _a = b / a;
    int solutions;

    float a2 = _a * _a;
    float q = (a2 - 3.0 * (c / a)) / 9.0;
    float r = (_a * (2.0 * a2 - 9.0 * (c / a)) + 27.0 * (d / a)) / 54.0;
    float r2 = r * r;
    float q3 = q * q * q;
    float A, B;
    _a /= 3.0;
    float t = r / sqrt(q3);
    t = t < -1.0 ? -1.0 : t;
    t = t > 1.0 ? 1.0 : t;
    t = acos(t);
    A = -pow(abs(r) + sqrt(r2 - q3), 1.0 / 3.0);
    A = r < 0.0 ? -A : A;
    B = A == 0.0 ? 0.0 : q / A;
    if (r2 < q3) {
        q = -2.0 * sqrt(q);
        coeffs[0] = q * cos(t / 3.0) - _a;
        coeffs[1] = q * cos((t + 2.0 * PI) / 3.0) - _a;
        coeffs[2] = q * cos((t - 2.0 * PI) / 3.0) - _a;
        solutions = 3;
    } else {
        coeffs[0] = (A + B) - _a;
        coeffs[1] = -0.5 * (A + B) - _a;
        coeffs[2] = 0.5 * sqrt(3.0) * (A - B);
        solutions = abs(coeffs[2]) < 1.0e-14 ? 2 : 1;
    }

    float min_distance = sign(cross_(ab, qa)) * length(qa); // distance from A
    float param = -dot(qa, ab) / dot(ab, ab);
    float distance = sign(cross_(p2 - p1, p2 - origin)) * length(p2 - origin); // distance from B
    if (abs(distance) < abs(min_distance)) {
        min_distance = distance;
        param = dot(origin - p1, p2 - p1) / dot(p2 - p1, p2 - p1);
    }
    for (int i = 0; i < solutions; ++i) {
        if (coeffs[i] > 0.0 && coeffs[i] < 1.0) {
            vec2 endpoint = p0 + ab * 2.0 * coeffs[i] + br * coeffs[i] * coeffs[i];
            float distance = sign(cross_(p2 - p0, endpoint - origin)) * length(endpoint - origin);
            if (abs(distance) <= abs(min_distance)) {
                min_distance = distance;
                param = coeffs[i];
            }
        }
    }
    vec2 v = vec2(min_distance, 0.0);
    v.y = param > 1.0 ? abs(dot(normalize(p2 - p1), normalize(p2 - origin))) : v.y;
    v.y = param < 0.0 ? abs(dot(normalize(ab), normalize(qa))) : v.y;

    return vec3(v, param);
}

vec3 get_pixel_distance(vec2 point) {
    vec3 shape_distance = get_distance(IDX_SHAPE, point);
    vec3 inner_distance = get_distance(IDX_INNER, point);
    vec3 outer_distance = get_distance(IDX_OUTER, point);
    float inner_d = median(inner_distance);
    float outer_d = median(outer_distance);

    bool inner = inner_d >= 0.0 && abs(inner_d) <= abs(outer_d);
    bool outer = outer_d <= 0.0 && abs(outer_d) < abs(inner_d);
    if (!inner && !outer)
        return shape_distance;

    vec3 d = inner ? inner_distance : outer_distance;
    vec3 contour_distance = ws.maximums[inner ? IDX_MAX_INNER : IDX_MAX_OUTER];

    float contour_d = median(contour_distance);
    d = (abs(contour_d) < abs(outer_d) && contour_d > median(d)) ? contour_distance : d;

    contour_distance = ws.min_absolute;
    contour_d = median(contour_distance);
    float d_d = median(d);

    d = abs(contour_d) < abs(d_d) ? contour_distance : d;
    d = median(d) == median(shape_distance) ? shape_distance : d;

    return d;
}
)SHADER";

// serializer //

namespace {
constexpr const float kserializer_scale = 64;
static_assert(kserializer_scale >= 8);
enum class SeriColor : int {
    Black = 0,
    Red = 1,
    Green = 2,
    Yellow = 3,
    Blue = 4,
    Magenta = 5,
    Cyan = 6,
    White = 7
};
struct vec2 {
    float x;
    float y;
};
namespace outline_functions {
struct GlyphLenCtx {
    int meta_size;
    int data_size;
};

int AddContourSize(const FT_Vector *to, void *user) {
    (void)to;
    struct GlyphLenCtx *ctx = (struct GlyphLenCtx *)user;
    ctx->data_size += 1;
    ctx->meta_size += 2; /* winding + nsegments */
    return 0;
}
int AddLinearSize(const FT_Vector *to, void *user) {
    (void)to;
    struct GlyphLenCtx *ctx = (struct GlyphLenCtx *)user;
    ctx->data_size += 1;
    ctx->meta_size += 2; /* color + npoints */
    return 0;
}
int AddQuadSize(const FT_Vector *control, const FT_Vector *to, void *user) {
    (void)control;
    (void)to;
    struct GlyphLenCtx *ctx = (struct GlyphLenCtx *)user;
    ctx->data_size += 2;
    ctx->meta_size += 2; /* color + npoints */
    return 0;
}
int AddCubicSize(const FT_Vector *control1, const FT_Vector *control2,
                 const FT_Vector *to, void *s) {
    (void)control1;
    (void)control2;
    (void)to;
    (void)s;
    fprintf(stderr, "Cubic segments not supported\n");
    return -1;
}
struct glyph_data_ctx {
    int meta_index;
    char *meta_buffer;

    vec2 *segment;
    int nsegments_index;
};

int AddContour(const FT_Vector *to, void *user) {
    struct glyph_data_ctx *ctx = (struct glyph_data_ctx *)user;
    ctx->segment += 1; /* Start contour on a fresh glyph. */
    ctx->segment[0].x = to->x / kserializer_scale;
    ctx->segment[0].y = to->y / kserializer_scale;
    ctx->meta_buffer[0] += 1;                /* Increase the number of contours. */
    ctx->meta_buffer[ctx->meta_index++] = 0; /* Set winding to zero */
    ctx->nsegments_index = ctx->meta_index++;
    ctx->meta_buffer[ctx->nsegments_index] = 0;

    return 0;
}
int AddLinear(const FT_Vector *to, void *user) {
    struct glyph_data_ctx *ctx = (struct glyph_data_ctx *)user;
    ctx->segment[1].x = to->x / kserializer_scale;
    ctx->segment[1].y = to->y / kserializer_scale;

    /* Some glyphs contain zero-dimensional segments, ignore those. */
    if (ctx->segment[1].x == ctx->segment[0].x && ctx->segment[1].y == ctx->segment[0].y)
        return 0;

    ctx->segment += 1;

    ctx->meta_buffer[ctx->meta_index++] = 0; /* Set color to 0 */
    ctx->meta_buffer[ctx->meta_index++] = 2;
    ctx->meta_buffer[ctx->nsegments_index]++;
    return 0;
}
int AddQuad(const FT_Vector *control, const FT_Vector *to, void *user) {
    struct glyph_data_ctx *ctx = (struct glyph_data_ctx *)user;

    ctx->segment[1].x = control->x / kserializer_scale;
    ctx->segment[1].y = control->y / kserializer_scale;
    ctx->segment[2].x = to->x / kserializer_scale;
    ctx->segment[2].y = to->y / kserializer_scale;

    /* Some glyphs contain "bugs", where a quad segment is actually a linear
       segment with a double point. Treat it as a linear segment. */
    if ((ctx->segment[1].x == ctx->segment[0].x &&
         ctx->segment[1].y == ctx->segment[0].y) ||
        (ctx->segment[2].x == ctx->segment[1].x &&
         ctx->segment[2].y == ctx->segment[1].y))
        return AddLinear(to, user);

    ctx->segment += 2;

    ctx->meta_buffer[ctx->meta_index++] = 0; /* Set color to 0 */
    ctx->meta_buffer[ctx->meta_index++] = 3;
    ctx->meta_buffer[ctx->nsegments_index]++;
    return 0;
}
}  // namespace outline_functions
constexpr const SeriColor start[3] = {SeriColor::Cyan, SeriColor::Magenta,
                                      SeriColor::Yellow};
void SwitchSeriColor(enum SeriColor *color, unsigned long long *seed,
                     enum SeriColor *_banned) {
    enum SeriColor banned = _banned ? *_banned : SeriColor::Black;
    enum SeriColor combined =
        (SeriColor)(static_cast<int>(*color) & static_cast<int>(banned));

    if (combined == SeriColor::Red || combined == SeriColor::Green ||
        combined == SeriColor::Blue) {
        *color =
            (SeriColor)(static_cast<int>(combined) ^ static_cast<int>(SeriColor::White));
        return;
    }
    if (*color == SeriColor::Black || *color == SeriColor::White) {
        *color = start[*seed % 3];
        *seed /= 3;
        return;
    }
    int shifted = static_cast<int>(*color) << (1 + (*seed & 1));
    *color = (SeriColor)((static_cast<int>(shifted) | static_cast<int>(shifted) >> 3) &
                         static_cast<int>(SeriColor::White));
    *seed >>= 1;
}
inline vec2 Mix(const vec2 a, const vec2 b, float weight) {
    return {a.x * (1.0f - weight) + b.x * weight, a.y * (1.0f - weight) + b.y * weight};
}
inline vec2 Subt(vec2 p1, vec2 p2) { return {p1.x - p2.x, p1.y - p2.y}; }
inline float Length(const vec2 v) { return (float)sqrt(v.x * v.x + v.y * v.y); }
inline vec2 Divide(const vec2 v, float f) { return {v.x / f, v.y / f}; }
inline float Cross(vec2 a, vec2 b) { return a.x * b.y - a.y * b.x; }
inline float Dot(vec2 a, vec2 b) { return a.x * b.x + a.y * b.y; }
inline bool IsCorner(const vec2 a, const vec2 b, float cross_threshold) {
    return Dot(a, b) <= 0 || fabs(Cross(a, b)) > cross_threshold;
}
inline vec2 Normalize(vec2 v) { return Divide(v, Length(v)); }
inline vec2 SegmentDirection(const vec2 *points, int npoints, float param) {
    return Mix(Subt(points[1], points[0]), Subt(points[npoints - 1], points[npoints - 2]),
               param);
}
inline vec2 SegmentPoint(const vec2 *points, int npoints, float param) {
    return Mix(Mix(points[0], points[1], param),
               Mix(points[npoints - 2], points[npoints - 1], param), param);
}
inline float ShoeLace(const vec2 a, const vec2 b) { return (b.x - a.x) * (a.y + b.y); }
void SerializeGlyph(FT_Face face, int code, char *meta_buffer,
                    float *point_buffer) noexcept {
    const auto load_char_err = !FT_Load_Char(face, code, FT_LOAD_NO_SCALE);
    assert("Failed to load char" && load_char_err);

    FT_Outline_Funcs fns;
    fns.shift = 0;
    fns.delta = 0;
    fns.move_to = outline_functions::AddContour;
    fns.line_to = outline_functions::AddLinear;
    fns.conic_to = outline_functions::AddQuad;
    fns.cubic_to = 0;

    struct outline_functions::glyph_data_ctx ctx;
    ctx.meta_buffer = meta_buffer;
    ctx.meta_index = 1;
    ctx.meta_buffer[0] = 0;
    /* Start 1 before the actual buffer. The pointer is moved in the move_to
       callback. FT_Outline_Decompose does not have a callback for finishing a
       contour. */
    ctx.segment = ((vec2 *)&point_buffer[0]) - 1;

    const auto decompose_err = !FT_Outline_Decompose(&face->glyph->outline, &fns, &ctx);
    assert("FT_Outline_Decompos failed" && decompose_err);

    /* Calculate windings. */
    int meta_index = 0;
    vec2 *point_ptr = (vec2 *)&point_buffer[0];

    int ncontours = meta_buffer[meta_index++];
    for (int i = 0; i < ncontours; ++i) {
        int winding_index = meta_index++;
        int nsegments = meta_buffer[meta_index++];

        float total = 0;
        if (nsegments == 1) {
            int npoints = meta_buffer[meta_index + 1];
            vec2 a = SegmentPoint(point_ptr, npoints, 0);
            vec2 b = SegmentPoint(point_ptr, npoints, 1 / 3.0f);
            vec2 c = SegmentPoint(point_ptr, npoints, 2 / 3.0f);
            total += ShoeLace(a, b);
            total += ShoeLace(b, c);
            total += ShoeLace(c, a);

            point_ptr += npoints - 1;
            meta_index += 2;

        } else if (nsegments == 2) {
            int npoints = meta_buffer[meta_index + 1];
            vec2 a = SegmentPoint(point_ptr, npoints, 0);
            vec2 b = SegmentPoint(point_ptr, npoints, 0.5);
            point_ptr += npoints - 1;
            meta_index += 2;
            npoints = meta_buffer[meta_index + 1];
            vec2 c = SegmentPoint(point_ptr, npoints, 0);
            vec2 d = SegmentPoint(point_ptr, npoints, 0.5);
            total += ShoeLace(a, b);
            total += ShoeLace(b, c);
            total += ShoeLace(c, d);
            total += ShoeLace(d, a);

            point_ptr += npoints - 1;
            meta_index += 2;
        } else {
            int prev_npoints = meta_buffer[meta_index + 2 * (nsegments - 2) + 1];
            vec2 *prev_ptr = point_ptr;
            for (int j = 0; j < nsegments - 1; ++j) {
                int _npoints = meta_buffer[meta_index + 2 * j + 1];
                prev_ptr += (_npoints - 1);
            }
            vec2 prev = SegmentPoint(prev_ptr, prev_npoints, 0);

            for (int j = 0; j < nsegments; ++j) {
                meta_index++; /* SeriColor, leave empty here. */
                int npoints = meta_buffer[meta_index++];

                vec2 cur = SegmentPoint(point_ptr, npoints, 0);

                total += ShoeLace(prev, cur);
                point_ptr += (npoints - 1);
                prev = cur;
            }
        }
        point_ptr += 1;
        meta_buffer[winding_index] = total > 0 ? 2 : 0;
    }

    /* Calculate coloring */
    float cross_threshold = (float)sin(3.0);
    unsigned long long seed = 0;

    meta_index = 0;
    point_ptr = (vec2 *)&point_buffer[0];

    int corners[30];
    int len_corners = 0;

    ncontours = meta_buffer[meta_index++];
    for (int i = 0; i < ncontours; ++i) {
        meta_index++; /* Winding */
        int nsegments = meta_buffer[meta_index++];
        int _meta = meta_index;
        vec2 *_point = point_ptr;

        len_corners = 0; /*clear*/

        if (nsegments) {
            int prev_npoints = meta_buffer[meta_index + 2 * (nsegments - 2) + 1];
            vec2 *prev_ptr = point_ptr;
            for (int j = 0; j < nsegments - 1; ++j)
                prev_ptr += (meta_buffer[meta_index + 2 * j + 1] - 1);
            vec2 prev_direction = SegmentDirection(prev_ptr, prev_npoints, 1);
            int index = 0;
            vec2 *cur_points = point_ptr;
            for (int j = 0; j < nsegments; ++j, ++index) {
                meta_index++; /* SeriColor, leave empty here. */
                int npoints = meta_buffer[meta_index++];

                vec2 cur_direction = SegmentDirection(cur_points, npoints, 0.0);
                vec2 new_prev_direction = SegmentDirection(cur_points, npoints, 1.0);

                if (IsCorner(Normalize(prev_direction), Normalize(cur_direction),
                             cross_threshold))
                    corners[len_corners++] = index;
                cur_points += (npoints - 1);
                prev_direction = new_prev_direction;
            }
        }

        /* Restore state */
        meta_index = _meta;
        point_ptr = _point;

        if (!len_corners) {
            /* Smooth contour */
            for (int j = 0; j < nsegments; ++j) {
                meta_buffer[meta_index++] = static_cast<int>(SeriColor::White);
                meta_index++; /* npoints */
            }
        } else if (len_corners == 1) {
            /* Teardrop */
            enum SeriColor colors[3] = {SeriColor::White, SeriColor::White};
            SwitchSeriColor(&colors[0], &seed, NULL);
            colors[2] = colors[0];
            SwitchSeriColor(&colors[2], &seed, NULL);

            int corner = corners[0];
            if (nsegments >= 3) {
                int m = nsegments;
                for (int i = 0; i < m; ++i) {
                    enum SeriColor c =
                        (colors + 1)[(int)(3 + 2.875 * i / (m - 1) - 1.4375 + .5) - 3];
                    meta_buffer[meta_index + 2 * ((corner + i) % m)] = (char)c;
                }
            } else if (nsegments >= 1) {
                /* TODO: whoa, split in thirds and stuff */
                fprintf(stderr, "Non-supported shape\n");
            }
        } else {
            /* Multiple corners. */
            int corner_count = len_corners;
            int spline = 0;
            int start = corners[0];
            int m = nsegments;
            enum SeriColor color = SeriColor::White;
            SwitchSeriColor(&color, &seed, NULL);
            enum SeriColor initial_color = color;
            for (int i = 0; i < m; ++i) {
                int index = (start + i) % m;

                if (spline + 1 < corner_count && corners[spline + 1] == index) {
                    ++spline;
                    enum SeriColor banned = (enum SeriColor)(
                        (spline == corner_count - 1) * static_cast<int>(initial_color));
                    SwitchSeriColor(&color, &seed, &banned);
                }
                meta_buffer[meta_index + 2 * index] = (char)color;
            }
        }

        /* Restore state */
        meta_index = _meta;
        point_ptr = _point;

        for (int j = 0; j < nsegments; ++j) {
            meta_index++;
            point_ptr += (meta_buffer[meta_index++] - 1);
        }
        point_ptr += 1;
    }
}

/* We need two rounds of decomposing, the first one will just figure out
   how much space we need to serialize the glyph, and the second one
   serializes it and generates colour mapping for the segments. */
void GlyphBufferSize(FT_Face face, int code, size_t *meta_size,
                     size_t *point_size) noexcept {
    const auto load_char_err = !FT_Load_Char(face, code, FT_LOAD_NO_SCALE);
    assert("Failed to load char" && load_char_err);

    FT_Outline_Funcs fns;
    fns.shift = 0;
    fns.delta = 0;
    fns.move_to = outline_functions::AddContourSize;
    fns.line_to = outline_functions::AddLinearSize;
    fns.conic_to = outline_functions::AddQuadSize;
    fns.cubic_to = outline_functions::AddCubicSize;
    struct outline_functions::GlyphLenCtx ctx = {1, 0};
    auto decompose_err = !FT_Outline_Decompose(&face->glyph->outline, &fns, &ctx);
    assert("FT_Outline_Decompos failed" && decompose_err);

    *meta_size = ctx.meta_size;
    *point_size = ctx.data_size * 2 * sizeof(float);
}
}  // namespace
// field fusion //
namespace {
struct UseFree {
    void operator()(void *x) { free(x); }
};
struct IndexEntry {
    float offset_x;
    float offset_y;
    float size_x;
    float size_y;
    float bearing_x;
    float bearing_y;
    float glyph_width;
    float glyph_height;
};
const GLfloat kmat4_zero_init[4][4] = {{0.0f, 0.0f, 0.0f, 0.0f},
                                       {0.0f, 0.0f, 0.0f, 0.0f},
                                       {0.0f, 0.0f, 0.0f, 0.0f},
                                       {0.0f, 0.0f, 0.0f, 0.0f}};
bool CompileShader(const char *source, GLenum type, GLuint *shader, const char *version) {
    /* Default to version */
    if (!version) version = "330 core";

    *shader = glCreateShader(type);
    if (!*shader) {
        fprintf(stderr, "failed to create shader\n");
    }

    const char *src[] = {"#version ", version, "\n", source};

    glShaderSource(*shader, 4, src, NULL);
    glCompileShader(*shader);

    GLint status;
    glGetShaderiv(*shader, GL_COMPILE_STATUS, &status);
    if (!status) {
        char log[1024];
        GLsizei len;
        glGetShaderInfoLog(*shader, 1024, &len, log);
        fprintf(stderr, "Error: compiling: %*s\n", len, log);
        return false;
    }

    return true;
}

void GenExtendedAscii(const ff_font_handle_t font_handle) noexcept {
    std::vector<char32_t> codepoints(0xE0);
    std::iota(codepoints.begin(), codepoints.end(), 0);
    ff_gen_glyphs(font_handle, codepoints);
}

struct Uniforms {
    int window_projection;
    int font_atlas_projection;
    int index;
    int atlas;
    int padding;
    int offset;
    int dpi;
    int units_per_em;
    int atlas_projection;
    int texture_offset;
    int translate;
    int scale;
    int range;
    int glyph_height;
    int meta_offset;
    int point_offset;
    int metadata;
    int point_data;
};

static FT_Library _ft_library;
static float _dpi[2];
static uint _gen_shader;
static uint _render_shader;
static Uniforms _uniforms;
static uint _bbox_vao;
static uint _bbox_vbo;
static int _max_texture_size;
static size_t _max_handle{0};
static std::unordered_map<ff_font_handle_t, ff_font_texture_pack_t> _fonts;
}  // namespace

void ff_ortho(float left, float right, float bottom, float top, float nearVal,
             float farVal, float dest[][4]) {
    GLfloat rl, tb, fn;

    memcpy(dest, kmat4_zero_init, sizeof(kmat4_zero_init));

    rl = 1.0f / (right - left);
    tb = 1.0f / (top - bottom);
    fn = -1.0f / (farVal - nearVal);

    dest[0][0] = 2.0f * rl;
    dest[1][1] = 2.0f * tb;
    dest[2][2] = 2.0f * fn;
    dest[3][0] = -(right + left) * rl;
    dest[3][1] = -(top + bottom) * tb;
    dest[3][2] = (farVal + nearVal) * fn;
    dest[3][3] = 1.0f;
}

void ff_initialize(const char *version) noexcept {
    const FT_Error error = FT_Init_FreeType(&_ft_library);
    assert("Failed to initialize freetype2" && !error);

    glGetIntegerv(GL_MAX_TEXTURE_SIZE, &_max_texture_size);

    unsigned vertex_shader, geometry_shader, fragment_shader;
    auto err = CompileShader(kmsdf_vertex, GL_VERTEX_SHADER, &vertex_shader, version);
    assert("Failed to compile msdf vertex shader" && err);
    err = CompileShader(kmsdf_fragment, GL_FRAGMENT_SHADER, &fragment_shader, version);
    assert("Failed to compile msdf fragment shader" && err);
    err = (_gen_shader = glCreateProgram());
    assert("Failed to generate shader program");

    glAttachShader(_gen_shader, vertex_shader);
    glAttachShader(_gen_shader, fragment_shader);
    glLinkProgram(_gen_shader);
    glDeleteShader(vertex_shader);
    glDeleteShader(fragment_shader);

    auto link_status = 0;
    glGetProgramiv(_gen_shader, GL_LINK_STATUS, &link_status);
    assert("Failed to link _gen_shader" && link_status);

    _uniforms.atlas_projection = glGetUniformLocation(_gen_shader, "projection");
    _uniforms.texture_offset = glGetUniformLocation(_gen_shader, "offset");
    _uniforms.glyph_height = glGetUniformLocation(_gen_shader, "glyph_height");
    _uniforms.translate = glGetUniformLocation(_gen_shader, "translate");
    _uniforms.scale = glGetUniformLocation(_gen_shader, "scale");
    _uniforms.range = glGetUniformLocation(_gen_shader, "range");
    _uniforms.meta_offset = glGetUniformLocation(_gen_shader, "meta_offset");
    _uniforms.point_offset = glGetUniformLocation(_gen_shader, "point_offset");
    _uniforms.metadata = glGetUniformLocation(_gen_shader, "metadata");
    _uniforms.point_data = glGetUniformLocation(_gen_shader, "point_data");

    CompileShader(kfont_vertex, GL_VERTEX_SHADER, &vertex_shader, version);
    err = CompileShader(kfont_geometry, GL_GEOMETRY_SHADER, &geometry_shader, version);
    assert("Failed to compile geometry shader" && err);
    err = CompileShader(kfont_fragment, GL_FRAGMENT_SHADER, &fragment_shader, version);
    assert("Failed to compile font fragment shader" && err);
    err = (_render_shader = glCreateProgram());
    assert("Faile to create _render_shader" && err);

    glAttachShader(_render_shader, vertex_shader);
    glAttachShader(_render_shader, geometry_shader);
    glAttachShader(_render_shader, fragment_shader);
    glLinkProgram(_render_shader);
    glDeleteShader(vertex_shader);
    glDeleteShader(geometry_shader);
    glDeleteShader(fragment_shader);

    glGetProgramiv(_render_shader, GL_LINK_STATUS, &link_status);
    assert("Failed to link _render_shader" && link_status);

    _uniforms.window_projection = glGetUniformLocation(_render_shader, "projection");
    _uniforms.font_atlas_projection =
        glGetUniformLocation(_render_shader, "font_projection");
    _uniforms.index = glGetUniformLocation(_render_shader, "font_index");
    _uniforms.atlas = glGetUniformLocation(_render_shader, "font_atlas");
    _uniforms.padding = glGetUniformLocation(_render_shader, "padding");
    _uniforms.dpi = glGetUniformLocation(_render_shader, "dpi");
    _uniforms.units_per_em = glGetUniformLocation(_render_shader, "units_per_em");
    _dpi[0] = 72.0;
    _dpi[1] = 72.0;

    glGenVertexArrays(1, &_bbox_vao);
    glGenBuffers(1, &_bbox_vbo);
    glBindBuffer(GL_ARRAY_BUFFER, _bbox_vbo);
    glBufferData(GL_ARRAY_BUFFER, 12 * sizeof(float), 0, GL_STREAM_READ);
    glBindBuffer(GL_ARRAY_BUFFER, 0);
}

[[nodiscard]] ff_font_handle_t ff_new_font(const char *path, const float scale,
                                     const float range, const int texture_width,
                                     const int texture_padding) noexcept {
    auto handle = _max_handle;
    _fonts[handle] = {};
    auto &font = _fonts[handle].font;
    auto &atlas = _fonts[handle].atlas;
    font.font_path = path;
    font.scale = scale;
    font.range = range;

    atlas.texture_width = texture_width;
    atlas.padding = texture_padding;
    glGenBuffers(1, &atlas.index_buffer);
    glGenTextures(1, &atlas.index_texture);
    glGenTextures(1, &atlas.atlas_texture);
    glGenFramebuffers(1, &atlas.atlas_framebuffer);

    auto err = !FT_New_Face(_ft_library, path, 0, &font.face);
    assert("Failed to create a new font face, font path is probably invalid" && err);
    FT_Select_Charmap(font.face, ft_encoding_unicode);
    font.vertical_advance = (float)(font.face->ascender - font.face->descender);

    glGenBuffers(1, &font.meta_input_buffer);
    glGenBuffers(1, &font.point_input_buffer);
    glGenTextures(1, &font.meta_input_texture);
    glGenTextures(1, &font.point_input_texture);
    GenExtendedAscii(handle);
    _max_handle += 1;
    return handle;
}

void ff_remove_font(const ff_font_handle_t handle) noexcept {
    {
        auto &fpack = _fonts.at(handle);
        FT_Done_Face(fpack.font.face);
        glDeleteBuffers(1, &fpack.font.meta_input_buffer);
        glDeleteBuffers(1, &fpack.font.point_input_buffer);
        glDeleteBuffers(1, &fpack.font.meta_input_texture);
        glDeleteBuffers(1, &fpack.font.point_input_texture);
        glDeleteBuffers(1, &fpack.atlas.index_buffer);
        glDeleteTextures(1, &fpack.atlas.index_texture);
        glDeleteTextures(1, &fpack.atlas.atlas_texture);
        glDeleteFramebuffers(1, &fpack.atlas.atlas_framebuffer);
    }
    _fonts.erase(handle);
}

void ff_gen_glyphs(const ff_font_handle_t font_handle,
                 const std::vector<char32_t> &codepoints) noexcept {
    auto &fpack = _fonts.at(font_handle);
    GLint original_viewport[4];
    glGetIntegerv(GL_VIEWPORT, original_viewport);
    int nrender = codepoints.size();

    if (nrender <= 0) return;

    /* Calculate the amount of memory needed on the GPU.*/
    std::unique_ptr<size_t[]> meta_sizes(new size_t[nrender]());
    std::unique_ptr<size_t[]> point_sizes(new size_t[nrender]());

    /* We will start with a square texture. */
    int new_texture_height = fpack.atlas.texture_height ? fpack.atlas.texture_height : 1;
    int new_index_size = fpack.atlas.nallocated ? fpack.atlas.nallocated : 1;

    /* Amount of new memory needed for the index. */
    std::unique_ptr<IndexEntry[]> atlas_index(new IndexEntry[nrender]());

    size_t meta_size_sum = 0, point_size_sum = 0;
    for (size_t i = 0; (int)i < (int)nrender; ++i) {  // MARK
        int code = codepoints[i];
        GlyphBufferSize(fpack.font.face, code, &meta_sizes[i], &point_sizes[i]);
        meta_size_sum += meta_sizes[i];
        point_size_sum += point_sizes[i];
    }

    /* Allocate the calculated amount. */
    std::unique_ptr<char[], UseFree> point_data((char *)calloc(point_size_sum, 1));
    std::unique_ptr<char[], UseFree> metadata((char *)calloc(meta_size_sum, 1));

    /* Serialize the glyphs into RAM. */
    char *meta_ptr = metadata.get();
    char *point_ptr = point_data.get();
    for (size_t i = 0; (int)i < (int)nrender; ++i) {
        float buffer_width, buffer_height;

        int code = codepoints[i];
        SerializeGlyph(fpack.font.face, code, meta_ptr, (GLfloat *)point_ptr);
        auto m = fpack.font.character_index.insert(code);
        m->advance[0] = (float)fpack.font.face->glyph->metrics.horiAdvance;
        m->advance[1] = (float)fpack.font.face->glyph->metrics.vertAdvance;

        buffer_width =
            fpack.font.face->glyph->metrics.width / kserializer_scale + fpack.font.range;
        buffer_height =
            fpack.font.face->glyph->metrics.height / kserializer_scale + fpack.font.range;
        buffer_width *= fpack.font.scale;
        buffer_height *= fpack.font.scale;

        meta_ptr += meta_sizes[i];
        point_ptr += point_sizes[i];

        if (fpack.atlas.offset_x + buffer_width > fpack.atlas.texture_width) {
            fpack.atlas.offset_y += (fpack.atlas.y_increment + fpack.atlas.padding);
            fpack.atlas.offset_x = 1;
            fpack.atlas.y_increment = 0;
        }
        fpack.atlas.y_increment = (size_t)buffer_height > fpack.atlas.y_increment
                                      ? (size_t)buffer_height
                                      : fpack.atlas.y_increment;

        atlas_index[i].offset_x = fpack.atlas.offset_x;
        atlas_index[i].offset_y = fpack.atlas.offset_y;
        atlas_index[i].size_x = buffer_width;
        atlas_index[i].size_y = buffer_height;
        atlas_index[i].bearing_x = fpack.font.face->glyph->metrics.horiBearingX;
        atlas_index[i].bearing_y = fpack.font.face->glyph->metrics.horiBearingY;
        atlas_index[i].glyph_width = fpack.font.face->glyph->metrics.width;
        atlas_index[i].glyph_height = fpack.font.face->glyph->metrics.height;

        fpack.atlas.offset_x += (size_t)buffer_width + fpack.atlas.padding;

        while ((fpack.atlas.offset_y + buffer_height) > new_texture_height) {
            new_texture_height *= 2;
        }
        assert("Exceeded maximum texture size" &&
               new_texture_height <= _max_texture_size);

        while ((int)(fpack.atlas.nglyphs + i) >= new_index_size) {
            new_index_size *= 2;
        }
    }

    /* Allocate and fill the buffers on GPU. */
    glBindBuffer(GL_ARRAY_BUFFER, fpack.font.meta_input_buffer);
    glBufferData(GL_ARRAY_BUFFER, meta_size_sum, metadata.get(), GL_DYNAMIC_READ);

    glBindBuffer(GL_ARRAY_BUFFER, fpack.font.point_input_buffer);
    glBufferData(GL_ARRAY_BUFFER, point_size_sum, point_data.get(), GL_DYNAMIC_READ);

    if ((int)fpack.atlas.nallocated == new_index_size) {
        glBindBuffer(GL_ARRAY_BUFFER, fpack.atlas.index_buffer);
    } else {
        GLuint new_buffer;
        glGenBuffers(1, &new_buffer);
        glBindBuffer(GL_ARRAY_BUFFER, new_buffer);
        glBufferData(GL_ARRAY_BUFFER, sizeof(IndexEntry) * new_index_size, 0,
                     GL_DYNAMIC_READ);
        assert("Out of gpu memory" && glGetError() != GL_OUT_OF_MEMORY);
        if (fpack.atlas.nglyphs) {
            glBindBuffer(GL_COPY_READ_BUFFER, fpack.atlas.index_buffer);
            glCopyBufferSubData(GL_COPY_READ_BUFFER, GL_ARRAY_BUFFER, 0, 0,
                                fpack.atlas.nglyphs * sizeof(IndexEntry));
            glBindBuffer(GL_COPY_READ_BUFFER, 0);
        }
        fpack.atlas.nallocated = new_index_size;
        glDeleteBuffers(1, &fpack.atlas.index_buffer);
        fpack.atlas.index_buffer = new_buffer;
    }
    const size_t index_size = nrender * sizeof(IndexEntry);
    glBufferSubData(GL_ARRAY_BUFFER, sizeof(IndexEntry) * fpack.atlas.nglyphs, index_size,
                    atlas_index.get());

    glBindBuffer(GL_ARRAY_BUFFER, 0);

    /* Link sampler textures to the buffers. */
    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_BUFFER, fpack.font.meta_input_texture);
    glTexBuffer(GL_TEXTURE_BUFFER, GL_R8UI, fpack.font.meta_input_buffer);
    glBindTexture(GL_TEXTURE_BUFFER, 0);

    glActiveTexture(GL_TEXTURE1);
    glBindTexture(GL_TEXTURE_BUFFER, fpack.font.point_input_texture);
    glTexBuffer(GL_TEXTURE_BUFFER, GL_R32F, fpack.font.point_input_buffer);
    glBindTexture(GL_TEXTURE_BUFFER, 0);

    glActiveTexture(GL_TEXTURE2);
    glBindTexture(GL_TEXTURE_BUFFER, fpack.atlas.index_texture);
    glTexBuffer(GL_TEXTURE_BUFFER, GL_R32F, fpack.atlas.index_buffer);
    glBindTexture(GL_TEXTURE_BUFFER, 0);

    glActiveTexture(GL_TEXTURE0);

    /* Generate the atlas texture and bind it as the framebuffer. */
    if (fpack.atlas.texture_height == new_texture_height) {
        /* No need to extend the texture. */
        glBindFramebuffer(GL_DRAW_FRAMEBUFFER, fpack.atlas.atlas_framebuffer);
        glBindTexture(GL_TEXTURE_2D, fpack.atlas.atlas_texture);
        glViewport(0, 0, fpack.atlas.texture_width, fpack.atlas.texture_height);
    } else {
        GLuint new_texture;
        GLuint new_framebuffer;
        glGenTextures(1, &new_texture);
        glGenFramebuffers(1, &new_framebuffer);
        glBindFramebuffer(GL_DRAW_FRAMEBUFFER, new_framebuffer);

        glBindTexture(GL_TEXTURE_2D, new_texture);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

        glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA32F, fpack.atlas.texture_width,
                     new_texture_height, 0, GL_RGBA, GL_FLOAT, NULL);
        assert("Out of gpu memory" && glGetError() != GL_OUT_OF_MEMORY);

        glFramebufferTexture2D(GL_DRAW_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D,
                               new_texture, 0);
        glViewport(0, 0, fpack.atlas.texture_width, new_texture_height);
        glClearColor(0.0, 0.0, 0.0, 1.0);
        glClear(GL_COLOR_BUFFER_BIT);

        if (fpack.atlas.texture_height) {
            /* Old texture had data -> copy. */
            glBindFramebuffer(GL_READ_FRAMEBUFFER, fpack.atlas.atlas_framebuffer);
            glBlitFramebuffer(0, 0, fpack.atlas.texture_width, fpack.atlas.texture_height,
                              0, 0, fpack.atlas.texture_width, fpack.atlas.texture_height,
                              GL_COLOR_BUFFER_BIT, GL_NEAREST);
            glBindFramebuffer(GL_READ_FRAMEBUFFER, 0);
        }

        glBindFramebuffer(GL_READ_FRAMEBUFFER, 0);
        fpack.atlas.texture_height = new_texture_height;
        glDeleteTextures(1, &fpack.atlas.atlas_texture);
        fpack.atlas.atlas_texture = new_texture;
        glDeleteFramebuffers(1, &fpack.atlas.atlas_framebuffer);
        fpack.atlas.atlas_framebuffer = new_framebuffer;
    }
    glBindTexture(GL_TEXTURE_2D, 0);

    GLfloat framebuffer_projection[4][4];
    ff_ortho(0, (GLfloat)fpack.atlas.texture_width, 0, (GLfloat)fpack.atlas.texture_height,
            -1.0, 1.0, framebuffer_projection);
    ff_ortho(-(GLfloat)fpack.atlas.texture_width, (GLfloat)fpack.atlas.texture_width,
            -(GLfloat)fpack.atlas.texture_height, (GLfloat)fpack.atlas.texture_height,
            -1.0, 1.0, fpack.atlas.projection);

    glUseProgram(_gen_shader);
    glUniform1i(_uniforms.metadata, 0);
    glUniform1i(_uniforms.point_data, 1);

    glUniformMatrix4fv(_uniforms.atlas_projection, 1, GL_FALSE,
                       (GLfloat *)framebuffer_projection);

    glUniform2f(_uniforms.scale, fpack.font.scale, fpack.font.scale);
    glUniform1f(_uniforms.range, fpack.font.range);
    glUniform1i(_uniforms.meta_offset, 0);
    glUniform1i(_uniforms.point_offset, 0);

    if (glCheckFramebufferStatus(GL_FRAMEBUFFER) != GL_FRAMEBUFFER_COMPLETE)
        fprintf(stderr, "msdfgl: framebuffer incomplete: %x\n",
                glCheckFramebufferStatus(GL_FRAMEBUFFER));

    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_BUFFER, fpack.font.meta_input_texture);

    glActiveTexture(GL_TEXTURE1);
    glBindTexture(GL_TEXTURE_BUFFER, fpack.font.point_input_texture);

    glBindVertexArray(_bbox_vao);
    glBindBuffer(GL_ARRAY_BUFFER, _bbox_vbo);

    glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 2 * sizeof(GLfloat), 0);
    glEnableVertexAttribArray(0);

    int meta_offset = 0;
    int point_offset = 0;
    for (int i = 0; i < nrender; ++i) {
        IndexEntry g = atlas_index[i];
        float w = g.size_x;
        float h = g.size_y;
        GLfloat bounding_box[] = {0, 0, w, 0, 0, h, 0, h, w, 0, w, h};
        glBufferSubData(GL_ARRAY_BUFFER, 0, sizeof(bounding_box), bounding_box);

        glUniform2f(
            _uniforms.translate,
            -g.bearing_x / kserializer_scale + fpack.font.range / 2.0f,
            (g.glyph_height - g.bearing_y) / kserializer_scale + fpack.font.range / 2.0f);

        glUniform2f(_uniforms.texture_offset, g.offset_x, g.offset_y);
        glUniform1i(_uniforms.meta_offset, meta_offset);
        glUniform1i(_uniforms.point_offset, point_offset / (2 * sizeof(GLfloat)));
        glUniform1f(_uniforms.glyph_height, g.size_y);

        /* No need for draw call if there are no contours */
        if (((unsigned char *)metadata.get())[meta_offset])
            glDrawArrays(GL_TRIANGLES, 0, 6);

        meta_offset += meta_sizes[i];
        point_offset += point_sizes[i];
    }

    glDisableVertexAttribArray(0);

    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_BUFFER, 0);

    glActiveTexture(GL_TEXTURE1);
    glBindTexture(GL_TEXTURE_BUFFER, 0);

    glUseProgram(0);

    glBindFramebuffer(GL_FRAMEBUFFER, 0);

    fpack.atlas.nglyphs += nrender;

    glViewport(original_viewport[0], original_viewport[1], original_viewport[2],
               original_viewport[3]);
}

void ff_draw(const ff_font_handle_t font_handle, const ff_glyphs_t &glyphs,
            const float *projection) noexcept {
    auto &fpack = _fonts.at(font_handle);

    GLuint glyph_buffer;
    GLuint vao;
    glGenBuffers(1, &glyph_buffer);
    glGenVertexArrays(1, &vao);
    glBindVertexArray(vao);
    glBindBuffer(GL_ARRAY_BUFFER, glyph_buffer);
    glBufferData(GL_ARRAY_BUFFER, glyphs.size() * sizeof(ff_glyph_t), &glyphs[0],
                 GL_DYNAMIC_DRAW);

    glEnableVertexAttribArray(0);
    glEnableVertexAttribArray(1);
    glEnableVertexAttribArray(2);
    glEnableVertexAttribArray(3);
    glEnableVertexAttribArray(4);
    glEnableVertexAttribArray(5);
    glEnableVertexAttribArray(6);

    auto position_offset = (void *)offsetof(ff_glyph_t, position.x);
    auto color_offset = (void *)offsetof(ff_glyph_t, color);
    auto codepoint_offset = (void *)offsetof(ff_glyph_t, codepoint);
    auto size_offset = (void *)offsetof(ff_glyph_t, size);
    auto offset_offset = (void *)offsetof(ff_glyph_t, characteristics.offset);
    auto skew_offset = (void *)offsetof(ff_glyph_t, characteristics.skew);
    auto strength_offset = (void *)offsetof(ff_glyph_t, characteristics.strength);

    glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, sizeof(ff_glyph_t), position_offset);
    glVertexAttribIPointer(1, 4, GL_UNSIGNED_BYTE, sizeof(ff_glyph_t), color_offset);
    glVertexAttribIPointer(2, 1, GL_INT, sizeof(ff_glyph_t), codepoint_offset);
    glVertexAttribPointer(3, 1, GL_FLOAT, GL_FALSE, sizeof(ff_glyph_t), size_offset);
    glVertexAttribPointer(4, 1, GL_FLOAT, GL_FALSE, sizeof(ff_glyph_t), offset_offset);
    glVertexAttribPointer(5, 1, GL_FLOAT, GL_FALSE, sizeof(ff_glyph_t), skew_offset);
    glVertexAttribPointer(6, 1, GL_FLOAT, GL_FALSE, sizeof(ff_glyph_t), strength_offset);

    /* Enable gamma correction if user didn't enabled it */
    auto is_srgb_enabled = glIsEnabled(GL_FRAMEBUFFER_SRGB);
    bool srgb_enabled_by_fn = !is_srgb_enabled;
    if (!is_srgb_enabled) glEnable(GL_FRAMEBUFFER_SRGB);

    glUseProgram(_render_shader);
    /* Bind atlas texture and index buffer. */
    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, fpack.atlas.atlas_texture);
    glUniform1i(_uniforms.atlas, 0);

    glActiveTexture(GL_TEXTURE1);
    glBindTexture(GL_TEXTURE_BUFFER, fpack.atlas.index_texture);
    glUniform1i(_uniforms.index, 1);

    glUniformMatrix4fv(_uniforms.font_atlas_projection, 1, GL_FALSE,
                       (GLfloat *)fpack.atlas.projection);

    glUniformMatrix4fv(_uniforms.window_projection, 1, GL_FALSE, projection);
    glUniform1f(_uniforms.padding, (GLfloat)(fpack.font.range / 2.0 * kserializer_scale));
    glUniform1f(_uniforms.units_per_em, (GLfloat)fpack.font.face->units_per_EM);
    glUniform2fv(_uniforms.dpi, 1, _dpi);

    /* Render the glyphs. */
    glDrawArrays(GL_POINTS, 0, glyphs.size());

    /* Clean up. */
    glActiveTexture(GL_TEXTURE1);
    glBindTexture(GL_TEXTURE_BUFFER, 0);

    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, 0);
    glUseProgram(0);

    /* if the user didn't enabled it, disable it */
    if (srgb_enabled_by_fn) glDisable(GL_FRAMEBUFFER_SRGB);

    glDisableVertexAttribArray(0);
    glDisableVertexAttribArray(1);
    glDisableVertexAttribArray(2);
    glDisableVertexAttribArray(3);
    glDisableVertexAttribArray(4);
    glDisableVertexAttribArray(5);
    glDisableVertexAttribArray(6);

    glBindBuffer(GL_ARRAY_BUFFER, 0);
    glBindVertexArray(0);
    glDeleteBuffers(1, &glyph_buffer);
    glDeleteVertexArrays(1, &vao);
}

[[nodiscard]] ff_glyphs_t ff_print_unicode(
    const ff_typography_t typography, const std::u32string_view buffer,
    const ff_position_t position, const int print_options,
    const ff_glyph_t::Characteristics characteristics) noexcept {
    auto &fpack = _fonts.at(typography.font);
    auto pos0 = ff_position_t{position.x, position.y + typography.size};

    std::vector<ff_glyph_t> result;
    result.reserve(buffer.size());
    for (size_t i = 0; i < buffer.size(); i++) {
        const auto &codepoint = (char32_t)buffer.at(i);

        auto idx = fpack.font.character_index.at(codepoint);
        if (idx == nullptr) {
            ff_gen_glyphs(typography.font, std::vector<char32_t>{codepoint});
            idx = fpack.font.character_index.at(codepoint);
            assert(idx != nullptr);
        }

        FT_Vector kerning{};
        const bool should_get_kerning = (print_options & ff_print_options_enable_kerning) and
                                        FT_HAS_KERNING(fpack.font.face) and (i > 0);
        if (should_get_kerning) {
            const auto &previous_character = buffer.at(i - 1);
            FT_Get_Kerning(fpack.font.face,
                           FT_Get_Char_Index(fpack.font.face, previous_character),
                           FT_Get_Char_Index(fpack.font.face, codepoint),
                           FT_KERNING_UNSCALED, &kerning);
        }

        result.push_back({});
        auto &new_glyph = result.back();
        new_glyph.position = pos0;
        new_glyph.color = typography.color;
        new_glyph.codepoint = idx->codepoint_index;
        new_glyph.size = typography.size;
        new_glyph.characteristics = characteristics;

        if (not(print_options & ff_print_options_print_vertically))
            pos0.x += (idx->advance[0] + kerning.x) *
                      (typography.size * _dpi[0] / 72.0f) / fpack.font.face->units_per_EM;
        else
            pos0.y += (idx->advance[1] + kerning.y) *
                      (typography.size * _dpi[0] / 72.0f) / fpack.font.face->units_per_EM;
    }

    return result;
}

[[nodiscard]] ff_dimensions_t ff_measure(const ff_font_handle_t font_handle,
                                   const std::u32string_view &buffer, const float size,
                                   const bool with_kerning) {
    auto &fpack = _fonts.at(font_handle);
    ff_dimensions_t result{};
    ff_glyphs_t glyphs;
    auto tmp_width{0.0f};

    for (size_t i = 0; i < buffer.size(); i++) {
        auto &codepoint = buffer.at(i);

        auto idx = fpack.font.character_index.at(codepoint);
        if (idx == nullptr) {
            ff_gen_glyphs(font_handle, {codepoint});
            idx = fpack.font.character_index.at(codepoint);
            assert(idx != nullptr);
        }

        FT_Vector kerning{};
        const bool should_get_kerning =
            with_kerning and FT_HAS_KERNING(fpack.font.face) and (i > 0);
        if (should_get_kerning) {
            const auto &previous_character = buffer.at(i - 1);
            FT_Get_Kerning(fpack.font.face,
                           FT_Get_Char_Index(fpack.font.face, previous_character),
                           FT_Get_Char_Index(fpack.font.face, codepoint),
                           FT_KERNING_UNSCALED, &kerning);
        }

        auto height = (idx->advance[1] + kerning.y) * (size * _dpi[1] / 72.0f) /
                      fpack.font.face->units_per_EM;
        result.height = std::max(result.height, height);
        result.width += (idx->advance[0] + kerning.x) * (size * _dpi[0] / 72.0f) /
                        fpack.font.face->units_per_EM;
    }

    result.width = std::max(result.width, tmp_width);

    return result;
}

void ff_terminate() noexcept {
    {
        std::vector<ff_font_handle_t> handles;
        for (auto &kv_pair : _fonts) handles.push_back(kv_pair.first);
        for (auto &handle : handles) {
            ff_remove_font(handle);
        }
    }

    FT_Done_FreeType(_ft_library);
}

// ff_map_t
ff_map_item_t *ff_map_t::at(char32_t codepoint) noexcept {
    auto within_extended_ascii_range = codepoint <= 0xE0;
    if (within_extended_ascii_range) return &extended_ascii_.at(codepoint);
    const auto contains = codepoint_map_.find(codepoint) != codepoint_map_.end();
    if (not contains) return {NULL};
    return &codepoint_map_.at(codepoint);
}

ff_map_item_t *ff_map_t::insert(const char32_t codepoint) noexcept {
    auto within_extended_ascii_range = codepoint <= 0xE0;
    if (within_extended_ascii_range) {
        auto &item = extended_ascii_.at(codepoint);
        item.codepoint = codepoint;
        item.codepoint_index = codepoint;
        return &item;
    };
    auto &new_item = codepoint_map_[codepoint];
    new_item.codepoint = codepoint;
    new_item.codepoint_index = 0xE0 + codepoint_map_.size() - 1;
    return &new_item;
}
// /impl
#endif
