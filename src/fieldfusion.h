#pragma once

#ifdef __cplusplus
extern "C" {
#endif

#include <ft2build.h>

#include FT_FREETYPE_H

#ifndef FIELDFUSION_DONT_INCLUDE_GLAD
#include <glad.h>
#endif
#include <GL/gl.h>

#include "assert.h"
#include "freetype/ftoutln.h"

typedef int ff_font_handle_t;

typedef struct {
    FT_ULong codepoint;
    int codepoint_index;
    float advance[2];
} ff_map_item_t;

typedef struct ht_codepoint_next_entry_t {
    bool not_empty;
    int key;
    ff_map_item_t value;
    struct ht_codepoint_next_entry_t *next;
} ht_codepoint_entry_t;

typedef struct {
    ht_codepoint_entry_t *entries;
} ht_codepoint_map_t;

typedef struct {
    ff_map_item_t extended_ascii_[0xff];
    ht_codepoint_map_t codepoint_map;
} ff_map_t;

typedef struct {
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
} ff_font_t;

typedef struct {
    int refcount; /* Amount of fonts using this atlas */
    int implicit; /* Set to 1 if the atlas was created automatically
                     and not by user */

    float projection[4][4];

    /**
     * 2D RGBA atlas texture containing all MSDF-glyph bitmaps.
     */
    unsigned atlas_texture;
    unsigned atlas_framebuffer;

    /**
     * 1D buffer containing glyph position information per character
     * in the atlas texture.
     */
    unsigned index_texture;
    unsigned index_buffer;

    /**
     * Amount of glyphs currently rendered on the textures.
     */
    size_t nglyphs;

    /**
     * The current size of the buffer index texture.
     */
    size_t nallocated;

    int texture_width;
    /**
     * The amount of allocated texture height.
     */
    int texture_height;

    /**
     * The location in the atlas where the next bitmap would be
     * rendered.
     */
    size_t offset_y;
    size_t offset_x;
    size_t y_increment;

    /**
     * Amount of pixels to leave blank between MSDF bitmaps.
     */
    int padding;
} ff_atlas_t;

typedef struct {
    float x;
    float y;
} ff_position_t;

typedef struct {
    /**
     * Y offset (for e.g. subscripts and superscripts).
     */
    float offset;

    /**
     * The amount of "lean" on the character. Positive leans to the
     * right, negative leans to the left. Skew can create /italics/
     * effect without loading a separate font atlas.
     */
    float skew;

    /**
     * The "boldness" of the character. 0.5 is normal strength, lower
     * is thinner and higher is thicker. Strength can create *bold*
     * effect without loading a separate font atlas.
     */
    float strength;
} ff_characteristics_t;

typedef struct {
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
    ff_characteristics_t characteristics;
} ff_glyph_t;

typedef struct {
    ff_font_t font;
    ff_atlas_t atlas;
} ff_font_texture_pack_t;

typedef struct ht_fpack_next_entry_t {
    bool not_empty;
    int key;
    ff_font_texture_pack_t value;
    struct ht_fpack_next_entry_t *next;
} ht_fpack_entry_t;

typedef struct {
    ht_fpack_entry_t *entries;
} ht_fpack_map_t;

typedef struct {
    float width;
    float height;
} ff_dimensions_t;

typedef struct {
    ff_font_handle_t font;
    float size;
    uint32_t color;
} ff_typography_t;

typedef struct {
    ff_glyph_t *data;
    ulong size;
    ulong capacity;
} ff_glyphs_vector_t;

typedef struct {
    float scale;
    float range;
    int texture_width;
    int texture_padding;
} ff_font_config_t;

typedef enum {
    ff_print_options_enable_kerning = 0x2,
    ff_print_options_print_vertically = 0x4,
} ff_print_options_t;

typedef struct {
    ff_typography_t typography;
    char32_t *str;
    ulong str_count;
    int print_flags;
    ff_characteristics_t characteristics;
} print_params_t;

typedef struct {
    float scr_left;
    float scr_right;
    float scr_top;
    float scr_bottom;
    float near;
    float far;
} ortho_projection_params_t;

void ff_initialize(const char *version) noexcept;
// scale 2  origi : 4
// range 2.2  origi: 2
// texture width 1024
// text paddin 2
ff_font_config_t ff_default_font_config(void);
ff_font_handle_t ff_new_font(const char *path,
                             ff_font_config_t config);
void ff_remove_font(ff_font_handle_t handle);
void ff_gen_glyphs(ff_font_handle_t, char32_t *codepoints,
                   ulong codepoints_count);
void ff_draw(ff_font_handle_t, ff_glyph_t *glyphs, ulong glyphs_count,
             float *projection);
ff_characteristics_t ff_get_default_characteristics();
int ff_get_default_print_flags();
ff_glyphs_vector_t ff_print_unicode(print_params_t params,
                                    ff_position_t);
ff_dimensions_t ff_measure(const ff_font_handle_t, char32_t *str,
                           ulong str_count, float size,
                           bool with_kerning);
void ff_terminate();

void ff_get_ortho_projection(ortho_projection_params_t params,
                             float dest[][4]);
ff_glyphs_vector_t ff_glyphs_vector_new();
ff_glyph_t *ff_glyphs_vector_get(ff_glyphs_vector_t *v, ulong index);
void ff_glyphs_vector_free(ff_glyphs_vector_t *v);
void ff_glyphs_vector_push(ff_glyphs_vector_t *v, ff_glyph_t glyph);
void ff_glyphs_vector_cat(ff_glyphs_vector_t *dest,
                          ff_glyphs_vector_t *src);

// #define FIELFUSION_IMPLEMENTATION
#ifdef FIELDFUSION_IMPLEMENTATION
#include <stdbool.h>
#include <sys/types.h>

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

static const char *kfont_geometry =
    "\
layout (points) in;\n\
layout (triangle_strip, max_vertices = 4) out;\n\
\n\
in VS_OUT {\n\
    int glyph;\n\
    vec4 color;\n\
    float size;\n\
    float y_offset;\n\
    float skewness;\n\
    float strength;\n\
} gs_in[];\n\
\n\
out vec2 text_pos;\n\
out vec4 text_color;\n\
out float strength;\n\
\n\
uniform mat4 projection;\n\
uniform float padding;\n\
uniform float units_per_em;\n\
uniform vec2 dpi;\n\
\n\
precision mediump samplerBuffer;\n\
uniform samplerBuffer font_index;\n\
\n\
\n\
void main() {\n\
    text_color = gs_in[0].color;\n\
    strength = gs_in[0].strength;\n\
\n\
    vec4 font_size = vec4(gs_in[0].size * dpi / 72.0 / units_per_em, 1.0, 1.0);\n\
\n\
    int _offset = 8 * gs_in[0].glyph;\n\
    vec2 text_offset = vec2(texelFetch(font_index, _offset + 0).r,\n\
                            texelFetch(font_index, _offset + 1).r);\n\
    vec2 glyph_texture_width = vec2(texelFetch(font_index, _offset + 2).r, 0.0 );\n\
    vec2 glyph_texture_height = vec2(0.0, texelFetch(font_index, _offset + 3).r);\n\
\n\
    vec4 bearing = vec4(texelFetch(font_index, _offset + 4).r,\n\
                        -texelFetch(font_index, _offset + 5).r, 0.0, 0.0) * font_size;\n\
\n\
    vec4 glyph_width = vec4(texelFetch(font_index, _offset + 6).r, 0.0, 0.0, 0.0) * font_size;\n\
    vec4 glyph_height = vec4(0.0, texelFetch(font_index, _offset + 7).r, 0.0, 0.0) * font_size;\n\
\n\
    vec4 padding_x = vec4(padding, 0.0, 0.0, 0.0) * font_size;\n\
    vec4 padding_y = vec4(0.0, padding, 0.0, 0.0) * font_size;\n\
    float skewness = gs_in[0].skewness;\n\
\n\
    vec4 p = gl_in[0].gl_Position + vec4(0.0, gs_in[0].y_offset, 0.0, 0.0);\n\
    vec4 _p = p;\n\
\n\
\n\
    // BL\n\
    _p = p + bearing + glyph_height - padding_x + padding_y;\n\
    _p.x += skewness * (p.y - _p.y);\n\
    gl_Position = projection * _p;\n\
    text_pos = text_offset + glyph_texture_height;\n\
    EmitVertex();\n\
\n\
    // BR\n\
    _p = p + bearing + glyph_height + glyph_width + padding_x + padding_y;\n\
    _p.x += skewness * (p.y - _p.y);\n\
    gl_Position = projection * _p;\n\
    text_pos = text_offset + glyph_texture_width + glyph_texture_height;\n\
    EmitVertex();\n\
\n\
    // TL\n\
    _p = p + bearing - padding_x - padding_y;\n\
    _p.x += skewness * (p.y - _p.y);\n\
    gl_Position = projection * _p;\n\
    text_pos = text_offset;\n\
    EmitVertex();\n\
\n\
    // TR\n\
    _p = p + bearing + glyph_width + padding_x - padding_y;\n\
    _p.x += skewness * (p.y - _p.y);\n\
    gl_Position = projection * _p;\n\
    text_pos = text_offset + glyph_texture_width;\n\
    EmitVertex();\n\
\n\
    EndPrimitive();\n\
}\n\
 ";

static const char *kmsdf_vertex =
    "\n\
layout (location = 0) in vec2 vertex;\n\
\n\
precision mediump float;\n\
uniform mat4 projection;\n\
uniform vec2 offset;\n\
\n\
void main() {\n\
    gl_Position = projection * vec4(vertex.xy + offset, 1.0, 1.0);\n\
}\n\
";

static const char *kfont_vertex =
    "\
layout (location = 0) in vec2 vertex;\n\
layout (location = 1) in uvec4 glyph_color;\n\
layout (location = 2) in int glyph_index;\n\
layout (location = 3) in float size;\n\
layout (location = 4) in float y_offset;\n\
layout (location = 5) in float skewness;\n\
layout (location = 6) in float strength;\n\
\n\
uniform mat4 projection;\n\
\n\
out VS_OUT {\n\
    int glyph;\n\
    vec4 color;\n\
    float size;\n\
    float y_offset;\n\
    float skewness;\n\
    float strength;\n\
\n\
} vs_out;\n\
\n\
void main() {\n\
    gl_Position = vec4(vertex.xy, 0.0, 1.0);\n\
    vs_out.glyph = glyph_index;\n\
    uvec4 c = glyph_color;\n\
    vs_out.color = vec4(float(c.a) / 255.0, float(c.b) / 255.0,\n\
                        float(c.g) / 255.0, float(c.r) / 255.0);\n\
    vs_out.size = size;\n\
    vs_out.y_offset = y_offset;\n\
    vs_out.skewness = skewness;\n\
    vs_out.strength = strength;\n\
}\n\
";

static const char *kmsdf_fragment =
    "\n\
#define IDX_CURR 0\n\
#define IDX_SHAPE 1\n\
#define IDX_INNER 2\n\
#define IDX_OUTER 3\n\
#define IDX_RED 0\n\
#define IDX_GREEN 1\n\
#define IDX_BLUE 2\n\
#define IDX_NEGATIVE 0\n\
#define IDX_POSITIVE 1\n\
#define IDX_MAX_INNER 0\n\
#define IDX_MAX_OUTER 1\n\
\n\
\n\
precision mediump float;\n\
\n\
precision mediump samplerBuffer;\n\
precision mediump usamplerBuffer;\n\
uniform usamplerBuffer metadata;\n\
uniform samplerBuffer point_data;\n\
\n\
#define meta_at(i) texelFetch(metadata, int(i)).r\n\
#define point_at(i) vec2(texelFetch(point_data, 2 * int(i)).r, \\n\
                         texelFetch(point_data, 2 * int(i) + 1).r)\n\
\n\
uniform vec2 offset;\n\
\n\
uniform vec2 translate;\n\
uniform vec2 scale;\n\
uniform float range;\n\
uniform int meta_offset;\n\
uniform int point_offset;\n\
uniform float glyph_height;\n\
\n\
out vec4 color;\n\
\n\
const float PI = 3.1415926535897932384626433832795;\n\
const float INFINITY = 3.402823466e+38;\n\
\n\
const uint BLACK = 0u;\n\
const uint RED = 1u;\n\
const uint GREEN = 2u;\n\
const uint BLUE = 4u;\n\
const uint YELLOW = RED | GREEN;\n\
const uint MAGENTA = BLUE | RED;\n\
const uint CYAN = BLUE | GREEN;\n\
const uint WHITE = RED | GREEN | BLUE;\n\
\n\
struct segment {\n\
    vec3 min_true;\n\
    vec2 mins[2];\n\
    int nearest_points;\n\
    int nearest_npoints;\n\
};\n\
\n\
struct workspace {\n\
    segment segments[4 * 3];\n\
\n\
    vec3 maximums[2];\n\
    vec3 min_absolute;\n\
};\n\
\n\
workspace ws;\n\
\n\
vec3 signed_distance_linear(vec2 p0, vec2 p1, vec2 origin);\n\
vec3 signed_distance_quad(vec2 p0, vec2 p1, vec2 p2, vec2 origin);\n\
void add_segment_true_distance(int segment_index, int npoints, int points, vec3 d);\n\
vec3 get_pixel_distance(vec2);\n\
\n\
vec2 orthonormal(vec2 v) {float len = length(v); return vec2(v.y / len, -v.x / len);}\n\
float cross_(vec2 a, vec2 b) { return a.x * b.y - a.y * b.x; }\n\
float median(vec3 d) {return max(min(d.r, d.g), min(max(d.r, d.g), d.b));}\n\
void add_segment_pseudo_distance(int segment_index, vec2 d);\n\
vec2 distance_to_pseudo_distance(int npoints, int points, vec3 d, vec2 p);\n\
bool point_facing_edge(int prev_npoints, int prev_points, int cur_npoints, int cur_points,\n\
                       int next_npoints, int next_points, vec2 p, float param);\n\
void add_segment(int prev_npoints, int prev_points, int cur_npoints, int cur_points,\n\
                 int next_npoints, int next_points, uint color, vec2 point);\n\
void set_contour_edge(int winding, vec2 point);\n\
float compute_distance(int segment_index, vec2 point);\n\
\n\
\n\
bool less(vec2 a, vec2 b) {\n\
    return abs(a.x) < abs(b.x) || (abs(a.x) == abs(b.x) && a.y < b.y);\n\
}\n\
\n\
\n\
void main() {\n\
    vec2 coords = gl_FragCoord.xy - offset;\n\
\n\
    vec2 p = ((coords + 0.49) / scale) - vec2(translate.x, -translate.y);\n\
    p.y  = (glyph_height / scale.y) - p.y;\n\
\n\
    ws.maximums[0].r = -INFINITY;\n\
    ws.maximums[1].r = -INFINITY;\n\
    ws.maximums[0].g = -INFINITY;\n\
    ws.maximums[1].g = -INFINITY;\n\
    ws.maximums[0].b = -INFINITY;\n\
    ws.maximums[1].b = -INFINITY;\n\
    ws.min_absolute.r = -INFINITY;\n\
    ws.min_absolute.g = -INFINITY;\n\
    ws.min_absolute.b = -INFINITY;\n\
\n\
    for (int i = 0; i < (4 * 3); ++i) {\n\
        ws.segments[i].mins[0].x = -INFINITY;\n\
        ws.segments[i].mins[1].x = -INFINITY;\n\
        ws.segments[i].min_true.x = -INFINITY;\n\
        ws.segments[i].nearest_points = -1;\n\
    }\n\
    int point_index = point_offset;\n\
    int meta_index = meta_offset;\n\
\n\
\n\
    uint ncontours = meta_at(meta_index++);\n\
\n\
    for (uint _i = 0u; _i < ncontours; ++_i) {\n\
        int winding = int(meta_at(meta_index++)) - 1;\n\
        uint nsegments = meta_at(meta_index++);\n\
\n\
        uint s_color = meta_at(meta_index);\n\
        uint s_npoints = meta_at(meta_index + 1);\n\
\n\
        /** TODO: Move the following checks to the preprocessor, no need to do\n\
                  them for every fragment. */\n\
        /* Ignore empty contours. */\n\
        if (nsegments == 0u) {\n\
            continue;\n\
        }\n\
\n\
        /* Ignore contours with just one linear segment, some fonts seem to have them. */\n\
        if (nsegments == 1u && s_npoints == 2u) {\n\
            point_index += 2;\n\
            meta_index += 2;\n\
            continue;\n\
        }\n\
\n\
        /* Ignore contours with just two linear segments, some fonts seem to have them. */\n\
        if (nsegments == 2u && s_npoints == 2u && meta_at(meta_index + 3) == 2u) {\n\
            point_index += 4;\n\
            meta_index += 4;\n\
            continue;\n\
        }\n\
\n\
        int cur_points = point_index;\n\
        uint cur_color = meta_at(meta_index + 2 * (int(nsegments) - 1));\n\
        uint cur_npoints = meta_at(meta_index + 2 * (int(nsegments) - 1) + 1);\n\
\n\
\n\
        uint prev_npoints = nsegments >= 2u ?\n\
            meta_at(meta_index + 2 * (int(nsegments) - 2) + 1) : s_npoints;\n\
        int prev_points = point_index;\n\
\n\
        for (uint _i = 0u; _i < nsegments - 1u; ++_i) {\n\
            uint npoints = meta_at(meta_index + 2 * int(_i) + 1);\n\
            cur_points += (int(npoints) - 1);\n\
        }\n\
\n\
        for (uint _i = 0u; (_i < (nsegments - 2u)) && nsegments >= 2u; ++_i) {\n\
            uint npoints = meta_at(meta_index + 2 * int(_i) + 1);\n\
            prev_points += (int(npoints) - 1);\n\
        }\n\
\n\
        for (uint _i = 0u; _i < nsegments; ++_i) {\n\
\n\
            add_segment(int(prev_npoints), prev_points, int(cur_npoints), cur_points,\n\
                        int(s_npoints), point_index, cur_color, p);\n\
\n\
            prev_points = cur_points;\n\
            prev_npoints = cur_npoints;\n\
\n\
            cur_points = point_index;\n\
            cur_npoints = s_npoints;\n\
            cur_color = s_color;\n\
\n\
            s_color = meta_at(meta_index++ + 2);\n\
            point_index += (int(s_npoints) - 1);\n\
            s_npoints = meta_at(meta_index++ + 2);\n\
        }\n\
        point_index += 1;\n\
\n\
        set_contour_edge(winding, p);\n\
    }\n\
\n\
    vec3 d = get_pixel_distance(p);\n\
\n\
    color = vec4(d / range + 0.5, 1.0);\n\
\n\
    // For testing\n\
    // color = median(color.rgb) > 0.5 ? vec4(1.0, 1.0, 1.0, 1.0) : vec4(0.0, 0.0, 0.0, 1.0);\n\
}\n\
\n\
void merge_segment(int s, int other) {\n\
    if (less(ws.segments[other].min_true.xy, ws.segments[s].min_true.xy)) {\n\
        ws.segments[s].min_true = ws.segments[other].min_true;\n\
\n\
        ws.segments[s].nearest_npoints = ws.segments[other].nearest_npoints;\n\
        ws.segments[s].nearest_points = ws.segments[other].nearest_points;\n\
    }\n\
    if (less(ws.segments[other].mins[IDX_NEGATIVE], ws.segments[s].mins[IDX_NEGATIVE]))\n\
        ws.segments[s].mins[IDX_NEGATIVE] = ws.segments[other].mins[IDX_NEGATIVE];\n\
    if (less(ws.segments[other].mins[IDX_POSITIVE], ws.segments[s].mins[IDX_POSITIVE])) {\n\
        ws.segments[s].mins[IDX_POSITIVE] = ws.segments[other].mins[IDX_POSITIVE];\n\
    }\n\
}\n\
\n\
void merge_multi_segment(int e, int other) {\n\
    merge_segment(e * 3 + IDX_RED, other * 3 + IDX_RED);\n\
    merge_segment(e * 3 + IDX_GREEN, other * 3 + IDX_GREEN);\n\
    merge_segment(e * 3 + IDX_BLUE, other * 3 + IDX_BLUE);\n\
}\n\
\n\
void add_segment(int prev_npoints, int prev_points, int cur_npoints, int cur_points,\n\
                 int next_npoints, int next_points, uint s_color, vec2 point) {\n\
\n\
    vec3 d;\n\
    if (cur_npoints == 2)\n\
        d = signed_distance_linear(point_at(cur_points),\n\
                                   point_at(cur_points + 1),\n\
                                   point);\n\
    else\n\
        d = signed_distance_quad(point_at(cur_points),\n\
                                 point_at(cur_points + 1),\n\
                                 point_at(cur_points + 2),\n\
                                 point);\n\
\n\
    if ((s_color & RED) > 0u)\n\
        add_segment_true_distance(IDX_CURR * 3 + IDX_RED, cur_npoints, cur_points, d);\n\
    if ((s_color & GREEN) > 0u)\n\
        add_segment_true_distance(IDX_CURR * 3 + IDX_GREEN, cur_npoints, cur_points, d);\n\
    if ((s_color & BLUE) > 0u)\n\
        add_segment_true_distance(IDX_CURR * 3 + IDX_BLUE, cur_npoints, cur_points, d);\n\
\n\
    if (point_facing_edge(prev_npoints, prev_points, cur_npoints, cur_points,\n\
                          next_npoints, next_points, point, d.z)) {\n\
\n\
        vec2 pd = distance_to_pseudo_distance(cur_npoints, cur_points, d, point);\n\
        if ((s_color & RED) > 0u)\n\
            add_segment_pseudo_distance(IDX_CURR * 3 + IDX_RED, pd);\n\
       if ((s_color & GREEN) > 0u)\n\
            add_segment_pseudo_distance(IDX_CURR * 3 + IDX_GREEN, pd);\n\
        if ((s_color & BLUE) > 0u)\n\
            add_segment_pseudo_distance(IDX_CURR * 3 + IDX_BLUE, pd);\n\
    }\n\
}\n\
\n\
vec3 get_distance(int segment_index, vec2 point) {\n\
    vec3 d;\n\
    d.r = compute_distance(segment_index * 3 + IDX_RED, point);\n\
    d.g = compute_distance(segment_index * 3 + IDX_GREEN, point);\n\
    d.b = compute_distance(segment_index * 3 + IDX_BLUE, point);\n\
    return d;\n\
}\n\
\n\
void set_contour_edge(int winding, vec2 point) {\n\
\n\
    vec3 d = get_distance(IDX_CURR, point);\n\
\n\
    merge_multi_segment(IDX_SHAPE, IDX_CURR);\n\
    if (winding > 0 && median(d) >= 0.0)\n\
        merge_multi_segment(IDX_INNER, IDX_CURR);\n\
    if (winding < 0 && median(d) <= 0.0)\n\
        merge_multi_segment(IDX_OUTER, IDX_CURR);\n\
\n\
    int i = winding < 0 ? IDX_MAX_INNER : IDX_MAX_OUTER;\n\
\n\
    ws.maximums[i] = (median(d) > median(ws.maximums[i])) ? d : ws.maximums[i];\n\
    ws.min_absolute = (abs(median(d)) < abs(median(ws.min_absolute))) ? d : ws.min_absolute;\n\
}\n\
\n\
vec2 segment_direction(int points, int npoints, float param) {\n\
    return mix(point_at(points + 1) - point_at(points),\n\
               point_at(points + npoints - 1) - point_at(points + npoints - 2),\n\
               param);\n\
}\n\
\n\
vec2 segment_point(int points, int npoints, float param) {\n\
    return mix(mix(point_at(points), point_at(points + 1), param),\n\
               mix(point_at(points + npoints - 2), point_at(points + npoints - 1), param),\n\
               param);\n\
}\n\
\n\
\n\
vec2 distance_to_pseudo_distance(int npoints, int points, vec3 d, vec2 p) {\n\
    if (d.z >= 0.0 && d.z <= 1.0)\n\
        return d.xy;\n\
\n\
    vec2 dir = normalize(segment_direction(points, npoints, d.z < 0.0 ? 0.0 : 1.0));\n\
    vec2 aq = p - segment_point(points, npoints, d.z < 0.0 ? 0.0 : 1.0);\n\
    float ts = dot(aq, dir);\n\
    if (d.z < 0.0 ? ts < 0.0 : ts > 0.0) {\n\
        float pseudo_distance = cross_(aq, dir);\n\
        if (abs(pseudo_distance) <= abs(d.x)) {\n\
            d.x = pseudo_distance;\n\
            d.y = 0.0;\n\
        }\n\
    }\n\
    return d.xy;\n\
}\n\
\n\
void add_segment_true_distance(int segment_index, int npoints, int points, vec3 d) {\n\
    bool is_less = less(d.xy, ws.segments[segment_index].min_true.xy);\n\
    ws.segments[segment_index].min_true =\n\
        is_less ? d : ws.segments[segment_index].min_true;\n\
\n\
    ws.segments[segment_index].nearest_points =\n\
        is_less ? points : ws.segments[segment_index].nearest_points;\n\
    ws.segments[segment_index].nearest_npoints =\n\
        is_less ? npoints : ws.segments[segment_index].nearest_npoints;\n\
}\n\
\n\
\n\
void add_segment_pseudo_distance(int segment_index, vec2 d) {\n\
    int i = d.x < 0.0 ? IDX_NEGATIVE : IDX_POSITIVE;\n\
    vec2 _d = ws.segments[segment_index].mins[i];\n\
    ws.segments[segment_index].mins[i] = less(d, _d) ? d : _d;\n\
}\n\
\n\
bool point_facing_edge(int prev_npoints, int prev_points, int cur_npoints, int cur_points,\n\
                       int next_npoints, int next_points, vec2 p, float param) {\n\
\n\
    if (param >= 0.0 && param <= 1.0)\n\
        return true;\n\
\n\
    vec2 prev_edge_dir = -normalize(segment_direction(prev_points, prev_npoints, 1.0));\n\
    vec2 edge_dir =\n\
        normalize(segment_direction(cur_points, cur_npoints, param < 0.0 ? 0.0 : 1.0)) *\n\
        (param < 0.0 ? 1.0 : -1.0);\n\
    vec2 next_edge_dir = normalize(segment_direction(next_points, next_npoints, 0.0));\n\
    vec2 point_dir = p - segment_point(cur_points, cur_npoints, param < 0.0 ? 0.0 : 1.0);\n\
    return dot(point_dir, edge_dir) >=\n\
           dot(point_dir, param < 0.0 ? prev_edge_dir : next_edge_dir);\n\
}\n\
\n\
float compute_distance(int segment_index, vec2 point) {\n\
\n\
    int i = ws.segments[segment_index].min_true.xy.x < 0.0 ? IDX_NEGATIVE : IDX_POSITIVE;\n\
    float min_distance = ws.segments[segment_index].mins[i].x;\n\
\n\
    if (ws.segments[segment_index].nearest_points == -1) return min_distance;\n\
    vec2 d = distance_to_pseudo_distance(ws.segments[segment_index].nearest_npoints,\n\
                                         ws.segments[segment_index].nearest_points,\n\
                                         ws.segments[segment_index].min_true, point);\n\
    min_distance = abs(d.x) < abs(min_distance) ? d.x : min_distance;\n\
\n\
    return min_distance;\n\
}\n\
\n\
vec3 signed_distance_linear(vec2 p0, vec2 p1, vec2 origin) {\n\
    vec2 aq = origin - p0;\n\
    vec2 ab = p1 - p0;\n\
    float param = dot(aq, ab) / dot(ab, ab);\n\
    vec2 eq = (param > .5 ? p1 : p0) - origin;\n\
    float endpoint_distance = length(eq);\n\
    if (param > 0.0 && param < 1.0) {\n\
        float ortho_distance = dot(orthonormal(ab), aq);\n\
        if (abs(ortho_distance) < endpoint_distance)\n\
            return vec3(ortho_distance, 0, param);\n\
    }\n\
    return vec3(sign(cross_(aq, ab)) *endpoint_distance,\n\
                abs(dot(normalize(ab), normalize(eq))),\n\
                param);\n\
}\n\
\n\
vec3 signed_distance_quad(vec2 p0, vec2 p1, vec2 p2, vec2 origin) {\n\
    vec2 qa = p0 - origin;\n\
    vec2 ab = p1 - p0;\n\
    vec2 br = p2 - p1 - ab;\n\
    float a = dot(br, br);\n\
    float b = 3.0 * dot(ab, br);\n\
    float c = 2.0 * dot(ab, ab) + dot(qa, br);\n\
    float d = dot(qa, ab);\n\
    float coeffs[3];\n\
    float _a = b / a;\n\
    int solutions;\n\
\n\
    float a2 = _a * _a;\n\
    float q = (a2 - 3.0 * (c / a)) / 9.0;\n\
    float r = (_a * (2.0 * a2 - 9.0 * (c / a)) + 27.0 * (d / a)) / 54.0;\n\
    float r2 = r * r;\n\
    float q3 = q * q * q;\n\
    float A, B;\n\
    _a /= 3.0;\n\
    float t = r / sqrt(q3);\n\
    t = t < -1.0 ? -1.0 : t;\n\
    t = t > 1.0 ? 1.0 : t;\n\
    t = acos(t);\n\
    A = -pow(abs(r) + sqrt(r2 - q3), 1.0 / 3.0);\n\
    A = r < 0.0 ? -A : A;\n\
   B = A == 0.0 ? 0.0 : q / A;\n\
    if (r2 < q3) {\n\
        q = -2.0 * sqrt(q);\n\
        coeffs[0] = q * cos(t / 3.0) - _a;\n\
        coeffs[1] = q * cos((t + 2.0 * PI) / 3.0) - _a;\n\
        coeffs[2] = q * cos((t - 2.0 * PI) / 3.0) - _a;\n\
        solutions = 3;\n\
    } else {\n\
        coeffs[0] = (A + B) - _a;\n\
        coeffs[1] = -0.5 * (A + B) - _a;\n\
        coeffs[2] = 0.5 * sqrt(3.0) * (A - B);\n\
        solutions = abs(coeffs[2]) < 1.0e-14 ? 2 : 1;\n\
    }\n\
\n\
    float min_distance = sign(cross_(ab, qa)) * length(qa); // distance from A\n\
    float param = -dot(qa, ab) / dot(ab, ab);\n\
    float distance = sign(cross_(p2 - p1, p2 - origin)) * length(p2 - origin); // distance from B\n\
    if (abs(distance) < abs(min_distance)) {\n\
        min_distance = distance;\n\
        param = dot(origin - p1, p2 - p1) / dot(p2 - p1, p2 - p1);\n\
    }\n\
    for (int i = 0; i < solutions; ++i) {\n\
        if (coeffs[i] > 0.0 && coeffs[i] < 1.0) {\n\
            vec2 endpoint = p0 + ab * 2.0 * coeffs[i] + br * coeffs[i] * coeffs[i];\n\
            float distance = sign(cross_(p2 - p0, endpoint - origin)) * length(endpoint - origin);\n\
            if (abs(distance) <= abs(min_distance)) {\n\
                min_distance = distance;\n\
                param = coeffs[i];\n\
            }\n\
        }\n\
    }\n\
    vec2 v = vec2(min_distance, 0.0);\n\
    v.y = param > 1.0 ? abs(dot(normalize(p2 - p1), normalize(p2 - origin))) : v.y;\n\
    v.y = param < 0.0 ? abs(dot(normalize(ab), normalize(qa))) : v.y;\n\
\n\
    return vec3(v, param);\n\
}\n\
\n\
vec3 get_pixel_distance(vec2 point) {\n\
    vec3 shape_distance = get_distance(IDX_SHAPE, point);\n\
    vec3 inner_distance = get_distance(IDX_INNER, point);\n\
    vec3 outer_distance = get_distance(IDX_OUTER, point);\n\
    float inner_d = median(inner_distance);\n\
    float outer_d = median(outer_distance);\n\
\n\
    bool inner = inner_d >= 0.0 && abs(inner_d) <= abs(outer_d);\n\
    bool outer = outer_d <= 0.0 && abs(outer_d) < abs(inner_d);\n\
    if (!inner && !outer)\n\
        return shape_distance;\n\
\n\
    vec3 d = inner ? inner_distance : outer_distance;\n\
    vec3 contour_distance = ws.maximums[inner ? IDX_MAX_INNER : IDX_MAX_OUTER];\n\
\n\
    float contour_d = median(contour_distance);\n\
    d = (abs(contour_d) < abs(outer_d) && contour_d > median(d)) ? contour_distance : d;\n\
\n\
    contour_distance = ws.min_absolute;\n\
    contour_d = median(contour_distance);\n\
    float d_d = median(d);\n\
\n\
    d = abs(contour_d) < abs(d_d) ? contour_distance : d;\n\
    d = median(d) == median(shape_distance) ? shape_distance : d;\n\
\n\
    return d;\n\
}\n\
";

// serializer //

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

static int AddContourSize(const FT_Vector *to, void *user) {
    (void)to;
    struct GlyphLenCtx *ctx = (struct GlyphLenCtx *)user;
    ctx->data_size += 1;
    ctx->meta_size += 2; /* winding + nsegments */
    return 0;
}
static int AddLinearSize(const FT_Vector *to, void *user) {
    (void)to;
    struct GlyphLenCtx *ctx = (struct GlyphLenCtx *)user;
    ctx->data_size += 1;
    ctx->meta_size += 2; /* color + npoints */
    return 0;
}
static int AddQuadSize(const FT_Vector *control, const FT_Vector *to,
                       void *user) {
    (void)control;
    (void)to;
    struct GlyphLenCtx *ctx = (struct GlyphLenCtx *)user;
    ctx->data_size += 2;
    ctx->meta_size += 2; /* color + npoints */
    return 0;
}
static int AddCubicSize(const FT_Vector *control1,
                        const FT_Vector *control2,
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

static int AddContour(const FT_Vector *to, void *user) {
    struct glyph_data_ctx *ctx = (struct glyph_data_ctx *)user;
    ctx->segment += 1; /* Start contour on a fresh glyph. */
    ctx->segment[0].x = to->x / kserializer_scale;
    ctx->segment[0].y = to->y / kserializer_scale;
    ctx->meta_buffer[0] += 1; /* Increase the number of contours. */
    ctx->meta_buffer[ctx->meta_index++] = 0; /* Set winding to zero */
    ctx->nsegments_index = ctx->meta_index++;
    ctx->meta_buffer[ctx->nsegments_index] = 0;

    return 0;
}
static int AddLinear(const FT_Vector *to, void *user) {
    struct glyph_data_ctx *ctx = (struct glyph_data_ctx *)user;
    ctx->segment[1].x = to->x / kserializer_scale;
    ctx->segment[1].y = to->y / kserializer_scale;

    /* Some glyphs contain zero-dimensional segments, ignore those. */
    if (ctx->segment[1].x == ctx->segment[0].x &&
        ctx->segment[1].y == ctx->segment[0].y)
        return 0;

    ctx->segment += 1;

    ctx->meta_buffer[ctx->meta_index++] = 0; /* Set color to 0 */
    ctx->meta_buffer[ctx->meta_index++] = 2;
    ctx->meta_buffer[ctx->nsegments_index]++;
    return 0;
}
static int AddQuad(const FT_Vector *control, const FT_Vector *to,
                   void *user) {
    struct glyph_data_ctx *ctx = (struct glyph_data_ctx *)user;

    ctx->segment[1].x = control->x / kserializer_scale;
    ctx->segment[1].y = control->y / kserializer_scale;
    ctx->segment[2].x = to->x / kserializer_scale;
    ctx->segment[2].y = to->y / kserializer_scale;

    /* Some glyphs contain "bugs", where a quad segment is actually a
       linear segment with a double point. Treat it as a linear
       segment. */
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
constexpr const SeriColor start[3] = {
    SeriColor::Cyan, SeriColor::Magenta, SeriColor::Yellow};
static void SwitchSeriColor(enum SeriColor *color,
                            unsigned long long *seed,
                            enum SeriColor *_banned) {
    enum SeriColor banned = _banned ? *_banned : SeriColor::Black;
    enum SeriColor combined = (SeriColor)(static_cast<int>(*color) &
                                          static_cast<int>(banned));

    if (combined == SeriColor::Red || combined == SeriColor::Green ||
        combined == SeriColor::Blue) {
        *color = (SeriColor)(static_cast<int>(combined) ^
                             static_cast<int>(SeriColor::White));
        return;
    }
    if (*color == SeriColor::Black || *color == SeriColor::White) {
        *color = start[*seed % 3];
        *seed /= 3;
        return;
    }
    int shifted = static_cast<int>(*color) << (1 + (*seed & 1));
    *color = (SeriColor)((static_cast<int>(shifted) |
                          static_cast<int>(shifted) >> 3) &
                         static_cast<int>(SeriColor::White));
    *seed >>= 1;
}
static vec2 Mix(const vec2 a, const vec2 b, float weight) {
    return {a.x * (1.0f - weight) + b.x * weight,
            a.y * (1.0f - weight) + b.y * weight};
}
static vec2 Subt(vec2 p1, vec2 p2) {
    return {p1.x - p2.x, p1.y - p2.y};
}
static float Length(const vec2 v) {
    return (float)sqrt(v.x * v.x + v.y * v.y);
}
static vec2 Divide(const vec2 v, float f) {
    return {v.x / f, v.y / f};
}
static float Cross(vec2 a, vec2 b) { return a.x * b.y - a.y * b.x; }
static float Dot(vec2 a, vec2 b) { return a.x * b.x + a.y * b.y; }
static bool IsCorner(const vec2 a, const vec2 b,
                     float cross_threshold) {
    return Dot(a, b) <= 0 || fabs(Cross(a, b)) > cross_threshold;
}
static vec2 Normalize(vec2 v) { return Divide(v, Length(v)); }
static vec2 SegmentDirection(const vec2 *points, int npoints,
                             float param) {
    return Mix(Subt(points[1], points[0]),
               Subt(points[npoints - 1], points[npoints - 2]), param);
}
static vec2 SegmentPoint(const vec2 *points, int npoints,
                         float param) {
    return Mix(Mix(points[0], points[1], param),
               Mix(points[npoints - 2], points[npoints - 1], param),
               param);
}
static float ShoeLace(const vec2 a, const vec2 b) {
    return (b.x - a.x) * (a.y + b.y);
}
static void SerializeGlyph(FT_Face face, int code, char *meta_buffer,
                           float *point_buffer) noexcept {
    const auto load_char_err =
        !FT_Load_Char(face, code, FT_LOAD_NO_SCALE);
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
    /* Start 1 before the actual buffer. The pointer is moved in the
       move_to callback. FT_Outline_Decompose does not have a callback
       for finishing a contour. */
    ctx.segment = ((vec2 *)&point_buffer[0]) - 1;

    const auto decompose_err =
        !FT_Outline_Decompose(&face->glyph->outline, &fns, &ctx);
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
            int prev_npoints =
                meta_buffer[meta_index + 2 * (nsegments - 2) + 1];
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
            int prev_npoints =
                meta_buffer[meta_index + 2 * (nsegments - 2) + 1];
            vec2 *prev_ptr = point_ptr;
            for (int j = 0; j < nsegments - 1; ++j)
                prev_ptr += (meta_buffer[meta_index + 2 * j + 1] - 1);
            vec2 prev_direction =
                SegmentDirection(prev_ptr, prev_npoints, 1);
            int index = 0;
            vec2 *cur_points = point_ptr;
            for (int j = 0; j < nsegments; ++j, ++index) {
                meta_index++; /* SeriColor, leave empty here. */
                int npoints = meta_buffer[meta_index++];

                vec2 cur_direction =
                    SegmentDirection(cur_points, npoints, 0.0);
                vec2 new_prev_direction =
                    SegmentDirection(cur_points, npoints, 1.0);

                if (IsCorner(Normalize(prev_direction),
                             Normalize(cur_direction),
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
                meta_buffer[meta_index++] =
                    static_cast<int>(SeriColor::White);
                meta_index++; /* npoints */
            }
        } else if (len_corners == 1) {
            /* Teardrop */
            enum SeriColor colors[3] = {SeriColor::White,
                                        SeriColor::White};
            SwitchSeriColor(&colors[0], &seed, NULL);
            colors[2] = colors[0];
            SwitchSeriColor(&colors[2], &seed, NULL);

            int corner = corners[0];
            if (nsegments >= 3) {
                int m = nsegments;
                for (int i = 0; i < m; ++i) {
                    enum SeriColor c =
                        (colors + 1)[(int)(3 + 2.875 * i / (m - 1) -
                                           1.4375 + .5) -
                                     3];
                    meta_buffer[meta_index + 2 * ((corner + i) % m)] =
                        (char)c;
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

                if (spline + 1 < corner_count &&
                    corners[spline + 1] == index) {
                    ++spline;
                    enum SeriColor banned = (enum SeriColor)(
                        (spline == corner_count - 1) *
                        static_cast<int>(initial_color));
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

/* We need two rounds of decomposing, the first one will just figure
   out how much space we need to serialize the glyph, and the second
   one serializes it and generates colour mapping for the segments. */
void glyph_buffer_size(FT_Face face, int code, size_t *meta_size,
                       size_t *point_size) noexcept {
    const auto load_char_err =
        !FT_Load_Char(face, code, FT_LOAD_NO_SCALE);
    assert("Failed to load char" && load_char_err);

    FT_Outline_Funcs fns;
    fns.shift = 0;
    fns.delta = 0;
    fns.move_to = outline_functions::AddContourSize;
    fns.line_to = outline_functions::AddLinearSize;
    fns.conic_to = outline_functions::AddQuadSize;
    fns.cubic_to = outline_functions::AddCubicSize;
    struct outline_functions::GlyphLenCtx ctx = {1, 0};
    auto decompose_err =
        !FT_Outline_Decompose(&face->glyph->outline, &fns, &ctx);
    assert("FT_Outline_Decompos failed" && decompose_err);

    *meta_size = ctx.meta_size;
    *point_size = ctx.data_size * 2 * sizeof(float);
}
// field fusion //

struct UseFree {
    void operator()(void *x) { free(x); }
};
typedef struct {
    float offset_x;
    float offset_y;
    float size_x;
    float size_y;
    float bearing_x;
    float bearing_y;
    float glyph_width;
    float glyph_height;
} ff_index_entry_t;

static const GLfloat kmat4_zero_init[4][4] = {
    {0.0f, 0.0f, 0.0f, 0.0f},
    {0.0f, 0.0f, 0.0f, 0.0f},
    {0.0f, 0.0f, 0.0f, 0.0f},
    {0.0f, 0.0f, 0.0f, 0.0f}};
static bool compile_shader(const char *source, GLenum type,
                           GLuint *shader, const char *version) {
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

static void gen_extended_ascii(
    const ff_font_handle_t font_handle) noexcept {
    char32_t codepoints[0xff];
    for (ulong i = 0; i < 0xff; i += 1) {
        codepoints[i] = i;
    }
    ff_gen_glyphs(font_handle, codepoints, 0xff);
}

typedef struct {
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
} ff_uniforms_t;

static FT_Library g_ft_library;
static float g_dpi[2];
static uint g_gen_shader;
static uint g_render_shader;
static ff_uniforms_t g_uniforms;
static uint g_bbox_vao;
static uint g_bbox_vbo;
static int g_max_texture_size;
static size_t g_max_handle = 0;
static ht_fpack_map_t g_fonts;

// NLM

ff_glyphs_vector_t ff_glyphs_vector_new() {
    ulong initial_size = 2;
    ulong elem_size = sizeof(ff_glyph_t);
    ff_glyphs_vector_t result = {
        .data = (ff_glyph_t *)calloc(elem_size, initial_size),
        .size = 0,
        .capacity = elem_size * initial_size};
    return result;
}

ff_glyph_t *ff_glyphs_vector_get(ff_glyphs_vector_t *v, ulong index) {
    assert(index < v->size);
    return &v->data[index];
}

void ff_glyphs_vector_free(ff_glyphs_vector_t *v) {
    free(v->data);
    v->data = 0;
    v->capacity = 0;
    v->size = 0;
}

void ff_glyphs_vector_push(ff_glyphs_vector_t *v, ff_glyph_t glyph) {
    ulong new_size = sizeof(ff_glyph_t) * (v->size + 1);

    if (new_size > v->capacity) {
        v->capacity *= 2;
        v->data = (ff_glyph_t *)realloc(v->data, v->capacity);
        assert(v->data != NULL && "bad alloc");
    }

    v->data[v->size++] = glyph;
}

void ff_glyphs_vector_cat(ff_glyphs_vector_t *dest,
                          ff_glyphs_vector_t *src) {
    ulong elem_size = sizeof(ff_glyph_t);
    ulong src_size = src->size * elem_size;

    if (src_size > dest->capacity) {
        dest->capacity += src_size;
        dest->data =
            (ff_glyph_t *)realloc(dest->data, dest->capacity);
    }

    memcpy(dest->data + dest->size, src->data, src_size);
}

static const uint ht_fpack_table_size = 0x200;

unsigned int ff_ht_int_hash(int key) {
    ulong value = ((key >> 16) ^ key) * 0x45d9f3b;
    value = ((value >> 16) ^ value) * 0x45d9f3b;
    value = (value >> 16) ^ value;

    return value;
}

bool ht_fpack_map_entry_empty(ht_fpack_entry_t *entry) {
    return !entry->not_empty;
}

ht_fpack_entry_t ht_fpack_map_entry_new(
    int key, const ff_font_texture_pack_t value) {
    ht_fpack_entry_t result = {
        .not_empty = true,
        .key = key,
        .value = value,
        .next = 0,
    };
    return result;
}

void ht_fpack_map_entry_free(ht_fpack_entry_t entry) {
    // TODO FIX
    ht_fpack_entry_t *head = entry.next;
    while (head != NULL) {
        ht_fpack_entry_t *tmp = head;
        head = head->next;
        free(tmp);
    }
}

ht_fpack_map_t ht_fpack_map_create() {
    ht_fpack_map_t hashtable = {0};

    hashtable.entries = (ht_fpack_entry_t *)calloc(
        ht_fpack_table_size, sizeof(ht_fpack_entry_t));

    return hashtable;
}

void ht_fpack_map_set(ht_fpack_map_t *hashtable, int key,
                      ff_font_texture_pack_t value) {
    unsigned int slot = ff_ht_int_hash(key) % ht_fpack_table_size;

    ht_fpack_entry_t *entry = &hashtable->entries[slot];

    if (ht_fpack_map_entry_empty(entry)) {
        hashtable->entries[slot] = ht_fpack_map_entry_new(key, value);
        return;
    }

    ht_fpack_entry_t *prev = {0};
    ht_fpack_entry_t *head = entry;

    while (head != NULL) {
        if (head->key == key) {
            entry->value = value;
            return;
        }

        // walk to next
        prev = head;
        head = prev->next;
    }

    prev->next =
        (ht_fpack_entry_t *)calloc(1, sizeof(ht_fpack_entry_t));
    ht_fpack_entry_t next = ht_fpack_map_entry_new(key, value);
    memcpy(prev->next, &next, sizeof(ht_fpack_entry_t));
}

ff_font_texture_pack_t *ht_fpack_map_get(ht_fpack_map_t *hashtable,
                                         int key) {
    unsigned int slot = ff_ht_int_hash(key) % ht_fpack_table_size;

    ht_fpack_entry_t entry = hashtable->entries[slot];

    if (ht_fpack_map_entry_empty(&entry)) {
        return NULL;
    }

    ht_fpack_entry_t *head = &entry;
    while (head != NULL) {
        if (head->key == key) {
            return &head->value;
        }

        head = head->next;
    }

    // reaching here means there were >= 1 entries but no key match
    return NULL;
}

void ht_fpack_map_free(ht_fpack_map_t *ht) {
    for (ulong i = 0; i < ht_fpack_table_size; ++i) {
        ht_fpack_entry_t entry = ht->entries[i];

        if (ht_fpack_map_entry_empty(&entry)) continue;

        ht_fpack_map_entry_free(entry);
    }
    free(ht->entries);
}

////////////////////////////

static const uint ht_codepoint_table_size = 0x200;

bool ht_codepoint_map_entry_empty(ht_codepoint_entry_t *entry) {
    return !entry->not_empty;
}

ht_codepoint_entry_t ht_codepoint_map_entry_new(int key,
                                                ff_map_item_t value) {
    ht_codepoint_entry_t result = {
        .not_empty = true,
        .key = key,
        .value = value,
        .next = 0,
    };
    return result;
}

void ht_codepoint_map_entry_free(ht_codepoint_entry_t entry) {
    ht_codepoint_entry_t *head = entry.next;
    while (head != NULL) {
        ht_codepoint_entry_t *tmp = head;
        head = head->next;
        free(tmp);
    }
}

ht_codepoint_map_t ht_codepoint_map_create() {
    ht_codepoint_map_t hashtable = {0};

    hashtable.entries = (ht_codepoint_entry_t *)calloc(
        ht_codepoint_table_size, sizeof(ht_codepoint_entry_t));

    return hashtable;
}

void ht_codepoint_map_set(ht_codepoint_map_t *hashtable, int key,
                          ff_map_item_t value) {
    unsigned int slot = ff_ht_int_hash(key) % ht_codepoint_table_size;

    ht_codepoint_entry_t *entry = &hashtable->entries[slot];

    if (ht_codepoint_map_entry_empty(entry)) {
        hashtable->entries[slot] =
            ht_codepoint_map_entry_new(key, value);
        return;
    }

    ht_codepoint_entry_t *prev = {0};
    ht_codepoint_entry_t *head = entry;

    while (head != NULL) {
        if (head->key == key) {
            entry->value = value;
            return;
        }

        // walk to next
        prev = head;
        head = prev->next;
    }

    prev->next = (ht_codepoint_entry_t *)calloc(
        1, sizeof(ht_codepoint_entry_t));
    ht_codepoint_entry_t next =
        ht_codepoint_map_entry_new(key, value);
    memcpy(prev->next, &next, sizeof(ht_codepoint_entry_t));
}

ff_map_item_t *ht_codepoint_map_get(ht_codepoint_map_t *hashtable,
                                    int key) {
    unsigned int slot = ff_ht_int_hash(key) % ht_codepoint_table_size;

    ht_codepoint_entry_t entry = hashtable->entries[slot];

    if (ht_codepoint_map_entry_empty(&entry)) {
        return NULL;
    }

    ht_codepoint_entry_t *head = &entry;
    while (head != NULL) {
        if (head->key == key) {
            return &head->value;
        }

        head = head->next;
    }

    // reaching here means there were >= 1 entries but no key match
    return NULL;
}

void ht_codepoint_map_free(ht_codepoint_map_t *ht) {
    for (ulong i = 0; i < ht_codepoint_table_size; ++i) {
        ht_codepoint_entry_t entry = ht->entries[i];

        if (ht_codepoint_map_entry_empty(&entry)) continue;

        ht_codepoint_map_entry_free(entry);
    }
    free(ht->entries);
}

void ff_get_ortho_projection(ortho_projection_params_t params,
                             float dest[][4]) {
    GLfloat rl, tb, fn;

    memcpy(dest, kmat4_zero_init, sizeof(kmat4_zero_init));

    rl = 1.0f / (params.scr_right - params.scr_left);
    tb = 1.0f / (params.scr_top - params.scr_bottom);
    fn = -1.0f / (params.far - params.near);

    dest[0][0] = 2.0f * rl;
    dest[1][1] = 2.0f * tb;
    dest[2][2] = 2.0f * fn;
    dest[3][0] = -(params.scr_right + params.scr_left) * rl;
    dest[3][1] = -(params.scr_top + params.scr_bottom) * tb;
    dest[3][2] = (params.far + params.near) * fn;
    dest[3][3] = 1.0f;
}

void ff_initialize(const char *version) noexcept {
    const FT_Error error = FT_Init_FreeType(&g_ft_library);
    assert("Failed to initialize freetype2" && !error);

    glGetIntegerv(GL_MAX_TEXTURE_SIZE, &g_max_texture_size);

    unsigned vertex_shader, geometry_shader, fragment_shader;
    auto err = compile_shader(kmsdf_vertex, GL_VERTEX_SHADER,
                              &vertex_shader, version);
    assert("Failed to compile msdf vertex shader" && err);
    err = compile_shader(kmsdf_fragment, GL_FRAGMENT_SHADER,
                         &fragment_shader, version);
    assert("Failed to compile msdf fragment shader" && err);
    err = (g_gen_shader = glCreateProgram());
    assert("Failed to generate shader program");

    glAttachShader(g_gen_shader, vertex_shader);
    glAttachShader(g_gen_shader, fragment_shader);
    glLinkProgram(g_gen_shader);
    glDeleteShader(vertex_shader);
    glDeleteShader(fragment_shader);

    auto link_status = 0;
    glGetProgramiv(g_gen_shader, GL_LINK_STATUS, &link_status);
    assert("Failed to link g_gen_shader" && link_status);

    g_uniforms.atlas_projection =
        glGetUniformLocation(g_gen_shader, "projection");
    g_uniforms.texture_offset =
        glGetUniformLocation(g_gen_shader, "offset");
    g_uniforms.glyph_height =
        glGetUniformLocation(g_gen_shader, "glyph_height");
    g_uniforms.translate =
        glGetUniformLocation(g_gen_shader, "translate");
    g_uniforms.scale = glGetUniformLocation(g_gen_shader, "scale");
    g_uniforms.range = glGetUniformLocation(g_gen_shader, "range");
    g_uniforms.meta_offset =
        glGetUniformLocation(g_gen_shader, "meta_offset");
    g_uniforms.point_offset =
        glGetUniformLocation(g_gen_shader, "point_offset");
    g_uniforms.metadata =
        glGetUniformLocation(g_gen_shader, "metadata");
    g_uniforms.point_data =
        glGetUniformLocation(g_gen_shader, "point_data");

    compile_shader(kfont_vertex, GL_VERTEX_SHADER, &vertex_shader,
                   version);
    err = compile_shader(kfont_geometry, GL_GEOMETRY_SHADER,
                         &geometry_shader, version);
    assert("Failed to compile geometry shader" && err);
    err = compile_shader(kfont_fragment, GL_FRAGMENT_SHADER,
                         &fragment_shader, version);
    assert("Failed to compile font fragment shader" && err);
    err = (g_render_shader = glCreateProgram());
    assert("Faile to create g_render_shader" && err);

    glAttachShader(g_render_shader, vertex_shader);
    glAttachShader(g_render_shader, geometry_shader);
    glAttachShader(g_render_shader, fragment_shader);
    glLinkProgram(g_render_shader);
    glDeleteShader(vertex_shader);
    glDeleteShader(geometry_shader);
    glDeleteShader(fragment_shader);

    glGetProgramiv(g_render_shader, GL_LINK_STATUS, &link_status);
    assert("Failed to link g_render_shader" && link_status);

    g_uniforms.window_projection =
        glGetUniformLocation(g_render_shader, "projection");
    g_uniforms.font_atlas_projection =
        glGetUniformLocation(g_render_shader, "font_projection");
    g_uniforms.index =
        glGetUniformLocation(g_render_shader, "font_index");
    g_uniforms.atlas =
        glGetUniformLocation(g_render_shader, "font_atlas");
    g_uniforms.padding =
        glGetUniformLocation(g_render_shader, "padding");
    g_uniforms.dpi = glGetUniformLocation(g_render_shader, "dpi");
    g_uniforms.units_per_em =
        glGetUniformLocation(g_render_shader, "units_per_em");
    g_dpi[0] = 72.0;
    g_dpi[1] = 72.0;

    glGenVertexArrays(1, &g_bbox_vao);
    glGenBuffers(1, &g_bbox_vbo);
    glBindBuffer(GL_ARRAY_BUFFER, g_bbox_vbo);
    glBufferData(GL_ARRAY_BUFFER, 12 * sizeof(float), 0,
                 GL_STREAM_READ);
    glBindBuffer(GL_ARRAY_BUFFER, 0);
}

ff_font_config_t ff_default_font_config(void) {
    return (ff_font_config_t){
        .scale = 2.4f,
        .range = 2.0f,
        .texture_width = 1024,
        .texture_padding = 4,
    };
}

ff_font_handle_t ff_new_font(const char *path,
                             ff_font_config_t config) {
    ff_font_handle_t handle = g_max_handle;
    ht_fpack_map_set(&g_fonts, handle, (ff_font_texture_pack_t){0});
    ff_font_texture_pack_t *fpack =
        ht_fpack_map_get(&g_fonts, handle);
    fpack->font.font_path = path;
    fpack->font.scale = config.scale;
    fpack->font.range = config.range;

    fpack->atlas.texture_width = config.texture_width;
    fpack->atlas.padding = config.texture_padding;
    glGenBuffers(1, &fpack->atlas.index_buffer);
    glGenTextures(1, &fpack->atlas.index_texture);
    glGenTextures(1, &fpack->atlas.atlas_texture);
    glGenFramebuffers(1, &fpack->atlas.atlas_framebuffer);

    auto err = !FT_New_Face(g_ft_library, path, 0, &fpack->font.face);
    assert(
        "Failed to create a new font face, font path is probably "
        "invalid" &&
        err);
    FT_Select_Charmap(fpack->font.face, ft_encoding_unicode);
    fpack->font.vertical_advance =
        (float)(fpack->font.face->ascender -
                fpack->font.face->descender);

    glGenBuffers(1, &fpack->font.meta_input_buffer);
    glGenBuffers(1, &fpack->font.point_input_buffer);
    glGenTextures(1, &fpack->font.meta_input_texture);
    glGenTextures(1, &fpack->font.point_input_texture);

    gen_extended_ascii(handle);
    g_max_handle += 1;
    return handle;
}

void ff_remove_font(const ff_font_handle_t handle) {
    ff_font_texture_pack_t *fpack =
        ht_fpack_map_get(&g_fonts, handle);
    if (fpack == NULL) return;
    FT_Done_Face(fpack->font.face);
    glDeleteBuffers(1, &fpack->font.meta_input_buffer);
    glDeleteBuffers(1, &fpack->font.point_input_buffer);
    glDeleteBuffers(1, &fpack->font.meta_input_texture);
    glDeleteBuffers(1, &fpack->font.point_input_texture);
    glDeleteBuffers(1, &fpack->atlas.index_buffer);
    glDeleteTextures(1, &fpack->atlas.index_texture);
    glDeleteTextures(1, &fpack->atlas.atlas_texture);
    glDeleteFramebuffers(1, &fpack->atlas.atlas_framebuffer);

    // _fonts.erase(handle);  // TODO
}

ff_map_item_t *ff_map_get(ff_map_t *o, char32_t codepoint) {
    if (codepoint < 0xff) return &o->extended_ascii_[codepoint];
    ff_map_item_t *item =
        ht_codepoint_map_get(&o->codepoint_map, codepoint);
    if (item != NULL) {
        return item;
    }
    return NULL;
}

ff_map_item_t *ff_map_insert(ff_map_t *o, const char32_t codepoint) {
    if (codepoint < 0xff) {
        o->extended_ascii_[codepoint].codepoint = codepoint;
        o->extended_ascii_[codepoint].codepoint_index =
            codepoint;  // TODO
        return &(o->extended_ascii_[codepoint]);
    }

    ht_codepoint_map_set(&o->codepoint_map, codepoint,
                         (ff_map_item_t){0});
    ff_map_item_t *item =
        ht_codepoint_map_get(&o->codepoint_map, codepoint);
    return item;
}

void ff_gen_glyphs(ff_font_handle_t handle, char32_t *codepoints,
                   ulong codepoints_count) {
    ff_font_texture_pack_t *fpack =
        ht_fpack_map_get(&g_fonts, handle);
    GLint original_viewport[4];
    glGetIntegerv(GL_VIEWPORT, original_viewport);
    int nrender = codepoints_count;

    if (nrender <= 0) return;

    /* Calculate the amount of memory needed on the GPU.*/
    ulong *meta_sizes = (ulong *)calloc(sizeof(ulong), nrender);
    ulong *point_sizes = (ulong *)calloc(sizeof(ulong), nrender);

    /* We will start with a square texture. */
    int new_texture_height =
        fpack->atlas.texture_height ? fpack->atlas.texture_height : 1;
    int new_index_size =
        fpack->atlas.nallocated ? fpack->atlas.nallocated : 1;

    /* Amount of new memory needed for the index. */
    ff_index_entry_t *atlas_index =
        (ff_index_entry_t *)calloc(sizeof(ff_index_entry_t), nrender);

    size_t meta_size_sum = 0, point_size_sum = 0;
    for (size_t i = 0; (int)i < (int)nrender; ++i) {  // MARK
        int code = codepoints[i];
        glyph_buffer_size(fpack->font.face, code, &meta_sizes[i],
                          &point_sizes[i]);
        meta_size_sum += meta_sizes[i];
        point_size_sum += point_sizes[i];
    }

    /* Allocate the calculated amount. */

    char *point_data = (char *)calloc(point_size_sum, 1);
    char *metadata = (char *)calloc(meta_size_sum, 1);

    /* Serialize the glyphs into RAM. */
    char *meta_ptr = metadata;
    char *point_ptr = point_data;
    for (size_t i = 0; (int)i < (int)nrender; ++i) {
        float buffer_width, buffer_height;

        int code = codepoints[i];
        SerializeGlyph(fpack->font.face, code, meta_ptr,
                       (GLfloat *)point_ptr);
        auto m = ff_map_insert(&fpack->font.character_index, code);
        m->advance[0] =
            (float)fpack->font.face->glyph->metrics.horiAdvance;
        m->advance[1] =
            (float)fpack->font.face->glyph->metrics.vertAdvance;

        buffer_width = fpack->font.face->glyph->metrics.width /
                           kserializer_scale +
                       fpack->font.range;
        buffer_height = fpack->font.face->glyph->metrics.height /
                            kserializer_scale +
                        fpack->font.range;
        buffer_width *= fpack->font.scale;
        buffer_height *= fpack->font.scale;

        meta_ptr += meta_sizes[i];
        point_ptr += point_sizes[i];

        if (fpack->atlas.offset_x + buffer_width >
            fpack->atlas.texture_width) {
            fpack->atlas.offset_y +=
                (fpack->atlas.y_increment + fpack->atlas.padding);
            fpack->atlas.offset_x = 1;
            fpack->atlas.y_increment = 0;
        }
        fpack->atlas.y_increment =
            (size_t)buffer_height > fpack->atlas.y_increment
                ? (size_t)buffer_height
                : fpack->atlas.y_increment;

        atlas_index[i].offset_x = fpack->atlas.offset_x;
        atlas_index[i].offset_y = fpack->atlas.offset_y;
        atlas_index[i].size_x = buffer_width;
        atlas_index[i].size_y = buffer_height;
        atlas_index[i].bearing_x =
            fpack->font.face->glyph->metrics.horiBearingX;
        atlas_index[i].bearing_y =
            fpack->font.face->glyph->metrics.horiBearingY;
        atlas_index[i].glyph_width =
            fpack->font.face->glyph->metrics.width;
        atlas_index[i].glyph_height =
            fpack->font.face->glyph->metrics.height;

        fpack->atlas.offset_x +=
            (size_t)buffer_width + fpack->atlas.padding;

        while ((fpack->atlas.offset_y + buffer_height) >
               new_texture_height) {
            new_texture_height *= 2;
        }
        assert("Exceeded maximum texture size" &&
               new_texture_height <= g_max_texture_size);

        while ((int)(fpack->atlas.nglyphs + i) >= new_index_size) {
            new_index_size *= 2;
        }
    }

    /* Allocate and fill the buffers on GPU. */
    glBindBuffer(GL_ARRAY_BUFFER, fpack->font.meta_input_buffer);
    glBufferData(GL_ARRAY_BUFFER, meta_size_sum, metadata,
                 GL_DYNAMIC_READ);

    glBindBuffer(GL_ARRAY_BUFFER, fpack->font.point_input_buffer);
    glBufferData(GL_ARRAY_BUFFER, point_size_sum, point_data,
                 GL_DYNAMIC_READ);

    if ((int)fpack->atlas.nallocated == new_index_size) {
        glBindBuffer(GL_ARRAY_BUFFER, fpack->atlas.index_buffer);
    } else {
        GLuint new_buffer;
        glGenBuffers(1, &new_buffer);
        glBindBuffer(GL_ARRAY_BUFFER, new_buffer);
        glBufferData(GL_ARRAY_BUFFER,
                     sizeof(ff_index_entry_t) * new_index_size, 0,
                     GL_DYNAMIC_READ);
        assert("Out of gpu memory" &&
               glGetError() != GL_OUT_OF_MEMORY);
        if (fpack->atlas.nglyphs) {
            glBindBuffer(GL_COPY_READ_BUFFER,
                         fpack->atlas.index_buffer);
            glCopyBufferSubData(
                GL_COPY_READ_BUFFER, GL_ARRAY_BUFFER, 0, 0,
                fpack->atlas.nglyphs * sizeof(ff_index_entry_t));
            glBindBuffer(GL_COPY_READ_BUFFER, 0);
        }
        fpack->atlas.nallocated = new_index_size;
        glDeleteBuffers(1, &fpack->atlas.index_buffer);
        fpack->atlas.index_buffer = new_buffer;
    }
    const size_t index_size = nrender * sizeof(ff_index_entry_t);
    glBufferSubData(GL_ARRAY_BUFFER,
                    sizeof(ff_index_entry_t) * fpack->atlas.nglyphs,
                    index_size, atlas_index);

    glBindBuffer(GL_ARRAY_BUFFER, 0);

    /* Link sampler textures to the buffers. */
    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_BUFFER, fpack->font.meta_input_texture);
    glTexBuffer(GL_TEXTURE_BUFFER, GL_R8UI,
                fpack->font.meta_input_buffer);
    glBindTexture(GL_TEXTURE_BUFFER, 0);

    glActiveTexture(GL_TEXTURE1);
    glBindTexture(GL_TEXTURE_BUFFER, fpack->font.point_input_texture);
    glTexBuffer(GL_TEXTURE_BUFFER, GL_R32F,
                fpack->font.point_input_buffer);
    glBindTexture(GL_TEXTURE_BUFFER, 0);

    glActiveTexture(GL_TEXTURE2);
    glBindTexture(GL_TEXTURE_BUFFER, fpack->atlas.index_texture);
    glTexBuffer(GL_TEXTURE_BUFFER, GL_R32F,
                fpack->atlas.index_buffer);
    glBindTexture(GL_TEXTURE_BUFFER, 0);

    glActiveTexture(GL_TEXTURE0);

    /* Generate the atlas texture and bind it as the framebuffer. */
    if (fpack->atlas.texture_height == new_texture_height) {
        /* No need to extend the texture. */
        glBindFramebuffer(GL_DRAW_FRAMEBUFFER,
                          fpack->atlas.atlas_framebuffer);
        glBindTexture(GL_TEXTURE_2D, fpack->atlas.atlas_texture);
        glViewport(0, 0, fpack->atlas.texture_width,
                   fpack->atlas.texture_height);
    } else {
        GLuint new_texture;
        GLuint new_framebuffer;
        glGenTextures(1, &new_texture);
        glGenFramebuffers(1, &new_framebuffer);
        glBindFramebuffer(GL_DRAW_FRAMEBUFFER, new_framebuffer);

        glBindTexture(GL_TEXTURE_2D, new_texture);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER,
                        GL_LINEAR);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER,
                        GL_LINEAR);

        glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA32F,
                     fpack->atlas.texture_width, new_texture_height,
                     0, GL_RGBA, GL_FLOAT, NULL);
        assert("Out of gpu memory" &&
               glGetError() != GL_OUT_OF_MEMORY);

        glFramebufferTexture2D(GL_DRAW_FRAMEBUFFER,
                               GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D,
                               new_texture, 0);
        glViewport(0, 0, fpack->atlas.texture_width,
                   new_texture_height);
        glClearColor(0.0, 0.0, 0.0, 1.0);
        glClear(GL_COLOR_BUFFER_BIT);

        if (fpack->atlas.texture_height) {
            /* Old texture had data -> copy. */
            glBindFramebuffer(GL_READ_FRAMEBUFFER,
                              fpack->atlas.atlas_framebuffer);
            glBlitFramebuffer(0, 0, fpack->atlas.texture_width,
                              fpack->atlas.texture_height, 0, 0,
                              fpack->atlas.texture_width,
                              fpack->atlas.texture_height,
                              GL_COLOR_BUFFER_BIT, GL_NEAREST);
            glBindFramebuffer(GL_READ_FRAMEBUFFER, 0);
        }

        glBindFramebuffer(GL_READ_FRAMEBUFFER, 0);
        fpack->atlas.texture_height = new_texture_height;
        glDeleteTextures(1, &fpack->atlas.atlas_texture);
        fpack->atlas.atlas_texture = new_texture;
        glDeleteFramebuffers(1, &fpack->atlas.atlas_framebuffer);
        fpack->atlas.atlas_framebuffer = new_framebuffer;
    }
    glBindTexture(GL_TEXTURE_2D, 0);

    GLfloat framebuffer_projection[4][4];
    ff_get_ortho_projection(
        (ortho_projection_params_t){
            .scr_left = 0,
            .scr_right = (float)fpack->atlas.texture_width,
            .scr_top = 0,
            .scr_bottom = (float)fpack->atlas.texture_height,
            .near = -1.0f,
            .far = 1.0f},
        framebuffer_projection);
    ff_get_ortho_projection(
        (ortho_projection_params_t){
            .scr_left = -(float)fpack->atlas.texture_width,
            .scr_right = (float)fpack->atlas.texture_width,
            .scr_top = -(float)fpack->atlas.texture_height,
            .scr_bottom = (float)fpack->atlas.texture_height,
            .near = -1.0f,
            .far = 1.0f},
        fpack->atlas.projection);

    glUseProgram(g_gen_shader);
    glUniform1i(g_uniforms.metadata, 0);
    glUniform1i(g_uniforms.point_data, 1);

    glUniformMatrix4fv(g_uniforms.atlas_projection, 1, GL_FALSE,
                       (GLfloat *)framebuffer_projection);

    glUniform2f(g_uniforms.scale, fpack->font.scale,
                fpack->font.scale);
    glUniform1f(g_uniforms.range, fpack->font.range);
    glUniform1i(g_uniforms.meta_offset, 0);
    glUniform1i(g_uniforms.point_offset, 0);

    if (glCheckFramebufferStatus(GL_FRAMEBUFFER) !=
        GL_FRAMEBUFFER_COMPLETE)
        fprintf(stderr, "msdfgl: framebuffer incomplete: %x\n",
                glCheckFramebufferStatus(GL_FRAMEBUFFER));

    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_BUFFER, fpack->font.meta_input_texture);

    glActiveTexture(GL_TEXTURE1);
    glBindTexture(GL_TEXTURE_BUFFER, fpack->font.point_input_texture);

    glBindVertexArray(g_bbox_vao);
    glBindBuffer(GL_ARRAY_BUFFER, g_bbox_vbo);

    glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE,
                          2 * sizeof(GLfloat), 0);
    glEnableVertexAttribArray(0);

    int meta_offset = 0;
    int point_offset = 0;
    for (int i = 0; i < nrender; ++i) {
        ff_index_entry_t g = atlas_index[i];
        float w = g.size_x;
        float h = g.size_y;
        GLfloat bounding_box[] = {0, 0, w, 0, 0, h, 0, h, w, 0, w, h};
        glBufferSubData(GL_ARRAY_BUFFER, 0, sizeof(bounding_box),
                        bounding_box);

        glUniform2f(
            g_uniforms.translate,
            -g.bearing_x / kserializer_scale +
                fpack->font.range / 2.0f,
            (g.glyph_height - g.bearing_y) / kserializer_scale +
                fpack->font.range / 2.0f);

        glUniform2f(g_uniforms.texture_offset, g.offset_x,
                    g.offset_y);
        glUniform1i(g_uniforms.meta_offset, meta_offset);
        glUniform1i(g_uniforms.point_offset,
                    point_offset / (2 * sizeof(GLfloat)));
        glUniform1f(g_uniforms.glyph_height, g.size_y);

        /* No need for draw call if there are no contours */
        if (((unsigned char *)metadata)[meta_offset])
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

    fpack->atlas.nglyphs += nrender;

    glViewport(original_viewport[0], original_viewport[1],
               original_viewport[2], original_viewport[3]);

    free(meta_sizes);
    free(point_sizes);
    free(atlas_index);
    free(point_data);
    free(metadata);
}

void ff_draw(ff_font_handle_t handle, ff_glyph_t *glyphs,
             ulong glyphs_count, float *projection) {
    ff_font_texture_pack_t *fpack =
        ht_fpack_map_get(&g_fonts, handle);

    GLuint glyph_buffer;
    GLuint vao;
    glGenBuffers(1, &glyph_buffer);
    glGenVertexArrays(1, &vao);
    glBindVertexArray(vao);
    glBindBuffer(GL_ARRAY_BUFFER, glyph_buffer);
    glBufferData(GL_ARRAY_BUFFER, glyphs_count * sizeof(ff_glyph_t),
                 &glyphs[0], GL_DYNAMIC_DRAW);

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
    auto offset_offset =
        (void *)offsetof(ff_glyph_t, characteristics.offset);
    auto skew_offset =
        (void *)offsetof(ff_glyph_t, characteristics.skew);
    auto strength_offset =
        (void *)offsetof(ff_glyph_t, characteristics.strength);

    glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE,
                          sizeof(ff_glyph_t), position_offset);
    glVertexAttribIPointer(1, 4, GL_UNSIGNED_BYTE, sizeof(ff_glyph_t),
                           color_offset);
    glVertexAttribIPointer(2, 1, GL_INT, sizeof(ff_glyph_t),
                           codepoint_offset);
    glVertexAttribPointer(3, 1, GL_FLOAT, GL_FALSE,
                          sizeof(ff_glyph_t), size_offset);
    glVertexAttribPointer(4, 1, GL_FLOAT, GL_FALSE,
                          sizeof(ff_glyph_t), offset_offset);
    glVertexAttribPointer(5, 1, GL_FLOAT, GL_FALSE,
                          sizeof(ff_glyph_t), skew_offset);
    glVertexAttribPointer(6, 1, GL_FLOAT, GL_FALSE,
                          sizeof(ff_glyph_t), strength_offset);

    /* Enable gamma correction if user didn't enabled it */
    auto is_srgb_enabled = glIsEnabled(GL_FRAMEBUFFER_SRGB);
    bool srgb_enabled_by_fn = !is_srgb_enabled;
    if (!is_srgb_enabled) glEnable(GL_FRAMEBUFFER_SRGB);

    glUseProgram(g_render_shader);
    /* Bind atlas texture and index buffer. */
    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, fpack->atlas.atlas_texture);
    glUniform1i(g_uniforms.atlas, 0);

    glActiveTexture(GL_TEXTURE1);
    glBindTexture(GL_TEXTURE_BUFFER, fpack->atlas.index_texture);
    glUniform1i(g_uniforms.index, 1);

    glUniformMatrix4fv(g_uniforms.font_atlas_projection, 1, GL_FALSE,
                       (GLfloat *)fpack->atlas.projection);

    glUniformMatrix4fv(g_uniforms.window_projection, 1, GL_FALSE,
                       projection);
    glUniform1f(
        g_uniforms.padding,
        (GLfloat)(fpack->font.range / 2.0 * kserializer_scale));
    glUniform1f(g_uniforms.units_per_em,
                (GLfloat)fpack->font.face->units_per_EM);
    glUniform2fv(g_uniforms.dpi, 1, g_dpi);

    /* Render the glyphs. */
    glDrawArrays(GL_POINTS, 0, glyphs_count);

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

ff_glyphs_vector_t ff_print_unicode(print_params_t params,
                                    ff_position_t position) {
    ff_font_texture_pack_t *fpack =
        ht_fpack_map_get(&g_fonts, params.typography.font);
    auto pos0 = ff_position_t{position.x,
                              position.y + params.typography.size};

    ff_glyphs_vector_t result = ff_glyphs_vector_new();
    // TODO result.reserve(buffer.size());
    for (size_t i = 0; i < params.str_count; i++) {
        // const auto &codepoint = (char32_t)buffer.at(i);
        char32_t codepoint = params.str[i];

        ff_map_item_t *idx =
            ff_map_get(&fpack->font.character_index, codepoint);
        if (idx == NULL) {
            ff_gen_glyphs(params.typography.font, &codepoint, 1);
            idx = ff_map_get(&fpack->font.character_index, codepoint);
            assert(idx != NULL);
        }

        FT_Vector kerning{};
        const bool should_get_kerning =
            (params.print_flags & ff_print_options_enable_kerning) &&
            FT_HAS_KERNING(fpack->font.face) && (i > 0);
        if (should_get_kerning) {
            char32_t previous_character = params.str[i - 1];
            FT_Get_Kerning(
                fpack->font.face,
                FT_Get_Char_Index(fpack->font.face,
                                  previous_character),
                FT_Get_Char_Index(fpack->font.face, codepoint),
                FT_KERNING_UNSCALED, &kerning);
        }

        ff_glyphs_vector_push(&result, (ff_glyph_t){0});
        ff_glyph_t *new_glyph = &result.data[result.size - 1];

        new_glyph->position = pos0;
        new_glyph->color = params.typography.color;
        new_glyph->codepoint = idx->codepoint_index;
        new_glyph->size = params.typography.size;
        new_glyph->characteristics = params.characteristics;

        if (!(params.print_flags & ff_print_options_print_vertically))
            pos0.x += (idx->advance[0] + kerning.x) *
                      (params.typography.size * g_dpi[0] / 72.0f) /
                      fpack->font.face->units_per_EM;
        else
            pos0.y += (idx->advance[1] + kerning.y) *
                      (params.typography.size * g_dpi[0] / 72.0f) /
                      fpack->font.face->units_per_EM;
    }

    return result;
}

ff_dimensions_t ff_measure(const ff_font_handle_t handle,
                           char32_t *str, ulong str_count, float size,
                           bool with_kerning) {
    ff_font_texture_pack_t *fpack =
        ht_fpack_map_get(&g_fonts, handle);

    ff_dimensions_t result = {0};
    auto tmp_width{0.0f};

    for (size_t i = 0; i < str_count; i++) {
        auto &codepoint = str[i];

        auto idx =
            ff_map_get(&fpack->font.character_index, codepoint);
        if (idx == nullptr) {
            ff_gen_glyphs(handle, &codepoint, 1);
            idx = ff_map_get(&fpack->font.character_index, codepoint);
            assert(idx != NULL);
        }

        FT_Vector kerning{};
        const bool should_get_kerning =
            with_kerning and FT_HAS_KERNING(fpack->font.face) and
            (i > 0);
        if (should_get_kerning) {
            const auto &previous_character = str[i - 1];
            FT_Get_Kerning(
                fpack->font.face,
                FT_Get_Char_Index(fpack->font.face,
                                  previous_character),
                FT_Get_Char_Index(fpack->font.face, codepoint),
                FT_KERNING_UNSCALED, &kerning);
        }

        auto height = (idx->advance[1] + kerning.y) *
                      (size * g_dpi[1] / 72.0f) /
                      fpack->font.face->units_per_EM;
        result.height = std::max(result.height, height);
        result.width += (idx->advance[0] + kerning.x) *
                        (size * g_dpi[0] / 72.0f) /
                        fpack->font.face->units_per_EM;
    }

    result.width = std::max(result.width, tmp_width);

    return result;
}

void ff_terminate() {
    for (ulong i = 0; i < g_max_handle; i += 1) ff_remove_font(i);
    FT_Done_FreeType(g_ft_library);
}

// /impl
#endif

#ifdef __cplusplus
}
#endif
