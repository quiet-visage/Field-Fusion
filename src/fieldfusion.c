#include "fieldfusion.h"

#include <math.h>
#include <sys/types.h>
#include <uchar.h>

// clang-format off
#define __FF_EMBED_FILE(var_name, file_name)          \
    __asm__("" #var_name ": .incbin \"" #file_name "\""); \
    __asm__("" #var_name "_end: .byte 0")                  

__FF_EMBED_FILE(g_font_vertex, shaders/ff_font.vert);
__FF_EMBED_FILE(g_font_fragment, shaders/ff_font.frag);
__FF_EMBED_FILE(g_font_geometry, shaders/ff_font.geo);
__FF_EMBED_FILE(g_msdf_vertex, shaders/ff_msdf.vert);
__FF_EMBED_FILE(g_msdf_fragment, shaders/ff_msdf.frag);
// clang-format on

extern const char g_font_fragment[];
extern const char g_font_geometry[];
extern const char g_msdf_vertex[];
extern const char g_font_vertex[];
extern const char g_msdf_fragment[];

static const float g_serializer_scale = 64;

typedef enum {
    ff_serializer_black_t = 0,
    ff_serializer_red_t = 1,
    ff_serializer_green_t = 2,
    ff_serializer_yellow_t = 3,
    ff_serializer_blue_t = 4,
    ff_serializer_magenta_t = 5,
    ff_serializer_cyan_t = 6,
    ff_serializer_white_t = 7
} ff_serializer_color_t;

typedef struct {
    float x;
    float y;
} vec2;

struct GlyphLenCtx {
    int meta_size;
    int data_size;
};

static int ff_serializer_add_contour_size(const FT_Vector *to,
                                          void *user) {
    (void)to;
    struct GlyphLenCtx *ctx = (struct GlyphLenCtx *)user;
    ctx->data_size += 1;
    ctx->meta_size += 2; /* winding + nsegments */
    return 0;
}
static int ff_serializer_add_linear_size(const FT_Vector *to,
                                         void *user) {
    (void)to;
    struct GlyphLenCtx *ctx = (struct GlyphLenCtx *)user;
    ctx->data_size += 1;
    ctx->meta_size += 2; /* color + npoints */
    return 0;
}
static int ff_serializer_add_quad_size(const FT_Vector *control,
                                       const FT_Vector *to,
                                       void *user) {
    (void)control;
    (void)to;
    struct GlyphLenCtx *ctx = (struct GlyphLenCtx *)user;
    ctx->data_size += 2;
    ctx->meta_size += 2; /* color + npoints */
    return 0;
}
static int ff_serializer_add_cubic_size(const FT_Vector *control1,
                                        const FT_Vector *control2,
                                        const FT_Vector *to,
                                        void *s) {
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

static int ff_serializer_add_contour(const FT_Vector *to,
                                     void *user) {
    struct glyph_data_ctx *ctx = (struct glyph_data_ctx *)user;
    ctx->segment += 1; /* Start contour on a fresh glyph. */
    ctx->segment[0].x = to->x / g_serializer_scale;
    ctx->segment[0].y = to->y / g_serializer_scale;
    ctx->meta_buffer[0] += 1; /* Increase the number of contours. */
    ctx->meta_buffer[ctx->meta_index++] = 0; /* Set winding to zero */
    ctx->nsegments_index = ctx->meta_index++;
    ctx->meta_buffer[ctx->nsegments_index] = 0;

    return 0;
}

static int ff_serializer_add_linear(const FT_Vector *to, void *user) {
    struct glyph_data_ctx *ctx = (struct glyph_data_ctx *)user;
    ctx->segment[1].x = to->x / g_serializer_scale;
    ctx->segment[1].y = to->y / g_serializer_scale;

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

static int ff_serializer_add_quad(const FT_Vector *control,
                                  const FT_Vector *to, void *user) {
    struct glyph_data_ctx *ctx = (struct glyph_data_ctx *)user;

    ctx->segment[1].x = control->x / g_serializer_scale;
    ctx->segment[1].y = control->y / g_serializer_scale;
    ctx->segment[2].x = to->x / g_serializer_scale;
    ctx->segment[2].y = to->y / g_serializer_scale;

    /* Some glyphs contain "bugs", where a quad segment is actually a
       linear segment with a double point. Treat it as a linear
       segment. */
    if ((ctx->segment[1].x == ctx->segment[0].x &&
         ctx->segment[1].y == ctx->segment[0].y) ||
        (ctx->segment[2].x == ctx->segment[1].x &&
         ctx->segment[2].y == ctx->segment[1].y))
        return ff_serializer_add_linear(to, user);

    ctx->segment += 2;

    ctx->meta_buffer[ctx->meta_index++] = 0; /* Set color to 0 */
    ctx->meta_buffer[ctx->meta_index++] = 3;
    ctx->meta_buffer[ctx->nsegments_index]++;
    return 0;
}

static void ff_serializer_switch_color(
    ff_serializer_color_t *color, unsigned long long *seed,
    ff_serializer_color_t *_banned) {
    static const ff_serializer_color_t start[3] = {
        ff_serializer_cyan_t, ff_serializer_magenta_t,
        ff_serializer_yellow_t};
    ff_serializer_color_t banned =
        _banned ? *_banned : ff_serializer_black_t;
    ff_serializer_color_t combined =
        (ff_serializer_color_t)((int)*color & (int)banned);

    if (combined == ff_serializer_red_t ||
        combined == ff_serializer_green_t ||
        combined == ff_serializer_blue_t) {
        *color = (ff_serializer_color_t)((int)combined ^
                                         (int)ff_serializer_white_t);
        return;
    }
    if (*color == ff_serializer_black_t ||
        *color == ff_serializer_white_t) {
        *color = start[*seed % 3];
        *seed /= 3;
        return;
    }
    int shifted = (int)(*color) << (1 + (*seed & 1));
    *color = (ff_serializer_color_t)(((int)(shifted) |
                                      (int)(shifted) >> 3) &
                                     (int)(ff_serializer_white_t));
    *seed >>= 1;
}

static vec2 ff_serializer_mix(const vec2 a, const vec2 b,
                              float weight) {
    return (vec2){a.x * (1.0f - weight) + b.x * weight,
                  a.y * (1.0f - weight) + b.y * weight};
}
static vec2 ff_serializer_subt(vec2 p1, vec2 p2) {
    return (vec2){p1.x - p2.x, p1.y - p2.y};
}
static float ff_serializer_length(const vec2 v) {
    return (float)sqrt(v.x * v.x + v.y * v.y);
}
static vec2 ff_serializer_divide(const vec2 v, float f) {
    return (vec2){v.x / f, v.y / f};
}
static float ff_serializer_cross(vec2 a, vec2 b) {
    return a.x * b.y - a.y * b.x;
}
static float ff_serializer_dot(vec2 a, vec2 b) {
    return a.x * b.x + a.y * b.y;
}
static bool ff_serializer_is_corner(const vec2 a, const vec2 b,
                                    float cross_threshold) {
    return ff_serializer_dot(a, b) <= 0 ||
           fabs(ff_serializer_cross(a, b)) > cross_threshold;
}
static vec2 ff_serializer_normalize(vec2 v) {
    return ff_serializer_divide(v, ff_serializer_length(v));
}
static vec2 ff_serializer_segment_direction(const vec2 *points,
                                            int npoints,
                                            float param) {
    return ff_serializer_mix(
        ff_serializer_subt(points[1], points[0]),
        ff_serializer_subt(points[npoints - 1], points[npoints - 2]),
        param);
}
static vec2 ff_serializer_segment_point(const vec2 *points,
                                        int npoints, float param) {
    return ff_serializer_mix(
        ff_serializer_mix(points[0], points[1], param),
        ff_serializer_mix(points[npoints - 2], points[npoints - 1],
                          param),
        param);
}
static float ff_serializer_shoelace(const vec2 a, const vec2 b) {
    return (b.x - a.x) * (a.y + b.y);
}
static int ff_serializer_serialize_glyph(FT_Face face, int code,
                                         char *meta_buffer,
                                         float *point_buffer) {
    if (FT_Load_Char(face, code, FT_LOAD_NO_SCALE)) return -1;

    FT_Outline_Funcs fns;
    fns.shift = 0;
    fns.delta = 0;
    fns.move_to = ff_serializer_add_contour;
    fns.line_to = ff_serializer_add_linear;
    fns.conic_to = ff_serializer_add_quad;
    fns.cubic_to = 0;

    struct glyph_data_ctx ctx;
    ctx.meta_buffer = meta_buffer;
    ctx.meta_index = 1;
    ctx.meta_buffer[0] = 0;
    /* Start 1 before the actual buffer. The pointer is moved in the
       move_to callback. FT_Outline_Decompose does not have a callback
       for finishing a contour. */
    ctx.segment = ((vec2 *)&point_buffer[0]) - 1;

    if (FT_Outline_Decompose(&face->glyph->outline, &fns, &ctx))
        return -1;

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
            vec2 a =
                ff_serializer_segment_point(point_ptr, npoints, 0);
            vec2 b = ff_serializer_segment_point(point_ptr, npoints,
                                                 1 / 3.0f);
            vec2 c = ff_serializer_segment_point(point_ptr, npoints,
                                                 2 / 3.0f);
            total += ff_serializer_shoelace(a, b);
            total += ff_serializer_shoelace(b, c);
            total += ff_serializer_shoelace(c, a);

            point_ptr += npoints - 1;
            meta_index += 2;

        } else if (nsegments == 2) {
            int npoints = meta_buffer[meta_index + 1];
            vec2 a =
                ff_serializer_segment_point(point_ptr, npoints, 0);
            vec2 b =
                ff_serializer_segment_point(point_ptr, npoints, 0.5);
            point_ptr += npoints - 1;
            meta_index += 2;
            npoints = meta_buffer[meta_index + 1];
            vec2 c =
                ff_serializer_segment_point(point_ptr, npoints, 0);
            vec2 d =
                ff_serializer_segment_point(point_ptr, npoints, 0.5);
            total += ff_serializer_shoelace(a, b);
            total += ff_serializer_shoelace(b, c);
            total += ff_serializer_shoelace(c, d);
            total += ff_serializer_shoelace(d, a);

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
            vec2 prev = ff_serializer_segment_point(prev_ptr,
                                                    prev_npoints, 0);

            for (int j = 0; j < nsegments; ++j) {
                meta_index++; /* Color, leave empty here. */
                int npoints = meta_buffer[meta_index++];

                vec2 cur = ff_serializer_segment_point(point_ptr,
                                                       npoints, 0);

                total += ff_serializer_shoelace(prev, cur);
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
            vec2 prev_direction = ff_serializer_segment_direction(
                prev_ptr, prev_npoints, 1);
            int index = 0;
            vec2 *cur_points = point_ptr;
            for (int j = 0; j < nsegments; ++j, ++index) {
                meta_index++; /* Color, leave empty here. */
                int npoints = meta_buffer[meta_index++];

                vec2 cur_direction = ff_serializer_segment_direction(
                    cur_points, npoints, 0.0);
                vec2 new_prev_direction =
                    ff_serializer_segment_direction(cur_points,
                                                    npoints, 1.0);

                if (ff_serializer_is_corner(
                        ff_serializer_normalize(prev_direction),
                        ff_serializer_normalize(cur_direction),
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
                meta_buffer[meta_index++] = ff_serializer_white_t;
                meta_index++; /* npoints */
            }
        } else if (len_corners == 1) {
            /* Teardrop */
            ff_serializer_color_t colors[3] = {ff_serializer_white_t,
                                               ff_serializer_white_t};
            ff_serializer_switch_color(&colors[0], &seed, NULL);
            colors[2] = colors[0];
            ff_serializer_switch_color(&colors[2], &seed, NULL);

            int corner = corners[0];
            if (nsegments >= 3) {
                int m = nsegments;
                for (int i = 0; i < m; ++i) {
                    ff_serializer_color_t c =
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
            ff_serializer_color_t color = ff_serializer_white_t;
            ff_serializer_switch_color(&color, &seed, NULL);
            ff_serializer_color_t initial_color = color;
            for (int i = 0; i < m; ++i) {
                int index = (start + i) % m;

                if (spline + 1 < corner_count &&
                    corners[spline + 1] == index) {
                    ++spline;
                    ff_serializer_color_t banned =
                        (ff_serializer_color_t)((spline ==
                                                 corner_count - 1) *
                                                initial_color);
                    ff_serializer_switch_color(&color, &seed,
                                               &banned);
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

    return 0;
}

/* We need two rounds of decomposing, the first one will just figure
   out how much space we need to serialize the glyph, and the second
   one serializes it and generates colour mapping for the segments. */
void glyph_buffer_size(FT_Face face, int code, size_t *meta_size,
                       size_t *point_size) {
    const int load_char_err =
        !FT_Load_Char(face, code, FT_LOAD_NO_SCALE);
    assert("Failed to load char" && load_char_err);

    FT_Outline_Funcs fns;
    fns.shift = 0;
    fns.delta = 0;
    fns.move_to = ff_serializer_add_contour_size;
    fns.line_to = ff_serializer_add_linear_size;
    fns.conic_to = ff_serializer_add_quad_size;
    fns.cubic_to = ff_serializer_add_cubic_size;
    struct GlyphLenCtx ctx = {1, 0};
    int decompose_err =
        !FT_Outline_Decompose(&face->glyph->outline, &fns, &ctx);
    assert("FT_Outline_Decompos failed" && decompose_err);

    *meta_size = ctx.meta_size;
    *point_size = ctx.data_size * 2 * sizeof(float);
}
// field fusion //

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
                           uint *shader, const char *version) {
    /* Default to versio */
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
        char log[1000];
        GLsizei len;
        glGetShaderInfoLog(*shader, 1000, &len, log);
        fprintf(stderr, "Error: compiling: %*s\n", len, log);
        return 0;
    }

    return 1;
}

static void gen_extended_ascii(const ff_font_handle_t font_handle) {
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

void ff_glyphs_vector_clear(ff_glyphs_vector_t *v) { v->size = 0; }

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

    ht_fpack_entry_t *entry = &hashtable->entries[slot];

    if (ht_fpack_map_entry_empty(entry)) {
        return NULL;
    }

    ht_fpack_entry_t *head = entry;
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

    ht_codepoint_entry_t *entry = &hashtable->entries[slot];

    if (ht_codepoint_map_entry_empty(entry)) {
        return NULL;
    }

    ht_codepoint_entry_t *head = entry;
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

void ff_initialize(const char *version) {
    const FT_Error error = FT_Init_FreeType(&g_ft_library);
    assert("Failed to initialize freetype2" && !error);

    glGetIntegerv(GL_MAX_TEXTURE_SIZE, &g_max_texture_size);

    unsigned vertex_shader, geometry_shader, fragment_shader;
    bool err = compile_shader(g_msdf_vertex, GL_VERTEX_SHADER,
                              &vertex_shader, version);
    assert("Failed to compile msdf vertex shader" && err);
    err = compile_shader(g_msdf_fragment, GL_FRAGMENT_SHADER,
                         &fragment_shader, version);
    assert("Failed to compile msdf fragment shader" && err);
    err = (g_gen_shader = glCreateProgram());
    assert("Failed to generate shader program");

    glAttachShader(g_gen_shader, vertex_shader);
    glAttachShader(g_gen_shader, fragment_shader);
    glLinkProgram(g_gen_shader);
    glDeleteShader(vertex_shader);
    glDeleteShader(fragment_shader);

    int link_status = 0;
    glGetProgramiv(g_gen_shader, GL_LINK_STATUS, &link_status);
    assert("Failed to link g_gen_shader" && link_status);

    g_uniforms.atlas_projection =
        glGetUniformLocation(g_gen_shader, "projection");
    g_uniforms.texture_offset =
        glGetUniformLocation(g_gen_shader, "offset");
    g_uniforms.glyph_height =
        glGetUniformLocation(g_gen_shader, "glyph_height");
    g_uniforms.offset = glGetUniformLocation(g_gen_shader, "offset");
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

    compile_shader(g_font_vertex, GL_VERTEX_SHADER, &vertex_shader,
                   version);
    err = compile_shader(g_font_geometry, GL_GEOMETRY_SHADER,
                         &geometry_shader, version);
    assert("Failed to compile geometry shader" && err);
    err = compile_shader(g_font_fragment, GL_FRAGMENT_SHADER,
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

    assert(g_uniforms.window_projection != -1);
    assert(g_uniforms.font_atlas_projection != -1);
    assert(g_uniforms.index != -1);
    assert(g_uniforms.atlas != -1);
    assert(g_uniforms.padding != -1);
    assert(g_uniforms.offset != -1);
    assert(g_uniforms.dpi != -1);
    assert(g_uniforms.units_per_em != -1);
    assert(g_uniforms.atlas_projection != -1);
    assert(g_uniforms.texture_offset != -1);
    assert(g_uniforms.translate != -1);
    assert(g_uniforms.scale != -1);
    assert(g_uniforms.range != -1);
    assert(g_uniforms.glyph_height != -1);
    assert(g_uniforms.meta_offset != -1);
    assert(g_uniforms.point_offset != -1);
    assert(g_uniforms.metadata != -1);
    assert(g_uniforms.point_data != -1);

    g_dpi[0] = 72.0;
    g_dpi[1] = 72.0;

    glGenVertexArrays(1, &g_bbox_vao);
    glGenBuffers(1, &g_bbox_vbo);
    glBindBuffer(GL_ARRAY_BUFFER, g_bbox_vbo);
    glBufferData(GL_ARRAY_BUFFER, 12 * sizeof(float), 0,
                 GL_STREAM_READ);
    glBindBuffer(GL_ARRAY_BUFFER, 0);

    g_fonts = ht_fpack_map_create();
}

ff_font_config_t ff_default_font_config(void) {
    return (ff_font_config_t){
        .scale = 2.0f,
        .range = 4.0f,
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
    fpack->font.character_index.codepoint_map =
        ht_codepoint_map_create();

    fpack->atlas.texture_width = config.texture_width;
    fpack->atlas.padding = config.texture_padding;
    glGenBuffers(1, &fpack->atlas.index_buffer);
    glGenTextures(1, &fpack->atlas.index_texture);
    glGenTextures(1, &fpack->atlas.atlas_texture);
    glGenFramebuffers(1, &fpack->atlas.atlas_framebuffer);

    int err = !FT_New_Face(g_ft_library, path, 0, &fpack->font.face);
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
    ht_codepoint_map_free(&fpack->font.character_index.codepoint_map);

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

int ff_gen_glyphs(ff_font_handle_t handle, char32_t *codepoints,
                  ulong codepoints_count) {
    GLint original_viewport[4];
    glGetIntegerv(GL_VIEWPORT, original_viewport);

    int retval = -2;
    int nrender = codepoints_count;

    if (nrender <= 0) return -1;

    ff_font_texture_pack_t *fpack =
        ht_fpack_map_get(&g_fonts, handle);
    assert(fpack != NULL);

    size_t *meta_sizes = NULL, *point_sizes = NULL;
    ff_index_entry_t *atlas_index = NULL;
    void *point_data = NULL, *metadata = NULL;

    /* We will start with a square texture. */
    int new_texture_height =
        fpack->atlas.texture_height ? fpack->atlas.texture_height : 1;
    int new_index_size =
        fpack->atlas.nallocated ? fpack->atlas.nallocated : 1;

    /* Calculate the amount of memory needed on the GPU.*/
    meta_sizes = (size_t *)calloc(nrender, sizeof(size_t));
    assert(meta_sizes);
    point_sizes = (size_t *)calloc(nrender, sizeof(size_t));
    assert(point_sizes);

    /* Amount of new memory needed for the index. */
    size_t index_size = nrender * sizeof(ff_index_entry_t);
    atlas_index = (ff_index_entry_t *)calloc(1, index_size);
    assert(atlas_index);

    size_t meta_size_sum = 0, point_size_sum = 0;
    for (size_t i = 0; (int)i < (int)nrender; ++i) {
        int index = codepoints[i];
        glyph_buffer_size(fpack->font.face, index, &meta_sizes[i],
                          &point_sizes[i]);

        meta_size_sum += meta_sizes[i];
        point_size_sum += point_sizes[i];
    }

    /* Allocate the calculated amount. */
    point_data = calloc(point_size_sum, 1);
    assert(point_data);
    metadata = calloc(meta_size_sum, 1);
    assert(metadata);

    /* Serialize the glyphs into RAM. */
    char *meta_ptr = (char *)metadata;
    char *point_ptr = (char *)point_data;
    for (size_t i = 0; (int)i < (int)nrender; ++i) {
        float buffer_width, buffer_height;

        int index = codepoints[i];
        ff_serializer_serialize_glyph(fpack->font.face, index,
                                      meta_ptr, (GLfloat *)point_ptr);
        ff_map_item_t *m =
            ff_map_insert(&fpack->font.character_index, index);
        m->codepoint_index = fpack->atlas.nglyphs + i;
        m->advance[0] =
            (float)fpack->font.face->glyph->metrics.horiAdvance;
        m->advance[1] =
            (float)fpack->font.face->glyph->metrics.vertAdvance;

        buffer_width = fpack->font.face->glyph->metrics.width /
                           g_serializer_scale +
                       fpack->font.range;
        buffer_height = fpack->font.face->glyph->metrics.height /
                            g_serializer_scale +
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

        atlas_index[i].offset_x = (GLfloat)fpack->atlas.offset_x;
        atlas_index[i].offset_y = (GLfloat)fpack->atlas.offset_y;
        atlas_index[i].size_x = buffer_width;
        atlas_index[i].size_y = buffer_height;
        atlas_index[i].bearing_x =
            (GLfloat)fpack->font.face->glyph->metrics.horiBearingX;
        atlas_index[i].bearing_y =
            (GLfloat)fpack->font.face->glyph->metrics.horiBearingY;
        atlas_index[i].glyph_width =
            (GLfloat)fpack->font.face->glyph->metrics.width;
        atlas_index[i].glyph_height =
            (GLfloat)fpack->font.face->glyph->metrics.height;

        fpack->atlas.offset_x +=
            (size_t)buffer_width + fpack->atlas.padding;

        while ((fpack->atlas.offset_y + buffer_height) >
               new_texture_height) {
            new_texture_height *= 2;
        }
        assert(new_texture_height <= g_max_texture_size);
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
        assert(glGetError() != GL_OUT_OF_MEMORY);
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

        assert(glGetError() != GL_OUT_OF_MEMORY);
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
            0, (GLfloat)fpack->atlas.texture_width, 0,
            (GLfloat)fpack->atlas.texture_height, -1.0, 1.0},
        framebuffer_projection);
    ff_get_ortho_projection(
        (ortho_projection_params_t){
            -(GLfloat)fpack->atlas.texture_width,
            (GLfloat)fpack->atlas.texture_width,
            -(GLfloat)fpack->atlas.texture_height,
            (GLfloat)fpack->atlas.texture_height, -1.0, 1.0},
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
            -g.bearing_x / g_serializer_scale +
                fpack->font.range / 2.0f,
            (g.glyph_height - g.bearing_y) / g_serializer_scale +
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
    retval = nrender;

    free(meta_sizes);
    free(point_sizes);
    free(atlas_index);
    free(point_data);
    free(metadata);

    glViewport(original_viewport[0], original_viewport[1],
               original_viewport[2], original_viewport[3]);

    return retval;
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

    void *position_offset = (void *)offsetof(ff_glyph_t, position.x);
    void *color_offset = (void *)offsetof(ff_glyph_t, color);
    void *codepoint_offset = (void *)offsetof(ff_glyph_t, codepoint);
    void *size_offset = (void *)offsetof(ff_glyph_t, size);
    void *offset_offset =
        (void *)offsetof(ff_glyph_t, characteristics.offset);
    void *skew_offset =
        (void *)offsetof(ff_glyph_t, characteristics.skew);
    void *strength_offset =
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
    bool is_srgb_enabled = glIsEnabled(GL_FRAMEBUFFER_SRGB);
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
        (GLfloat)(fpack->font.range / 2.0 * g_serializer_scale));
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

ff_characteristics_t ff_get_default_characteristics() {
    return (ff_characteristics_t){
        .offset = 0.f, .skew = 0.f, .strength = .5f};
}

int ff_get_default_print_flags() {
    return ff_print_options_enable_kerning;
}

int ff_utf8_to_utf32(char32_t *dest, const char *src, ulong count) {
    mbstate_t state = {0};
    size_t rc;
    while ((rc = mbrtoc32(dest, src, count, &state))) {
        assert(rc != (size_t)-3 && "no surrogate pairs in UTF-32");
        if (rc == (size_t)-1) return -1;  // invalid input
        if (rc == (size_t)-2) return -1;  // truncated in
        src += rc;
        dest += 1;
    }
    return 0;
}

int ff_utf32_to_utf8(char *dest, const char32_t *src, ulong count) {
    mbstate_t state = {0};
    for (size_t i = 0; i < count; i += 1) {
        size_t rc = c32rtomb(dest, src[i], &state);
        if (rc == (size_t)-1) return -1;
    }
    return 0;
}

void ff_print_utf8(ff_glyphs_vector_t *vec, ff_utf8_str_t utf8_string,
                   ff_print_params_t params, ff_position_t position) {
    char32_t converted_utf32[utf8_string.size];
    ff_utf8_to_utf32(converted_utf32, utf8_string.data,
                     utf8_string.size);
    ff_utf32_str_t utf32_string = {.data = converted_utf32,
                                   .size = utf8_string.size};
    ff_print_utf32(vec, utf32_string, params, position);
}

void ff_print_utf32(ff_glyphs_vector_t *vec, ff_utf32_str_t str,
                    ff_print_params_t params,
                    ff_position_t position) {
    ff_font_texture_pack_t *fpack =
        ht_fpack_map_get(&g_fonts, params.typography.font);
    ff_position_t pos0 = {position.x,
                          position.y + params.typography.size};

    for (size_t i = 0; i < str.size; i++) {
        // const auto &codepoint = (char32_t)buffer.at(i);
        char32_t codepoint = str.data[i];

        ff_map_item_t *idx =
            ff_map_get(&fpack->font.character_index, codepoint);
        if (idx == NULL) {
            ff_gen_glyphs(params.typography.font, &codepoint, 1);
            idx = ff_map_get(&fpack->font.character_index, codepoint);
            assert(idx != NULL);
        }

        FT_Vector kerning = {0};
        const bool should_get_kerning =
            (params.print_flags & ff_print_options_enable_kerning) &&
            FT_HAS_KERNING(fpack->font.face) && (i > 0);
        if (should_get_kerning) {
            char32_t previous_character = str.data[i - 1];
            FT_Get_Kerning(
                fpack->font.face,
                FT_Get_Char_Index(fpack->font.face,
                                  previous_character),
                FT_Get_Char_Index(fpack->font.face, codepoint),
                FT_KERNING_UNSCALED, &kerning);
        }

        ff_glyphs_vector_push(vec, (ff_glyph_t){0});
        ff_glyph_t *new_glyph = &vec->data[vec->size - 1];

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
}

ff_dimensions_t ff_measure(const ff_font_handle_t handle,
                           char32_t *str, ulong str_count, float size,
                           bool with_kerning) {
    ff_font_texture_pack_t *fpack =
        ht_fpack_map_get(&g_fonts, handle);

    ff_dimensions_t result = {0};

    for (size_t i = 0; i < str_count; i++) {
        char32_t codepoint = str[i];

        ff_map_item_t *idx =
            ff_map_get(&fpack->font.character_index, codepoint);
        if (idx == NULL) {
            ff_gen_glyphs(handle, &codepoint, 1);
            idx = ff_map_get(&fpack->font.character_index, codepoint);
            assert(idx != NULL);
        }

        FT_Vector kerning = {0};
        const bool should_get_kerning =
            with_kerning && FT_HAS_KERNING(fpack->font.face) &&
            (i > 0);
        if (should_get_kerning) {
            char32_t previous_character = str[i - 1];
            FT_Get_Kerning(
                fpack->font.face,
                FT_Get_Char_Index(fpack->font.face,
                                  previous_character),
                FT_Get_Char_Index(fpack->font.face, codepoint),
                FT_KERNING_UNSCALED, &kerning);
        }

        float height = (idx->advance[1] + kerning.y) *
                       (size * g_dpi[1] / 72.0f) /
                       fpack->font.face->units_per_EM;
        result.height = fmax(result.height, height);
        result.width += (idx->advance[0] + kerning.x) *
                        (size * g_dpi[0] / 72.0f) /
                        fpack->font.face->units_per_EM;
    }

    return result;
}

void ff_terminate() {
    for (ulong i = 0; i < g_max_handle; i += 1) ff_remove_font(i);
    ht_fpack_map_free(&g_fonts);
    FT_Done_FreeType(g_ft_library);
}
