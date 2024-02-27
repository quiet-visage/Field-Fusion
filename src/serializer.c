#include "serializer.h"

#include <assert.h>
#include <freetype/ftoutln.h>
#include <math.h>
#include <stdbool.h>

const float g_serializer_scale = 64;

enum ff_serializer_color_t {
    ff_serializer_black_t = 0,
    ff_serializer_red_t = 1,
    ff_serializer_green_t = 2,
    ff_serializer_yellow_t = 3,
    ff_serializer_blue_t = 4,
    ff_serializer_magenta_t = 5,
    ff_serializer_cyan_t = 6,
    ff_serializer_white_t = 7
};

struct vec2 {
    float x;
    float y;
};

struct glyph_len_ctx {
    int meta_size;
    int data_size;
};

static int ff_serializer_add_contour_size(const FT_Vector *to,
                                          void *user) {
    (void)to;
    struct glyph_len_ctx *ctx = (struct glyph_len_ctx *)user;
    ctx->data_size += 1;
    ctx->meta_size += 2; /* winding + nsegments */
    return 0;
}
static int ff_serializer_add_linear_size(const FT_Vector *to,
                                         void *user) {
    (void)to;
    struct glyph_len_ctx *ctx = (struct glyph_len_ctx *)user;
    ctx->data_size += 1;
    ctx->meta_size += 2; /* color + npoints */
    return 0;
}
static int ff_serializer_add_quad_size(const FT_Vector *control,
                                       const FT_Vector *to,
                                       void *user) {
    (void)control;
    (void)to;
    struct glyph_len_ctx *ctx = (struct glyph_len_ctx *)user;
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

    struct vec2 *segment;
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
    enum ff_serializer_color_t *color, unsigned long long *seed,
    enum ff_serializer_color_t *_banned) {
    static const enum ff_serializer_color_t start[3] = {
        ff_serializer_cyan_t, ff_serializer_magenta_t,
        ff_serializer_yellow_t};
    enum ff_serializer_color_t banned =
        _banned ? *_banned : ff_serializer_black_t;
    enum ff_serializer_color_t combined =
        (enum ff_serializer_color_t)((int)*color & (int)banned);

    if (combined == ff_serializer_red_t ||
        combined == ff_serializer_green_t ||
        combined == ff_serializer_blue_t) {
        *color = (enum ff_serializer_color_t)(
            (int)combined ^ (int)ff_serializer_white_t);
        return;
    }
    if (*color == ff_serializer_black_t ||
        *color == ff_serializer_white_t) {
        *color = start[*seed % 3];
        *seed /= 3;
        return;
    }
    int shifted = (int)(*color) << (1 + (*seed & 1));
    *color = (enum ff_serializer_color_t)(
        ((int)(shifted) | (int)(shifted) >> 3) &
        (int)(ff_serializer_white_t));
    *seed >>= 1;
}

static struct vec2 ff_serializer_mix(const struct vec2 a,
                                     const struct vec2 b,
                                     float weight) {
    return (struct vec2){a.x * (1.0f - weight) + b.x * weight,
                         a.y * (1.0f - weight) + b.y * weight};
}
static struct vec2 ff_serializer_subt(struct vec2 p1,
                                      struct vec2 p2) {
    return (struct vec2){p1.x - p2.x, p1.y - p2.y};
}
static float ff_serializer_length(const struct vec2 v) {
    return (float)sqrt(v.x * v.x + v.y * v.y);
}
static struct vec2 ff_serializer_divide(const struct vec2 v,
                                        float f) {
    return (struct vec2){v.x / f, v.y / f};
}
static float ff_serializer_cross(struct vec2 a, struct vec2 b) {
    return a.x * b.y - a.y * b.x;
}
static float ff_serializer_dot(struct vec2 a, struct vec2 b) {
    return a.x * b.x + a.y * b.y;
}
static bool ff_serializer_is_corner(const struct vec2 a,
                                    const struct vec2 b,
                                    float cross_threshold) {
    return ff_serializer_dot(a, b) <= 0 ||
           fabs(ff_serializer_cross(a, b)) > cross_threshold;
}
static struct vec2 ff_serializer_normalize(struct vec2 v) {
    return ff_serializer_divide(v, ff_serializer_length(v));
}
static struct vec2 ff_serializer_segment_direction(
    const struct vec2 *points, int npoints, float param) {
    return ff_serializer_mix(
        ff_serializer_subt(points[1], points[0]),
        ff_serializer_subt(points[npoints - 1], points[npoints - 2]),
        param);
}
static struct vec2 ff_serializer_segment_point(
    const struct vec2 *points, int npoints, float param) {
    return ff_serializer_mix(
        ff_serializer_mix(points[0], points[1], param),
        ff_serializer_mix(points[npoints - 2], points[npoints - 1],
                          param),
        param);
}
static float ff_serializer_shoelace(const struct vec2 a,
                                    const struct vec2 b) {
    return (b.x - a.x) * (a.y + b.y);
}

int ff_serializer_serialize_glyph(FT_Face face, int code,
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
    ctx.segment = ((struct vec2 *)&point_buffer[0]) - 1;

    if (FT_Outline_Decompose(&face->glyph->outline, &fns, &ctx))
        return -1;

    /* Calculate windings. */
    int meta_index = 0;
    struct vec2 *point_ptr = (struct vec2 *)&point_buffer[0];

    int ncontours = meta_buffer[meta_index++];
    for (int i = 0; i < ncontours; ++i) {
        int winding_index = meta_index++;
        int nsegments = meta_buffer[meta_index++];

        float total = 0;
        if (nsegments == 1) {
            int npoints = meta_buffer[meta_index + 1];
            struct vec2 a =
                ff_serializer_segment_point(point_ptr, npoints, 0);
            struct vec2 b = ff_serializer_segment_point(
                point_ptr, npoints, 1 / 3.0f);
            struct vec2 c = ff_serializer_segment_point(
                point_ptr, npoints, 2 / 3.0f);
            total += ff_serializer_shoelace(a, b);
            total += ff_serializer_shoelace(b, c);
            total += ff_serializer_shoelace(c, a);

            point_ptr += npoints - 1;
            meta_index += 2;

        } else if (nsegments == 2) {
            int npoints = meta_buffer[meta_index + 1];
            struct vec2 a =
                ff_serializer_segment_point(point_ptr, npoints, 0);
            struct vec2 b =
                ff_serializer_segment_point(point_ptr, npoints, 0.5);
            point_ptr += npoints - 1;
            meta_index += 2;
            npoints = meta_buffer[meta_index + 1];
            struct vec2 c =
                ff_serializer_segment_point(point_ptr, npoints, 0);
            struct vec2 d =
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
            struct vec2 *prev_ptr = point_ptr;
            for (int j = 0; j < nsegments - 1; ++j) {
                int _npoints = meta_buffer[meta_index + 2 * j + 1];
                prev_ptr += (_npoints - 1);
            }
            struct vec2 prev = ff_serializer_segment_point(
                prev_ptr, prev_npoints, 0);

            for (int j = 0; j < nsegments; ++j) {
                meta_index++; /* Color, leave empty here. */
                int npoints = meta_buffer[meta_index++];

                struct vec2 cur = ff_serializer_segment_point(
                    point_ptr, npoints, 0);

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
    point_ptr = (struct vec2 *)&point_buffer[0];

    int corners[30];
    int len_corners = 0;

    ncontours = meta_buffer[meta_index++];
    for (int i = 0; i < ncontours; ++i) {
        meta_index++; /* Winding */
        int nsegments = meta_buffer[meta_index++];
        int _meta = meta_index;
        struct vec2 *_point = point_ptr;

        len_corners = 0; /*clear*/

        if (nsegments) {
            int prev_npoints =
                meta_buffer[meta_index + 2 * (nsegments - 2) + 1];
            struct vec2 *prev_ptr = point_ptr;
            for (int j = 0; j < nsegments - 1; ++j)
                prev_ptr += (meta_buffer[meta_index + 2 * j + 1] - 1);
            struct vec2 prev_direction =
                ff_serializer_segment_direction(prev_ptr,
                                                prev_npoints, 1);
            int index = 0;
            struct vec2 *cur_points = point_ptr;
            for (int j = 0; j < nsegments; ++j, ++index) {
                meta_index++; /* Color, leave empty here. */
                int npoints = meta_buffer[meta_index++];

                struct vec2 cur_direction =
                    ff_serializer_segment_direction(cur_points,
                                                    npoints, 0.0);
                struct vec2 new_prev_direction =
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
            enum ff_serializer_color_t colors[3] = {
                ff_serializer_white_t, ff_serializer_white_t};
            ff_serializer_switch_color(&colors[0], &seed, NULL);
            colors[2] = colors[0];
            ff_serializer_switch_color(&colors[2], &seed, NULL);

            int corner = corners[0];
            if (nsegments >= 3) {
                int m = nsegments;
                for (int i = 0; i < m; ++i) {
                    enum ff_serializer_color_t c =
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
            enum ff_serializer_color_t color = ff_serializer_white_t;
            ff_serializer_switch_color(&color, &seed, NULL);
            enum ff_serializer_color_t initial_color = color;
            for (int i = 0; i < m; ++i) {
                int index = (start + i) % m;

                if (spline + 1 < corner_count &&
                    corners[spline + 1] == index) {
                    ++spline;
                    enum ff_serializer_color_t banned =
                        (enum ff_serializer_color_t)(
                            (spline == corner_count - 1) *
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
    struct glyph_len_ctx ctx = {1, 0};
    int decompose_err =
        !FT_Outline_Decompose(&face->glyph->outline, &fns, &ctx);
    assert("FT_Outline_Decompos failed" && decompose_err);

    *meta_size = ctx.meta_size;
    *point_size = ctx.data_size * 2 * sizeof(float);
}
