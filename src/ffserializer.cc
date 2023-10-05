#include "ffserializer.hh"

#include <cmath>

#include "fferror.hh"
#include "freetype/freetype.h"
#include "freetype/ftoutln.h"

namespace ff {
namespace internal {
enum Color : int { BLACK = 0, RED = 1, GREEN = 2, YELLOW = 3, BLUE = 4, MAGENTA = 5, CYAN = 6, WHITE = 7 };
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
    struct GlyphLenCtx *ctx = (struct GlyphLenCtx *)user;
    ctx->data_size += 1;
    ctx->meta_size += 2; /* winding + nsegments */
    return 0;
}
int AddLinearSize(const FT_Vector *to, void *user) {
    struct GlyphLenCtx *ctx = (struct GlyphLenCtx *)user;
    ctx->data_size += 1;
    ctx->meta_size += 2; /* color + npoints */
    return 0;
}
int AddQuadSize(const FT_Vector *control, const FT_Vector *to, void *user) {
    struct GlyphLenCtx *ctx = (struct GlyphLenCtx *)user;
    ctx->data_size += 2;
    ctx->meta_size += 2; /* color + npoints */
    return 0;
}
int AddCubicSize(const FT_Vector *control1, const FT_Vector *control2, const FT_Vector *to, void *s) {
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
    if (ctx->segment[1].x == ctx->segment[0].x && ctx->segment[1].y == ctx->segment[0].y) return 0;

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
    if ((ctx->segment[1].x == ctx->segment[0].x && ctx->segment[1].y == ctx->segment[0].y) ||
        (ctx->segment[2].x == ctx->segment[1].x && ctx->segment[2].y == ctx->segment[1].y))
        return AddLinear(to, user);

    ctx->segment += 2;

    ctx->meta_buffer[ctx->meta_index++] = 0; /* Set color to 0 */
    ctx->meta_buffer[ctx->meta_index++] = 3;
    ctx->meta_buffer[ctx->nsegments_index]++;
    return 0;
}
}  // namespace outline_functions
constexpr const Color start[3] = {CYAN, MAGENTA, YELLOW};
void SwitchColor(enum Color *color, unsigned long long *seed, enum Color *_banned) {
    enum Color banned = _banned ? *_banned : BLACK;
    enum Color combined = (Color)(*color & banned);

    if (combined == RED || combined == GREEN || combined == BLUE) {
        *color = (Color)(combined ^ WHITE);
        return;
    }
    if (*color == BLACK || *color == WHITE) {
        *color = start[*seed % 3];
        *seed /= 3;
        return;
    }
    int shifted = *color << (1 + (*seed & 1));
    *color = (Color)((shifted | shifted >> 3) & WHITE);
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
    return Mix(Subt(points[1], points[0]), Subt(points[npoints - 1], points[npoints - 2]), param);
}
inline vec2 segment_point(const vec2 *points, int npoints, float param) {
    return Mix(Mix(points[0], points[1], param), Mix(points[npoints - 2], points[npoints - 1], param), param);
}
inline float shoelace(const vec2 a, const vec2 b) { return (b.x - a.x) * (a.y + b.y); }
}  // namespace internal

Result<void> SerializeGlyph(FT_Face face, int code, char *meta_buffer, float *point_buffer) noexcept {
    if (FT_Load_Char(face, code, FT_LOAD_NO_SCALE)) return Error::FtLoadCharFail;

    FT_Outline_Funcs fns;
    fns.shift = 0;
    fns.delta = 0;
    fns.move_to = internal::outline_functions::AddContour;
    fns.line_to = internal::outline_functions::AddLinear;
    fns.conic_to = internal::outline_functions::AddQuad;
    fns.cubic_to = 0;

    struct internal::outline_functions::glyph_data_ctx ctx;
    ctx.meta_buffer = meta_buffer;
    ctx.meta_index = 1;
    ctx.meta_buffer[0] = 0;
    /* Start 1 before the actual buffer. The pointer is moved in the move_to
       callback. FT_Outline_Decompose does not have a callback for finishing a
       contour. */
    ctx.segment = ((internal::vec2 *)&point_buffer[0]) - 1;

    if (FT_Outline_Decompose(&face->glyph->outline, &fns, &ctx)) return Error::FtDecomposeFail;

    /* Calculate windings. */
    int meta_index = 0;
    internal::vec2 *point_ptr = (internal::vec2 *)&point_buffer[0];

    int ncontours = meta_buffer[meta_index++];
    for (int i = 0; i < ncontours; ++i) {
        int winding_index = meta_index++;
        int nsegments = meta_buffer[meta_index++];

        float total = 0;
        if (nsegments == 1) {
            int npoints = meta_buffer[meta_index + 1];
            internal::vec2 a = internal::segment_point(point_ptr, npoints, 0);
            internal::vec2 b = internal::segment_point(point_ptr, npoints, 1 / 3.0f);
            internal::vec2 c = internal::segment_point(point_ptr, npoints, 2 / 3.0f);
            total += internal::shoelace(a, b);
            total += internal::shoelace(b, c);
            total += internal::shoelace(c, a);

            point_ptr += npoints - 1;
            meta_index += 2;

        } else if (nsegments == 2) {
            int npoints = meta_buffer[meta_index + 1];
            internal::vec2 a = internal::segment_point(point_ptr, npoints, 0);
            internal::vec2 b = internal::segment_point(point_ptr, npoints, 0.5);
            point_ptr += npoints - 1;
            meta_index += 2;
            npoints = meta_buffer[meta_index + 1];
            internal::vec2 c = internal::segment_point(point_ptr, npoints, 0);
            internal::vec2 d = internal::segment_point(point_ptr, npoints, 0.5);
            total += internal::shoelace(a, b);
            total += internal::shoelace(b, c);
            total += internal::shoelace(c, d);
            total += internal::shoelace(d, a);

            point_ptr += npoints - 1;
            meta_index += 2;
        } else {
            int prev_npoints = meta_buffer[meta_index + 2 * (nsegments - 2) + 1];
            internal::vec2 *prev_ptr = point_ptr;
            for (int j = 0; j < nsegments - 1; ++j) {
                int _npoints = meta_buffer[meta_index + 2 * j + 1];
                prev_ptr += (_npoints - 1);
            }
            internal::vec2 prev = internal::segment_point(prev_ptr, prev_npoints, 0);

            for (int j = 0; j < nsegments; ++j) {
                meta_index++; /* Color, leave empty here. */
                int npoints = meta_buffer[meta_index++];

                internal::vec2 cur = internal::segment_point(point_ptr, npoints, 0);

                total += internal::shoelace(prev, cur);
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
    point_ptr = (internal::vec2 *)&point_buffer[0];

    int corners[30];
    int len_corners = 0;

    ncontours = meta_buffer[meta_index++];
    for (int i = 0; i < ncontours; ++i) {
        meta_index++; /* Winding */
        int nsegments = meta_buffer[meta_index++];
        int _meta = meta_index;
        internal::vec2 *_point = point_ptr;

        len_corners = 0; /*clear*/

        if (nsegments) {
            int prev_npoints = meta_buffer[meta_index + 2 * (nsegments - 2) + 1];
            internal::vec2 *prev_ptr = point_ptr;
            for (int j = 0; j < nsegments - 1; ++j) prev_ptr += (meta_buffer[meta_index + 2 * j + 1] - 1);
            internal::vec2 prev_direction = internal::SegmentDirection(prev_ptr, prev_npoints, 1);
            int index = 0;
            internal::vec2 *cur_points = point_ptr;
            for (int j = 0; j < nsegments; ++j, ++index) {
                meta_index++; /* Color, leave empty here. */
                int npoints = meta_buffer[meta_index++];

                internal::vec2 cur_direction = internal::SegmentDirection(cur_points, npoints, 0.0);
                internal::vec2 new_prev_direction = internal::SegmentDirection(cur_points, npoints, 1.0);

                if (internal::IsCorner(internal::Normalize(prev_direction),
                                       internal::Normalize(cur_direction), cross_threshold))
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
                meta_buffer[meta_index++] = internal::WHITE;
                meta_index++; /* npoints */
            }
        } else if (len_corners == 1) {
            /* Teardrop */
            enum internal::Color colors[3] = {internal::WHITE, internal::WHITE};
            internal::SwitchColor(&colors[0], &seed, NULL);
            colors[2] = colors[0];
            internal::SwitchColor(&colors[2], &seed, NULL);

            int corner = corners[0];
            if (nsegments >= 3) {
                int m = nsegments;
                for (int i = 0; i < m; ++i) {
                    enum internal::Color c = (colors + 1)[(int)(3 + 2.875 * i / (m - 1) - 1.4375 + .5) - 3];
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
            enum internal::Color color = internal::WHITE;
            internal::SwitchColor(&color, &seed, NULL);
            enum internal::Color initial_color = color;
            for (int i = 0; i < m; ++i) {
                int index = (start + i) % m;

                if (spline + 1 < corner_count && corners[spline + 1] == index) {
                    ++spline;
                    enum internal::Color banned =
                        (enum internal::Color)((spline == corner_count - 1) * initial_color);
                    internal::SwitchColor(&color, &seed, &banned);
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

    return {};
}

/* We need two rounds of decomposing, the first one will just figure out
   how much space we need to serialize the glyph, and the second one
   serializes it and generates colour mapping for the segments. */
Result<void> GlyphBufferSize(FT_Face face, int code, size_t *meta_size, size_t *point_size) noexcept {
    if (FT_Load_Char(face, code, FT_LOAD_NO_SCALE)) return Error::FtLoadCharFail;

    FT_Outline_Funcs fns;
    fns.shift = 0;
    fns.delta = 0;
    fns.move_to = internal::outline_functions::AddContourSize;
    fns.line_to = internal::outline_functions::AddLinearSize;
    fns.conic_to = internal::outline_functions::AddQuadSize;
    fns.cubic_to = internal::outline_functions::AddCubicSize;
    struct internal::outline_functions::GlyphLenCtx ctx = {1, 0};
    if (FT_Outline_Decompose(&face->glyph->outline, &fns, &ctx)) return Error::FtDecomposeFail;

    *meta_size = ctx.meta_size;
    *point_size = ctx.data_size * 2 * sizeof(float);

    return {};
}
}  // namespace ff
