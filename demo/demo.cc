#include "msdf.hh"
#include <GL/glew.h>
#include <GLFW/glfw3.h>
#include <cassert>
#include <iostream>
#include <string>

extern const char *VRTX_SRC;
extern const char *FRGT_SRC;
constexpr const int WIN_WIDTH = 1366;
constexpr const int WIN_HEIGHT = 768;
constexpr const float INITIAL_FONT_SIZE = 8.0f;
constexpr const float FONT_SIZE_INCREMENT = 4.0f;
constexpr const float LINE_PADDING = 2.0f;
constexpr const int LINE_REPEAT = 12;
constexpr const long WHITE = 0xffffffff;
static const std::u32string TEXT = U"The quick brown fox jumps over the lazy dog";
static const std::u32string UNICODE_TEXT = U"Быстрая бурая лиса перепрыгивает через ленивую собаку";
constexpr const char *WIN_TITLE = "msdf demo";
constexpr const char *FONT{"/usr/share/fonts/TTF/JetBrainsMonoNerdFontMono-Regular.ttf"};

template <typename Lambda> struct Guard {
    Lambda _callback;
    [[nodiscard("callback gets called immediatly if dicarded")]] explicit Guard(Lambda callback)
        : _callback(std::move(callback)) {}
    Guard(const Guard &) = delete;
    Guard(Guard &&) = delete;
    Guard &operator=(const Guard &) = delete;
    Guard &operator=(Guard &&) = delete;
    ~Guard() { _callback(); };
};

static GLFWwindow *window;
void init_gl_ctx() {
    assert(glfwInit() == GLFW_TRUE);
    glfwWindowHint(GLFW_RESIZABLE, GLFW_FALSE);
    window = glfwCreateWindow(WIN_WIDTH, WIN_HEIGHT, WIN_TITLE, 0, 0);
    glfwMakeContextCurrent(window);
    assert(glewInit() == GLEW_OK);
}

void destroy_gl_ctx() { glfwTerminate(); }

int main() {
    init_gl_ctx();
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glClearColor(0.25f, 0.25f, 0.25f, 1.0f);

    auto maybe_ctx = msdf::Context::create("330");
    if (msdf::is_error(maybe_ctx)) {
        std::cerr << "msdf error: " << std::get<std::exception>(maybe_ctx).what() << std::endl;
        return 1;
    }
    auto &ctx = msdf::unwrap(maybe_ctx);
    auto autofree_ctx = Guard([&ctx]() { ctx.destroy(); });
    auto maybe_fnt = msdf::Font::create(ctx, FONT);
    if (msdf::is_error(maybe_fnt)) {
        std::cerr << "msdf font: " << std::get<std::exception>(maybe_fnt).what() << std::endl;
        return 1;
    }
    auto &fnt = msdf::unwrap(maybe_fnt);
    auto autofree_fnt = Guard([&fnt]() { fnt.destroy(); });

    auto atlas = msdf::Atlas::create(ctx, WIN_WIDTH, 2.0f);

    fnt.generate_ascii(ctx, atlas);

    std::vector<msdf::Glyph> glyphs;
    glyphs.reserve(255);
    float y0 = INITIAL_FONT_SIZE;
    int size0 = INITIAL_FONT_SIZE;
    for (size_t i = 0; i < LINE_REPEAT - 1; i++) {
        auto line = fnt.print_unicode(ctx, atlas, TEXT, 0, y0, WHITE, size0);
        glyphs.insert(glyphs.end(), line.begin(), line.end());
        size0 += FONT_SIZE_INCREMENT;
        y0 += size0 + LINE_PADDING;
    }
    size0 -= FONT_SIZE_INCREMENT * 2;
    auto unicode_line = fnt.print_unicode(ctx, atlas, UNICODE_TEXT, 0, y0, WHITE, size0);
    glyphs.insert(glyphs.end(), unicode_line.begin(), unicode_line.end());

    float projection[4][4];
    msdf::ortho(0, WIN_WIDTH, WIN_HEIGHT, 0, -1.0f, 1.0f, projection);

    auto shader_program = glCreateProgram();
    {
        auto vshader = glCreateShader(GL_VERTEX_SHADER);
        auto fshader = glCreateShader(GL_FRAGMENT_SHADER);
        glShaderSource(vshader, 1, &VRTX_SRC, 0);
        glShaderSource(fshader, 1, &FRGT_SRC, 0);
        glCompileShader(vshader);
        glCompileShader(fshader);
        glAttachShader(shader_program, vshader);
        glAttachShader(shader_program, fshader);
        glLinkProgram(shader_program);
    }

    for (; not glfwWindowShouldClose(window);) {
        glClear(GL_COLOR_BUFFER_BIT);

        { fnt.render(ctx, atlas, glyphs, (float *)projection); }

        glUseProgram(shader_program);
        glBindTexture(GL_TEXTURE_2D, atlas._atlas_texture);
        glBegin(GL_QUADS);
        glVertex4f(-1, 0.0, 0, 1);
        glVertex4f(1, 0.0, 1, 1);
        glVertex4f(1, -1, 1, 0);
        glVertex4f(-1, -1, 0, 0);
        glEnd();
        glUseProgram(0);

        glfwSwapBuffers(window);
        glfwPollEvents();
    }

    destroy_gl_ctx();
}

const char *VRTX_SRC = R"SHADER(#version 330 core
layout (location = 0) in vec4 vertex;
out vec2 text_pos;
void main() {
    gl_Position = vec4(vertex.xy, 0.0, 1.0);
    text_pos = vertex.zw;
};
)SHADER";

const char *FRGT_SRC = R"SHADER(#version 330 core
precision mediump float;
in vec2 text_pos;
uniform sampler2D tex;
out vec4 color;
void main() {
    color = texture(tex, text_pos);
};
)SHADER";