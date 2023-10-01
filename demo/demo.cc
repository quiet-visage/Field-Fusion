#include <GL/glew.h>
#include <GLFW/glfw3.h>

#include <cassert>
#include <iostream>
#include <string>

#include "fieldfusion.hh"

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

template <typename Lambda>
struct SG {
    Lambda _callback;
    [[nodiscard("callback gets called immediatly if dicarded")]] explicit SG(Lambda callback)
        : _callback(std::move(callback)) {}
    SG(const SG &) = delete;
    SG(SG &&) = delete;
    SG &operator=(const SG &) = delete;
    SG &operator=(SG &&) = delete;
    ~SG() { _callback(); };
};

#define defer(code) SG([&]() { code; })

static GLFWwindow *window;
void init_gl_ctx() {
    assert(glfwInit() == GLFW_TRUE);
    glfwWindowHint(GLFW_RESIZABLE, GLFW_FALSE);
    window = glfwCreateWindow(WIN_WIDTH, WIN_HEIGHT, WIN_TITLE, 0, 0);
    glfwMakeContextCurrent(window);
    assert(glewInit() == GLEW_OK);
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glClearColor(0.25f, 0.25f, 0.25f, 1.0f);
}

void destroy_gl_ctx() { glfwTerminate(); }

int get_shader_program() {
    auto shader_program = glCreateProgram();

    auto vshader = glCreateShader(GL_VERTEX_SHADER);
    auto fshader = glCreateShader(GL_FRAGMENT_SHADER);
    auto freevshader = defer(glDeleteShader(vshader));
    auto freefshader = defer(glDeleteShader(fshader));

    glShaderSource(vshader, 1, &VRTX_SRC, 0);
    glShaderSource(fshader, 1, &FRGT_SRC, 0);
    glCompileShader(vshader);
    glCompileShader(fshader);
    glAttachShader(shader_program, vshader);
    glAttachShader(shader_program, fshader);
    glLinkProgram(shader_program);

    return shader_program;
}

int main() {
    init_gl_ctx();

    auto ctx = ff::Context::create("330");
    if (not ctx) {
        std::cerr << "field fusion error occurred. code: " << (int)ctx.error_ << std::endl;
        return 1;
    }
    auto free_ctx = defer(ctx->destroy());

    auto fnt = ff::Font::create(ctx, FONT);
    if (not fnt) {
        std::cerr << "field fusion error occurred. code: " << (int)fnt.error_ << std::endl;
        return 1;
    }
    auto free_fnt = defer(fnt->destroy());

    auto atlas = ff::Atlas::create(ctx, WIN_WIDTH, 2.0f);
    fnt->generate_ascii(ctx, atlas);

    ff::Glyphs glyphs;
    glyphs.reserve(255);
    float y0 = INITIAL_FONT_SIZE;
    int size0 = INITIAL_FONT_SIZE;
    for (size_t i = 0; i < LINE_REPEAT - 1; i++) {
        auto line = fnt->print_unicode(ctx, atlas, TEXT, 0, y0, WHITE, size0);
        if (not line) {
            std::cerr << "field fusion error occurred. code: " << (int)line.error_ << std::endl;
            return 1;
        }
        glyphs.insert(glyphs.end(), line->begin(), line->end());
        size0 += FONT_SIZE_INCREMENT;
        y0 += size0 + LINE_PADDING;
    }
    size0 -= FONT_SIZE_INCREMENT * 2;
    auto unicode_line = fnt->print_unicode(ctx, atlas, UNICODE_TEXT, 0, y0, WHITE, size0);
    glyphs.insert(glyphs.end(), unicode_line->begin(), unicode_line->end());

    float projection[4][4];
    ff::ortho(0, WIN_WIDTH, WIN_HEIGHT, 0, -1.0f, 1.0f, projection);

    auto shader_program = get_shader_program();
    return 1;

    for (; not glfwWindowShouldClose(window);) {
        glClear(GL_COLOR_BUFFER_BIT);

        { fnt->render(ctx, atlas, glyphs, (float *)projection); }

        glUseProgram(shader_program);
        glBindTexture(GL_TEXTURE_2D, atlas.atlas_texture_);
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