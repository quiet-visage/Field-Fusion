#include <GL/glew.h>
#include <GLFW/glfw3.h>

#include <cassert>
#include <iostream>
#include <string>

#include "fieldfusion.hh"

extern const char *kvertex_shader_src;
extern const char *kfragment_shader_src;
constexpr const int kwindow_width = 1366;
constexpr const int kwindow_height = 768;
constexpr const float kinitial_font_size = 8.0f;
constexpr const float kfont_size_increment = 4.0f;
constexpr const float kline_padding = 2.0f;
constexpr const int kline_repeat = 12;
constexpr const long kwhite = 0xffffffff;
static const std::u32string ktext = U"The quick brown fox jumps over the lazy dog";
static const std::u32string kunicode_text = U"Быстрая бурая лиса перепрыгивает через ленивую собаку";
constexpr const char *kwin_title = "msdf demo";
constexpr const char *kfont_path{"/usr/share/fonts/TTF/JetBrainsMonoNerdFontMono-Regular.ttf"};

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
    window = glfwCreateWindow(kwindow_width, kwindow_height, kwin_title, 0, 0);
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

    glShaderSource(vshader, 1, &kvertex_shader_src, 0);
    glShaderSource(fshader, 1, &kfragment_shader_src, 0);
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
    ff::Font fnt(kfont_path);
    auto fnt_res = fnt.init_face(ctx);
    if (not fnt_res) {
        std::cerr << "field fusion error occurred. code: " << (int)fnt_res.error_ << std::endl;
        return 1;
    }
    fnt.init_textures();
    auto free_fnt = defer(fnt.destroy());
    ff::Atlas atlas(1024, 2);
    atlas.init_textures();
    fnt.generate_ascii(ctx, atlas);
    ff::Glyphs glyphs;
    glyphs.reserve(255);

    float y0 = kinitial_font_size;
    int size0 = kinitial_font_size;
    for (size_t i = 0; i < kline_repeat - 1; i++) {
        auto line = fnt.print_unicode(ctx, atlas, ktext, 0, y0, kwhite, size0);
        if (not line) {
            std::cerr << "field fusion error occurred. code: " << (int)line.error_ << std::endl;
            return 1;
        }
        glyphs.insert(glyphs.end(), line->begin(), line->end());
        size0 += kfont_size_increment;
        y0 += size0 + kline_padding;
    }

    size0 -= kfont_size_increment * 2;
    auto unicode_line = fnt.print_unicode(ctx, atlas, kunicode_text, 0, y0, kwhite, size0);
    if (not unicode_line) {
        std::cerr << "field fusion error occurred. code: " << (int)unicode_line.error_ << std::endl;
        return 1;
    }

    glyphs.insert(glyphs.end(), unicode_line->begin(), unicode_line->end());
    float projection[4][4];
    ff::ortho(0, kwindow_width, kwindow_height, 0, -1.0f, 1.0f, projection);

    auto shader_program = get_shader_program();

    for (; not glfwWindowShouldClose(window);) {
        glClear(GL_COLOR_BUFFER_BIT);
        { fnt.draw(ctx, atlas, glyphs, (float *)projection); }
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

const char *kvertex_shader_src = R"SHADER(#version 330 core
layout (location = 0) in vec4 vertex;
out vec2 text_pos;
void main() {
    gl_Position = vec4(vertex.xy, 0.0, 1.0);
    text_pos = vertex.zw;
};
)SHADER";

const char *kfragment_shader_src = R"SHADER(#version 330 core
precision mediump float;
in vec2 text_pos;
uniform sampler2D tex;
out vec4 color;
void main() {
    color = texture(tex, text_pos);
};
)SHADER";