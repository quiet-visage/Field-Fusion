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
constexpr const float kfont_size_increment = 2.0f;
constexpr const float kline_padding = 2.0f;
constexpr const int kline_repeat = 12;
constexpr const long kwhite = 0xffffffff;
static const std::u32string ktext = U"The quick brown fox jumps over the lazy dog";
static const std::u32string kunicode_text = U"Быстрая бурая лиса перепрыгивает через ленивую собаку";
constexpr const char *kwin_title = "msdf demo";
constexpr const char *kfont_path{"/usr/share/fonts/TTF/JetBrainsMonoNerdFontMono-Regular.ttf"};

template <typename Lambda>
struct ScopeGuard {
    Lambda _callback;
    [[nodiscard("callback gets called immediatly if dicarded")]] explicit ScopeGuard(Lambda callback)
        : _callback(std::move(callback)) {}
    ScopeGuard(const ScopeGuard &) = delete;
    ScopeGuard(ScopeGuard &&) = delete;
    ScopeGuard &operator=(const ScopeGuard &) = delete;
    ScopeGuard &operator=(ScopeGuard &&) = delete;
    ~ScopeGuard() { _callback(); };
};

#define defer(code) ScopeGuard([&]() { code; })

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
    auto freeglctx = defer(destroy_gl_ctx());

    auto fusion = ff::FieldFusion();
    auto result = fusion.init("330");
    if (not result) {
        std::cerr << "failed to initialize Field Fusion, error code : " << (int)result.error_ << std::endl;
        return 1;
    }
    auto freefusion = defer(fusion.destroy());

    auto fnt_handle = fusion.new_font(kfont_path);
    if (not fnt_handle) {
        std::cerr << "failed to initialize font " << kfont_path << ", error code : " << (int)result.error_
                  << std::endl;
        return 1;
    }
    auto &fnt = fusion.fonts_.at(fnt_handle);
    ff::Atlas atlas = fusion.new_atlas(kwindow_width);
    {
        auto genasci = fusion.generate_ascii(atlas, fnt);
        if (not genasci) {
            std::cerr << "failed to generate glyphs" << std::endl;
            return 1;
        }
    }

    ff::Glyphs glyphs;
    glyphs.reserve(0xff);
    float y0 = kinitial_font_size;
    int size0 = kinitial_font_size;
    for (size_t i = 0; i < kline_repeat - 1; i++) {
        auto line = fusion.print_unicode(atlas, fnt, ktext, 200, y0, kwhite, size0);

        if (not line) {
            std::cerr << "field fusion error occurred. code: " << (int)line.error_ << std::endl;
            return 1;
        }

        glyphs.insert(glyphs.end(), line.value().begin(), line.value().end());
        size0 += kfont_size_increment;
        y0 += size0 + kline_padding;
    }

    y0 += size0 + kline_padding;
    size0 -= kfont_size_increment * 2;
    auto unicode_line = fusion.print_unicode(atlas, fnt, kunicode_text, 200, y0, kwhite, size0);
    if (not unicode_line) {
        std::cerr << "field fusion error occurred. code: " << (int)unicode_line.error_ << std::endl;
        return 1;
    }
    glyphs.insert(glyphs.end(), unicode_line.value().begin(), unicode_line.value().end());

    auto vertical_line =
        fusion.print_unicode(atlas, fnt, U"Field Fusion", 100, 0 + size0, 0xffffffff, 20.0f, 1, 1);
    if (not vertical_line) {
        std::cerr << "field fusion error occurred. code: " << (int)unicode_line.error_ << std::endl;
        return 1;
    }
    glyphs.insert(glyphs.end(), vertical_line.value().begin(), vertical_line.value().end());

    float projection[4][4];
    ff::ortho(0, kwindow_width, kwindow_height, 0, -1.0f, 1.0f, projection);

    auto shader_program = get_shader_program();

    for (; not glfwWindowShouldClose(window);) {
        glClear(GL_COLOR_BUFFER_BIT);
        {
            auto result = fusion.draw(atlas, fnt, glyphs, (float *)projection);
            if (not result) std::cerr << "failed to draw glyphs" << std::endl;
        }
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