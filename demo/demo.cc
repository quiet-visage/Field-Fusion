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
constexpr const char *kregular_font_path{"jetbrainsfont/fonts/ttf/JetBrainsMono-Regular.ttf"};
constexpr const char *kitalic_font_path{"jetbrainsfont/fonts/ttf/JetBrainsMono-MediumItalic.ttf"};

static GLFWwindow *window;
void InitGlCtx() {
    assert(glfwInit() == GLFW_TRUE);
    glfwWindowHint(GLFW_RESIZABLE, GLFW_FALSE);
    window = glfwCreateWindow(kwindow_width, kwindow_height, kwin_title, 0, 0);
    glfwMakeContextCurrent(window);
    assert(glewInit() == GLEW_OK);
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glClearColor(0.25f, 0.25f, 0.25f, 1.0f);
}

void DestroyGlCtx() { glfwTerminate(); }

int GetShaderProgram() {
    auto shader_program = glCreateProgram();

    auto vshader = glCreateShader(GL_VERTEX_SHADER);
    auto fshader = glCreateShader(GL_FRAGMENT_SHADER);

    glShaderSource(vshader, 1, &kvertex_shader_src, 0);
    glShaderSource(fshader, 1, &kfragment_shader_src, 0);
    glCompileShader(vshader);
    glCompileShader(fshader);
    glAttachShader(shader_program, vshader);
    glAttachShader(shader_program, fshader);
    glLinkProgram(shader_program);

    glDeleteShader(vshader);
    glDeleteShader(fshader);
    return shader_program;
}

int main() {
    InitGlCtx();

    auto fusion = ff::FieldFusion();
    auto stat = fusion.Init("330");
    if (not stat) {
        std::cerr << "failed to initialize field fusion" << std::endl;
        return 1;
    }
    auto regular_atlas = fusion.NewAtlas(kwindow_width);
    auto regular_font_handle = fusion.NewFont(regular_atlas, kregular_font_path).value();
    auto italic_atlas = fusion.NewAtlas(kwindow_width);
    auto italic_font_handle = fusion.NewFont(italic_atlas, kitalic_font_path).value();
    auto &regular_font = fusion.fonts_.at(regular_font_handle);
    auto &italic_font = fusion.fonts_.at(italic_font_handle);

    ff::Glyphs glyphs;
    glyphs.reserve(0xff);
    float y0 = kinitial_font_size;
    int size0 = kinitial_font_size;
    for (size_t i = 0; i < kline_repeat - 1; i++) {
        auto line = fusion.PrintUnicode(regular_atlas, regular_font, ktext, 200, y0, kwhite, size0).value();
        ff::GlyphsCat(glyphs, line);

        glyphs.insert(glyphs.end(), line.begin(), line.end());
        size0 += kfont_size_increment;
        y0 += size0 + kline_padding;
    }

    y0 += size0 + kline_padding;
    size0 -= kfont_size_increment * 2;
    auto unicode_line =
        fusion.PrintUnicode(regular_atlas, regular_font, kunicode_text, 200, y0, 0xffda09ff, size0).value();
    ff::GlyphsCat(glyphs, unicode_line);

    auto vertical_line =
        fusion
            .PrintUnicode(italic_atlas, italic_font, U"Field Fusion", 100, 0 + size0, 0xff0000ff, 20.0f, 1, 1)
            .value();
    ff::GlyphsCat(glyphs, vertical_line);

    float projection[4][4];
    ff::Ortho(0, kwindow_width, kwindow_height, 0, -1.0f, 1.0f, projection);

    auto shader_program = GetShaderProgram();

    for (; not glfwWindowShouldClose(window);) {
        glClear(GL_COLOR_BUFFER_BIT);
        { (void)fusion.Draw(regular_atlas, regular_font, glyphs, (float *)projection); }
        glUseProgram(shader_program);
        glBindTexture(GL_TEXTURE_2D, regular_atlas.atlas_texture);
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

    fusion.Destroy();
    DestroyGlCtx();
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