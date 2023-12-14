#define GLFW_INCLUDE_NONE
#include <GLFW/glfw3.h>
#define GLAD_GL_IMPLEMENTATION
#include <glad.h>

#define FIELDFUSION_DONT_INCLUDE_GLAD
#define FIELDFUSION_IMPLEMENTATION
#include <fieldfusion.h>

static const int kwindow_width = 1366;
static const int kwindow_height = 768;
static const float kinitial_font_size = 8.0f;
static const float kfont_size_increment = 2.0f;
static const float kline_padding = 2.0f;
static const int kline_repeat = 12;
static const long kwhite = 0xffffffff;
// static const std::u32string ktext =
//     U"The quick brown fox jumps over the lazy dog";
// static const std::u32string kunicode_text =
//     U"Быстрая бурая лиса перепрыгивает через ленивую собаку";
static const char *kwin_title = "msdf demo";
static const char *kregular_font_path{
    "jetbrainsfont/fonts/ttf/JetBrainsMono-Regular.ttf"};
static const char *kitalic_font_path{
    "jetbrainsfont/fonts/ttf/JetBrainsMono-MediumItalic.ttf"};

static GLFWwindow *window;
void InitGlCtx() {
    assert(glfwInit() == GLFW_TRUE);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 4);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 6);
    glfwWindowHint(GLFW_RESIZABLE, GLFW_FALSE);
    window = glfwCreateWindow(kwindow_width, kwindow_height,
                              kwin_title, 0, 0);
    glfwMakeContextCurrent(window);
    assert(gladLoadGL(glfwGetProcAddress));
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glClearColor(0.25f, 0.25f, 0.25f, 1.0f);
}

void DestroyGlCtx() { glfwTerminate(); }

// std::u32string to_unicode(const std::string &s) {
//     std::wstring_convert<std::codecvt_utf8<char32_t>, char32_t>
//     conv; return conv.from_bytes(s);
// }

// ff_glyphs_t get_variable_size_glyphs(
//     const ff_font_handle_t font_handle) {
//     ff_glyphs_t glyphs;
//     float y0 = kinitial_font_size;
//     int size0 = kinitial_font_size;
//     for (size_t i = 0; i < kline_repeat - 1; i++) {
//         auto tmp_glyphs = ff_print_unicode(
//             {font_handle, (float)size0, kwhite}, ktext, {200, y0},
//             0);
//         ff_glyphs_cat(glyphs, tmp_glyphs);
//         size0 += kfont_size_increment;
//         y0 += size0 + kline_padding;
//     }
//     return glyphs;
// }

int main() {
    InitGlCtx();
    // std::u32string details =
    //     U"Vendor : " +
    //     to_unicode(reinterpret_cast<const char
    //     *>(glGetString(GL_VENDOR))) + U"\nRenderer : " +
    //     to_unicode(reinterpret_cast<const char
    //     *>(glGetString(GL_RENDERER))) + U"\nVersion : " +
    //     to_unicode(reinterpret_cast<const char
    //     *>(glGetString(GL_VERSION))) + U"\nShader Language Version
    //     : " + to_unicode(
    //         reinterpret_cast<const char
    //         *>(glGetString(GL_SHADING_LANGUAGE_VERSION)));

    ff_initialize("330");
    auto regular_font =
        ff_new_font(kregular_font_path, ff_default_font_config());
    // auto italic_font = ff_new_font(kitalic_font_path);

    // auto glyphs = ff_print_unicode(
    //     {regular_font, 14, kwhite},
    //     U"Быстрая бурая лиса перепрыгивает через ленивую собаку",
    //     {0, 0});
    // auto detail_glyphs =
    //     ffPrintUnicode({regular_font, 14, kwhite}, details, {0,
    //     kwindow_height * 0.5f});
    // ffGlyphsCat(glyphs, detail_glyphs);
    // auto unicode_text_glyphs =
    //     ffPrintUnicode({regular_font, 14.0f, 0xffda09ff},
    //     kunicode_text,
    //                      {kwindow_width * 0.5f, kwindow_height *
    //                      0.5f});
    // ffGlyphsCat(glyphs, unicode_text_glyphs);
    // auto vertical_line = ffPrintUnicode(
    //     {italic_font, 20.0f, 0xff0000ff}, U"Field Fusion",
    //     {100, 14.0f}, ffPrintOptions::kPrintVertically |
    //     ffPrintOptions::kEnableKerning);

    // float projection[4][4];
    // ff_ortho(0, kwindow_width, kwindow_height, 0, -1.0f, 1.0f,
    //          projection);

    // for (; not glfwWindowShouldClose(window);) {
    //     glClear(GL_COLOR_BUFFER_BIT);
    //     { (void)ff_draw(regular_font, glyphs, (float *)projection);
    //     }
    //     // { (void)ff_draw(italic_font, vertical_line, (float
    //     // *)projection); }
    //     glfwSwapBuffers(window);
    //     glfwPollEvents();
    // }

    ff_terminate();
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
