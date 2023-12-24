#include <bits/types/mbstate_t.h>
#include <uchar.h>
#define GLFW_INCLUDE_NONE
#include <GLFW/glfw3.h>
#define GLAD_GL_IMPLEMENTATION
#include <glad.h>

#define FIELDFUSION_DONT_INCLUDE_GLAD
#define FIELDFUSION_IMPLEMENTATION
#include <fieldfusion.h>
#include <stdlib.h>

static const int kwindow_width = 1366;
static const int kwindow_height = 768;
// static const float kinitial_font_size = 8.0f;
// static const float kfont_size_increment = 2.0f;
// static const float kline_padding = 2.0f;
// static const int kline_repeat = 12;
// static const long kwhite = 0xffffffff;
// static const std::u32string ktext =
//     U"The quick brown fox jumps over the lazy dog";
// static const std::u32string kunicode_text =
//     U"Быстрая бурая лиса перепрыгивает через ленивую собаку";
static const char *kwin_title = "msdf demo";
static const char *kregular_font_path =
    "/usr/share/fonts/MapleMono-Regular.ttf";
// static const char *kitalic_font_path{
//     "jetbrainsfont/fonts/ttf/JetBrainsMono-MediumItalic.ttf"};

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

void DestroyGlCtx() {
    glfwDestroyWindow(window);
    glfwTerminate();
}

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

    ff_initialize("440");
    int regular_font =
        ff_new_font(kregular_font_path, ff_default_font_config());

    ff_glyphs_vector_t glyphs = ff_glyphs_vector_create();
    char32_t dest[16];
    ff_utf8_to_utf32(dest, "    hello world", 15);
    char dest1[16];
    ff_utf32_to_utf8(dest1, dest, 15);

    ff_print_utf32(
        &glyphs, (ff_utf32_str_t){.data = dest, .size = 15},
        (ff_print_params_t){
            .typography = (ff_typography_t){.font = regular_font,
                                            .size = 12.f,
                                            .color = 0xffffffff},
            .print_flags = ff_get_default_print_flags(),
            .characteristics = ff_get_default_characteristics(),
            .draw_spaces = false},

        (ff_position_t){.x = 100, .y = 200});

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

    float projection[4][4];
    ff_get_ortho_projection(
        (ortho_projection_params_t){.scr_left = 0,
                                    .scr_right = kwindow_width,
                                    .scr_bottom = kwindow_height,
                                    .scr_top = 0,
                                    .near = -1.0f,
                                    .far = 1.0f},
        projection);

    for (; !glfwWindowShouldClose(window);) {
        glClear(GL_COLOR_BUFFER_BIT);
        ff_draw(regular_font, glyphs.data, glyphs.size,
                (float *)projection);
        //     }
        //     // { (void)ff_draw(italic_font, vertical_line, (float
        //     // *)projection); }
        glfwSwapBuffers(window);
        glfwPollEvents();
    }
    ff_glyphs_vector_destroy(&glyphs);

    ff_terminate();
    DestroyGlCtx();
}
