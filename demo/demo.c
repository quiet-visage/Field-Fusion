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

int main() {
    InitGlCtx();

    ff_initialize("440");

    struct ff_glyphs_vector glyphs = ff_glyphs_vector_create();
    char32_t dest[16];
    ff_utf8_to_utf32(dest, "    hello world", 15);
    char dest1[16];
    ff_utf32_to_utf8(dest1, dest, 15);

    ff_print_utf32(
        &glyphs, (struct ff_utf32_str){.data = dest, .size = 15},
        (struct ff_print_params){
            .typography =
                (struct ff_typography){
                    .font = 0, .size = 12.f, .color = 0xffffffff},
            .print_flags = ff_get_default_print_flags(),
            .characteristics = ff_get_default_characteristics(),
            .draw_spaces = false},

        (struct ff_position){.x = 100, .y = 200});

    float projection[4][4];
    ff_get_ortho_projection(
        (struct ff_ortho_params){.scr_left = 0,
                                 .scr_right = kwindow_width,
                                 .scr_bottom = kwindow_height,
                                 .scr_top = 0,
                                 .near = -1.0f,
                                 .far = 1.0f},
        projection);

    for (; !glfwWindowShouldClose(window);) {
        glClear(GL_COLOR_BUFFER_BIT);
        ff_draw(0, glyphs.data, glyphs.size, (float *)projection);
        glfwSwapBuffers(window);
        glfwPollEvents();
    }
    ff_glyphs_vector_destroy(&glyphs);

    ff_terminate();
    DestroyGlCtx();
}
