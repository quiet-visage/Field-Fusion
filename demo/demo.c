#define GLFW_INCLUDE_NONE
#include <GLFW/glfw3.h>
#define GLAD_GL_IMPLEMENTATION
#include <glad.h>

#define FIELDFUSION_DONT_INCLUDE_GLAD
#define FIELDFUSION_IMPLEMENTATION
#include <fieldfusion.h>

static const int kwindow_width = 1366;
static const int kwindow_height = 768;
static const char *kwin_title = "msdf demo";

static GLFWwindow *window;
void init_gl_ctx() {
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

void destroy_gl_ctx() {
    glfwDestroyWindow(window);
    glfwTerminate();
}

int main() {
    init_gl_ctx();
    ff_initialize("440");

    ff_glyph_vec_t glyphs = ff_glyph_vec_create();
    int dest[16];
    ff_utf8_to_utf32(dest, "    hello world", 15);
    char dest1[16];
    ff_utf32_to_utf8(dest1, dest, 15);
    ff_glyphs_vec_prealloc(&glyphs, 16);

    ff_print_utf32(glyphs.data, &glyphs.size, dest, 15,
                   (ff_typo_t){0, 16.f, 0xffffffff}, 100, 200,
                   ff_flag_default, 0);

    float projection[4][4];
    ff_get_ortho_projection(0, kwindow_width, kwindow_height, 0,
                            -1.0f, 1.0f, projection);

    for (; !glfwWindowShouldClose(window);) {
        glClear(GL_COLOR_BUFFER_BIT);
        ff_draw(0, glyphs.data, glyphs.size, (float *)projection);
        glfwSwapBuffers(window);
        glfwPollEvents();
    }
    ff_glyph_vec_destroy(&glyphs);

    ff_terminate();
    destroy_gl_ctx();
}
