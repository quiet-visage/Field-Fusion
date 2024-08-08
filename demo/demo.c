#define GLFW_INCLUDE_NONE
#include <GLFW/glfw3.h>
#define GLAD_GL_IMPLEMENTATION
#include <assert.h>
#include <glad.h>

#define FIELDFUSION_DONT_INCLUDE_GLAD
#define FIELDFUSION_IMPLEMENTATION
#include <fieldfusion.h>

static const int kwindow_width = 1366;
static const int kwindow_height = 768;
static const char *kwin_title = "msdf demo";
#define LOREM "Lorem ipsum odor amet, consectetuer adipiscing elit."


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

    FF_Typo typo = {0, 16.f, 0xffffffff};
    FF_Style style = ff_style_create();
    float projection[4][4];
    ff_get_ortho_projection(0, kwindow_width, kwindow_height, 0,
                            -1.0f, 1.0f, projection);

    for (; !glfwWindowShouldClose(window);) {
        glClear(GL_COLOR_BUFFER_BIT);

        float y = 0.;
        float orig_typo_size = style.typo.size;
        for (size_t i = 0; i < 10; ++i) {
            ff_draw_str8(LOREM, sizeof(LOREM) - 1, 10, y,
                         (float *)projection, style);
            style.typo.size += 2.;
            y += style.typo.size;
        }
        style.typo.size = orig_typo_size;

        float x = 0.;
        style.flags |= FF_FLAG_PRINT_VERTICALLY;
        for (size_t i = 0; i < 10; ++i) {
            float w = ff_draw_str8(LOREM, sizeof(LOREM) - 1, x, y,
                                   (float *)projection, style)
                          .width;
            x += w + 10;
            style.typo.size += 2.;
        }
        style.flags &= ~FF_FLAG_PRINT_VERTICALLY;
        style.typo.size = orig_typo_size;

        glfwSwapBuffers(window);
        glfwPollEvents();
    }

    ff_terminate();
    destroy_gl_ctx();
}
