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
#define RUSSIAN_TEXT                                                                                               \
    L"Russian: \nБоится ли кто-нибудь перемен? Что может "                         \
    L"произойти\nбез изменений? Что же тогда более приятно или "     \
    L"более \nсоответствует природе вселенной? И можно ли "              \
    L"\nпринимать ванну, если не изменится древесина? И \nможно ли " \
    L"насытиться, если не изменится пища? И \nможно ли сделать "       \
    L"что-то еще полезное без изменений?"
#define CHINESE_TEXT                                                          \
    L"Chinese: \n有人害怕改变吗？ 没有变化可以发生什么？ " \
    L"\n那么什么更令人愉快或更适合普遍的性质呢？ "        \
    L"你能洗个澡吗,除非木头换了换衣\n服? "                     \
    L"除非食物变了，你还能得到营养吗？ "                      \
    L"还有其他有用的东西可以在没有变化的\n情况下完成吗？"
#define JAPANESE_TEXT                                                                            \
    L"Japanese: \n変化を恐れている人はいますか？ "                                \
    L"何が変更せ\nずに場所を取ることができますか？ "                        \
    L"それでは、普遍的な性質にもっと喜ばれ\nる、またはより適したも" \
    L"のは何ですか？ "                                                                    \
    L"木が変化しない限り、あなたは風呂に\n入ることができますか？ "   \
    L"食べ物が変化しない限り、あなたは栄養を与えるこ\nとができます" \
    L"か？ "                                                                                   \
    L"そして、有用な他の何かを変更せずに達成することができますか？"
#define PARAGRAPH_GAP 10.

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
        style.typo.size = 22;
        y += PARAGRAPH_GAP;

        y += ff_draw_str32(
                 RUSSIAN_TEXT,
                 sizeof(RUSSIAN_TEXT) / sizeof(*RUSSIAN_TEXT) - 1, x,
                 y, (float *)projection, style)
                 .height +
             PARAGRAPH_GAP;

        y += ff_draw_str32(
                 CHINESE_TEXT,
                 sizeof(CHINESE_TEXT) / sizeof(*CHINESE_TEXT) - 1, x,
                 y, (float *)projection, style)
                 .height +
             PARAGRAPH_GAP;

        ff_draw_str32(
            JAPANESE_TEXT,
            sizeof(JAPANESE_TEXT) / sizeof(*JAPANESE_TEXT) - 1, x, y,
            (float *)projection, style);

        style.typo.size = orig_typo_size;
        glfwSwapBuffers(window);
        glfwPollEvents();
    }

    ff_terminate();
    destroy_gl_ctx();
}
