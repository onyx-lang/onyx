#include <stdio.h>
//#include <GLFW/glfw3.h>

#if defined(_WIN32) || defined(_WIN64)
    #include "small_windows.h"
#endif

#if defined(__unix__)
    #include <unistd.h>
#endif

#define ONYX_LIBRARY_NAME test_library
#include "onyx_library.h"

ONYX_DEF(foo, (), ()) {
    printf("This worked!\n");
    /*glfwInit();
    GLFWwindow *window = glfwCreateWindow(800, 600, "WOOT!", NULL, NULL);
    while (!glfwWindowShouldClose(window)) {
        glfwPollEvents();
        glfwSwapBuffers(window);
    }
    glfwDestroyWindow(window);*/
    return NULL;
}

ONYX_DEF(add, (INT, INT), (INT)) {
    int a = params->data[0].of.i32;
    int b = params->data[1].of.i32;
    results->data[0] = WASM_I32_VAL(a + b);
    return NULL;
}

ONYX_DEF(print_string, (PTR, INT), ()) {
    char *start = ONYX_PTR(params->data[0].of.i32);
    int  length = params->data[1].of.i32;

#if defined(_WIN32) || defined(_WIN64)
    HANDLE h = GetStdHandle(STD_OUTPUT_HANDLE);
    WriteFile(h, start, length, NULL, NULL);
#endif
#if defined(__unix__)
    write(1, start, length);
#endif
    return NULL;
}

ONYX_LIBRARY {
    ONYX_FUNC(foo)
    ONYX_FUNC(add)
    ONYX_FUNC(print_string)

    NULL
};