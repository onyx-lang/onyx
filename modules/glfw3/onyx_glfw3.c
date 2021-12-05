#include "onyx_library.h"
#include <GLFW/glfw3.h>

#define ONYX_LIBRARY_NAME onyx_glfw3

ONYX_DEF(glfwInit, (), (INT)) {
    results->data[0] = WASM_I32_VAL(glfwInit());
    return NULL;
}

ONYX_DEF(glfwTerminate, (), ()) {
    glfwTerminate();
    return NULL;
}

ONYX_DEF(glfwGetVersion, (PTR, PTR, PTR), ()) {
    glfwGetVersion((int *) ONYX_PTR(params->data[0].of.i32),
                   (int *) ONYX_PTR(params->data[1].of.i32),
                   (int *) ONYX_PTR(params->data[2].of.i32));
    return NULL;
}

ONYX_DEF(glfwCreateWindow, (INT, INT, PTR, LONG, LONG), (LONG)) {
    GLFWwindow* window = glfwCreateWindow(
                     params->data[0].of.i32, params->data[1].of.i32,
                     ONYX_PTR(params->data[2].of.i32),
                     (GLFWmonitor *) params->data[3].of.i64,
                     (GLFWwindow  *) params->data[4].of.i64);

    wasm_val_init_ptr(&results->data[0], window);
    return NULL;
}

ONYX_DEF(glfwDestroyWindow, (LONG), ()) {
    GLFWwindow *window = (GLFWwindow *) params->data[0].of.i64;
    glfwDestroyWindow(window);
    return NULL;
}

ONYX_DEF(glfwWindowShouldClose, (LONG), (BOOL)) {
    GLFWwindow *window = (GLFWwindow *) params->data[0].of.i64;
    results->data[0] = WASM_I32_VAL(glfwWindowShouldClose(window) == GLFW_TRUE);
    return NULL;
}

ONYX_DEF(glfwSetWindowShouldClose, (LONG, BOOL), ()) {
    GLFWwindow *window = (GLFWwindow *) params->data[0].of.i64;
    glfwSetWindowShouldClose(window, params->data[1].of.i32);
    return NULL;
}

ONYX_DEF(glfwDefaultWindowHints, (), ()) {
    glfwDefaultWindowHints();
    return NULL;
}

ONYX_DEF(glfwWindowHint, (INT, INT), ()) {
    glfwWindowHint(params->data[0].of.i32, params->data[1].of.i32);
    return NULL;
}

ONYX_DEF(glfwSetWindowTitle, (LONG, PTR), ()) {
    GLFWwindow *window = (GLFWwindow *) params->data[0].of.i64;
    glfwSetWindowTitle(window, ONYX_PTR(params->data[1].of.i32));
    return NULL;
}

ONYX_DEF(glfwGetWindowPos, (LONG, PTR, PTR), ()) {
    GLFWwindow *window = (GLFWwindow *) params->data[0].of.i64;
    glfwGetWindowPos(window, (int *) ONYX_PTR(params->data[1].of.i32), (int *) ONYX_PTR(params->data[2].of.i32));
    return NULL;
}

ONYX_DEF(glfwSetWindowPos, (LONG, INT, INT), ()) {
    GLFWwindow *window = (GLFWwindow *) params->data[0].of.i64;
    glfwSetWindowPos(window, params->data[1].of.i32, params->data[2].of.i32);
    return NULL;
}

// ...
ONYX_DEF(glfwPollEvents, (), ()) {
    glfwPollEvents();
    return NULL;
}

ONYX_DEF(glfwWaitEvents, (), ()) {
    glfwWaitEvents();
    return NULL;
}

ONYX_DEF(glfwWaitEventsTimeout, (DOUBLE), ()) {
    glfwWaitEventsTimeout(params->data[0].of.f64);
    return NULL;
}

ONYX_DEF(glfwPostEmptyEvent, (), ()) {
    glfwPostEmptyEvent();
    return NULL;
}

ONYX_DEF(glfwSwapBuffers, (LONG), ()) {
    GLFWwindow *window = (GLFWwindow *) params->data[0].of.i64;
    glfwSwapBuffers(window);
    return NULL;
}

ONYX_LIBRARY {
    ONYX_FUNC(glfwInit)
    ONYX_FUNC(glfwTerminate)
    ONYX_FUNC(glfwGetVersion)
    ONYX_FUNC(glfwCreateWindow)
    ONYX_FUNC(glfwDestroyWindow)
    ONYX_FUNC(glfwWindowShouldClose)
    ONYX_FUNC(glfwSetWindowShouldClose)
    ONYX_FUNC(glfwDefaultWindowHints)
    ONYX_FUNC(glfwWindowHint)
    ONYX_FUNC(glfwSetWindowTitle)
    ONYX_FUNC(glfwGetWindowPos)
    ONYX_FUNC(glfwSetWindowPos)
    ONYX_FUNC(glfwPollEvents)
    ONYX_FUNC(glfwWaitEvents)
    ONYX_FUNC(glfwWaitEventsTimeout)
    ONYX_FUNC(glfwPostEmptyEvent)
    ONYX_FUNC(glfwSwapBuffers)

    NULL
};