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

ONYX_DEF(glfwMakeContextCurrent, (LONG), ()) {
    GLFWwindow *window = (GLFWwindow *) params->data[0].of.i64;
    glfwMakeContextCurrent(window);
    return NULL;
}

ONYX_DEF(glfwGetCurrentContext, (), (LONG)) {
    GLFWwindow *window = glfwGetCurrentContext();
    wasm_val_init_ptr(&results->data[0], window);
    return NULL;
}

ONYX_DEF(glfwSwapInterval, (INT), ()) {
    glfwSwapInterval(params->data[0].of.i32);
    return NULL;
}

ONYX_DEF(glfwExtensionSupported, (PTR), (BOOL)) {
    char *ext_name = ONYX_PTR(params->data[0].of.i32);
    results->data[0] = WASM_I32_VAL(glfwExtensionSupported(ext_name) == GLFW_TRUE);
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

ONYX_DEF(glfwGetWindowSize, (LONG, PTR, PTR), ()) {
    GLFWwindow *window = (GLFWwindow *) params->data[0].of.i64;
    glfwGetWindowSize(window, (int *) ONYX_PTR(params->data[1].of.i32), (int *) ONYX_PTR(params->data[2].of.i32));
    return NULL;
}

ONYX_DEF(glfwSetWindowSize, (LONG, INT, INT), ()) {
    GLFWwindow *window = (GLFWwindow *) params->data[0].of.i64;
    glfwSetWindowSize(window, params->data[1].of.i32, params->data[2].of.i32);
    return NULL;
}

ONYX_DEF(glfwSetWindowSizeLimits, (LONG, INT, INT, INT, INT), ()) {
    GLFWwindow *window = (GLFWwindow *) params->data[0].of.i64;
    glfwSetWindowSizeLimits(window, params->data[1].of.i32, params->data[2].of.i32,
                                    params->data[3].of.i32, params->data[4].of.i32);
    return NULL;
}

ONYX_DEF(glfwSetWindowAspectRatio, (LONG, INT, INT), ()) {
    GLFWwindow *window = (GLFWwindow *) params->data[0].of.i64;
    glfwSetWindowAspectRatio(window, params->data[1].of.i32, params->data[2].of.i32);
    return NULL;
}

ONYX_DEF(glfwGetFramebufferSize, (LONG, PTR, PTR), ()) {
    GLFWwindow *window = (GLFWwindow *) params->data[0].of.i64;
    glfwGetFramebufferSize(window, (int *) ONYX_PTR(params->data[1].of.i32), (int *) ONYX_PTR(params->data[2].of.i32));
    return NULL;
}

ONYX_DEF(glfwGetWindowFrameSize, (LONG, PTR, PTR, PTR, PTR), ()) {
    GLFWwindow *window = (GLFWwindow *) params->data[0].of.i64;
    glfwGetWindowFrameSize(window, (int *) ONYX_PTR(params->data[1].of.i32), (int *) ONYX_PTR(params->data[2].of.i32),
                                   (int *) ONYX_PTR(params->data[3].of.i32), (int *) ONYX_PTR(params->data[4].of.i32));
    return NULL;
}

ONYX_DEF(glfwIconifyWindow, (LONG), ()) {
    GLFWwindow *window = (GLFWwindow *) params->data[0].of.i64;
    glfwIconifyWindow(window);
    return NULL;
}

ONYX_DEF(glfwRestoreWindow, (LONG), ()) {
    GLFWwindow *window = (GLFWwindow *) params->data[0].of.i64;
    glfwRestoreWindow(window);
    return NULL;
}

ONYX_DEF(glfwMaximizeWindow, (LONG), ()) {
    GLFWwindow *window = (GLFWwindow *) params->data[0].of.i64;
    glfwMaximizeWindow(window);
    return NULL;
}

ONYX_DEF(glfwShowWindow, (LONG), ()) {
    GLFWwindow *window = (GLFWwindow *) params->data[0].of.i64;
    glfwShowWindow(window);
    return NULL;
}

ONYX_DEF(glfwHideWindow, (LONG), ()) {
    GLFWwindow *window = (GLFWwindow *) params->data[0].of.i64;
    glfwHideWindow(window);
    return NULL;
}

ONYX_DEF(glfwFocusWindow, (LONG), ()) {
    GLFWwindow *window = (GLFWwindow *) params->data[0].of.i64;
    glfwFocusWindow(window);
    return NULL;
}

ONYX_DEF(glfwGetWindowMonitor, (LONG), (LONG)) {
    GLFWwindow *window = (GLFWwindow *) params->data[0].of.i64;
    GLFWmonitor *monitor = glfwGetWindowMonitor(window);
    wasm_val_init_ptr(&results->data[0], monitor);
    return NULL;
}

ONYX_DEF(glfwSetWindowMonitor, (LONG, LONG, INT, INT, INT, INT, INT), ()) {
    GLFWwindow *window = (GLFWwindow *) params->data[0].of.i64;
    GLFWmonitor *monitor = (GLFWmonitor *) params->data[1].of.i64;
    glfwSetWindowMonitor(window, monitor, params->data[2].of.i32, params->data[3].of.i32, params->data[4].of.i32, params->data[5].of.i32, params->data[6].of.i32);
    return NULL;
}

ONYX_DEF(glfwGetWindowAttrib, (LONG, INT), (INT)) {
    GLFWwindow *window = (GLFWwindow *) params->data[0].of.i64;
    results->data[0] = WASM_I32_VAL(glfwGetWindowAttrib(window, params->data[1].of.i32));
    return NULL;
}

ONYX_DEF(glfwSetWindowUserPointer, (LONG, INT), ()) {
    GLFWwindow *window = (GLFWwindow *) params->data[0].of.i64;
    glfwSetWindowUserPointer(window, (void *) (unsigned long) params->data[1].of.i32);
    return NULL;
}

ONYX_DEF(glfwGetWindowUserPointer, (LONG), (INT)) {
    GLFWwindow *window = (GLFWwindow *) params->data[0].of.i64;
    results->data[0] = WASM_I32_VAL((unsigned int) (unsigned long) glfwGetWindowUserPointer(window));
    return NULL;
}

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
    ONYX_FUNC(glfwMakeContextCurrent)
    ONYX_FUNC(glfwGetCurrentContext)
    ONYX_FUNC(glfwSwapInterval)
    ONYX_FUNC(glfwExtensionSupported)
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