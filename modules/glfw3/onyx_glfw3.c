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

// glfwGetInputMode :: (window: GLFWwindow_p, mode: i32) -> i32 ---
// glfwSetInputMode :: (window: GLFWwindow_p, mode, value: i32) -> void ---
// glfwRawMouseMotionSupported :: () -> i32 ---
ONYX_DEF(glfwGetInputMode, (LONG, INT), (INT)) {
    GLFWwindow *window = (GLFWwindow *) params->data[0].of.i64;
    results->data[0] = WASM_I32_VAL(glfwGetInputMode(window, params->data[1].of.i32));
    return NULL;
}

ONYX_DEF(glfwSetInputMode, (LONG, INT, INT), ()) {
    GLFWwindow *window = (GLFWwindow *) params->data[0].of.i64;
    glfwSetInputMode(window, params->data[1].of.i32, params->data[2].of.i32);
    return NULL;
}

ONYX_DEF(glfwRawMouseMotionSupported, (), (INT)) {
    results->data[0] = WASM_I32_VAL(glfwRawMouseMotionSupported());
    return NULL;
}

// // glfwGetKeyName :: (key, scancode: i32) -> cstr ---
ONYX_DEF(glfwGetKeyScancode, (INT), (INT)) {
    results->data[0] = WASM_I32_VAL(params->data[0].of.i32);
    return NULL;
}

ONYX_DEF(glfwGetKey, (LONG, INT), (INT)) {
    GLFWwindow *window = (GLFWwindow *) params->data[0].of.i64;
    results->data[0] = WASM_I32_VAL(glfwGetKey(window, params->data[1].of.i32));
    return NULL;
}

ONYX_DEF(glfwGetMouseButton, (LONG, INT), (INT)) {
    GLFWwindow *window = (GLFWwindow *) params->data[0].of.i64;
    results->data[0] = WASM_I32_VAL(glfwGetMouseButton(window, params->data[1].of.i32));
    return NULL;
}

ONYX_DEF(glfwGetCursorPos, (LONG, PTR, PTR), ()) {
    GLFWwindow *window = (GLFWwindow *) params->data[0].of.i64;
    glfwGetCursorPos(window, (double *) ONYX_PTR(params->data[1].of.i32), (double *) ONYX_PTR(params->data[2].of.i32));
    return NULL;
}

ONYX_DEF(glfwSetCursorPos, (LONG, DOUBLE, DOUBLE), ()) {
    GLFWwindow *window = (GLFWwindow *) params->data[0].of.i64;
    glfwSetCursorPos(window, params->data[1].of.f64, params->data[2].of.f64);
    return NULL;
}

ONYX_DEF(glfwCreateCursor, (PTR, INT, INT), (LONG)) {
    GLFWimage image = {
        *(int *) ONYX_PTR(params->data[0].of.i32 + 0),
        *(int *) ONYX_PTR(params->data[0].of.i32 + 4),
        ONYX_PTR(params->data[0].of.i32 + 8),
    };

    GLFWcursor *cursor = glfwCreateCursor(&image, params->data[1].of.i32, params->data[2].of.i32);
    wasm_val_init_ptr(&results->data[0], cursor);
    return NULL;
}

ONYX_DEF(glfwCreateStandardCursor, (INT), (LONG)) {
    GLFWcursor *cursor = glfwCreateStandardCursor(params->data[0].of.i32);
    wasm_val_init_ptr(&results->data[0], cursor);
    return NULL;
}

ONYX_DEF(glfwDestroyCursor, (LONG), ()) {
    glfwDestroyCursor((GLFWcursor *) params->data[0].of.i64);
    return NULL;
}

ONYX_DEF(glfwSetCursor, (LONG, LONG), ()) {
    GLFWwindow *window = (GLFWwindow *) params->data[0].of.i64;
    GLFWcursor *cursor = (GLFWcursor *) params->data[1].of.i64;
    glfwSetCursor(window, cursor);
    return NULL;
}

// // glfwSetKeyCallback
// // glfwSetCharCallback
// // glfwSetCharModsCallback
// // glfwSetMouseButtonCallback
// // glfwSetCursorPosCallback
// // glfwSetCursorEnterCallback
// // glfwSetScrollCallback
// // glfwSetDropCallback
ONYX_DEF(glfwJoystickPresent, (INT), (INT)) {
    results->data[0] = WASM_I32_VAL(glfwJoystickPresent(params->data[0].of.i32));
    return NULL;
}
// // glfwGetJoystickAxes :: (jid: i32, count: ^i32) -> ^f32 ---
// // glfwGetJoystickButtons :: (jid: i32, count: ^i32) -> ^u8 ---
// // glfwGetJoystickHats :: (jid: i32, count: ^i32) -> ^u8 ---
// // glfwGetJoystickName :: (jid: i32) -> ^u8 ---
// // glfwGetJoystickGUID :: (jid: i32) -> ^u8 ---
ONYX_DEF(glfwSetJoystickUserPointer, (INT, PTR), ()) {
    glfwSetJoystickUserPointer(params->data[0].of.i32, (void *) (unsigned long) params->data[1].of.i32);
    return NULL;
}

ONYX_DEF(glfwGetJoystickUserPointer, (INT), (PTR)) {
    results->data[0] = WASM_I32_VAL((unsigned int) (unsigned long) glfwGetJoystickUserPointer(params->data[0].of.i32));
    return NULL;
}

ONYX_DEF(glfwJoystickIsGamepad, (INT), (INT)) {
    results->data[0] = WASM_I32_VAL(glfwJoystickIsGamepad(params->data[0].of.i32));
    return NULL;
}

ONYX_DEF(glfwUpdateGamepadMappings, (PTR), (INT)) {
    results->data[0] = WASM_I32_VAL(glfwUpdateGamepadMappings(ONYX_PTR(params->data[0].of.i32)));
    return NULL;
}

ONYX_DEF(glfwGetGamepadState, (INT, PTR), (INT)) {
    results->data[0] = WASM_I32_VAL(glfwGetGamepadState(params->data[0].of.i32, (GLFWgamepadstate *) ONYX_PTR(params->data[1].of.i32)));
    return NULL;
}

ONYX_DEF(glfwSetClipboardString, (LONG, PTR), ()) {
    GLFWwindow *window = (GLFWwindow *) params->data[0].of.i64;
    glfwSetClipboardString(window, ONYX_PTR(params->data[1].of.i32));
    return NULL;
}

ONYX_DEF(glfwGetTime, (), (DOUBLE)) {
    results->data[0] = WASM_F64_VAL(glfwGetTime());
    return NULL;
}

ONYX_DEF(glfwSetTime, (DOUBLE), ()) {
    glfwSetTime(params->data[0].of.f64);
    return NULL;
}

ONYX_DEF(glfwGetTimerValue, (), (LONG)) {
    results->data[0] = WASM_I64_VAL(glfwGetTimerValue());
    return NULL;
}

ONYX_DEF(glfwGetTimerFrequency, (), (LONG)) {
    results->data[0] = WASM_I64_VAL(glfwGetTimerFrequency());
    return NULL;
}

// // glfwSetJoystickCallback
// // glfwGetGamepadName :: (jid: i32) -> cstr ---
// // glfwGetClipboardString :: (window: GLFWwindow_p) -> cstr ---

ONYX_DEF(glfwGetMonitors, (INT, PTR, PTR), ()) {
    int monitor_count = params->data[0].of.i32;
    int actual_count;
    GLFWmonitor** monitors = glfwGetMonitors(&actual_count);
    int monitor_ptr = params->data[1].of.i32;

    int count = (actual_count < monitor_count) ? actual_count : monitor_count;
    for (int i=0; i<count; i++) {
        *(unsigned long *) ONYX_PTR(monitor_ptr + i * 8) = (unsigned long) monitors[i];
    }

    *(int *) ONYX_PTR(params->data[2].of.i32) = count;
    return NULL;
}

ONYX_DEF(glfwGetPrimaryMonitor, (), (LONG)) {
    results->data[0] = WASM_I64_VAL((unsigned long) glfwGetPrimaryMonitor());
    return NULL;
}

ONYX_DEF(glfwGetMonitorPos, (LONG, PTR, PTR), ()) {
    GLFWmonitor *monitor = (GLFWmonitor *) params->data[0].of.i64;
    glfwGetMonitorPos(monitor, (int *) ONYX_PTR(params->data[1].of.i32), (int *) ONYX_PTR(params->data[2].of.i32));
    return NULL;
}

ONYX_DEF(glfwGetMonitorWorkarea, (LONG, PTR, PTR, PTR, PTR), ()) {
    GLFWmonitor *monitor = (GLFWmonitor *) params->data[0].of.i64;
    glfwGetMonitorWorkarea(monitor, (int *) ONYX_PTR(params->data[1].of.i32), (int *) ONYX_PTR(params->data[2].of.i32), (int *) ONYX_PTR(params->data[3].of.i32), (int *) ONYX_PTR(params->data[4].of.i32));
    return NULL;
}

ONYX_DEF(glfwGetMonitorPhysicalSize, (LONG, PTR, PTR), ()) {
    GLFWmonitor *monitor = (GLFWmonitor *) params->data[0].of.i64;
    glfwGetMonitorPhysicalSize(monitor, (int *) ONYX_PTR(params->data[1].of.i32), (int *) ONYX_PTR(params->data[2].of.i32));
    return NULL;
}

ONYX_DEF(glfwGetMonitorContentScale, (LONG, PTR, PTR), ()) {
    GLFWmonitor *monitor = (GLFWmonitor *) params->data[0].of.i64;
    glfwGetMonitorContentScale(monitor, (float *) ONYX_PTR(params->data[1].of.i32), (float *) ONYX_PTR(params->data[2].of.i32));
    return NULL;
}

ONYX_DEF(glfwSetMonitorUserPointer, (LONG, PTR), ()) {
    GLFWmonitor *monitor = (GLFWmonitor *) params->data[0].of.i64;
    glfwSetMonitorUserPointer(monitor, (void *) (unsigned long) params->data[0].of.i32);
    return NULL;
}

ONYX_DEF(glfwGetMonitorUserPointer, (LONG), (PTR)) {
    GLFWmonitor *monitor = (GLFWmonitor *) params->data[0].of.i64;
    results->data[0] = WASM_I32_VAL((unsigned int) (unsigned long) glfwGetMonitorUserPointer(monitor));
    return NULL;
}

// // glfwGetMonitorName :: (monitor: GLFWmonitor_p) -> cstr ---
// // glfwSetMonitorCallback
// glfwGetVideoModes :: (monitor: GLFWmonitor_p, max_modes: i32, out_modes: ^GLFWvidmode, out_count: ^i32) -> void ---
// glfwGetVideoMode :: (monitor: GLFWmonitor_p, out_mode: ^GLFWvidmode) -> void ---
// glfwSetGamma :: (monitor: GLFWmonitor_p, gamma: f32) -> void ---
// // glfwGetGammaRamp :: (monitor: GLFWmonitor_p, out_ramp: ^GLFWgammramp) -> void ---
// // glfwSetGammaRamp :: (monitor: GLFWmonitor_p, ramp: ^GLFWgammramp) -> void ---



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

    ONYX_FUNC(glfwGetInputMode)
    ONYX_FUNC(glfwSetInputMode)
    ONYX_FUNC(glfwRawMouseMotionSupported)
    ONYX_FUNC(glfwGetKeyScancode)
    ONYX_FUNC(glfwGetKey)
    ONYX_FUNC(glfwGetMouseButton)
    ONYX_FUNC(glfwGetCursorPos)
    ONYX_FUNC(glfwSetCursorPos)
    ONYX_FUNC(glfwCreateCursor)
    ONYX_FUNC(glfwCreateStandardCursor)
    ONYX_FUNC(glfwDestroyCursor)
    ONYX_FUNC(glfwSetCursor)
    ONYX_FUNC(glfwJoystickPresent)
    ONYX_FUNC(glfwSetJoystickUserPointer)
    ONYX_FUNC(glfwGetJoystickUserPointer)
    ONYX_FUNC(glfwJoystickIsGamepad)
    ONYX_FUNC(glfwUpdateGamepadMappings)
    ONYX_FUNC(glfwGetGamepadState)
    ONYX_FUNC(glfwSetClipboardString)
    ONYX_FUNC(glfwGetTime)
    ONYX_FUNC(glfwSetTime)
    ONYX_FUNC(glfwGetTimerValue)
    ONYX_FUNC(glfwGetTimerFrequency)
    ONYX_FUNC(glfwGetMonitors)
    ONYX_FUNC(glfwGetPrimaryMonitor)
    ONYX_FUNC(glfwGetMonitorPos)
    ONYX_FUNC(glfwGetMonitorWorkarea)
    ONYX_FUNC(glfwGetMonitorPhysicalSize)
    ONYX_FUNC(glfwGetMonitorContentScale)
    ONYX_FUNC(glfwSetMonitorUserPointer)
    ONYX_FUNC(glfwGetMonitorUserPointer)


    // // glfwGetKeyName :: (key, scancode: i32) -> cstr ---

// // glfwSetKeyCallback
// // glfwSetCharCallback
// // glfwSetCharModsCallback
// // glfwSetMouseButtonCallback
// // glfwSetCursorPosCallback
// // glfwSetCursorEnterCallback
// // glfwSetScrollCallback
// // glfwSetDropCallback
// // glfwGetJoystickAxes :: (jid: i32, count: ^i32) -> ^f32 ---
// // glfwGetJoystickButtons :: (jid: i32, count: ^i32) -> ^u8 ---
// // glfwGetJoystickHats :: (jid: i32, count: ^i32) -> ^u8 ---
// // glfwGetJoystickName :: (jid: i32) -> ^u8 ---
// // glfwGetJoystickGUID :: (jid: i32) -> ^u8 ---


    NULL
};