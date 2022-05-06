//
// THIS FILE WAS AUTOMATICALLY GENERATED.
//

#include <GLFW/glfw3.h>

#define __glfwGetLoadProcAddress() (unsigned long long) &glfwGetProcAddress

#define _EXPAND(...) __VA_ARGS__
#define GLFW_HOOK(callback_name, c_args, wasm_args) \
    static wasm_func_t* __glfw_callback_##callback_name ; \
    static void __glfw_##callback_name (GLFWwindow *window, _EXPAND c_args) { \
        wasm_val_t args[] = { WASM_I64_VAL((unsigned long long) window), _EXPAND wasm_args }; \
        wasm_val_vec_t args_array = WASM_ARRAY_VEC(args); \
        wasm_val_vec_t results = WASM_EMPTY_VEC; \
        runtime->wasm_func_call(__glfw_callback_##callback_name , &args_array, &results); \
    } \
    ONYX_DEF(callback_name, (LONG, PTR, INT), ()) { \
        GLFWwindow *window = (GLFWwindow *) params->data[0].of.i64; \
        char name[512]; \
        strncpy(name, ONYX_PTR(params->data[1].of.i32), params->data[2].of.i32); \
        name[params->data[2].of.i32] = '\0'; \
        __glfw_callback_##callback_name = runtime->wasm_extern_as_func(runtime->wasm_extern_lookup_by_name(runtime->wasm_module, runtime->wasm_instance, name)); \
        callback_name(window, __glfw_##callback_name); \
        return NULL; \
    }


#define ONYX_LIBRARY_NAME onyx_glfw3
#include "onyx_library.h"

#define P(i, k) (params->data[i].of.k)

ONYX_DEF(glfwInit, (), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(glfwInit());
    return NULL;
}

ONYX_DEF(glfwTerminate, (), ()) {
    glfwTerminate();
    return NULL;
}

ONYX_DEF(glfwInitHint, (WASM_I32, WASM_I32), ()) {
    glfwInitHint(P(0, i32), P(1, i32));
    return NULL;
}

ONYX_DEF(glfwGetVersion, (WASM_I32, WASM_I32, WASM_I32), ()) {
    glfwGetVersion(ONYX_PTR(P(0, i32)), ONYX_PTR(P(1, i32)), ONYX_PTR(P(2, i32)));
    return NULL;
}

ONYX_DEF(glfwGetError, (WASM_I32), ()) {
    glfwGetError(ONYX_PTR(P(0, i32)));
    return NULL;
}

ONYX_DEF(glfwMakeContextCurrent, (WASM_I64), ()) {
    glfwMakeContextCurrent((GLFWwindow *) P(0, i64));
    return NULL;
}

ONYX_DEF(glfwGetCurrentContext, (), (WASM_I64)) {
    results->data[0] = WASM_I64_VAL(glfwGetCurrentContext());
    return NULL;
}

ONYX_DEF(glfwSwapInterval, (WASM_I32), ()) {
    glfwSwapInterval(P(0, i32));
    return NULL;
}

ONYX_DEF(glfwExtensionSupported, (WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(glfwExtensionSupported(ONYX_PTR(P(0, i32))));
    return NULL;
}

ONYX_DEF(glfwGetInputMode, (WASM_I64, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(glfwGetInputMode((GLFWwindow *) P(0, i64), P(1, i32)));
    return NULL;
}

ONYX_DEF(glfwSetInputMode, (WASM_I64, WASM_I32, WASM_I32), ()) {
    glfwSetInputMode((GLFWwindow *) P(0, i64), P(1, i32), P(2, i32));
    return NULL;
}

ONYX_DEF(glfwRawMouseMotionSupported, (), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(glfwRawMouseMotionSupported());
    return NULL;
}

ONYX_DEF(glfwGetKeyName, (WASM_I32, WASM_I32), (WASM_I64)) {
    results->data[0] = WASM_I64_VAL(glfwGetKeyName(P(0, i32), P(1, i32)));
    return NULL;
}

ONYX_DEF(glfwGetKeyScancode, (WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(glfwGetKeyScancode(P(0, i32)));
    return NULL;
}

ONYX_DEF(glfwGetKey, (WASM_I64, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(glfwGetKey((GLFWwindow *) P(0, i64), P(1, i32)));
    return NULL;
}

ONYX_DEF(glfwGetMouseButton, (WASM_I64, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(glfwGetMouseButton((GLFWwindow *) P(0, i64), P(1, i32)));
    return NULL;
}

ONYX_DEF(glfwGetCursorPos, (WASM_I64, WASM_I32, WASM_I32), ()) {
    glfwGetCursorPos((GLFWwindow *) P(0, i64), ONYX_PTR(P(1, i32)), ONYX_PTR(P(2, i32)));
    return NULL;
}

ONYX_DEF(glfwSetCursorPos, (WASM_I64, WASM_F64, WASM_F64), ()) {
    glfwSetCursorPos((GLFWwindow *) P(0, i64), P(1, f64), P(2, f64));
    return NULL;
}

ONYX_DEF(glfwCreateCursor, (WASM_I32, WASM_I32, WASM_I32), (WASM_I64)) {
    results->data[0] = WASM_I64_VAL(glfwCreateCursor(ONYX_PTR(P(0, i32)), P(1, i32), P(2, i32)));
    return NULL;
}

ONYX_DEF(glfwCreateStandardCursor, (WASM_I32), (WASM_I64)) {
    results->data[0] = WASM_I64_VAL(glfwCreateStandardCursor(P(0, i32)));
    return NULL;
}

ONYX_DEF(glfwDestroyCursor, (WASM_I64), ()) {
    glfwDestroyCursor((GLFWcursor *) P(0, i64));
    return NULL;
}

ONYX_DEF(glfwSetCursor, (WASM_I64, WASM_I64), ()) {
    glfwSetCursor((GLFWwindow *) P(0, i64), (GLFWcursor *) P(1, i64));
    return NULL;
}


                GLFW_HOOK(glfwSetKeyCallback, (int key, int scancode, int actions, int mods),
                    (WASM_I32_VAL(key), WASM_I32_VAL(scancode), WASM_I32_VAL(actions), WASM_I32_VAL(mods)))

                GLFW_HOOK(glfwSetCharCallback, (unsigned int ch),
                    (WASM_I32_VAL(ch)))

                GLFW_HOOK(glfwSetCharModsCallback, (unsigned int ch, int mods),
                    (WASM_I32_VAL(ch), WASM_I32_VAL(mods)))

                GLFW_HOOK(glfwSetMouseButtonCallback, (int button, int action, int mods),
                    (WASM_I32_VAL(button), WASM_I32_VAL(action), WASM_I32_VAL(mods)))

                GLFW_HOOK(glfwSetCursorPosCallback, (double xpos, double ypos),
                    (WASM_F64_VAL(xpos), WASM_F64_VAL(ypos)))

                GLFW_HOOK(glfwSetCursorEnterCallback, (int entered),
                    (WASM_I32_VAL(entered)))

                GLFW_HOOK(glfwSetScrollCallback, (double dx, double dy),
                    (WASM_F64_VAL(dx), WASM_F64_VAL(dy)))
ONYX_DEF(glfwJoystickPresent, (WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(glfwJoystickPresent(P(0, i32)));
    return NULL;
}

ONYX_DEF(glfwGetJoystickAxes, (WASM_I32, WASM_I32), (WASM_I64)) {
    results->data[0] = WASM_I64_VAL(glfwGetJoystickAxes(P(0, i32), ONYX_PTR(P(1, i32))));
    return NULL;
}

ONYX_DEF(glfwGetJoystickButtons, (WASM_I32, WASM_I32), (WASM_I64)) {
    results->data[0] = WASM_I64_VAL(glfwGetJoystickButtons(P(0, i32), ONYX_PTR(P(1, i32))));
    return NULL;
}

ONYX_DEF(glfwGetJoystickHats, (WASM_I32, WASM_I32), (WASM_I64)) {
    results->data[0] = WASM_I64_VAL(glfwGetJoystickHats(P(0, i32), ONYX_PTR(P(1, i32))));
    return NULL;
}

ONYX_DEF(glfwGetJoystickName, (WASM_I32), (WASM_I64)) {
    results->data[0] = WASM_I64_VAL(glfwGetJoystickName(P(0, i32)));
    return NULL;
}

ONYX_DEF(glfwGetJoystickGUID, (WASM_I32), (WASM_I64)) {
    results->data[0] = WASM_I64_VAL(glfwGetJoystickGUID(P(0, i32)));
    return NULL;
}

ONYX_DEF(glfwSetJoystickUserPointer, (WASM_I32, WASM_I32), ()) {
    glfwSetJoystickUserPointer(P(0, i32), ONYX_PTR(P(1, i32)));
    return NULL;
}

ONYX_DEF(glfwGetJoystickUserPointer, (WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(glfwGetJoystickUserPointer(P(0, i32)));
    return NULL;
}

ONYX_DEF(glfwJoystickIsGamepad, (WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(glfwJoystickIsGamepad(P(0, i32)));
    return NULL;
}

ONYX_DEF(glfwUpdateGamepadMappings, (WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(glfwUpdateGamepadMappings(ONYX_PTR(P(0, i32))));
    return NULL;
}

ONYX_DEF(glfwGetGamepadName, (WASM_I32), (WASM_I64)) {
    results->data[0] = WASM_I64_VAL(glfwGetGamepadName(P(0, i32)));
    return NULL;
}

ONYX_DEF(glfwGetGamepadState, (WASM_I32, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(glfwGetGamepadState(P(0, i32), ONYX_PTR(P(1, i32))));
    return NULL;
}

ONYX_DEF(glfwSetClipboardString, (WASM_I64, WASM_I32), ()) {
    glfwSetClipboardString((GLFWwindow *) P(0, i64), ONYX_PTR(P(1, i32)));
    return NULL;
}

ONYX_DEF(glfwGetClipboardString, (WASM_I64), (WASM_I64)) {
    results->data[0] = WASM_I64_VAL(glfwGetClipboardString((GLFWwindow *) P(0, i64)));
    return NULL;
}

ONYX_DEF(glfwGetTime, (), (WASM_F64)) {
    results->data[0] = WASM_F64_VAL(glfwGetTime());
    return NULL;
}

ONYX_DEF(glfwSetTime, (WASM_F64), ()) {
    glfwSetTime(P(0, f64));
    return NULL;
}

ONYX_DEF(glfwGetTimerValue, (), (WASM_I64)) {
    results->data[0] = WASM_I64_VAL(glfwGetTimerValue());
    return NULL;
}

ONYX_DEF(glfwGetTimerFrequency, (), (WASM_I64)) {
    results->data[0] = WASM_I64_VAL(glfwGetTimerFrequency());
    return NULL;
}

ONYX_DEF(glfwGetMonitors, (WASM_I32), (WASM_I64)) {
    results->data[0] = WASM_I64_VAL(glfwGetMonitors(ONYX_PTR(P(0, i32))));
    return NULL;
}

ONYX_DEF(glfwGetPrimaryMonitor, (), (WASM_I64)) {
    results->data[0] = WASM_I64_VAL(glfwGetPrimaryMonitor());
    return NULL;
}

ONYX_DEF(glfwGetMonitorPos, (WASM_I64, WASM_I32, WASM_I32), ()) {
    glfwGetMonitorPos((GLFWmonitor *) P(0, i64), ONYX_PTR(P(1, i32)), ONYX_PTR(P(2, i32)));
    return NULL;
}

ONYX_DEF(glfwGetMonitorWorkarea, (WASM_I64, WASM_I32, WASM_I32, WASM_I32, WASM_I32), ()) {
    glfwGetMonitorWorkarea((GLFWmonitor *) P(0, i64), ONYX_PTR(P(1, i32)), ONYX_PTR(P(2, i32)), ONYX_PTR(P(3, i32)), ONYX_PTR(P(4, i32)));
    return NULL;
}

ONYX_DEF(glfwGetMonitorPhysicalSize, (WASM_I64, WASM_I32, WASM_I32), ()) {
    glfwGetMonitorPhysicalSize((GLFWmonitor *) P(0, i64), ONYX_PTR(P(1, i32)), ONYX_PTR(P(2, i32)));
    return NULL;
}

ONYX_DEF(glfwGetMonitorContentScale, (WASM_I64, WASM_I32, WASM_I32), ()) {
    glfwGetMonitorContentScale((GLFWmonitor *) P(0, i64), ONYX_PTR(P(1, i32)), ONYX_PTR(P(2, i32)));
    return NULL;
}

ONYX_DEF(glfwGetMonitorName, (WASM_I64), (WASM_I64)) {
    results->data[0] = WASM_I64_VAL(glfwGetMonitorName((GLFWmonitor *) P(0, i64)));
    return NULL;
}

ONYX_DEF(glfwSetMonitorUserPointer, (WASM_I64, WASM_I32), ()) {
    glfwSetMonitorUserPointer((GLFWmonitor *) P(0, i64), ONYX_PTR(P(1, i32)));
    return NULL;
}

ONYX_DEF(glfwGetMonitorUserPointer, (WASM_I64), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(glfwGetMonitorUserPointer((GLFWmonitor *) P(0, i64)));
    return NULL;
}

ONYX_DEF(glfwGetVideoModes, (WASM_I64, WASM_I32), (WASM_I64)) {
    results->data[0] = WASM_I64_VAL(glfwGetVideoModes((GLFWmonitor *) P(0, i64), ONYX_PTR(P(1, i32))));
    return NULL;
}

ONYX_DEF(glfwGetVideoMode, (WASM_I64), (WASM_I64)) {
    results->data[0] = WASM_I64_VAL(glfwGetVideoMode((GLFWmonitor *) P(0, i64)));
    return NULL;
}

ONYX_DEF(glfwSetGamma, (WASM_I64, WASM_F32), ()) {
    glfwSetGamma((GLFWmonitor *) P(0, i64), P(1, f32));
    return NULL;
}

ONYX_DEF(glfwCreateWindow, (WASM_I32, WASM_I32, WASM_I32, WASM_I64, WASM_I64), (WASM_I64)) {
    results->data[0] = WASM_I64_VAL(glfwCreateWindow(P(0, i32), P(1, i32), ONYX_PTR(P(2, i32)), (GLFWmonitor *) P(3, i64), (GLFWwindow *) P(4, i64)));
    return NULL;
}

ONYX_DEF(glfwDestroyWindow, (WASM_I64), ()) {
    glfwDestroyWindow((GLFWwindow *) P(0, i64));
    return NULL;
}

ONYX_DEF(glfwWindowShouldClose, (WASM_I64), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(glfwWindowShouldClose((GLFWwindow *) P(0, i64)));
    return NULL;
}

ONYX_DEF(glfwSetWindowShouldClose, (WASM_I64, WASM_I32), ()) {
    glfwSetWindowShouldClose((GLFWwindow *) P(0, i64), P(1, i32));
    return NULL;
}

ONYX_DEF(glfwDefaultWindowHints, (), ()) {
    glfwDefaultWindowHints();
    return NULL;
}

ONYX_DEF(glfwWindowHint, (WASM_I32, WASM_I32), ()) {
    glfwWindowHint(P(0, i32), P(1, i32));
    return NULL;
}

ONYX_DEF(glfwSetWindowTitle, (WASM_I64, WASM_I32), ()) {
    glfwSetWindowTitle((GLFWwindow *) P(0, i64), ONYX_PTR(P(1, i32)));
    return NULL;
}

ONYX_DEF(glfwSetWindowIcon, (WASM_I64, WASM_I32, WASM_I32), ()) {
    glfwSetWindowIcon((GLFWwindow *) P(0, i64), P(1, i32), ONYX_PTR(P(2, i32)));
    return NULL;
}

ONYX_DEF(glfwGetWindowPos, (WASM_I64, WASM_I32, WASM_I32), ()) {
    glfwGetWindowPos((GLFWwindow *) P(0, i64), ONYX_PTR(P(1, i32)), ONYX_PTR(P(2, i32)));
    return NULL;
}

ONYX_DEF(glfwSetWindowPos, (WASM_I64, WASM_I32, WASM_I32), ()) {
    glfwSetWindowPos((GLFWwindow *) P(0, i64), P(1, i32), P(2, i32));
    return NULL;
}

ONYX_DEF(glfwGetWindowSize, (WASM_I64, WASM_I32, WASM_I32), ()) {
    glfwGetWindowSize((GLFWwindow *) P(0, i64), ONYX_PTR(P(1, i32)), ONYX_PTR(P(2, i32)));
    return NULL;
}

ONYX_DEF(glfwSetWindowSize, (WASM_I64, WASM_I32, WASM_I32), ()) {
    glfwSetWindowSize((GLFWwindow *) P(0, i64), P(1, i32), P(2, i32));
    return NULL;
}

ONYX_DEF(glfwSetWindowSizeLimits, (WASM_I64, WASM_I32, WASM_I32, WASM_I32, WASM_I32), ()) {
    glfwSetWindowSizeLimits((GLFWwindow *) P(0, i64), P(1, i32), P(2, i32), P(3, i32), P(4, i32));
    return NULL;
}

ONYX_DEF(glfwSetWindowAspectRatio, (WASM_I64, WASM_I32, WASM_I32), ()) {
    glfwSetWindowAspectRatio((GLFWwindow *) P(0, i64), P(1, i32), P(2, i32));
    return NULL;
}

ONYX_DEF(glfwGetFramebufferSize, (WASM_I64, WASM_I32, WASM_I32), ()) {
    glfwGetFramebufferSize((GLFWwindow *) P(0, i64), ONYX_PTR(P(1, i32)), ONYX_PTR(P(2, i32)));
    return NULL;
}

ONYX_DEF(glfwGetWindowFrameSize, (WASM_I64, WASM_I32, WASM_I32, WASM_I32, WASM_I32), ()) {
    glfwGetWindowFrameSize((GLFWwindow *) P(0, i64), ONYX_PTR(P(1, i32)), ONYX_PTR(P(2, i32)), ONYX_PTR(P(3, i32)), ONYX_PTR(P(4, i32)));
    return NULL;
}

ONYX_DEF(glfwIconifyWindow, (WASM_I64), ()) {
    glfwIconifyWindow((GLFWwindow *) P(0, i64));
    return NULL;
}

ONYX_DEF(glfwRestoreWindow, (WASM_I64), ()) {
    glfwRestoreWindow((GLFWwindow *) P(0, i64));
    return NULL;
}

ONYX_DEF(glfwMaximizeWindow, (WASM_I64), ()) {
    glfwMaximizeWindow((GLFWwindow *) P(0, i64));
    return NULL;
}

ONYX_DEF(glfwShowWindow, (WASM_I64), ()) {
    glfwShowWindow((GLFWwindow *) P(0, i64));
    return NULL;
}

ONYX_DEF(glfwHideWindow, (WASM_I64), ()) {
    glfwHideWindow((GLFWwindow *) P(0, i64));
    return NULL;
}

ONYX_DEF(glfwFocusWindow, (WASM_I64), ()) {
    glfwFocusWindow((GLFWwindow *) P(0, i64));
    return NULL;
}

ONYX_DEF(glfwGetWindowMonitor, (WASM_I64), (WASM_I64)) {
    results->data[0] = WASM_I64_VAL(glfwGetWindowMonitor((GLFWwindow *) P(0, i64)));
    return NULL;
}

ONYX_DEF(glfwSetWindowMonitor, (WASM_I64, WASM_I64, WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32), ()) {
    glfwSetWindowMonitor((GLFWwindow *) P(0, i64), (GLFWmonitor *) P(1, i64), P(2, i32), P(3, i32), P(4, i32), P(5, i32), P(6, i32));
    return NULL;
}

ONYX_DEF(glfwGetWindowAttrib, (WASM_I64, WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(glfwGetWindowAttrib((GLFWwindow *) P(0, i64), P(1, i32)));
    return NULL;
}

ONYX_DEF(glfwSetWindowUserPointer, (WASM_I64, WASM_I32), ()) {
    glfwSetWindowUserPointer((GLFWwindow *) P(0, i64), ONYX_PTR(P(1, i32)));
    return NULL;
}

ONYX_DEF(glfwGetWindowUserPointer, (WASM_I64), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(glfwGetWindowUserPointer((GLFWwindow *) P(0, i64)));
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

ONYX_DEF(glfwWaitEventsTimeout, (WASM_F64), ()) {
    glfwWaitEventsTimeout(P(0, f64));
    return NULL;
}

ONYX_DEF(glfwPostEmptyEvent, (), ()) {
    glfwPostEmptyEvent();
    return NULL;
}

ONYX_DEF(glfwSwapBuffers, (WASM_I64), ()) {
    glfwSwapBuffers((GLFWwindow *) P(0, i64));
    return NULL;
}


                GLFW_HOOK(glfwSetWindowSizeCallback, (int width, int height),
                    (WASM_I32_VAL(width), WASM_I32_VAL(height)))
ONYX_DEF(__glfwGetLoadProcAddress, (), (WASM_I64)) {
    results->data[0] = WASM_I64_VAL(__glfwGetLoadProcAddress());
    return NULL;
}



ONYX_LIBRARY {
    ONYX_FUNC(glfwInit)
    ONYX_FUNC(glfwTerminate)
    ONYX_FUNC(glfwInitHint)
    ONYX_FUNC(glfwGetVersion)
    ONYX_FUNC(glfwGetError)
    ONYX_FUNC(glfwMakeContextCurrent)
    ONYX_FUNC(glfwGetCurrentContext)
    ONYX_FUNC(glfwSwapInterval)
    ONYX_FUNC(glfwExtensionSupported)
    ONYX_FUNC(glfwGetInputMode)
    ONYX_FUNC(glfwSetInputMode)
    ONYX_FUNC(glfwRawMouseMotionSupported)
    ONYX_FUNC(glfwGetKeyName)
    ONYX_FUNC(glfwGetKeyScancode)
    ONYX_FUNC(glfwGetKey)
    ONYX_FUNC(glfwGetMouseButton)
    ONYX_FUNC(glfwGetCursorPos)
    ONYX_FUNC(glfwSetCursorPos)
    ONYX_FUNC(glfwCreateCursor)
    ONYX_FUNC(glfwCreateStandardCursor)
    ONYX_FUNC(glfwDestroyCursor)
    ONYX_FUNC(glfwSetCursor)
    ONYX_FUNC(glfwSetKeyCallback)
    ONYX_FUNC(glfwSetCharCallback)
    ONYX_FUNC(glfwSetCharModsCallback)
    ONYX_FUNC(glfwSetMouseButtonCallback)
    ONYX_FUNC(glfwSetCursorPosCallback)
    ONYX_FUNC(glfwSetCursorEnterCallback)
    ONYX_FUNC(glfwSetScrollCallback)
    ONYX_FUNC(glfwJoystickPresent)
    ONYX_FUNC(glfwGetJoystickAxes)
    ONYX_FUNC(glfwGetJoystickButtons)
    ONYX_FUNC(glfwGetJoystickHats)
    ONYX_FUNC(glfwGetJoystickName)
    ONYX_FUNC(glfwGetJoystickGUID)
    ONYX_FUNC(glfwSetJoystickUserPointer)
    ONYX_FUNC(glfwGetJoystickUserPointer)
    ONYX_FUNC(glfwJoystickIsGamepad)
    ONYX_FUNC(glfwUpdateGamepadMappings)
    ONYX_FUNC(glfwGetGamepadName)
    ONYX_FUNC(glfwGetGamepadState)
    ONYX_FUNC(glfwSetClipboardString)
    ONYX_FUNC(glfwGetClipboardString)
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
    ONYX_FUNC(glfwGetMonitorName)
    ONYX_FUNC(glfwSetMonitorUserPointer)
    ONYX_FUNC(glfwGetMonitorUserPointer)
    ONYX_FUNC(glfwGetVideoModes)
    ONYX_FUNC(glfwGetVideoMode)
    ONYX_FUNC(glfwSetGamma)
    ONYX_FUNC(glfwCreateWindow)
    ONYX_FUNC(glfwDestroyWindow)
    ONYX_FUNC(glfwWindowShouldClose)
    ONYX_FUNC(glfwSetWindowShouldClose)
    ONYX_FUNC(glfwDefaultWindowHints)
    ONYX_FUNC(glfwWindowHint)
    ONYX_FUNC(glfwSetWindowTitle)
    ONYX_FUNC(glfwSetWindowIcon)
    ONYX_FUNC(glfwGetWindowPos)
    ONYX_FUNC(glfwSetWindowPos)
    ONYX_FUNC(glfwGetWindowSize)
    ONYX_FUNC(glfwSetWindowSize)
    ONYX_FUNC(glfwSetWindowSizeLimits)
    ONYX_FUNC(glfwSetWindowAspectRatio)
    ONYX_FUNC(glfwGetFramebufferSize)
    ONYX_FUNC(glfwGetWindowFrameSize)
    ONYX_FUNC(glfwIconifyWindow)
    ONYX_FUNC(glfwRestoreWindow)
    ONYX_FUNC(glfwMaximizeWindow)
    ONYX_FUNC(glfwShowWindow)
    ONYX_FUNC(glfwHideWindow)
    ONYX_FUNC(glfwFocusWindow)
    ONYX_FUNC(glfwGetWindowMonitor)
    ONYX_FUNC(glfwSetWindowMonitor)
    ONYX_FUNC(glfwGetWindowAttrib)
    ONYX_FUNC(glfwSetWindowUserPointer)
    ONYX_FUNC(glfwGetWindowUserPointer)
    ONYX_FUNC(glfwPollEvents)
    ONYX_FUNC(glfwWaitEvents)
    ONYX_FUNC(glfwWaitEventsTimeout)
    ONYX_FUNC(glfwPostEmptyEvent)
    ONYX_FUNC(glfwSwapBuffers)
    ONYX_FUNC(glfwSetWindowSizeCallback)
    ONYX_FUNC(__glfwGetLoadProcAddress)
    NULL
};