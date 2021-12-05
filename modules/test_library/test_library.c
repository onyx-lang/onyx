#include "onyx_library.h"
#include <stdio.h>
#include <unistd.h>

#define ONYX_LIBRARY_NAME test_library

ONYX_DEF(foo, (), ()) {
    printf("This worked!\n");
    return NULL;
}

ONYX_DEF(add, (INT, INT), (INT)) {
    int a = params->data[0].of.i32;
    int b = params->data[1].of.i32;
    results->data[0] = WASM_I32_VAL(a + b);
    return NULL;
}

ONYX_DEF(print_string, (PTR, INT), ()) {
    char *start = wasm_memory_data(wasm_memory) + params->data[0].of.i32;
    int  length = params->data[1].of.i32;

    write(1, start, length);
    return NULL;
}

ONYX_LIBRARY {
    ONYX_FUNC(foo)
    ONYX_FUNC(add)
    ONYX_FUNC(print_string)

    NULL
};