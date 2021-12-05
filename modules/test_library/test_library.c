#include "onyx_library.h"
#include <stdio.h>

#define ONYX_LIBRARY_NAME test_library

ONYX_DEF(foo, (), ()) {
    printf("This worked!\n");
    return NULL;
}

ONYX_LIBRARY {
    ONYX_FUNC(foo)
};