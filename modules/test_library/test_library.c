#include "onyx_module.h"
#include <stdio.h>

#define ONYX_MODULE_NAME test_library

ONYX_DEF(foo, (), ()) {
    printf("This worked!\n");
    return NULL;
}

ONYX_MODULE {
    ONYX_FUNC(foo)
};