#define ONYX_LIBRARY_NAME stb_image
#include "onyx_library.h"

static wasm_func_t* stbi_heap_alloc = NULL;
static void *__onyx_stbi_heap_alloc_wrapper(int size) {
    if (stbi_heap_alloc == NULL) {
        wasm_extern_t *stbi_heap_alloc_extern = runtime->wasm_extern_lookup_by_name(runtime->wasm_module, runtime->wasm_instance, "stbi_heap_alloc");
        stbi_heap_alloc = runtime->wasm_extern_as_func(stbi_heap_alloc_extern);
        assert(stbi_heap_alloc != NULL);
    }

    wasm_val_t args[] = { WASM_I32_VAL(size) };
    wasm_val_t results[1];
    wasm_val_vec_t args_arr = WASM_ARRAY_VEC(args);
    wasm_val_vec_t results_arr = WASM_ARRAY_VEC(results);

    runtime->wasm_func_call(stbi_heap_alloc, &args_arr, &results_arr);
    return ONYX_PTR(results[0].of.i32);
}

static wasm_func_t* stbi_heap_resize = NULL;
static void *__onyx_stbi_heap_resize_wrapper(void* ptr, int size) {
    if (stbi_heap_resize == NULL) {
        wasm_extern_t *stbi_heap_resize_extern = runtime->wasm_extern_lookup_by_name(runtime->wasm_module, runtime->wasm_instance, "stbi_heap_resize");
        stbi_heap_resize = runtime->wasm_extern_as_func(stbi_heap_resize_extern);
    }

    int onyx_ptr = 0;
    if (ptr != NULL) onyx_ptr = (int) ((char*) ptr - runtime->wasm_memory_data(runtime->wasm_memory));

    wasm_val_t args[] = { WASM_I32_VAL(onyx_ptr), WASM_I32_VAL(size) };
    wasm_val_t results[1];
    wasm_val_vec_t args_arr = WASM_ARRAY_VEC(args);
    wasm_val_vec_t results_arr = WASM_ARRAY_VEC(results);

    runtime->wasm_func_call(stbi_heap_resize, &args_arr, &results_arr);
    return ONYX_PTR(results[0].of.i32);
}

static wasm_func_t* stbi_heap_free = NULL;
static void __onyx_stbi_heap_free_wrapper(void *ptr) {
    if (stbi_heap_free == NULL) {
        wasm_extern_t *stbi_heap_free_extern = runtime->wasm_extern_lookup_by_name(runtime->wasm_module, runtime->wasm_instance, "stbi_heap_free");
        stbi_heap_free = runtime->wasm_extern_as_func(stbi_heap_free_extern);
    }

    if (ptr == NULL) return;

    int onyx_ptr = (int) ((char*) ptr - runtime->wasm_memory_data(runtime->wasm_memory));

    wasm_val_t args[] = { WASM_I32_VAL(onyx_ptr) };
    wasm_val_vec_t results = {0,0};
    wasm_val_vec_t args_arr = WASM_ARRAY_VEC(args);

    runtime->wasm_func_call(stbi_heap_free, &args_arr, &results);
}

#define STBI_MALLOC(s)        __onyx_stbi_heap_alloc_wrapper(s)
#define STBI_REALLOC(p,newsz) __onyx_stbi_heap_resize_wrapper(p,newsz)
#define STBI_FREE(p)          __onyx_stbi_heap_free_wrapper(p)

#define STB_IMAGE_IMPLEMENTATION
#include "stb_image.h"

#define PTR WASM_I32
#define INT WASM_I32
#define FLOAT WASM_F32
#define PARAM(n, k) (params->data[n].of.k)

ONYX_DEF(stbi_load, (PTR, PTR, PTR, PTR, INT), (PTR)) {
    unsigned char *data = stbi_load(
            (char *) ONYX_PTR(PARAM(0, i32)),
            (int *) ONYX_PTR(PARAM(1, i32)),
            (int *) ONYX_PTR(PARAM(2, i32)),
            (int *) ONYX_PTR(PARAM(3, i32)),
            PARAM(4, i32));

    if (data == NULL) results->data[0] = WASM_I32_VAL(0);
    else              results->data[0] = WASM_I32_VAL(data - (unsigned char *) runtime->wasm_memory_data(runtime->wasm_memory));
    return NULL;
} 

ONYX_DEF(stbi_load_from_memory, (PTR, INT, PTR, PTR, PTR, INT), (PTR)) {
    unsigned char *data = stbi_load_from_memory(
            (unsigned char *) ONYX_PTR(PARAM(0, i32)),
            PARAM(1, i32),
            (int *) ONYX_PTR(PARAM(2, i32)),
            (int *) ONYX_PTR(PARAM(3, i32)),
            (int *) ONYX_PTR(PARAM(4, i32)),
            PARAM(5, i32));

    if (data == NULL) results->data[0] = WASM_I32_VAL(0);
    else              results->data[0] = WASM_I32_VAL(data - (unsigned char *) runtime->wasm_memory_data(runtime->wasm_memory));
    return NULL;
}

ONYX_DEF(stbi_load_gif_from_memory, (PTR, INT, PTR, PTR, PTR, PTR, PTR, INT), (PTR)) {
    unsigned char *data = stbi_load_gif_from_memory(
            (unsigned char *) ONYX_PTR(PARAM(0, i32)),
            PARAM(1, i32),
            (int **) ONYX_PTR(PARAM(2, i32)),
            (int *)  ONYX_PTR(PARAM(3, i32)),
            (int *)  ONYX_PTR(PARAM(4, i32)),
            (int *)  ONYX_PTR(PARAM(5, i32)),
            (int *)  ONYX_PTR(PARAM(6, i32)),
            PARAM(7, i32));

    if (data == NULL) results->data[0] = WASM_I32_VAL(0);
    else              results->data[0] = WASM_I32_VAL(data - (unsigned char *) runtime->wasm_memory_data(runtime->wasm_memory));
    return NULL;
}

ONYX_DEF(stbi_image_free, (PTR), ()) {
    stbi_image_free(ONYX_PTR(PARAM(0, i32)));
    return NULL;
}

ONYX_LIBRARY {
    ONYX_FUNC(stbi_load)
    ONYX_FUNC(stbi_load_from_memory)
    ONYX_FUNC(stbi_load_gif_from_memory)
    ONYX_FUNC(stbi_image_free)

    NULL
};
