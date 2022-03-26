#define ONYX_LIBRARY_NAME stb_truetype
#include "onyx_library.h"

static wasm_func_t* heap_alloc = NULL;
static void *__onyx_heap_alloc_wrapper(int size, void* userdata) {
    if (heap_alloc == NULL) {
        wasm_extern_t *heap_alloc_extern = runtime->wasm_extern_lookup_by_name(runtime->wasm_module, runtime->wasm_instance, "stbtt_heap_alloc");
        heap_alloc = runtime->wasm_extern_as_func(heap_alloc_extern);
    }

    wasm_val_t args[] = { WASM_I32_VAL(size) };
    wasm_val_t results[1];
    wasm_val_vec_t args_arr = WASM_ARRAY_VEC(args);
    wasm_val_vec_t results_arr = WASM_ARRAY_VEC(results);

    runtime->wasm_func_call(heap_alloc, &args_arr, &results_arr);
    return ONYX_PTR(results[0].of.i32);
}

static wasm_func_t* heap_free = NULL;
static void __onyx_heap_free_wrapper(void *ptr, void* userdata) {
    if (heap_free == NULL) {
        wasm_extern_t *heap_free_extern = runtime->wasm_extern_lookup_by_name(runtime->wasm_module, runtime->wasm_instance, "stbtt_heap_free");
        heap_free = runtime->wasm_extern_as_func(heap_free_extern);
    }

    int onyx_ptr = (int) ((char*) ptr - runtime->wasm_memory_data(runtime->wasm_memory));

    wasm_val_t args[] = { WASM_I32_VAL(onyx_ptr) };
    wasm_val_vec_t results = {0,0};
    wasm_val_vec_t args_arr = WASM_ARRAY_VEC(args);

    runtime->wasm_func_call(heap_free, &args_arr, &results);
}

#define STBTT_malloc __onyx_heap_alloc_wrapper
#define STBTT_free   __onyx_heap_free_wrapper

#define STB_TRUETYPE_IMPLEMENTATION
#define STB_RECT_PACK_IMPLEMENTATION
#include "stb_rect_pack.h"
#include "stb_truetype.h"


#define PTR WASM_I32
#define INT WASM_I32
#define FLOAT WASM_F32
#define PARAM(n, k) (params->data[n].of.k)

//
// Only wrapping the small subset of the features that I need.
//
ONYX_DEF(stbtt_PackBegin, (PTR, PTR, INT, INT, INT, INT), (INT)) {
    results->data[0] = WASM_I32_VAL(stbtt_PackBegin(ONYX_PTR(PARAM(0, i32)), ONYX_PTR(PARAM(1, i32)), PARAM(2, i32), PARAM(3, i32), PARAM(4, i32), PARAM(5, i32), NULL));
    return NULL;
}

ONYX_DEF(stbtt_PackEnd, (PTR), ()) {
    stbtt_PackEnd(ONYX_PTR(PARAM(0, i32)));
    return NULL;
}

ONYX_DEF(stbtt_PackSetOversampling, (PTR, INT, INT), ()) {
    stbtt_PackSetOversampling(ONYX_PTR(PARAM(0, i32)), PARAM(1, i32), PARAM(2, i32));
    return NULL;
}

ONYX_DEF(stbtt_PackFontRange, (PTR, PTR, INT, FLOAT, INT, INT, PTR), (INT)) {
    results->data[0] = WASM_I32_VAL(
        stbtt_PackFontRange(ONYX_PTR(PARAM(0, i32)), ONYX_PTR(PARAM(1, i32)),
        PARAM(2, i32), PARAM(3, f32), PARAM(4, i32), PARAM(5, i32), ONYX_PTR(PARAM(6, i32)))
    );

    return NULL;
}

ONYX_DEF(stbtt_GetPackedQuad, (PTR, INT, INT, INT, PTR, PTR, PTR, INT), ()) {
    stbtt_GetPackedQuad(ONYX_PTR(PARAM(0, i32)), PARAM(1, i32), PARAM(2, i32), PARAM(3, i32), ONYX_PTR(PARAM(4, i32)), ONYX_PTR(PARAM(5, i32)), ONYX_PTR(PARAM(6, i32)), PARAM(7, i32));
    return NULL;
}

ONYX_LIBRARY {
    ONYX_FUNC(stbtt_PackBegin)
    ONYX_FUNC(stbtt_PackEnd)
    ONYX_FUNC(stbtt_PackSetOversampling)
    ONYX_FUNC(stbtt_PackFontRange)
    ONYX_FUNC(stbtt_GetPackedQuad)

    NULL
};
