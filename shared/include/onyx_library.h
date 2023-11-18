#ifndef ONYX_LIBRARY_H
#define ONYX_LIBRARY_H

#include "wasm.h"

#if defined(_WIN32) || defined(_WIN64)
    #define ONYX_EXPORT extern __declspec(dllexport)
    #define ONYX_IMPORT extern __declspec(dllimport)
#endif

#if defined(__unix__) || defined(__APPLE__)
    #define ONYX_EXPORT
    #define ONYX_IMPORT
#endif

typedef struct OnyxRuntime {
    wasm_instance_t* wasm_instance;
    wasm_module_t* wasm_module;
    wasm_memory_t* wasm_memory;
    wasm_engine_t *wasm_engine;
    wasm_extern_vec_t wasm_imports;

    int argc;
    char **argv;

    // There shouldn't need to be this much stuff in here, but because Wasmer doesn't ship a "wasmerdll.lib"
    // file for windows, it is impossible for it to link successfully against the function provided in onyx.exe.
    // Therefore, we must serve as the linker and do this manually. Hopefully that library file will be
    // shipped soon so this can go away...
    char* (*wasm_memory_data)(wasm_memory_t *wasm_memory);
    wasm_extern_t* (*wasm_extern_lookup_by_name)(wasm_module_t* module, wasm_instance_t* instance, const char* name);
    wasm_func_t* (*wasm_extern_as_func)(wasm_extern_t* ext);
    wasm_trap_t* (*wasm_func_call)(const wasm_func_t* wasm_func, const wasm_val_vec_t* args, wasm_val_vec_t* results);
    wasm_instance_t* (*wasm_instance_new)(wasm_store_t* store, const wasm_module_t* module, const wasm_extern_vec_t* imports, wasm_trap_t** traps);
    void (*onyx_print_trap)(wasm_trap_t* trap);
    wasm_store_t *(*wasm_store_new)(wasm_engine_t *engine);
    void (*wasm_store_delete)(wasm_store_t *store);

    // This is only set when using the OVMwasm runtime, as Wasmer's C-api does not allow
    // for this function to exist, yet.
    wasm_table_t *wasm_func_table;
    void (*(*wasm_func_from_idx)(wasm_table_t *table, unsigned int index, char *signature))(void);

    void (*wasm_instance_delete)(wasm_instance_t *instance);

    wasm_store_t *wasm_store;
} OnyxRuntime;

OnyxRuntime* runtime;

typedef struct WasmValkindBuffer {
    unsigned int count;
    wasm_valkind_t types[20];
} WasmValkindBuffer;

typedef struct WasmFuncDefinition {
    char* module_name;
    char* import_name;
    wasm_func_callback_t func;

    WasmValkindBuffer *params;
    WasmValkindBuffer *results;
} WasmFuncDefinition;

#define STRINGIFY1(a) #a
#define STRINGIFY2(a) STRINGIFY1(a)
#define CONCAT2(a, b) a ## _ ## b
#define CONCAT3(a, b, c) a ## _ ## b ## _ ## c
#define ONYX_MODULE_NAME_GEN(m) CONCAT2(__onyx_library, m)
#define ONYX_LINK_NAME_GEN(m) CONCAT2(onyx_library, m)
#define ONYX_FUNC_NAME(m, n) CONCAT3(__onyx_internal, m, n)
#define ONYX_DEF_NAME(m, n) CONCAT3(__onyx_internal_def, m, n)
#define ONYX_PARAM_NAME(m, n) CONCAT3(__onyx_internal_param_buffer, m, n)
#define ONYX_RESULT_NAME(m, n) CONCAT3(__onyx_internal_result_buffer, m, n)
#define ONYX_IMPORT_NAME(m, n) STRINGIFY1(m) "_" #n

#define NUM_VALS(...) (sizeof((wasm_valkind_t []){ 0, __VA_ARGS__ }) / sizeof(wasm_valkind_t))
#define _VALS(...) { NUM_VALS(__VA_ARGS__) - 1, __VA_ARGS__ }

#define ONYX_DEF(name, params_types, result_types) \
    static wasm_trap_t* ONYX_FUNC_NAME(ONYX_LIBRARY_NAME, name)(const wasm_val_vec_t* params, wasm_val_vec_t* results); \
    static struct WasmValkindBuffer  ONYX_PARAM_NAME(ONYX_LIBRARY_NAME, name) = _VALS params_types; \
    static struct WasmValkindBuffer  ONYX_RESULT_NAME(ONYX_LIBRARY_NAME, name) = _VALS result_types; \
    static struct WasmFuncDefinition ONYX_DEF_NAME(ONYX_LIBRARY_NAME, name) = { STRINGIFY2(ONYX_LIBRARY_NAME), #name, ONYX_FUNC_NAME(ONYX_LIBRARY_NAME, name), & ONYX_PARAM_NAME(ONYX_LIBRARY_NAME, name), & ONYX_RESULT_NAME(ONYX_LIBRARY_NAME, name) }; \
    \
    static wasm_trap_t* ONYX_FUNC_NAME(ONYX_LIBRARY_NAME, name)(const wasm_val_vec_t* params, wasm_val_vec_t* results)

#define ONYX_FUNC(name) & ONYX_DEF_NAME(ONYX_LIBRARY_NAME, name),
#define ONYX_LIBRARY \
    extern struct WasmFuncDefinition *ONYX_MODULE_NAME_GEN(ONYX_LIBRARY_NAME)[]; \
    ONYX_EXPORT WasmFuncDefinition** ONYX_LINK_NAME_GEN(ONYX_LIBRARY_NAME)(OnyxRuntime* in_runtime) { \
        runtime = in_runtime; \
        return ONYX_MODULE_NAME_GEN(ONYX_LIBRARY_NAME); \
    } \
    struct WasmFuncDefinition *ONYX_MODULE_NAME_GEN(ONYX_LIBRARY_NAME)[] =

#define WASM_PTR WASM_I32

// Shorter names
#ifndef ONYX_NO_SHORT_NAMES
#undef  BOOL
#undef  INT
#undef  LONG
#undef  FLOAT
#undef  DOUBLE
#define BOOL WASM_I32
#define INT WASM_I32
#define LONG WASM_I64
#define FLOAT WASM_F32
#define DOUBLE WASM_F64
#define PTR WASM_PTR
#endif

#define ONYX_PTR(p) ((void*) (p != 0 ? (runtime->wasm_memory_data(runtime->wasm_memory) + p) : NULL))
#define ONYX_UNPTR(p) ((int) (p != NULL ? ((char *) p - runtime->wasm_memory_data(runtime->wasm_memory)) : 0))


//
// Below are definitions that allow you to use the heap_resize and heap_free
// functions to allocate and free memory inside Onyx's memory space, but from
// a C-library. These functions were very useful, but they had some nasty
// problems that cannot be easily addressed.
//
// The largest problem was that the WASM embedder made no guarantees about how
// the underlying memory used would be positioned or when it could relocate.
// If a C-library were to invoke heap_resize, and that resize caused the
// memory_grow intrinsic to be invoked, the embedder could move the memory to
// a new address. This would mean that ALL existing pointers into Onyx's memory
// given to the C-library would be invalidated, almost always causing a segfault.
//
// There isn't a great way around this, since the idea of wrapping a C library
// isn't necessarily supported by WASM at any level. The current work around
// is to have the C library allocate using malloc(), then return the pointer
// as a cptr(T), and then have Onyx copy that to a new buffer, and immediately
// free the cptr(T), generally using a destructor of some kind from the C library.
//

// #ifdef ONYX_HEAP_FUNCTIONS

// static wasm_func_t* __onyx_heap_resize_function = NULL;
// static void *__onyx_heap_resize(void* ptr, int size) {
//     if (__onyx_heap_resize_function == NULL) {
//         wasm_extern_t *__extern = runtime->wasm_extern_lookup_by_name(runtime->wasm_module, runtime->wasm_instance, "__heap_resize");
//         __onyx_heap_resize_function = runtime->wasm_extern_as_func(__extern);
//     }

//     int onyx_ptr = ONYX_UNPTR(ptr);

//     wasm_val_t args[] = { WASM_I32_VAL(onyx_ptr), WASM_I32_VAL(size) };
//     wasm_val_t results[1];
//     wasm_val_vec_t args_arr = WASM_ARRAY_VEC(args);
//     wasm_val_vec_t results_arr = WASM_ARRAY_VEC(results);

//     runtime->wasm_func_call(__onyx_heap_resize_function, &args_arr, &results_arr);
//     return ONYX_PTR(results[0].of.i32);
// }

// static wasm_func_t* __onyx_heap_free_function = NULL;
// static void __onyx_heap_free(void *ptr) {
//     if (__onyx_heap_free_function == NULL) {
//         wasm_extern_t *__extern = runtime->wasm_extern_lookup_by_name(runtime->wasm_module, runtime->wasm_instance, "__heap_free");
//         __onyx_heap_free_function = runtime->wasm_extern_as_func(__extern);
//     }

//     if (ptr == NULL) return;
//     int onyx_ptr = ONYX_UNPTR(ptr);

//     wasm_val_t args[] = { WASM_I32_VAL(onyx_ptr) };
//     wasm_val_vec_t results = {0,0};
//     wasm_val_vec_t args_arr = WASM_ARRAY_VEC(args);

//     runtime->wasm_func_call(__onyx_heap_free_function, &args_arr, &results);
// }


// #endif

#endif
