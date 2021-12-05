
#include "wasm.h"

#define NUM_VALS(...) (sizeof((wasm_valkind_t []){ 0, __VA_ARGS__ }) / sizeof(wasm_valkind_t))
#define _VALS(...) { NUM_VALS(__VA_ARGS__) - 1, __VA_ARGS__ }

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
#define CONCAT2(a, b) a ## _ ## b
#define CONCAT3(a, b, c) a ## _ ## b ## _ ## c
#define ONYX_MODULE_NAME_GEN(m) CONCAT2(__onyx_module, m)
#define ONYX_FUNC_NAME(m, n) CONCAT3(__onyx_internal, m, n)
#define ONYX_DEF_NAME(m, n) CONCAT3(__onyx_internal_def, m, n)
#define ONYX_PARAM_NAME(m, n) CONCAT3(__onyx_internal_param_buffer, m, n)
#define ONYX_RESULT_NAME(m, n) CONCAT3(__onyx_internal_result_buffer, m, n)
#define ONYX_IMPORT_NAME(m, n) STRINGIFY1(m) "_" #n

#define ONYX_DEF(name, params_types, result_types) \
    static wasm_trap_t* ONYX_FUNC_NAME(ONYX_MODULE_NAME, name)(const wasm_val_vec_t* params, wasm_val_vec_t* results); \
    static struct WasmValkindBuffer  ONYX_PARAM_NAME(ONYX_MODULE_NAME, name) = _VALS params_types; \
    static struct WasmValkindBuffer  ONYX_RESULT_NAME(ONYX_MODULE_NAME, name) = _VALS result_types; \
    static struct WasmFuncDefinition ONYX_DEF_NAME(ONYX_MODULE_NAME, name) = { STRINGIFY1(ONYX_MODULE_NAME), #name, ONYX_FUNC_NAME(ONYX_MODULE_NAME, name), & ONYX_PARAM_NAME(ONYX_MODULE_NAME, name), & ONYX_RESULT_NAME(ONYX_MODULE_NAME, name) }; \
    \
    static wasm_trap_t* ONYX_FUNC_NAME(ONYX_MODULE_NAME, name)(const wasm_val_vec_t* params, wasm_val_vec_t* results)

#define ONYX_FUNC(name) & ONYX_DEF_NAME(ONYX_MODULE_NAME, name),
#define ONYX_MODULE struct WasmFuncDefinition *ONYX_MODULE_NAME_GEN(ONYX_MODULE_NAME) [] =

// Shorter names
#define I32 WASM_I32
#define I64 WASM_I64
#define F32 WASM_F32
#define F64 WASM_F64

