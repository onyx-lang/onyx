

#include "ovm_wasm.h"


WASM_DECLARE_VEC_IMPL(extern, *)

wasm_externkind_t wasm_extern_kind(const wasm_extern_t* ext) {
    return ext->type->kind;
}

wasm_externtype_t* wasm_extern_type(const wasm_extern_t* ext) {
    return (wasm_externtype_t *) ext->type;
}

wasm_extern_t* wasm_func_as_extern(wasm_func_t* ext)     { return (wasm_extern_t *) ext; }
wasm_extern_t* wasm_global_as_extern(wasm_global_t* ext) { return (wasm_extern_t *) ext; }
wasm_extern_t* wasm_table_as_extern(wasm_table_t* ext)   { return (wasm_extern_t *) ext; }
wasm_extern_t* wasm_memory_as_extern(wasm_memory_t* ext) { return (wasm_extern_t *) ext; }

wasm_func_t* wasm_extern_as_func(wasm_extern_t* ext)     { return (ext && ext->type->kind == WASM_EXTERN_FUNC) ? (wasm_func_t *) ext : NULL; }
wasm_global_t* wasm_extern_as_global(wasm_extern_t* ext) { return (ext && ext->type->kind == WASM_EXTERN_GLOBAL) ? (wasm_global_t *) ext : NULL; }
wasm_table_t* wasm_extern_as_table(wasm_extern_t* ext)   { return (ext && ext->type->kind == WASM_EXTERN_TABLE) ? (wasm_table_t *) ext : NULL; }
wasm_memory_t* wasm_extern_as_memory(wasm_extern_t* ext) { return (ext && ext->type->kind == WASM_EXTERN_MEMORY) ? (wasm_memory_t *) ext : NULL; }

const wasm_extern_t* wasm_func_as_extern_const(const wasm_func_t* ext)     { return (const wasm_extern_t *) ext; }
const wasm_extern_t* wasm_global_as_extern_const(const wasm_global_t* ext) { return (const wasm_extern_t *) ext; }
const wasm_extern_t* wasm_table_as_extern_const(const wasm_table_t* ext)   { return (const wasm_extern_t *) ext; }
const wasm_extern_t* wasm_memory_as_extern_const(const wasm_memory_t* ext) { return (const wasm_extern_t *) ext; }

const wasm_func_t* wasm_extern_as_func_const(const wasm_extern_t* ext)     { return ext->type->kind == WASM_EXTERN_FUNC ? (const wasm_func_t *) ext : NULL; }
const wasm_global_t* wasm_extern_as_global_const(const wasm_extern_t* ext) { return ext->type->kind == WASM_EXTERN_GLOBAL ? (const wasm_global_t *) ext : NULL; }
const wasm_table_t* wasm_extern_as_table_const(const wasm_extern_t* ext)   { return ext->type->kind == WASM_EXTERN_TABLE ? (const wasm_table_t *) ext : NULL; }
const wasm_memory_t* wasm_extern_as_memory_const(const wasm_extern_t* ext) { return ext->type->kind == WASM_EXTERN_MEMORY ? (const wasm_memory_t *) ext : NULL; }

