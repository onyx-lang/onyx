
#include "ovm_wasm.h"
#include "vm.h"

//
// wasm_byte_t
//

WASM_DECLARE_VEC_IMPL(byte, )

//
// wasm_valtype_t
//

static wasm_valtype_t
    valtype_i32     = { WASM_I32 },
    valtype_i64     = { WASM_I64 },
    valtype_f32     = { WASM_F32 },
    valtype_f64     = { WASM_F64 },
    valtype_anyref  = { WASM_ANYREF },
    valtype_funcref = { WASM_FUNCREF };


wasm_valtype_t *wasm_valtype_new(wasm_valkind_t kind) {
    switch (kind) {
        case WASM_I32:     return &valtype_i32;
        case WASM_I64:     return &valtype_i64;
        case WASM_F32:     return &valtype_f32;
        case WASM_F64:     return &valtype_f64;
        case WASM_ANYREF:  return &valtype_anyref;
        case WASM_FUNCREF: return &valtype_funcref;
        default: assert(0);
    }
}

void wasm_valtype_delete(wasm_valtype_t *type) {}

wasm_valkind_t wasm_valtype_kind(const wasm_valtype_t *type) {
    assert(type);
    return type->kind;
}

WASM_DECLARE_VEC_IMPL(valtype, *)


// wasm_functype_t

wasm_functype_t *wasm_functype_new(wasm_valtype_vec_t *params, wasm_valtype_vec_t *results) {
    wasm_functype_t *functype = malloc(sizeof(*functype));
    functype->type.kind = WASM_EXTERN_FUNC;
    functype->type.func.params = *params;
    functype->type.func.results = *results;

    return functype;
}

void wasm_functype_delete(wasm_functype_t *functype) {
    if (functype) free(functype);
}

const wasm_valtype_vec_t* wasm_functype_params(const wasm_functype_t *functype) {
    return &functype->type.func.params;
}

const wasm_valtype_vec_t* wasm_functype_results(const wasm_functype_t *functype) {
    return &functype->type.func.results;
}

bool wasm_functype_equals(wasm_functype_t *a, wasm_functype_t *b) {
    if (a->type.func.params.size != b->type.func.params.size) return false;
    if (a->type.func.results.size != b->type.func.results.size) return false;

    fori (i, 0, (int) a->type.func.params.size) {
        if (a->type.func.params.data[i]->kind != b->type.func.params.data[i]->kind) return false;
    }

    fori (i, 0, (int) a->type.func.results.size) {
        if (a->type.func.results.data[i]->kind != b->type.func.results.data[i]->kind) return false;
    }

    return true;
}

WASM_DECLARE_VEC_IMPL(functype, *)


// wasm_globaltype_t

wasm_globaltype_t *wasm_globaltype_new(wasm_valtype_t *valtype, wasm_mutability_t mut) {
    wasm_globaltype_t *globaltype = malloc(sizeof(*globaltype));
    globaltype->type.kind = WASM_EXTERN_GLOBAL;
    globaltype->type.global.content = valtype;
    globaltype->type.global.mutability = mut;

    return globaltype;
}

void wasm_globaltype_delete(wasm_globaltype_t *globaltype) {
    if (globaltype) free(globaltype);
}

const wasm_valtype_t* wasm_globaltype_content(const wasm_globaltype_t *globaltype) {
    return globaltype->type.global.content;
}

wasm_mutability_t wasm_globaltype_mutability(const wasm_globaltype_t *globaltype) {
    return globaltype->type.global.mutability;
}

WASM_DECLARE_VEC_IMPL(globaltype, *)


// wasm_tabletype_t

wasm_tabletype_t *wasm_tabletype_new(wasm_valtype_t *valtype, const wasm_limits_t *limits) {
    wasm_tabletype_t *tabletype = malloc(sizeof(*tabletype));
    tabletype->type.kind = WASM_EXTERN_TABLE;
    tabletype->type.table.element = valtype;
    tabletype->type.table.limits = *limits;

    return tabletype;
}

void wasm_tabletype_delete(wasm_tabletype_t *tabletype) {
    if (tabletype) free(tabletype);
}

const wasm_valtype_t* wasm_tabletype_element(const wasm_tabletype_t *tabletype) {
    return tabletype->type.table.element;
}

const wasm_limits_t* wasm_tabletype_limits(const wasm_tabletype_t *tabletype) {
    return &tabletype->type.table.limits;
}

WASM_DECLARE_VEC_IMPL(tabletype, *)

// wasm_memorytype_t

wasm_memorytype_t *wasm_memorytype_new(const wasm_limits_t *limits) {
    wasm_memorytype_t *memorytype = malloc(sizeof(*memorytype));
    memorytype->type.kind = WASM_EXTERN_MEMORY;
    memorytype->type.memory.limits = *limits;

    return memorytype;
}

void wasm_memorytype_delete(wasm_memorytype_t *memorytype) {
    if (memorytype) free(memorytype);
}

const wasm_limits_t* wasm_memorytype_limits(const wasm_memorytype_t *memorytype) {
    return &memorytype->type.memory.limits;
}

WASM_DECLARE_VEC_IMPL(memorytype, *)

// wasm_externtype_t

wasm_externkind_t wasm_externtype_kind(const wasm_externtype_t *externtype) {
    return externtype->kind;
}

WASM_DECLARE_VEC_IMPL(externtype, *)

wasm_externtype_t* wasm_functype_as_externtype(wasm_functype_t* ext)     { return (wasm_externtype_t *) ext; }
wasm_externtype_t* wasm_globaltype_as_externtype(wasm_globaltype_t* ext) { return (wasm_externtype_t *) ext; }
wasm_externtype_t* wasm_tabletype_as_externtype(wasm_tabletype_t* ext)   { return (wasm_externtype_t *) ext; }
wasm_externtype_t* wasm_memorytype_as_externtype(wasm_memorytype_t* ext) { return (wasm_externtype_t *) ext; }

wasm_functype_t* wasm_externtype_as_functype(wasm_externtype_t* ext)     { return ext->kind == WASM_EXTERN_FUNC ? (wasm_functype_t *) ext : NULL; }
wasm_globaltype_t* wasm_externtype_as_globaltype(wasm_externtype_t* ext) { return ext->kind == WASM_EXTERN_GLOBAL ? (wasm_globaltype_t *) ext : NULL; }
wasm_tabletype_t* wasm_externtype_as_tabletype(wasm_externtype_t* ext)   { return ext->kind == WASM_EXTERN_TABLE ? (wasm_tabletype_t *) ext : NULL; }
wasm_memorytype_t* wasm_externtype_as_memorytype(wasm_externtype_t* ext) { return ext->kind == WASM_EXTERN_MEMORY ? (wasm_memorytype_t *) ext : NULL; }

const wasm_externtype_t* wasm_functype_as_externtype_const(const wasm_functype_t* ext)     { return (const wasm_externtype_t *) ext; }
const wasm_externtype_t* wasm_globaltype_as_externtype_const(const wasm_globaltype_t* ext) { return (const wasm_externtype_t *) ext; }
const wasm_externtype_t* wasm_tabletype_as_externtype_const(const wasm_tabletype_t* ext)   { return (const wasm_externtype_t *) ext; }
const wasm_externtype_t* wasm_memorytype_as_externtype_const(const wasm_memorytype_t* ext) { return (const wasm_externtype_t *) ext; }

const wasm_functype_t* wasm_externtype_as_functype_const(const wasm_externtype_t* ext)     { return ext->kind == WASM_EXTERN_FUNC ? (const wasm_functype_t *) ext : NULL; }
const wasm_globaltype_t* wasm_externtype_as_globaltype_const(const wasm_externtype_t* ext) { return ext->kind == WASM_EXTERN_GLOBAL ? (const wasm_globaltype_t *) ext : NULL; }
const wasm_tabletype_t* wasm_externtype_as_tabletype_const(const wasm_externtype_t* ext)   { return ext->kind == WASM_EXTERN_TABLE ? (const wasm_tabletype_t *) ext : NULL; }
const wasm_memorytype_t* wasm_externtype_as_memorytype_const(const wasm_externtype_t* ext) { return ext->kind == WASM_EXTERN_MEMORY ? (const wasm_memorytype_t *) ext : NULL; }



// wasm_importtype_t

wasm_importtype_t *wasm_importtype_new(wasm_name_t *module, wasm_name_t* name, wasm_externtype_t *ext) {
    wasm_importtype_t *importtype = malloc(sizeof(*importtype));
    importtype->module_name = *module;
    importtype->import_name = *name;
    importtype->type = ext;

    return importtype;
}

void wasm_importtype_delete(wasm_importtype_t *importtype) {
    if (importtype) free(importtype);
}

const wasm_name_t* wasm_importtype_module(const wasm_importtype_t* importtype) {
    return &importtype->module_name;
}

const wasm_name_t* wasm_importtype_name(const wasm_importtype_t* importtype) {
    return &importtype->import_name;
}

const wasm_externtype_t* wasm_importtype_type(const wasm_importtype_t* importtype) {
    return importtype->type;
}

WASM_DECLARE_VEC_IMPL(importtype, *)

// wasm_exporttype_t

wasm_exporttype_t *wasm_exporttype_new(wasm_name_t* name, wasm_externtype_t *ext) {
    wasm_exporttype_t *exporttype = malloc(sizeof(*exporttype));
    exporttype->name = *name;
    exporttype->type = ext;

    return exporttype;
}

void wasm_exporttype_delete(wasm_exporttype_t *exporttype) {
    if (exporttype) free(exporttype);
}

const wasm_name_t* wasm_exporttype_name(const wasm_exporttype_t* exporttype) {
    return &exporttype->name;
}

const wasm_externtype_t* wasm_exporttype_type(const wasm_exporttype_t* exporttype) {
    return exporttype->type;
}

WASM_DECLARE_VEC_IMPL(exporttype, *)
