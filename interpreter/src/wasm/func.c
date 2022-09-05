
#include "ovm_wasm.h"
#include "vm.h"

wasm_func_t *wasm_func_new(wasm_store_t *store, const wasm_functype_t *type, wasm_func_callback_t callback) {
    wasm_func_t *func = bh_alloc(store->engine->store->arena_allocator, sizeof(*func));
    func->inner.type = wasm_functype_as_externtype_const(type);
    func->inner.func.type = type;
    func->inner.func.env_present = false;
    func->inner.func.env = NULL;
    func->inner.func.func_ptr = (void (*)()) callback;
    func->inner.func.finalizer = NULL;

    return func;
}

wasm_func_t *wasm_func_new_with_env(wasm_store_t *store, const wasm_functype_t *type,
    wasm_func_callback_with_env_t callback, void *env, void (*finalizer)(void *)) {
    
    wasm_func_t *func = bh_alloc(store->engine->store->arena_allocator, sizeof(*func));
    func->inner.type = wasm_functype_as_externtype_const(type);
    func->inner.func.type = type;
    func->inner.func.env_present = true;
    func->inner.func.env = env;
    func->inner.func.func_ptr = (void (*)()) callback;
    func->inner.func.finalizer = finalizer;

    return func;
}

wasm_functype_t *wasm_func_type(const wasm_func_t *func) {
    return (wasm_functype_t *) func->inner.func.type;
}

size_t wasm_func_param_arity(const wasm_func_t *func) {
    // Wow this is gross...
    return func->inner.func.type->type.func.params.size;
}

size_t wasm_func_result_arity(const wasm_func_t *func) {
    // Wow this is gross...
    return func->inner.func.type->type.func.results.size;
}

wasm_trap_t *wasm_func_call(const wasm_func_t *func, const wasm_val_vec_t *args, wasm_val_vec_t *results) {
    if (func->inner.func.env_present) {
        wasm_func_callback_with_env_t cb = (wasm_func_callback_with_env_t) func->inner.func.func_ptr;
        wasm_trap_t *trap = cb(func->inner.func.env, args, results);

        if (func->inner.func.finalizer) {
            func->inner.func.finalizer(func->inner.func.env);
        }

        return trap;

    } else {
        wasm_func_callback_t cb = (wasm_func_callback_t) func->inner.func.func_ptr;
        wasm_trap_t *trap = cb(args, results);
        return trap;
    }
}
