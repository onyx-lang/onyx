#include "bh.h"
#include "astnodes.h"
#include "wasm.h"
#include "wasmer.h"

#ifndef WASMER_VERSION
    #error "Currently, building the Onyx compiler with built-in execution support requires the Wasmer library to be compiled and linked."
#endif

void onyx_run_wasm(bh_buffer wasm_bytes) {
    wasm_config_t*   config = NULL;
    wasi_config_t*   wasi_config = NULL;
    wasi_env_t*      wasi_env = NULL;
    wasm_engine_t*   engine = NULL;
    wasm_store_t*    store = NULL;
    wasm_module_t*   module = NULL;
    wasm_instance_t* instance = NULL;

    config = wasm_config_new();
    if (!config) goto error_handling;

    // Prefer the LLVM compile because it is faster. This should be configurable from the command line and/or a top-level directive.
    if (wasmer_is_compiler_available(LLVM)) {
        wasm_config_set_compiler(config, LLVM);
    }

    wasi_config = wasi_config_new("onyx");
    if (context.options->passthrough_argument_count > 0) {
        fori (i, 0, context.options->passthrough_argument_count) {
            wasi_config_arg(wasi_config, context.options->passthrough_argument_data[i]);
        }
    }

    wasi_config_preopen_dir(wasi_config, "./");

    wasi_env  = wasi_env_new(wasi_config);
    if (!wasi_env) goto error_handling;

    engine = wasm_engine_new_with_config(config);
    if (!engine) goto error_handling;

    store  = wasm_store_new(engine);
    if (!store) goto error_handling;

    wasm_byte_vec_t wasm_data;
    wasm_data.size = wasm_bytes.length;
    wasm_data.data = wasm_bytes.data;

    module = wasm_module_new(store, &wasm_data);
    if (!module) goto error_handling;

    wasm_extern_vec_t imports = WASM_EMPTY_VEC;
    wasi_get_imports(store, module, wasi_env, &imports);

    wasm_trap_t* traps = NULL;

    instance = wasm_instance_new(store, module, &imports, &traps);
    if (!instance) goto error_handling;

    // Find the start function
    i32 start_function_idx = -1;
    wasm_exporttype_vec_t export_types;
    wasm_module_exports(module, &export_types);
    fori (i, 0, (i64) export_types.size) {
        wasm_exporttype_t* export_type = export_types.data[i];
        const wasm_name_t* export_name = wasm_exporttype_name(export_type);
        
        if (!strcmp(export_name->data, "_start")) {
            start_function_idx = i;
            break;
        }
    }

    wasm_extern_vec_t exports;
    wasm_instance_exports(instance, &exports);

    wasm_extern_t* start_extern = exports.data[start_function_idx];
    wasm_func_t*   start_func   = wasm_extern_as_func(start_extern);

    wasm_val_vec_t args;
    wasm_val_vec_t results;
    wasm_val_vec_new_uninitialized(&args, 0);

    wasm_func_call(start_func, &args, &results);

    goto cleanup;

error_handling:
    bh_printf("An error occured trying to run the WASM module...\n");

cleanup:
    if (instance)    wasm_instance_delete(instance);
    if (module)      wasm_module_delete(module);
    if (store)       wasm_store_delete(store);
    if (engine)      wasm_engine_delete(engine);
    if (wasi_env)    wasi_env_delete(wasi_env);
    return;
}