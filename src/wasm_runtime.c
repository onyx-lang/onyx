#include "bh.h"
#include "utils.h"
#include "astnodes.h"
#include "wasm.h"
#include "wasmer.h"

#ifdef _BH_LINUX
    #include <pthread.h>
    #include <signal.h>
    #include <sys/wait.h>
#endif

#ifndef WASMER_VERSION
    #error "Currently, building the Onyx compiler with built-in execution support requires the Wasmer library to be compiled and linked."
#endif

static wasm_config_t*    wasm_config;
static wasi_config_t*    wasi_config;
static wasi_env_t*       wasi_env;
static wasm_engine_t*    wasm_engine;
static wasm_store_t*     wasm_store;
static wasm_extern_vec_t wasm_imports;
static wasm_module_t*    wasm_module;
static wasm_memory_t*    wasm_memory;

b32 wasm_name_equals(const wasm_name_t* name1, const wasm_name_t* name2) {
    if (name1->size != name2->size) return 0;
    return !strncmp(name1->data, name2->data, name1->size);
}

b32 wasm_name_equals_string(const wasm_name_t* name1, const char* name2) {
    u32 name2_size = strlen(name2);
    if (name1->size != name2_size) return 0;
    return !strncmp(name1->data, name2, name1->size);
}

wasm_extern_t* wasm_extern_lookup_by_name(wasm_module_t* module, wasm_instance_t* instance, const char* name) {
    i32 name_len = strlen(name);

    i32 idx = -1;
    wasm_exporttype_vec_t export_types;
    wasm_module_exports(module, &export_types);
    fori (i, 0, (i64) export_types.size) {
        wasm_exporttype_t* export_type = export_types.data[i];
        const wasm_name_t* export_name = wasm_exporttype_name(export_type);

        if (!strncmp(export_name->data, name, name_len)) {
            idx = i;
            break;
        }
    }

    if (idx == -1) return NULL;

    wasm_extern_vec_t exports;
    wasm_instance_exports(instance, &exports);

    return exports.data[idx];
}


typedef struct OnyxThread {
    i32 id;
    i32 tls_base;
    i32 funcidx;
    i32 dataptr;
    wasm_instance_t* instance;

    #ifdef _BH_LINUX
        pthread_t thread;
    #endif

    #ifdef _BH_WINDOWS
        HANDLE thread_handle;
        i32    thread_id;
    #endif
} OnyxThread;

static bh_arr(OnyxThread) threads = NULL;

#ifdef _BH_LINUX
static void *onyx_run_thread(void *data) {
#endif
#ifdef _BH_WINDOWS
static i32 onyx_run_thread(void *data) {
#endif
    OnyxThread *thread = (OnyxThread *) data;

    wasm_trap_t* traps = NULL;
    thread->instance = wasm_instance_new(wasm_store, wasm_module, &wasm_imports, &traps);

    wasm_extern_t* start_extern = wasm_extern_lookup_by_name(wasm_module, thread->instance, "_thread_start");
    wasm_func_t*   start_func   = wasm_extern_as_func(start_extern);

    wasm_extern_t* exit_extern = wasm_extern_lookup_by_name(wasm_module, thread->instance, "_thread_exit");
    wasm_func_t*   exit_func   = wasm_extern_as_func(exit_extern);

    wasm_trap_t* trap=NULL;

    { // Call the _thread_start procedure
        wasm_val_t args[]    = { WASM_I32_VAL(thread->id), WASM_I32_VAL(thread->tls_base), WASM_I32_VAL (thread->funcidx), WASM_I32_VAL(thread->dataptr) };
        wasm_val_vec_t results;
        wasm_val_vec_t args_array = WASM_ARRAY_VEC(args);

        trap = wasm_func_call(start_func, &args_array, &results);
        if (trap != NULL) {
            wasm_message_t msg;
            wasm_trap_message(trap, &msg);
            bh_printf("TRAP: %b\n", msg.data, msg.size);

            wasm_frame_t *origin = wasm_trap_origin(trap);
            bh_printf("HERE: func[%d] at %p.\n", wasm_frame_func_index(origin), wasm_frame_module_offset(origin));
        }
    }

    { // Call the _thread_exit procedure
        wasm_val_t args[]    = { WASM_I32_VAL(thread->id) };
        wasm_val_vec_t results;
        wasm_val_vec_t args_array = WASM_ARRAY_VEC(args);

        trap = wasm_func_call(exit_func, &args_array, &results);
    }

    return 0;
}

static wasm_trap_t* onyx_spawn_thread_impl(const wasm_val_vec_t* params, wasm_val_vec_t* results) {
    if (threads == NULL) bh_arr_new(global_heap_allocator, threads, 128);
    bh_arr_insert_end(threads, 1);
    OnyxThread *thread = &bh_arr_last(threads);

    thread->id       = params->data[0].of.i32;
    thread->tls_base = params->data[1].of.i32;
    thread->funcidx  = params->data[2].of.i32;
    thread->dataptr  = params->data[3].of.i32;

    #ifdef _BH_LINUX
        pthread_create(&thread->thread, NULL, onyx_run_thread, thread);
    #endif

    #ifdef _BH_WINDOWS
        thread->thread_handle = CreateThread(NULL, 0, onyx_run_thread, thread, 0, &thread->thread_id);
    #endif

    results->data[0] = WASM_I32_VAL(1);
    return NULL;
}

static wasm_trap_t* onyx_kill_thread_impl(const wasm_val_vec_t* params, wasm_val_vec_t* results) {
    i32 thread_id = params->data[0].of.i32;

    i32 i = 0;
    bh_arr_each(OnyxThread, thread, threads) {
        if (thread->id == thread_id) {
            #ifdef _BH_LINUX
            pthread_kill(thread->thread, SIGKILL);
            #endif

            #ifdef _BH_WINDOWS
            TerminateThread(thread->thread_handle, 0);
            #endif

            bh_arr_deleten(threads, i, 1);
            results->data[0] = WASM_I32_VAL(1);
            return NULL;
        }

        i++;
    }

    results->data[0] = WASM_I32_VAL(0);
    return NULL;
}

static wasm_trap_t* onyx_spawn_process_impl(const wasm_val_vec_t* params, wasm_val_vec_t* results) {
    char* process_str = (char *) wasm_memory_data(wasm_memory) + params->data[0].of.i32;
    i32   process_len = params->data[1].of.i32;

    char process_path[1024];
    process_len = bh_min(1023, process_len);
    memcpy(process_path, process_str, process_len);
    process_path[process_len] = '\0';

    #ifdef _BH_LINUX
        if (fork() == 0) {
            execv(process_path, NULL);
        }

        i32 status;
        wait(&status);
        results->data[0] = WASM_I32_VAL(WEXITSTATUS(status));
    #endif

    return NULL;
}

void onyx_run_wasm(bh_buffer wasm_bytes) {
    wasm_instance_t* instance = NULL;
    wasmer_features_t* features = NULL;

    wasm_config = wasm_config_new();
    if (!wasm_config) goto error_handling;

    // Prefer the LLVM compile because it is faster. This should be configurable from the command line and/or a top-level directive.
    if (wasmer_is_compiler_available(LLVM)) {
        wasm_config_set_compiler(wasm_config, LLVM);
    }

    features = wasmer_features_new();
    wasmer_features_simd(features, 1);
    wasmer_features_threads(features, 1);
    wasmer_features_bulk_memory(features, 1);
    wasm_config_set_features(wasm_config, features);

    wasi_config = wasi_config_new("onyx");
    if (context.options->passthrough_argument_count > 0) {
        fori (i, 0, context.options->passthrough_argument_count) {
            wasi_config_arg(wasi_config, context.options->passthrough_argument_data[i]);
        }
    }

    wasi_config_preopen_dir(wasi_config, "./");

    wasi_env  = wasi_env_new(wasi_config);
    if (!wasi_env) goto error_handling;

    wasm_engine = wasm_engine_new_with_config(wasm_config);
    if (!wasm_engine) goto error_handling;

    wasm_store  = wasm_store_new(wasm_engine);
    if (!wasm_store) goto error_handling;

    wasm_byte_vec_t wasm_data;
    wasm_data.size = wasm_bytes.length;
    wasm_data.data = wasm_bytes.data;

    wasm_module = wasm_module_new(wasm_store, &wasm_data);
    if (!wasm_module) goto error_handling;

    wasmer_named_extern_vec_t wasi_imports;
    wasi_get_unordered_imports(wasm_store, wasm_module, wasi_env, &wasi_imports);

    wasm_importtype_vec_t module_imports;    // @Free
    wasm_module_imports(wasm_module, &module_imports);

    wasm_imports = (wasm_extern_vec_t) WASM_EMPTY_VEC;
    wasm_extern_vec_new_uninitialized(&wasm_imports, module_imports.size); // @Free

    fori (i, 0, (i32) module_imports.size) {
        const wasm_name_t* module_name = wasm_importtype_module(module_imports.data[i]);
        const wasm_name_t* import_name = wasm_importtype_name(module_imports.data[i]);

        wasm_extern_t* import = NULL;

        // First try WASI
        fori (j, 0, (i32) wasi_imports.size) {
            const wasm_name_t* wasi_module_name = wasmer_named_extern_module(wasi_imports.data[j]);
            const wasm_name_t* wasi_import_name = wasmer_named_extern_name(wasi_imports.data[j]);
            if (wasm_name_equals(module_name, wasi_module_name) && wasm_name_equals(import_name, wasi_import_name)) {
                import = (wasm_extern_t *) wasmer_named_extern_unwrap(wasi_imports.data[j]);
                goto import_found;
            }
        }

        if (wasm_name_equals_string(module_name, "onyx")) {
            if (wasm_name_equals_string(import_name, "memory")) {
                if (wasm_memory == NULL) {
                    wasm_limits_t limits = { 1024, 65536 };
                    wasm_memorytype_t* memory_type = wasm_memorytype_new(&limits);
                    wasm_memory = wasm_memory_new(wasm_store, memory_type);
                }

                import = wasm_memory_as_extern(wasm_memory);
                goto import_found;
            }
        }

        if (wasm_name_equals_string(module_name, "env")) {
            if (wasm_name_equals_string(import_name, "spawn_thread")) {
                wasm_functype_t* func_type = wasm_functype_new_4_1(
                    wasm_valtype_new_i32(), wasm_valtype_new_i32(), wasm_valtype_new_i32(), wasm_valtype_new_i32(),
                    wasm_valtype_new_i32());

                wasm_func_t* wasm_func = wasm_func_new(wasm_store, func_type, onyx_spawn_thread_impl);
                import = wasm_func_as_extern(wasm_func);
                goto import_found;
            }

            if (wasm_name_equals_string(import_name, "kill_thread")) {
                wasm_functype_t* func_type = wasm_functype_new_1_1(wasm_valtype_new_i32(), wasm_valtype_new_i32());

                wasm_func_t* wasm_func = wasm_func_new(wasm_store, func_type, onyx_kill_thread_impl);
                import = wasm_func_as_extern(wasm_func);
                goto import_found;
            }

            if (wasm_name_equals_string(import_name, "spawn_process")) {
                wasm_functype_t* func_type = wasm_functype_new_2_1(wasm_valtype_new_i32(), wasm_valtype_new_i32(), wasm_valtype_new_i32());

                wasm_func_t* wasm_func = wasm_func_new(wasm_store, func_type, onyx_spawn_process_impl);
                import = wasm_func_as_extern(wasm_func);
                goto import_found;
            }
        }

        goto bad_import;

    import_found:
        wasm_imports.data[i] = import;
        continue;


    bad_import:
        bh_printf("Couldn't find import %b.%b.\n", module_name->data, module_name->size, import_name->data, import_name->size);
        return;
    }

    wasm_trap_t* traps = NULL;

    instance = wasm_instance_new(wasm_store, wasm_module, &wasm_imports, &traps);
    if (!instance) goto error_handling;

    wasm_extern_t* start_extern = wasm_extern_lookup_by_name(wasm_module, instance, "_start");
    wasm_func_t*   start_func   = wasm_extern_as_func(start_extern);

    wasm_val_vec_t args;
    wasm_val_vec_t results;
    wasm_val_vec_new_uninitialized(&args, 0);

    wasm_func_call(start_func, &args, &results);

    goto cleanup;

error_handling:
    bh_printf("An error occured trying to run the WASM module...\n");
    i32 len = wasmer_last_error_length();
    char *buf = alloca(len + 1);
    wasmer_last_error_message(buf, len);
    bh_printf("%b\n", buf, len);

cleanup:
    if (instance)    wasm_instance_delete(instance);
    if (wasm_module) wasm_module_delete(wasm_module);
    if (wasm_store)  wasm_store_delete(wasm_store);
    if (wasm_engine) wasm_engine_delete(wasm_engine);
    return;
}
