#include "bh.h"
#include "utils.h"
#include "astnodes.h"
#include "wasm.h"
#include "wasmer.h"
#include "onyx_library.h"

#ifdef _BH_LINUX
    #include <pthread.h>
    #include <signal.h>
    #include <sys/wait.h>
    #include <dlfcn.h>
#endif

#ifndef WASMER_VERSION
    #error "Currently, building the Onyx compiler with built-in execution support requires the Wasmer library to be compiled and linked."
#endif

static wasm_config_t*    wasm_config;
static wasm_engine_t*    wasm_engine;
static wasm_store_t*     wasm_store;
static wasm_extern_vec_t wasm_imports;
wasm_instance_t*  wasm_instance;
wasm_module_t*    wasm_module;
wasm_memory_t*    wasm_memory;

OnyxRuntime wasm_runtime;

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



typedef void *(*LibraryLinker)(OnyxRuntime *runtime);
static bh_arr(WasmFuncDefinition **) linkable_functions = NULL;
static bh_arr(char *) library_paths = NULL;

static void onyx_load_library(char *name) {
    #ifdef _BH_LINUX
        #define DIR_SEPARATOR '/'
    #endif
    #ifdef _BH_WINDOWS
        #define DIR_SEPARATOR '\\'
    #endif

    char *library = name;
    fori (i, 0, (i32) strlen(name)) {
        if (name[i] == DIR_SEPARATOR) library = &name[i + 1];
    }

    char *library_load_name_tmp = bh_bprintf("onyx_library_%s", library);
    char *library_load_name = alloca(strlen(library_load_name_tmp) + 1);
    strcpy(library_load_name, library_load_name_tmp);
    LibraryLinker library_load;

    #ifdef _BH_LINUX
    char *library_name = bh_lookup_file(name, ".", ".so", 1, (const char **) library_paths, 1);
    void* handle = dlopen(library_name, RTLD_LAZY);
    if (handle == NULL) {
        printf("ERROR LOADING LIBRARY %s: %s\n", name, dlerror());
        return;
    }

    library_load = (LibraryLinker) dlsym(handle, library_load_name);
    if (library_load == NULL) {
        printf("ERROR RESOLVING '%s': %s\n", library_load_name, dlerror());
        return;
    }
    #endif

    #ifdef _BH_WINDOWS
    char *library_name = bh_lookup_file(name, ".", ".dll", 1, (const char **) library_paths, 1);
    HMODULE handle = LoadLibraryA(library_name);
    if (handle == NULL) {
        printf("ERROR LOADING LIBRARY %s: %d\n", name, GetLastError());
        return;
    }

    library_load = (LibraryLinker) GetProcAddress(handle, library_load_name);
    if (library_load == NULL) {
        printf("ERROR RESOLVING '%s': %d\n", library_load_name, GetLastError());
        return;
    }
    #endif

    WasmFuncDefinition** funcs = library_load(runtime);
    bh_arr_push(linkable_functions, funcs);
}

static void onyx_lookup_and_load_custom_libraries(bh_buffer wasm_bytes) {
    i32 cursor = 8; // skip the magic number and version
    while (cursor < wasm_bytes.length) {
        u64 section_number = uleb128_to_uint(wasm_bytes.data, &cursor);
        u64 section_size   = uleb128_to_uint(wasm_bytes.data, &cursor);

        i32 section_start = cursor;
        if (section_number == 0) {
            u64 name_len = uleb128_to_uint(wasm_bytes.data, &cursor);
            if (!strncmp(wasm_bytes.data + cursor, "_onyx_libs", name_len)) {
                cursor += name_len;
                u64 lib_count = uleb128_to_uint(wasm_bytes.data, &cursor);

                fori (i, 0, (i64) lib_count) {
                    u64 lib_path_length = uleb128_to_uint(wasm_bytes.data, &cursor);
                    lib_path_length = bh_min(lib_path_length, 512);

                    char *lib_path = malloc(lib_path_length);
                    strncpy(lib_path, wasm_bytes.data + cursor, lib_path_length);
                    lib_path[lib_path_length] = '\0';
                    cursor += lib_path_length;

                    bh_arr_push(library_paths, lib_path);
                }

                lib_count = uleb128_to_uint(wasm_bytes.data, &cursor);

                fori (i, 0, (i64) lib_count) {
                    u64 lib_name_length = uleb128_to_uint(wasm_bytes.data, &cursor);
                    lib_name_length = bh_min(lib_name_length, 256);

                    char library_name[256];
                    strncpy(library_name, wasm_bytes.data + cursor, lib_name_length);
                    library_name[lib_name_length] = '\0';
                    cursor += lib_name_length;

                    onyx_load_library(library_name);
                }
                break;
            }
        }

        cursor = section_start + section_size;
    }
}

// Returns 1 if successful
b32 onyx_run_wasm(bh_buffer wasm_bytes, int argc, char *argv[]) {
    runtime = &wasm_runtime;

    bh_arr_new(bh_heap_allocator(), linkable_functions, 4);
    onyx_lookup_and_load_custom_libraries(wasm_bytes);

    wasmer_features_t* features = NULL;
    wasm_trap_t* run_trap = NULL;

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

    wasm_engine = wasm_engine_new_with_config(wasm_config);
    if (!wasm_engine) goto error_handling;

    wasm_store  = wasm_store_new(wasm_engine);
    if (!wasm_store) goto error_handling;

    wasm_byte_vec_t wasm_data;
    wasm_data.size = wasm_bytes.length;
    wasm_data.data = wasm_bytes.data;

    wasm_module = wasm_module_new(wasm_store, &wasm_data);
    if (!wasm_module) goto error_handling;

    wasm_importtype_vec_t module_imports;    // @Free
    wasm_module_imports(wasm_module, &module_imports);

    wasm_imports = (wasm_extern_vec_t) WASM_EMPTY_VEC;
    wasm_extern_vec_new_uninitialized(&wasm_imports, module_imports.size); // @Free

    fori (i, 0, (i32) module_imports.size) {
        const wasm_name_t* module_name = wasm_importtype_module(module_imports.data[i]);
        const wasm_name_t* import_name = wasm_importtype_name(module_imports.data[i]);

        wasm_extern_t* import = NULL;

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

        bh_arr_each(WasmFuncDefinition **, library_funcs, linkable_functions) {
            WasmFuncDefinition **pcurrent_function = *library_funcs;
            while (*pcurrent_function != NULL) {
                WasmFuncDefinition *cf = *pcurrent_function;
                if (wasm_name_equals_string(module_name, cf->module_name) && wasm_name_equals_string(import_name, cf->import_name)) {
                    wasm_valtype_vec_t wasm_params;
                    wasm_valtype_vec_new_uninitialized(&wasm_params, cf->params->count);
                    fori (k, 0, cf->params->count) wasm_params.data[k] = wasm_valtype_new(cf->params->types[k]);

                    wasm_valtype_vec_t wasm_results;
                    wasm_valtype_vec_new_uninitialized(&wasm_results, cf->results->count);
                    fori (k, 0, cf->results->count) wasm_results.data[k] = wasm_valtype_new(cf->results->types[k]);

                    wasm_functype_t* wasm_functype = wasm_functype_new(&wasm_params, &wasm_results);

                    wasm_func_t* wasm_func = wasm_func_new(wasm_store, wasm_functype, cf->func);
                    import = wasm_func_as_extern(wasm_func);
                    goto import_found;
                }

                pcurrent_function += 1;
            }
        }

        goto bad_import;

    import_found:
        wasm_imports.data[i] = import;
        continue;


    bad_import:
        bh_printf("Couldn't find import %b.%b.\n", module_name->data, module_name->size, import_name->data, import_name->size);
        return 0;
    }

    wasm_trap_t* traps = NULL;

    wasm_instance = wasm_instance_new(wasm_store, wasm_module, &wasm_imports, &traps);
    if (!wasm_instance) goto error_handling;

    wasm_runtime.wasm_instance = wasm_instance;
    wasm_runtime.wasm_module = wasm_module;
    wasm_runtime.wasm_memory = wasm_memory;
    wasm_runtime.wasm_store = wasm_store;
    wasm_runtime.wasm_imports = wasm_imports;
    
    // See comment in onyx_library.h about us being the linker.
    wasm_runtime.wasm_memory_data = &wasm_memory_data;
    wasm_runtime.wasm_extern_lookup_by_name = &wasm_extern_lookup_by_name;
    wasm_runtime.wasm_extern_as_func = &wasm_extern_as_func;
    wasm_runtime.wasm_func_call = &wasm_func_call;
    wasm_runtime.wasm_instance_new = &wasm_instance_new;

    wasm_extern_t* start_extern = wasm_extern_lookup_by_name(wasm_module, wasm_instance, "_start");
    wasm_func_t*   start_func   = wasm_extern_as_func(start_extern);

    wasm_val_vec_t args;
    wasm_val_vec_t results;
    wasm_val_vec_new_uninitialized(&args, 0);

    run_trap = wasm_func_call(start_func, &args, &results);

#if 0
    if (run_trap != NULL) {
        wasm_message_t msg;
        wasm_trap_message(run_trap, &msg);
        bh_printf("TRAP: %b\n", msg.data, msg.size);

        wasm_frame_t *origin = wasm_trap_origin(run_trap);
        bh_printf("HERE: func[%d] at %p.\n", wasm_frame_func_index(origin), wasm_frame_module_offset(origin));
    }
#endif

    goto cleanup;

error_handling:
    bh_printf("An error occured trying to run the WASM module...\n");
    i32 len = wasmer_last_error_length();
    char *buf = alloca(len + 1);
    wasmer_last_error_message(buf, len);
    bh_printf("%b\n", buf, len);

cleanup:
    if (wasm_instance) wasm_instance_delete(wasm_instance);
    if (wasm_module) wasm_module_delete(wasm_module);
    if (wasm_store)  wasm_store_delete(wasm_store);
    if (wasm_engine) wasm_engine_delete(wasm_engine);
    return run_trap == NULL;
}
