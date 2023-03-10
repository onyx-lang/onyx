#include "bh.h"
#include "utils.h"
#include "astnodes.h"
#include "wasm.h"
#include "onyx_library.h"
#include "dyncall.h"

#ifndef USE_OVM_DEBUGGER
    #include "wasmer.h"
#endif

#ifdef _BH_LINUX
    #include <pthread.h>
    #include <signal.h>
    #include <sys/wait.h>
    #include <dlfcn.h>
#endif

static wasm_config_t*    wasm_config;
static wasm_engine_t*    wasm_engine;
static wasm_store_t*     wasm_store;
static wasm_extern_vec_t wasm_imports;
static bh_buffer         wasm_raw_bytes;
wasm_instance_t*  wasm_instance;
wasm_module_t*    wasm_module;
wasm_memory_t*    wasm_memory;

// @Temporary
static DCCallVM *dcCallVM;

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

b32 wasm_name_starts_with(const wasm_name_t* name, const char *prefix) {
    u32 prefix_size = strlen(prefix);
    if (name->size < prefix_size) return 0;
    return !strncmp(name->data, prefix, prefix_size);
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

typedef struct LinkLibraryContext {
    bh_buffer wasm_bytes;
    bh_arr(char *) library_paths;
} LinkLibraryContext;


static void *locate_symbol_in_dynamic_library(LinkLibraryContext *ctx, char *libname, char *sym) {
    #ifdef _BH_LINUX
    char *library_name = bh_lookup_file(libname, ".", ".so", 1, (const char **) ctx->library_paths, 1);
    void* handle = dlopen(library_name, RTLD_LAZY);
    if (handle == NULL) {
        return NULL;
    }

    return dlsym(handle, sym);
    #endif

    #ifdef _BH_WINDOWS
    char *library_name = bh_lookup_file(libname, ".", ".dll", 1, (const char **) ctx->library_paths, 1);
    HMODULE handle = LoadLibraryA(library_name);
    if (handle == NULL) {
        return NULL;
    }

    return GetProcAddress(handle, sym);
    #endif

    return NULL;
}

typedef void *(*LinkLibraryer)(OnyxRuntime *runtime);

static WasmFuncDefinition** onyx_load_library(LinkLibraryContext *ctx, char *name) {
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

    LinkLibraryer library_load = locate_symbol_in_dynamic_library(ctx, name, library_load_name);
    if (library_load == NULL) {
        printf("ERROR RESOLVING '%s': %s\n", library_load_name, dlerror());
        return NULL;
    }

    return library_load(runtime);
}

static void lookup_and_load_custom_libraries(LinkLibraryContext *ctx, bh_arr(WasmFuncDefinition **)* p_out) {
    bh_arr(WasmFuncDefinition **) out = *p_out;

    bh_buffer wasm_bytes = ctx->wasm_bytes;

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
                    bh_path_convert_separators(lib_path);
                    cursor += lib_path_length;

                    bh_arr_push(ctx->library_paths, lib_path);
                }

                lib_count = uleb128_to_uint(wasm_bytes.data, &cursor);

                fori (i, 0, (i64) lib_count) {
                    u64 lib_name_length = uleb128_to_uint(wasm_bytes.data, &cursor);
                    lib_name_length = bh_min(lib_name_length, 256);

                    char library_name[256];
                    strncpy(library_name, wasm_bytes.data + cursor, lib_name_length);
                    library_name[lib_name_length] = '\0';
                    cursor += lib_name_length;

                    WasmFuncDefinition** lib = onyx_load_library(ctx, library_name);
                    if (lib) {
                        bh_arr_push(out, lib);
                    }
                }
                break;
            }
        }

        cursor = section_start + section_size;
    }

    *p_out = out;
}


typedef struct DynCallContext {
    void (*func)();
    char types[64];
} DynCallContext;

static wasm_trap_t *__wasm_dyncall(void *env, const wasm_val_vec_t *args, wasm_val_vec_t *res) {
    DynCallContext *ctx = env;
    dcReset(dcCallVM);

    int arg_idx = 0;
    for (int i = 1; i < 64; i++) {
        switch (ctx->types[i]) {
            case '\0': goto arguments_placed;
            case 'i':  dcArgInt(dcCallVM, args->data[arg_idx++].of.i32);               break;
            case 'l':  dcArgLongLong(dcCallVM, args->data[arg_idx++].of.i64);          break;
            case 'f':  dcArgFloat(dcCallVM, args->data[arg_idx++].of.f32);             break;
            case 'd':  dcArgFloat(dcCallVM, args->data[arg_idx++].of.f64);             break;
            case 'p':  dcArgPointer(dcCallVM, ONYX_PTR(args->data[arg_idx++].of.i32)); break;
            case 'v':                                                                  break;
            case 's':
                dcArgPointer(dcCallVM, ONYX_PTR(args->data[arg_idx++].of.i32));
                dcArgInt(dcCallVM, args->data[arg_idx++].of.i32);
                break;
            default: assert(("bad dynamic call type", 0));
        }
    }

  arguments_placed:
    switch (ctx->types[0]) {
        case 'i': res->data[0] = WASM_I32_VAL(dcCallInt(dcCallVM, ctx->func));       break;
        case 'l': res->data[0] = WASM_I64_VAL(dcCallLongLong(dcCallVM, ctx->func));  break;
        case 'f': res->data[0] = WASM_F32_VAL(dcCallFloat(dcCallVM, ctx->func));     break;
        case 'd': res->data[0] = WASM_F64_VAL(dcCallDouble(dcCallVM, ctx->func));    break;
        case 'p': res->data[0] = WASM_I64_VAL((u64) dcCallPointer(dcCallVM, ctx->func));   break;
        case 'v': dcCallVoid(dcCallVM, ctx->func);
    }

    return NULL;
}

static wasm_func_t *link_and_prepare_dyncall_function(
        wasm_externtype_t* type,
        LinkLibraryContext *lib_ctx,
        wasm_name_t library_name,
        wasm_name_t function_name)
{
    //
    // When using any dynamic functions, a DCCallVM has to be created.
    if (!dcCallVM) {
        dcCallVM = dcNewCallVM(4096);
        dcMode(dcCallVM, DC_CALL_C_DEFAULT);
    }

    char lib_name[256];
    strncpy(lib_name, library_name.data, bh_min(256, library_name.size));

    u32 index;
    char func_name[256];
    for (index = 0; index < function_name.size; index++) {
        if (function_name.data[index] == ':') break;
        func_name[index] = function_name.data[index];
    }
    func_name[index++] = '\0';

    char dynamic_types[64];
    for (; index < function_name.size; index++) {
        dynamic_types[index] = function_name.data[index];
    }
    dynamic_types[index] = '\0';

    void (*func)() = locate_symbol_in_dynamic_library(lib_ctx, lib_name, func_name);
    if (!func) return NULL;

    wasm_functype_t *functype = wasm_externtype_as_functype(type);

    DynCallContext* dcc = bh_alloc_item(bh_heap_allocator(), DynCallContext);
    dcc->func = func;
    memcpy(&dcc->types, dynamic_types, 64);

    wasm_func_t *wasm_func = wasm_func_new_with_env(wasm_store, functype, &__wasm_dyncall, dcc, NULL);
    return wasm_func;
}

static void onyx_print_trap(wasm_trap_t* trap) {
    wasm_message_t msg;
    wasm_trap_message(trap, &msg);
    bh_printf("TRAP: %b\n", msg.data, msg.size);

    i32 func_name_section = 0;

    i32 cursor = 8; // skip the magic number and version
    while (cursor < wasm_raw_bytes.length) {
        u64 section_number = uleb128_to_uint(wasm_raw_bytes.data, &cursor);
        u64 section_size   = uleb128_to_uint(wasm_raw_bytes.data, &cursor);

        i32 section_start = cursor;
        if (section_number == 0) {
            u64 name_len = uleb128_to_uint(wasm_raw_bytes.data, &cursor);
            if (!strncmp(wasm_raw_bytes.data + cursor, "_onyx_func_offsets", name_len)) {
                cursor += name_len;
                func_name_section = cursor;
                break;
            }
        }

        cursor = section_start + section_size;
    }

    if (func_name_section == 0) return;

    bh_printf("TRACE:\n");
    wasm_frame_vec_t frames;
    wasm_trap_trace(trap, &frames);
    fori (i, 0, (i32) frames.size) {
        i32 func_idx   = wasm_frame_func_index(frames.data[i]);
        i32 mod_offset = wasm_frame_module_offset(frames.data[i]);

        i32 cursor = func_name_section + 4 * func_idx;
        i32 func_offset = *(i32 *) (wasm_raw_bytes.data + cursor);
        char* func_name = wasm_raw_bytes.data + func_name_section + func_offset;

        bh_printf("    func[%d]:%p at %s\n", func_idx, mod_offset, func_name);
    }
}

static void cleanup_wasm_objects() {
    if (wasm_instance) wasm_instance_delete(wasm_instance);
    if (wasm_module) wasm_module_delete(wasm_module);
    if (wasm_store)  wasm_store_delete(wasm_store);
    if (wasm_engine) wasm_engine_delete(wasm_engine);
}

//
// This could be cleaned up a bit, as this function directly modifies various global variables.
// Those being wasm_memory and wasm_imports.
static b32 link_wasm_imports(
        bh_arr(WasmFuncDefinition **) linkable_functions,
        LinkLibraryContext *lib_ctx,
        wasm_module_t *wasm_module)
{
    wasm_importtype_vec_t module_imports;
    wasm_module_imports(wasm_module, &module_imports);

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

        if (wasm_name_starts_with(module_name, "dyncall:")) {
            wasm_name_t library_name = *module_name;
            library_name.data += 8;
            library_name.size -= 8;

            wasm_name_t function_name = *import_name;

            wasm_func_t *wasm_func = link_and_prepare_dyncall_function(
                    (wasm_externtype_t *) wasm_importtype_type(module_imports.data[i]),
                    lib_ctx,
                    library_name,
                    function_name
            );

            if (!wasm_func) {
                goto bad_import;
            }

            import = wasm_func_as_extern(wasm_func);
            goto import_found;
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

    return 1;
}

void onyx_run_initialize(b32 debug_enabled) {
    wasm_config = wasm_config_new();
    if (!wasm_config) {
        cleanup_wasm_objects();
        return;
    }

#ifdef USE_OVM_DEBUGGER
    void wasm_config_enable_debug(wasm_config_t *config, int value);
    wasm_config_enable_debug(wasm_config, debug_enabled);
#endif

#ifndef USE_OVM_DEBUGGER
    if (debug_enabled) {
        printf("Warning: --debug does nothing if libovmwasm.so is not being used!\n");
    }

    // Prefer the LLVM compile because it is faster. This should be configurable from the command line and/or a top-level directive.
    if (wasmer_is_compiler_available(LLVM)) {
        wasm_config_set_compiler(wasm_config, LLVM);
    }

    wasmer_features_t* features = wasmer_features_new();
    wasmer_features_simd(features, 1);
    wasmer_features_threads(features, 1);
    wasmer_features_bulk_memory(features, 1);
    wasm_config_set_features(wasm_config, features);
#endif

    wasm_engine = wasm_engine_new_with_config(wasm_config);
    if (!wasm_engine) {
        cleanup_wasm_objects();
        return;
    }

    wasm_store  = wasm_store_new(wasm_engine);
    if (!wasm_store) {
        cleanup_wasm_objects();
        return;
    }

    // See comment in onyx_library.h about us being the linker.
    wasm_runtime.wasm_memory_data = &wasm_memory_data;
    wasm_runtime.wasm_extern_lookup_by_name = &wasm_extern_lookup_by_name;
    wasm_runtime.wasm_extern_as_func = &wasm_extern_as_func;
    wasm_runtime.wasm_func_call = &wasm_func_call;
    wasm_runtime.wasm_instance_new = &wasm_instance_new;
    wasm_runtime.wasm_store_new = &wasm_store_new;
    wasm_runtime.wasm_store_delete = &wasm_store_delete;
    wasm_runtime.onyx_print_trap = &onyx_print_trap;
}

b32 onyx_run_wasm(bh_buffer wasm_bytes, int argc, char *argv[]) {
    runtime = &wasm_runtime;

    if (strncmp(wasm_bytes.data, "ONYX", 4)) {
        printf("Bad magic bytes for Onyx binary.\n");
        return 0;
    }

    wasm_bytes.data[0] = '\0';
    wasm_bytes.data[1] = 'a';
    wasm_bytes.data[2] = 's';
    wasm_bytes.data[3] = 'm';
    wasm_raw_bytes = wasm_bytes;

    bh_arr(WasmFuncDefinition **) linkable_functions = NULL;
    bh_arr_new(bh_heap_allocator(), linkable_functions, 4);

    LinkLibraryContext lib_ctx;
    lib_ctx.wasm_bytes = wasm_bytes;
    lib_ctx.library_paths = NULL;
    lookup_and_load_custom_libraries(&lib_ctx, &linkable_functions);

    wasm_byte_vec_t wasm_data;
    wasm_data.size = wasm_bytes.length;
    wasm_data.data = wasm_bytes.data;

    wasm_module = wasm_module_new(wasm_store, &wasm_data);
    if (!wasm_module) {
        cleanup_wasm_objects();
        return 0;
    }

    wasm_imports = (wasm_extern_vec_t) WASM_EMPTY_VEC;
    if (!link_wasm_imports(linkable_functions, &lib_ctx, wasm_module)) {
        return 0;
    }

    bh_arr_free(lib_ctx.library_paths);

    wasm_trap_t* traps = NULL;

    wasm_instance = wasm_instance_new(wasm_store, wasm_module, &wasm_imports, &traps);
    if (!wasm_instance) {
        cleanup_wasm_objects();
        return 0;
    }

    wasm_runtime.wasm_engine = wasm_engine;
    wasm_runtime.wasm_module = wasm_module;
    wasm_runtime.wasm_imports = wasm_imports;
    wasm_runtime.wasm_memory = wasm_memory;
    wasm_runtime.wasm_instance = wasm_instance;
    
    wasm_runtime.argc = argc;
    wasm_runtime.argv = argv;

    wasm_extern_t* start_extern = wasm_extern_lookup_by_name(wasm_module, wasm_instance, "_start");
    wasm_func_t*   start_func   = wasm_extern_as_func(start_extern);

    wasm_val_vec_t args;
    wasm_val_vec_t results;
    wasm_val_vec_new_uninitialized(&args, 0);
    wasm_val_vec_new_uninitialized(&results, 1);

    wasm_trap_t *run_trap = wasm_func_call(start_func, &args, &results);

#if 1
    if (run_trap != NULL) onyx_print_trap(run_trap);
#endif

    goto end;

    bh_printf("An error occured trying to run the WASM module...\n");

#ifndef USE_OVM_DEBUGGER
    i32 len = wasmer_last_error_length();
    char *buf = alloca(len + 1);
    wasmer_last_error_message(buf, len);
    bh_printf("%b\n", buf, len);
#endif

end:
    cleanup_wasm_objects();
    return run_trap == NULL;
}
