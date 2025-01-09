#include "bh.h"
#include "utils.h"
#include "astnodes.h"
#include "wasm.h"
#include "onyx_library.h"

#ifdef USE_DYNCALL
    #include "dyncall.h"
    #include "dyncall_callback.h"
    static DCCallVM *dcCallVM;
#endif

#ifndef USE_OVM_DEBUGGER
    #include "wasmer.h"
#endif

#if defined(_BH_LINUX) || defined(_BH_DARWIN)
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


static void *locate_symbol_in_dynamic_library_raw(char *libname, char *sym) {
#if defined(_BH_LINUX) || defined(_BH_DARWIN)
    void* handle = dlopen(libname, RTLD_LAZY);
    if (handle == NULL) {
        return NULL;
    }

    return dlsym(handle, sym);
#endif

#ifdef _BH_WINDOWS
    HMODULE handle = LoadLibraryA(libname);
    if (handle == NULL) {
        return NULL;
    }

    return GetProcAddress(handle, sym);
#endif

    return NULL;
}

static void *locate_symbol_in_dynamic_library(LinkLibraryContext *ctx, char *libname, char *sym) {
    char *library_name;
    bh_allocator alloc = bh_heap_allocator();

    #ifdef _BH_LINUX
    library_name = bh_lookup_file(libname, ".", ".so", (const char **) ctx->library_paths, NULL, alloc);
    #endif

    #ifdef _BH_DARWIN
    library_name = bh_lookup_file(libname, ".", ".dylib", (const char **) ctx->library_paths, NULL, alloc);
    #endif

    #ifdef _BH_WINDOWS
    library_name = bh_lookup_file(libname, ".", ".dll", (const char **) ctx->library_paths, NULL, alloc);
    #endif

    return locate_symbol_in_dynamic_library_raw(library_name, sym);
}

typedef void *(*LinkLibraryer)(OnyxRuntime *runtime);

static WasmFuncDefinition** onyx_load_library(LinkLibraryContext *ctx, char *name) {
    #if defined(_BH_LINUX) || defined(_BH_DARWIN)
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
        printf("ERROR RESOLVING '%s'\n", library_load_name);
        return NULL;
    }

    return library_load(runtime);
}

static void lookup_and_load_custom_libraries(LinkLibraryContext *ctx, bh_arr(WasmFuncDefinition **)* p_out) {
    bh_arr(WasmFuncDefinition **) out = *p_out;

    char *onyx_path = getenv("ONYX_PATH");
    if (onyx_path) {
        bh_arr_push(ctx->library_paths, bh_aprintf(bh_heap_allocator(), "%s/lib", onyx_path));
    }

    bh_buffer wasm_bytes = ctx->wasm_bytes;

    i32 cursor = 8; // skip the magic number and version
    while (cursor < wasm_bytes.length) {
        u64 section_number = uleb128_to_uint(wasm_bytes.data, &cursor);
        u64 section_size   = uleb128_to_uint(wasm_bytes.data, &cursor);

        i32 section_start = cursor;
        if (section_number == 0) {
            u64 name_len = uleb128_to_uint(wasm_bytes.data, &cursor);
            if (!strncmp((const char *) wasm_bytes.data + cursor, "_onyx_libs", name_len)) {
                cursor += name_len;
                u64 lib_count = uleb128_to_uint(wasm_bytes.data, &cursor);

                fori (i, 0, (i64) lib_count) {
                    u64 lib_path_length = uleb128_to_uint(wasm_bytes.data, &cursor);
                    lib_path_length = bh_min(lib_path_length, 512);

                    char *lib_path = malloc(lib_path_length);
                    strncpy(lib_path, (const char *) wasm_bytes.data + cursor, lib_path_length);
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
                    strncpy(library_name, (const char *) wasm_bytes.data + cursor, lib_name_length);
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


#ifdef USE_DYNCALL

//
// This code implements the ability to use `dyncall:some_library.so` as the
// module import name to dynamically load a shared library at runtime.
//

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
            case 'd':  dcArgDouble(dcCallVM, args->data[arg_idx++].of.f64);            break;
            case 'p':  dcArgPointer(dcCallVM, ONYX_PTR(args->data[arg_idx].of.i32)); arg_idx++; break;
            case 'v':  arg_idx++;                                                      break;
            case 's':
                dcArgPointer(dcCallVM, ONYX_PTR(args->data[arg_idx].of.i32));
                arg_idx++;
                dcArgInt(dcCallVM, args->data[arg_idx++].of.i32);
                break;

            default: assert("bad dynamic call type" && 0);
        }
    }

  arguments_placed:
    switch (ctx->types[0]) {
        case 'i': res->data[0] = WASM_I32_VAL(dcCallInt(dcCallVM, ctx->func));           break;
        case 'l': res->data[0] = WASM_I64_VAL(dcCallLongLong(dcCallVM, ctx->func));      break;
        case 'f': res->data[0] = WASM_F32_VAL(dcCallFloat(dcCallVM, ctx->func));         break;
        case 'd': res->data[0] = WASM_F64_VAL(dcCallDouble(dcCallVM, ctx->func));        break;
        case 'p': res->data[0] = WASM_I64_VAL((u64) dcCallPointer(dcCallVM, ctx->func)); break;
        case 'v': dcCallVoid(dcCallVM, ctx->func);                                       break;
    }

    dcReset(dcCallVM);
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

    char lib_name[256] = {0};
    strncpy(lib_name, library_name.data, bh_min(256, library_name.size));

    u32 index;
    char func_name[256] = {0};
    for (index = 0; index < function_name.size; index++) {
        if (function_name.data[index] == ':') {
            index ++;
            break;
        }

        func_name[index] = function_name.data[index];
    }

    char dynamic_types[64] = {0};
    for (u32 write_index = 0; index < function_name.size; write_index++, index++) {
        dynamic_types[write_index] = function_name.data[index];
    }

    void (*func)() = locate_symbol_in_dynamic_library_raw(lib_name, func_name);
    if (!func) return NULL;

    wasm_functype_t *functype = wasm_externtype_as_functype(type);

    DynCallContext* dcc = bh_alloc_item(bh_heap_allocator(), DynCallContext);
    dcc->func = func;
    memcpy(&dcc->types, dynamic_types, 64);

    wasm_func_t *wasm_func = wasm_func_new_with_env(wasm_store, functype, &__wasm_dyncall, dcc, NULL);
    return wasm_func;
}


//
// This code implements the ability to wrap a wasm function in a callback
// that can be handed to a library that expects a C function.
//

typedef struct DynCallbackContext {
    wasm_func_t *wasm_func;
    char *sig;
} DynCallbackContext;

static DCsigchar __wasm_dyncallback(DCCallback *cb, DCArgs *args, DCValue *result, void *userdata) {
    DynCallbackContext *ctx = userdata;
    int arg_count = bh_str_last_index_of(ctx->sig, ')') - 1;

    wasm_val_vec_t wasm_args;
    wasm_val_vec_new_uninitialized(&wasm_args, arg_count);

    for (int i = 0; i < arg_count; i++) {
        switch (ctx->sig[i]) {
            case 'B': wasm_args.data[i] = WASM_I32_VAL(dcbArgBool(args)); break;
            case 'c': wasm_args.data[i] = WASM_I32_VAL(dcbArgChar(args)); break;
            case 'C': wasm_args.data[i] = WASM_I32_VAL(dcbArgUChar(args)); break;
            case 's': wasm_args.data[i] = WASM_I32_VAL(dcbArgShort(args)); break;
            case 'S': wasm_args.data[i] = WASM_I32_VAL(dcbArgUShort(args)); break;
            case 'i': wasm_args.data[i] = WASM_I32_VAL(dcbArgInt(args)); break;
            case 'I': wasm_args.data[i] = WASM_I32_VAL(dcbArgUInt(args)); break;
            case 'j': wasm_args.data[i] = WASM_I64_VAL(dcbArgLong(args)); break;
            case 'J': wasm_args.data[i] = WASM_I64_VAL(dcbArgULong(args)); break;
            case 'l': wasm_args.data[i] = WASM_I64_VAL(dcbArgLongLong(args)); break;
            case 'L': wasm_args.data[i] = WASM_I64_VAL(dcbArgULongLong(args)); break;
            case 'f': wasm_args.data[i] = WASM_F32_VAL(dcbArgFloat(args)); break;
            case 'd': wasm_args.data[i] = WASM_F64_VAL(dcbArgDouble(args)); break;
            case 'p': wasm_args.data[i] = WASM_I64_VAL((uintptr_t) dcbArgPointer(args)); break;
        }
    }

    wasm_val_vec_t wasm_results;
    wasm_val_vec_new_uninitialized(&wasm_results, 1);

    wasm_func_call(ctx->wasm_func, &wasm_args, &wasm_results);

    switch (ctx->sig[arg_count + 1]) {
        case 'B': result->B = wasm_results.data[0].of.i32; break;
        case 'c': result->c = wasm_results.data[0].of.i32; break;
        case 'C': result->C = wasm_results.data[0].of.i32; break;
        case 's': result->s = wasm_results.data[0].of.i32; break;
        case 'S': result->S = wasm_results.data[0].of.i32; break;
        case 'i': result->i = wasm_results.data[0].of.i32; break;
        case 'I': result->I = wasm_results.data[0].of.i32; break;
        case 'j': result->j = wasm_results.data[0].of.i64; break;
        case 'J': result->J = wasm_results.data[0].of.i64; break;
        case 'l': result->l = wasm_results.data[0].of.i64; break;
        case 'L': result->L = wasm_results.data[0].of.i64; break;
        case 'f': result->f = wasm_results.data[0].of.f32; break;
        case 'd': result->d = wasm_results.data[0].of.f64; break;
        case 'p': result->p = (void *) wasm_results.data[0].of.i64; break;
    }

    wasm_val_vec_delete(&wasm_args);
    wasm_val_vec_delete(&wasm_results);

    return ctx->sig[arg_count + 1];
}

static void (* wasm_func_from_idx(wasm_table_t *func_table, unsigned int index, char *signature))(void) {
    wasm_ref_t *func_ref = wasm_table_get(func_table, index);
    assert(func_ref); // TODO: Switch to trap

    wasm_func_t *func = wasm_ref_as_func(func_ref);

    DynCallbackContext *dcc = bh_alloc_item(bh_heap_allocator(), DynCallbackContext);
    dcc->wasm_func = func;
    dcc->sig = signature;

    return (void (*)()) dcbNewCallback(signature, &__wasm_dyncallback, dcc);
}

#endif // USE_DYNCALL

static char *lookup_func_name_in_name_section(u32 funcidx, i32 name_section, i32 *out_len) {
    if (name_section == 0) return NULL;

    i32 cursor = name_section;

    // This is not the most robust checking, since there are other name
    // subsections that could come before the func section. For the
    // moment, Onyx only produces the func subsection in output_name_section.
    u32 name_kind = uleb128_to_uint(wasm_raw_bytes.data, &cursor);
    if (name_kind == 1) {
        u32 func_count = uleb128_to_uint(wasm_raw_bytes.data, &cursor);
        fori (i, 0, func_count) {
            u32 idx = uleb128_to_uint(wasm_raw_bytes.data, &cursor);
            u32 len = uleb128_to_uint(wasm_raw_bytes.data, &cursor);

            if (idx == funcidx) {
                *out_len = len;
                return &wasm_raw_bytes.data[cursor];
            }

            cursor += len;
        }
    }

    return NULL;
}

static void onyx_print_trap(wasm_trap_t* trap) {
    wasm_message_t msg;
    wasm_trap_message(trap, &msg);
    bh_printf("TRAP: %b\n", msg.data, msg.size);

    i32 name_section = 0;

    i32 cursor = 8; // skip the magic number and version
    while (cursor < wasm_raw_bytes.length) {
        u64 section_number = uleb128_to_uint(wasm_raw_bytes.data, &cursor);
        u64 section_size   = uleb128_to_uint(wasm_raw_bytes.data, &cursor);

        i32 section_start = cursor;
        if (section_number == 0) {
            u64 name_len = uleb128_to_uint(wasm_raw_bytes.data, &cursor);
            if (!strncmp((const char *) wasm_raw_bytes.data + cursor, "name", name_len)) {
                cursor += name_len;
                name_section = cursor;
                break;
            }
        }

        cursor = section_start + section_size;
    }

    bh_printf("TRACE:\n");
    wasm_frame_vec_t frames;
    wasm_trap_trace(trap, &frames);
    fori (i, 0, (i32) frames.size) {
        u32 func_idx   = wasm_frame_func_index(frames.data[i]);
        i32 mod_offset = wasm_frame_module_offset(frames.data[i]);

        i32   func_name_length;
        char* func_name = lookup_func_name_in_name_section(func_idx, name_section, &func_name_length);

        if (func_name) {
            bh_printf("    func[%d]:%p at %b\n", func_idx, mod_offset, func_name, func_name_length);
        } else {
            bh_printf("    func[%d]:%p\n", func_idx, mod_offset);
        }
    }
}

static void cleanup_wasm_objects() {
    if (wasm_instance) wasm_instance_delete(wasm_instance);
    if (wasm_module) wasm_module_delete(wasm_module);
    if (wasm_store)  wasm_store_delete(wasm_store);
    if (wasm_engine) wasm_engine_delete(wasm_engine);
}

static wasm_trap_t *__error_on_call(void *env, const wasm_val_vec_t *args, wasm_val_vec_t *results) {
    printf("[ERROR] Attempted to invoke imported function with no definition, '%s'\n", (char *) env);
    exit(1);
    return NULL;
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
                    const wasm_externtype_t *memory_extern_type = wasm_importtype_type(module_imports.data[i]);
                    const wasm_memorytype_t *expected_memory_type = wasm_externtype_as_memorytype_const(memory_extern_type);
                    const wasm_limits_t *memory_limits = wasm_memorytype_limits(expected_memory_type);

                    wasm_limits_t limits = { memory_limits->min, memory_limits->max };
                    wasm_memorytype_t* memory_type = wasm_memorytype_new(&limits);
                    wasm_memory = wasm_memory_new(wasm_store, memory_type);
                }

                import = wasm_memory_as_extern(wasm_memory);
                goto import_found;
            }
        }

#ifdef USE_DYNCALL
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
#endif

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

        //
        // If no matching function is not found, do not produce an error. Instead
        // make an empty function with the correct type that will produce an error
        // and crash when it is called. This enables safely having undefined references
        // in the program, so long as they are not called. It also enables binding
        // generators like `jsbindgen`, as during the generation, the imported functions
        // will not be called.
        {
            char *funcname = bh_aprintf(bh_heap_allocator(), "%b.%b", module_name->data, module_name->size, import_name->data, import_name->size);

            wasm_externtype_t *import_type = (wasm_externtype_t *) wasm_importtype_type(module_imports.data[i]);
            wasm_functype_t *wasm_functype = wasm_externtype_as_functype(import_type);
            wasm_func_t* wasm_func = wasm_func_new_with_env(wasm_store, wasm_functype, __error_on_call, funcname, NULL);
            import = wasm_func_as_extern(wasm_func);
        }

    import_found:
        wasm_imports.data[i] = import;
        continue;

    bad_import:
        bh_printf("Unable to find import '%b.%b'.\n", module_name->data, module_name->size, import_name->data, import_name->size);
        return 0;
    }

    return 1;
}

void onyx_run_initialize(b32 debug_enabled, const char *debug_socket) {
    wasm_config = wasm_config_new();
    if (!wasm_config) {
        cleanup_wasm_objects();
        return;
    }

#ifdef USE_OVM_DEBUGGER
    void wasm_config_enable_debug(wasm_config_t *config, int value);
    wasm_config_enable_debug(wasm_config, debug_enabled);

    #if defined(_BH_LINUX) || defined(_BH_DARWIN)
        i32 getpid();
        i32 pid = getpid();

        const char *socket_path = NULL;
        if (debug_socket != NULL) {
            socket_path = debug_socket;

        } else {
            char *env_path = getenv("ONYX_PATH");
            socket_path = bh_aprintf(bh_heap_allocator(), "%s/debug.%d", env_path, pid);
        }

        void wasm_config_set_listen_path(wasm_config_t *config, const char *listen_path);
        wasm_config_set_listen_path(wasm_config, socket_path);
    #endif
#endif

#ifndef USE_OVM_DEBUGGER
    if (debug_enabled) {
        printf("Warning: --debug does nothing if libovmwasm.so is not being used!\n");
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
    wasm_runtime.wasm_instance_delete = &wasm_instance_delete;
    wasm_runtime.wasm_store_new = &wasm_store_new;
    wasm_runtime.wasm_store_delete = &wasm_store_delete;
    wasm_runtime.onyx_print_trap = &onyx_print_trap;
}

b32 onyx_run_wasm_code(bh_buffer wasm_bytes, int argc, char *argv[]) {
    runtime = &wasm_runtime;
    wasm_raw_bytes = wasm_bytes;

    bh_arr(WasmFuncDefinition **) linkable_functions = NULL;
    bh_arr_new(bh_heap_allocator(), linkable_functions, 4);

    LinkLibraryContext lib_ctx;
    lib_ctx.wasm_bytes = wasm_bytes;
    lib_ctx.library_paths = NULL;
    lookup_and_load_custom_libraries(&lib_ctx, &linkable_functions);

    wasm_byte_vec_t wasm_data;
    wasm_data.size = wasm_bytes.length;
    wasm_data.data = (wasm_byte_t *) wasm_bytes.data;

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
    wasm_runtime.wasm_store = wasm_store;
    wasm_runtime.wasm_func_table = wasm_extern_as_table(
        wasm_extern_lookup_by_name(wasm_module, wasm_instance, "__indirect_function_table")
    );

#ifdef USE_DYNCALL
    wasm_runtime.wasm_func_from_idx = wasm_func_from_idx;
#endif

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
