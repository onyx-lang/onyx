#include "bh.h"
#include "utils.h"
#include "astnodes.h"
#include "wasm.h"
#include "wasmer.h"

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
static wasi_config_t*    wasi_config;
static wasi_env_t*       wasi_env;
static wasm_engine_t*    wasm_engine;
static wasm_store_t*     wasm_store;
static wasm_extern_vec_t wasm_imports;
static wasm_module_t*    wasm_module;
wasm_memory_t*    wasm_memory;

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

#define WASM_INTEROP(name) static wasm_trap_t * name (const wasm_val_vec_t *params, wasm_val_vec_t *results)


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

    // NOTE: This is cached in a local variable because there is a tiny chance that if a lot of threads are created
    // then the backing array for the thread handles will move and thread* we have well not be valid. I'm betting on this
    // not happening before now in the function; however, it *could* happen before we call thread_exit. This would be bad
    // because of the normal reasons why accessing memory that you don't own any more is bad.
    i32 thread_id = thread->id;

    { // Call the _thread_start procedure
        wasm_val_t args[]    = { WASM_I32_VAL(thread_id), WASM_I32_VAL(thread->tls_base), WASM_I32_VAL (thread->funcidx), WASM_I32_VAL(thread->dataptr) };
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
        wasm_val_t args[]    = { WASM_I32_VAL(thread_id) };
        wasm_val_vec_t results;
        wasm_val_vec_t args_array = WASM_ARRAY_VEC(args);

        trap = wasm_func_call(exit_func, &args_array, &results);
    }

    return 0;
}

WASM_INTEROP(onyx_spawn_thread_impl) {
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
        // thread->thread_handle = CreateThread(NULL, 0, onyx_run_thread, thread, 0, &thread->thread_id);
        thread->thread_handle = (HANDLE) _beginthreadex(NULL, 0, onyx_run_thread, thread, 0, &thread->thread_id);
    #endif

    results->data[0] = WASM_I32_VAL(1);
    return NULL;
}

WASM_INTEROP(onyx_kill_thread_impl) {
    i32 thread_id = params->data[0].of.i32;

    i32 i = 0;
    bh_arr_each(OnyxThread, thread, threads) {
        if (thread->id == thread_id) {
            #ifdef _BH_LINUX
            pthread_kill(thread->thread, SIGKILL);
            #endif

            #ifdef _BH_WINDOWS
            TerminateThread(thread->thread_handle, 0);
            CloseHandle(thread->thread_handle);
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

#define ONYX_PROCESS_MAGIC_NUMBER 0xdeadfadebabecafe
typedef struct OnyxProcess {
    u64 magic_number;

#ifdef _BH_LINUX
    // Pipes
    i32 proc_to_host[2];
    i32 host_to_proc[2];

    pid_t pid;
#endif

#ifdef _BH_WINDOWS
    HANDLE proc_to_host_read;
    HANDLE proc_to_host_write;
    HANDLE host_to_proc_read;
    HANDLE host_to_proc_write;

    PROCESS_INFORMATION proc_info;
#endif
} OnyxProcess;

WASM_INTEROP(onyx_process_spawn_impl) {
    char* process_str = (char *) wasm_memory_data(wasm_memory) + params->data[0].of.i32;
    i32   process_len = params->data[1].of.i32;
    i32   args_ptr    = params->data[2].of.i32;
    i32   args_len    = params->data[3].of.i32;
    b32   blocking_io = !params->data[4].of.i32;

    char process_path[1024];
    process_len = bh_min(1023, process_len);
    memcpy(process_path, process_str, process_len);
    process_path[process_len] = '\0';

    OnyxProcess *process = bh_alloc_item(global_heap_allocator, OnyxProcess);
    memset(process, 0, sizeof(*process));
    process->magic_number = ONYX_PROCESS_MAGIC_NUMBER;

    #ifdef _BH_LINUX
        char **process_args = bh_alloc_array(global_scratch_allocator, char *, args_len + 2);
        byte_t* data = wasm_memory_data(wasm_memory);
        byte_t* array_loc = data + args_ptr;
        fori (i, 0, args_len) {
            char *arg_str = data + *(i32 *) (array_loc + i * 2 * POINTER_SIZE);
            i32   arg_len = *(i32 *) (array_loc + i * 2 * POINTER_SIZE + POINTER_SIZE);

            char *arg = bh_alloc_array(global_scratch_allocator, char, arg_len + 1);
            memcpy(arg, arg_str, arg_len);
            arg[arg_len] = '\0';
            process_args[i + 1] = arg;
        }
        process_args[0] = process_path;
        process_args[args_len + 1] = NULL;

        if (pipe(process->proc_to_host) || pipe(process->host_to_proc)) {
            wasm_val_init_ptr(&results->data[0], NULL); // Failed to run
            return NULL;
        }

        pid_t pid;
        switch (pid = fork()) {
            case -1: // Bad fork
                wasm_val_init_ptr(&results->data[0], NULL); // Failed to run

                close(process->proc_to_host[0]);
                close(process->proc_to_host[1]);
                close(process->host_to_proc[0]);
                close(process->host_to_proc[1]);
                break;

            case 0: // Child process
                close(process->proc_to_host[0]);
                close(process->host_to_proc[1]);
                dup2(process->host_to_proc[0], 0); // Map the output to the pipe
                dup2(process->proc_to_host[1], 1); // Map the output to the pipe
                dup2(process->proc_to_host[1], 2); // Stderr to stdout

                if (!blocking_io) {
                    fcntl(0, F_SETFL, O_NONBLOCK);
                    fcntl(1, F_SETFL, O_NONBLOCK);
                }

                execv(process_path, process_args);
                exit(-1);
                break;

            default: {
                process->pid = pid;
                close(process->host_to_proc[0]);
                close(process->proc_to_host[1]);

                wasm_val_init_ptr(&results->data[0], process);
                break;
            }
        }

    #endif

    #ifdef _BH_WINDOWS
        // CLEANUP CLEANUP CLEANUP: This is so freaking bad...
        char cmdLine[2048];
        memset(cmdLine, 0, 2048);
        strncat(cmdLine, process_path, 2047);

        byte_t* data = wasm_memory_data(wasm_memory);
        byte_t* array_loc = data + args_ptr;
        fori (i, 0, args_len) {
            char *arg_str = data + *(i32 *) (array_loc + i * 2 * POINTER_SIZE);
            i32   arg_len = *(i32 *) (array_loc + i * 2 * POINTER_SIZE + 4);

            strncat(cmdLine, " ", 2047);
            strncat(cmdLine, arg_str, arg_len);
        }

        STARTUPINFOA startup;
        memset(&startup, 0, sizeof startup);
        startup.cb = sizeof(startup);

        SECURITY_ATTRIBUTES saAttr;
        saAttr.nLength = sizeof(SECURITY_ATTRIBUTES);
        saAttr.lpSecurityDescriptor = NULL;
        saAttr.bInheritHandle = 1;

        BOOL success = 1;
        success = success && CreatePipe(&process->host_to_proc_read, &process->host_to_proc_write, &saAttr, 4096);
        success = success && CreatePipe(&process->proc_to_host_read, &process->proc_to_host_write, &saAttr, 4096);
        if (!success) {
            // printf("FAILED TO CREATE PIPES: %d\n", GetLastError());
            wasm_val_init_ptr(&results->data[0], NULL); // Failed to run @LEAK
            return NULL;
        }

        success = SetHandleInformation(process->proc_to_host_read, 1 /* HANDLE_FLAG_INHERIT */, 0);
        success = success && SetHandleInformation(process->host_to_proc_write, 1 /* HANDLE_FLAG_INHERIT */, 0);
        if (!success) {
            // printf("FAILED TO CONFIGURE PIPES: %d\n", GetLastError());
            wasm_val_init_ptr(&results->data[0], NULL); // Failed to run @LEAK
            return NULL;
        }

        startup.hStdInput  = process->host_to_proc_read;
        startup.hStdOutput = process->proc_to_host_write;
        startup.hStdError = process->proc_to_host_write;

        startup.dwFlags |= STARTF_USESTDHANDLES;

        memset(&process->proc_info, 0, sizeof process->proc_info);

        success = CreateProcessA(process_path, cmdLine, &saAttr, &saAttr, 1, 0, NULL, NULL, &startup, &process->proc_info);
        if (!success) {
            // printf("FAILED TO CREATE PROCESS: %d\n", GetLastError());
            wasm_val_init_ptr(&results->data[0], NULL); // Failed to run @LEAK
            return NULL;
        }

        CloseHandle(process->proc_to_host_write);
        CloseHandle(process->host_to_proc_read);
        wasm_val_init_ptr(&results->data[0], process);
    #endif

    return NULL;
}

WASM_INTEROP(onyx_process_read_impl) {
    OnyxProcess *process = (OnyxProcess *) params->data[0].of.i64;
    if (process == NULL || process->magic_number != ONYX_PROCESS_MAGIC_NUMBER) {
        results->data[0] = WASM_I32_VAL(0);
        return NULL;
    }

    i32 output_ptr = params->data[1].of.i32;
    i32 output_len = params->data[2].of.i32;
    u8 *buffer = wasm_memory_data(wasm_memory) + output_ptr;

    i32 bytes_read;
    #ifdef _BH_LINUX
        bytes_read = read(process->proc_to_host[0], buffer, output_len);
        bytes_read = bh_max(bytes_read, 0);  // Silently consume errors
    #endif

    #ifdef _BH_WINDOWS
        BOOL success = ReadFile(process->proc_to_host_read, buffer, output_len, &bytes_read, NULL);
        if (!success) bytes_read = 0; 
    #endif

    results->data[0] = WASM_I32_VAL(bytes_read);
    return NULL;
}

WASM_INTEROP(onyx_process_write_impl) {
    OnyxProcess *process = (OnyxProcess *) params->data[0].of.i64;
    if (process == NULL || process->magic_number != ONYX_PROCESS_MAGIC_NUMBER) {
        results->data[0] = WASM_I32_VAL(0);
        return NULL;
    }

    i32 input_ptr = params->data[1].of.i32;
    i32 input_len = params->data[2].of.i32;
    u8 *buffer = wasm_memory_data(wasm_memory) + input_ptr;

    i32 bytes_written;
    #ifdef _BH_LINUX
        bytes_written = write(process->host_to_proc[1], buffer, input_len);
        bytes_written = bh_max(bytes_written, 0);  // Silently consume errors
    #endif

    #ifdef _BH_WINDOWS
        BOOL success = WriteFile(process->host_to_proc_write, buffer, input_len, &bytes_written, NULL);
        if (!success) bytes_written = 0;
    #endif

    results->data[0] = WASM_I32_VAL(bytes_written);
    return NULL;
}

WASM_INTEROP(onyx_process_kill_impl) {
    OnyxProcess *process = (OnyxProcess *) params->data[0].of.i64;
    if (process == NULL || process->magic_number != ONYX_PROCESS_MAGIC_NUMBER) {
        results->data[0] = WASM_I32_VAL(0);
        return NULL;
    }

    #ifdef _BH_LINUX
        i32 failed = kill(process->pid, SIGKILL);
        results->data[0] = WASM_I32_VAL(!failed);
    #endif

    #ifdef _BH_WINDOWS
        BOOL success = TerminateProcess(process->proc_info.hProcess, 1);
        results->data[0] = WASM_I32_VAL(success ? 1 : 0);
    #endif

    return NULL;
}

WASM_INTEROP(onyx_process_wait_impl) {
    OnyxProcess *process = (OnyxProcess *) params->data[0].of.i64;
    if (process == NULL || process->magic_number != ONYX_PROCESS_MAGIC_NUMBER) {
        results->data[0] = WASM_I32_VAL(1);
        return NULL;
    }

    #ifdef _BH_LINUX
        i32 status;
        waitpid(process->pid, &status, 0);

        i32 exit_code = WEXITSTATUS(status);
        results->data[0] = WASM_I32_VAL(exit_code != 0 ? 2 : 0);
    #endif

    #ifdef _BH_WINDOWS
        DWORD exitCode;
        while (1) {
            if (!WaitForSingleObject(process->proc_info.hProcess, INFINITE)) {
                // HACK HACK HACK
                DWORD error = GetLastError();
                if (error != 109 && error != 6) {
                    // printf("ERROR IN WAIT FOR SINGLE: %d\n", error);
                    results->data[0] = WASM_I32_VAL(1);
                    return NULL; 
                }
            }

            if (!GetExitCodeProcess(process->proc_info.hProcess, &exitCode)) {
                // HACK HACK HACK
                // Apparently, I'm doing something wrong (maybe?) where the process handle becomes
                // invalid and causes error 6 "invalid handle". So I think I can safely assume that
                // if that is the case, then the process exited? probably successfuly? hopefully?
                // Honestly I don't know and I can't find any documentation describing when a process
                // handle goes invalid, other than after you close it explicitly. But in the run_tests
                // script, I'm not calling either process_kill or process_destroy, which are the only
                // other functions that close the process handle. So I'm left in the dark as to why this
                // is happening, but oh well. This works for now.
                //                                                           - brendanfh 2021/12/03
                if (GetLastError() == 6) {
                    exitCode = 0;
                    break;
                }

                results->data[0] = WASM_I32_VAL(3);
                return NULL;
            }

            // 259 is STILL_ACTIVE (aka STATUS_PENDING), which means that the process has not yet exited
            if (exitCode != 259) break;
        }

        results->data[0] = WASM_I32_VAL(exitCode != 0 ? 2 : 0);
    #endif

    return NULL;
}

WASM_INTEROP(onyx_process_destroy_impl) {
    OnyxProcess *process = (OnyxProcess *) params->data[0].of.i64;
    if (process == NULL || process->magic_number != ONYX_PROCESS_MAGIC_NUMBER) {
        return NULL;
    }

    #ifdef _BH_LINUX
        close(process->proc_to_host[0]);
        close(process->host_to_proc[1]);
    #endif

    #ifdef _BH_WINDOWS
        if (!CloseHandle(process->proc_info.hThread)
         || !CloseHandle(process->proc_info.hProcess)) {
            // printf("ERROR CLOSING HANDLES: %d\n", GetLastError());
         }
    #endif

    bh_free(global_heap_allocator, process);

    return NULL;
}

#include "onyx_library.h"

typedef void *(*LibraryLinker)();
static bh_arr(WasmFuncDefinition **) linkable_functions = NULL;

static void onyx_load_library(char *name) {
    char *library_load_name = bh_aprintf(global_scratch_allocator, "onyx_library_%s", name);
    LibraryLinker library_load;

    #ifdef _BH_LINUX
    char *library_name = bh_aprintf(global_scratch_allocator, "%s.so", name);
    void* handle = dlopen(library_name, RTLD_LAZY);
    if (handle == NULL) {
        printf("ERROR LOADING LIBRARY %s: %s\n", name, dlerror());
        return;
    }

    library_load = (LibraryLinker) dlsym(handle, library_load_name);
    #endif

    WasmFuncDefinition** funcs = library_load();
    bh_arr_push(linkable_functions, funcs);
}

// Returns 1 if successful
b32 onyx_run_wasm(bh_buffer wasm_bytes) {
    bh_arr_new(global_heap_allocator, linkable_functions, 4);
    onyx_load_library("test_library");

    wasm_instance_t* instance = NULL;
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

            if (wasm_name_equals_string(import_name, "process_spawn")) {
                wasm_valtype_t* ps[5] = {
                    wasm_valtype_new_i32(), wasm_valtype_new_i32(),
                    wasm_valtype_new_i32(), wasm_valtype_new_i32(),
                    wasm_valtype_new_i32()
                };
                wasm_valtype_t* rs[1] = { wasm_valtype_new_i64() };
                wasm_valtype_vec_t params, results;
                wasm_valtype_vec_new(&params, 5, ps);
                wasm_valtype_vec_new(&results, 1, rs);
                wasm_functype_t* func_type = wasm_functype_new(&params, &results);

                wasm_func_t* wasm_func = wasm_func_new(wasm_store, func_type, onyx_process_spawn_impl);
                import = wasm_func_as_extern(wasm_func);
                goto import_found;
            }

            if (wasm_name_equals_string(import_name, "process_read")) {
                wasm_functype_t* func_type = wasm_functype_new_3_1(
                    wasm_valtype_new_i64(), wasm_valtype_new_i32(), wasm_valtype_new_i32(),
                    wasm_valtype_new_i32());

                wasm_func_t* wasm_func = wasm_func_new(wasm_store, func_type, onyx_process_read_impl);
                import = wasm_func_as_extern(wasm_func);
                goto import_found;
            }

            if (wasm_name_equals_string(import_name, "process_write")) {
                wasm_functype_t* func_type = wasm_functype_new_3_1(
                    wasm_valtype_new_i64(), wasm_valtype_new_i32(), wasm_valtype_new_i32(),
                    wasm_valtype_new_i32());

                wasm_func_t* wasm_func = wasm_func_new(wasm_store, func_type, onyx_process_write_impl);
                import = wasm_func_as_extern(wasm_func);
                goto import_found;
            }

            if (wasm_name_equals_string(import_name, "process_kill")) {
                wasm_functype_t* func_type = wasm_functype_new_1_1(wasm_valtype_new_i64(), wasm_valtype_new_i32());

                wasm_func_t* wasm_func = wasm_func_new(wasm_store, func_type, onyx_process_kill_impl);
                import = wasm_func_as_extern(wasm_func);
                goto import_found;
            }

            if (wasm_name_equals_string(import_name, "process_wait")) {
                wasm_functype_t* func_type = wasm_functype_new_1_1(wasm_valtype_new_i64(), wasm_valtype_new_i32());

                wasm_func_t* wasm_func = wasm_func_new(wasm_store, func_type, onyx_process_wait_impl);
                import = wasm_func_as_extern(wasm_func);
                goto import_found;
            }

            if (wasm_name_equals_string(import_name, "process_destroy")) {
                wasm_functype_t* func_type = wasm_functype_new_1_0(wasm_valtype_new_i64());

                wasm_func_t* wasm_func = wasm_func_new(wasm_store, func_type, onyx_process_destroy_impl);
                import = wasm_func_as_extern(wasm_func);
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

    instance = wasm_instance_new(wasm_store, wasm_module, &wasm_imports, &traps);
    if (!instance) goto error_handling;

    wasm_extern_t* start_extern = wasm_extern_lookup_by_name(wasm_module, instance, "_start");
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
    if (instance)    wasm_instance_delete(instance);
    if (wasm_module) wasm_module_delete(wasm_module);
    if (wasm_store)  wasm_store_delete(wasm_store);
    if (wasm_engine) wasm_engine_delete(wasm_engine);
    return run_trap == NULL;
}
