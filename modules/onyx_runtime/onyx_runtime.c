
#define BH_DEFINE
#include "bh.h"

#define ONYX_LIBRARY_NAME onyx_runtime
#define ONYX_NO_SHORT_NAMES
#include "onyx_library.h"

#ifdef _BH_LINUX
    #include <pthread.h>
    #include <signal.h>
    #include <sys/wait.h>
    #include <dlfcn.h>
#endif

#include "types.h"  // For POINTER_SIZE

#define ONYX_FILE_ERROR_NONE 0
#define ONYX_FILE_ERROR_NOT_FOUND 1
#define ONYX_FILE_ERROR_EXISTS 2
#define ONYX_FILE_ERROR_PERMISSION 3
#define ONYX_FILE_ERROR_BAD_FILE 4
#define ONYX_FILE_ERROR_BAD_MODE 5

ONYX_DEF(__file_open_impl, (WASM_I32, WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {
    char *path_ptr = ONYX_PTR(params->data[0].of.i32);
    int   path_len = params->data[1].of.i32;

    char path[512] = {0};
    strncpy(path, path_ptr, path_len);
    path[path_len] = 0;

    int mode = params->data[2].of.i32;

    bh_file_mode bh_mode;
    switch (mode) {
        case 1: bh_mode = BH_FILE_MODE_READ; break;
        case 2: bh_mode = BH_FILE_MODE_WRITE; break;
        case 3: bh_mode = BH_FILE_MODE_APPEND; break;
    }

    bh_file file;
    bh_file_error error = bh_file_open_mode(&file, bh_mode, path);
    if (error == BH_FILE_ERROR_INVALID) {
        results->data[0] = WASM_I32_VAL(ONYX_FILE_ERROR_NOT_FOUND);
        return NULL;
    }

    *(u64 *) ONYX_PTR(params->data[3].of.i32) = (u64) file.fd;
    results->data[0] = WASM_I32_VAL(ONYX_FILE_ERROR_NONE);
    return NULL;
}

ONYX_DEF(__file_close, (WASM_I64), (WASM_I32)) {
    i64 fd = params->data[0].of.i64;

    bh_file file = { (bh_file_descriptor) fd };
    bh_file_error error = bh_file_close(&file);
    if (error == BH_FILE_ERROR_INVALID) {
        results->data[0] = WASM_I32_VAL(ONYX_FILE_ERROR_NOT_FOUND);
        return NULL;
    }

    results->data[0] = WASM_I32_VAL(ONYX_FILE_ERROR_NONE);
    return NULL;
}

ONYX_DEF(__file_exists, (WASM_I32, WASM_I32), (WASM_I32)) {
    char *path_ptr = ONYX_PTR(params->data[0].of.i32);
    int   path_len = params->data[1].of.i32;

    char path[512] = {0};
    strncpy(path, path_ptr, path_len);
    path[path_len] = 0;

    results->data[0] = WASM_I32_VAL(bh_file_exists(path));
    return NULL;
}

ONYX_DEF(__file_seek, (WASM_I64, WASM_I32, WASM_I32), (WASM_I32)) {
    i64 fd = params->data[0].of.i64;
    i32 offset = params->data[1].of.i32;
    i32 whence = params->data[2].of.i32;

    bh_file file = { (bh_file_descriptor) fd };
    bh_file_whence bh_whence;
    switch (whence) {
        case 0: bh_whence = BH_FILE_WHENCE_BEGIN; break;
        case 1: bh_whence = BH_FILE_WHENCE_CURRENT; break;
        case 2: bh_whence = BH_FILE_WHENCE_END; break;
    }

    i64 new_offset = bh_file_seek(&file, offset, whence);
    results->data[0] = WASM_I32_VAL((i32) new_offset);
    return NULL;
}

ONYX_DEF(__file_tell, (WASM_I64), (WASM_I32)) {
    i64 fd = params->data[0].of.i64;
    bh_file file = { (bh_file_descriptor) fd };
    results->data[0] = WASM_I32_VAL(bh_file_tell(&file));
    return NULL;
}

ONYX_DEF(__file_read, (WASM_I64, WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {
    i64 fd = params->data[0].of.i64;
    bh_file file = { (bh_file_descriptor) fd };
    b32 success = bh_file_read_at(&file,
            bh_file_tell(&file),
            ONYX_PTR(params->data[1].of.i32),
            params->data[2].of.i32,
            (i64 *) ONYX_PTR(params->data[3].of.i32));

    results->data[0] = WASM_I32_VAL(0);
    if (!success) results->data[0] = WASM_I32_VAL(6);
    return NULL;
}

ONYX_DEF(__file_write, (WASM_I64, WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {
    i64 fd = params->data[0].of.i64;
    bh_file file = { (bh_file_descriptor) fd };
    b32 success = bh_file_write_at(&file,
            bh_file_tell(&file),
            ONYX_PTR(params->data[1].of.i32),
            params->data[2].of.i32,
            (i64 *) ONYX_PTR(params->data[3].of.i32));

    results->data[0] = WASM_I32_VAL(0);
    if (!success) results->data[0] = WASM_I32_VAL(6);
    return NULL;
}

ONYX_DEF(__file_flush, (WASM_I64), (WASM_I32)) {
    i64 fd = params->data[0].of.i64;
    bh_file file = { (bh_file_descriptor) fd };
    bh_file_flush(&file);
    results->data[0] = WASM_I32_VAL(0);
    return NULL;
}


//
// THREADS
//
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
    thread->instance = runtime->wasm_instance_new(runtime->wasm_store, runtime->wasm_module, &runtime->wasm_imports, &traps);

    wasm_extern_t* start_extern = runtime->wasm_extern_lookup_by_name(runtime->wasm_module, thread->instance, "_thread_start");
    wasm_func_t*   start_func   = runtime->wasm_extern_as_func(start_extern);

    wasm_extern_t* exit_extern = runtime->wasm_extern_lookup_by_name(runtime->wasm_module, thread->instance, "_thread_exit");
    wasm_func_t*   exit_func   = runtime->wasm_extern_as_func(exit_extern);

    wasm_trap_t* trap=NULL;

    // NOTE: This is cached in a local variable because there is a tiny chance that if a lot of threads are created
    // then the backing array for the thread handles will move and thread* we have well not be valid. I'm betting on this
    // not happening before now in the function; however, it *could* happen before we call thread_exit. This would be bad
    // because of the normal reasons why accessing memory that you don't own any more is bad.
    i32 thread_id = thread->id;

    { // Call the _thread_start procedure
        wasm_val_t args[]    = { WASM_I32_VAL(thread_id), WASM_I32_VAL(thread->tls_base), WASM_I32_VAL(thread->funcidx), WASM_I32_VAL(thread->dataptr) };
        wasm_val_vec_t results;
        wasm_val_vec_t args_array = WASM_ARRAY_VEC(args);

        trap = runtime->wasm_func_call(start_func, &args_array, &results);
        if (trap != NULL) {
            /*
            When proper linking is available for Wasmer, re-enable this code. At the moment
            I don't see too much value in passing all of these functions on the Runtime object
            especially since it is a hack to pass functions there anyway.

            wasm_message_t msg;
            wasm_trap_message(trap, &msg);
            bh_printf("TRAP: %b\n", msg.data, msg.size);

            wasm_frame_t *origin = wasm_trap_origin(trap);
            bh_printf("HERE: func[%d] at %p.\n", wasm_frame_func_index(origin), wasm_frame_module_offset(origin));
            */
            bh_printf("ERROR: WebAssembly trap in thread: %d\n", thread_id);
        }
    }

    { // Call the _thread_exit procedure
        wasm_val_t args[]    = { WASM_I32_VAL(thread_id) };
        wasm_val_vec_t results = { 0, 0 };
        wasm_val_vec_t args_array = WASM_ARRAY_VEC(args);

        trap = runtime->wasm_func_call(exit_func, &args_array, &results);
    }

    return 0;
}

ONYX_DEF(__spawn_thread, (WASM_I32, WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {
    if (threads == NULL) bh_arr_new(bh_heap_allocator(), threads, 128);
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

ONYX_DEF(__kill_thread, (WASM_I32), (WASM_I32)) {
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




//
// PROCESS
//
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

ONYX_DEF(__process_spawn, (WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32), (WASM_I64)) {
    char* process_str = ONYX_PTR(params->data[0].of.i32);
    i32   process_len = params->data[1].of.i32;
    i32   args_ptr    = params->data[2].of.i32;
    i32   args_len    = params->data[3].of.i32;
    b32   blocking_io = !params->data[4].of.i32;

    char process_path[1024];
    process_len = bh_min(1023, process_len);
    memcpy(process_path, process_str, process_len);
    process_path[process_len] = '\0';

    OnyxProcess *process = malloc(sizeof(OnyxProcess));
    memset(process, 0, sizeof(*process));
    process->magic_number = ONYX_PROCESS_MAGIC_NUMBER;

    #ifdef _BH_LINUX
        char **process_args = alloca(sizeof(char *) * (args_len + 2));
        byte_t* array_loc = ONYX_PTR(args_ptr);
        fori (i, 0, args_len) {
            char *arg_str = ONYX_PTR(*(i32 *) (array_loc + i * 2 * POINTER_SIZE));
            i32   arg_len = *(i32 *) (array_loc + i * 2 * POINTER_SIZE + POINTER_SIZE);

            char *arg = alloca(sizeof(char) * (arg_len + 1));
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

        byte_t* array_loc = ONYX_PTR(args_ptr);
        fori (i, 0, args_len) {
            char *arg_str = ONYX_PTR(*(i32 *) (array_loc + i * 2 * POINTER_SIZE));
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

        i32 success = 1;
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
            printf("FAILED TO CREATE PROCESS: %d\n", GetLastError());
            wasm_val_init_ptr(&results->data[0], NULL); // Failed to run @LEAK
            return NULL;
        }

        CloseHandle(process->proc_to_host_write);
        CloseHandle(process->host_to_proc_read);
        wasm_val_init_ptr(&results->data[0], process);
    #endif

    return NULL;
}

ONYX_DEF(__process_read, (WASM_I64, WASM_I32, WASM_I32), (WASM_I32)) {
    OnyxProcess *process = (OnyxProcess *) params->data[0].of.i64;
    if (process == NULL || process->magic_number != ONYX_PROCESS_MAGIC_NUMBER) {
        results->data[0] = WASM_I32_VAL(0);
        return NULL;
    }

    i32 output_ptr = params->data[1].of.i32;
    i32 output_len = params->data[2].of.i32;
    u8 *buffer = ONYX_PTR(output_ptr);

    i32 bytes_read;
    #ifdef _BH_LINUX
        bytes_read = read(process->proc_to_host[0], buffer, output_len);
        bytes_read = bh_max(bytes_read, 0);  // Silently consume errors
    #endif

    #ifdef _BH_WINDOWS
        i32 success = ReadFile(process->proc_to_host_read, buffer, output_len, &bytes_read, NULL);
        if (!success) bytes_read = 0;
    #endif

    results->data[0] = WASM_I32_VAL(bytes_read);
    return NULL;
}

ONYX_DEF(__process_write, (WASM_I64, WASM_I32, WASM_I32), (WASM_I32)) {
    OnyxProcess *process = (OnyxProcess *) params->data[0].of.i64;
    if (process == NULL || process->magic_number != ONYX_PROCESS_MAGIC_NUMBER) {
        results->data[0] = WASM_I32_VAL(0);
        return NULL;
    }

    i32 input_ptr = params->data[1].of.i32;
    i32 input_len = params->data[2].of.i32;
    u8 *buffer = ONYX_PTR(input_ptr);

    i32 bytes_written;
    #ifdef _BH_LINUX
        bytes_written = write(process->host_to_proc[1], buffer, input_len);
        bytes_written = bh_max(bytes_written, 0);  // Silently consume errors
    #endif

    #ifdef _BH_WINDOWS
        i32 success = WriteFile(process->host_to_proc_write, buffer, input_len, &bytes_written, NULL);
        if (!success) bytes_written = 0;
    #endif

    results->data[0] = WASM_I32_VAL(bytes_written);
    return NULL;
}

ONYX_DEF(__process_kill, (WASM_I64), (WASM_I32)) {
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
        i32 success = TerminateProcess(process->proc_info.hProcess, 1);
        results->data[0] = WASM_I32_VAL(success ? 1 : 0);
    #endif

    return NULL;
}

ONYX_DEF(__process_wait, (WASM_I64), (WASM_I32)) {
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

ONYX_DEF(__process_destroy, (WASM_I64), ()) {
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

    free(process);

    return NULL;
}

ONYX_LIBRARY {
    ONYX_FUNC(__file_open_impl)
    ONYX_FUNC(__file_close)
    ONYX_FUNC(__file_exists)
    ONYX_FUNC(__file_seek)
    ONYX_FUNC(__file_tell)
    ONYX_FUNC(__file_read)
    ONYX_FUNC(__file_write)
    ONYX_FUNC(__file_flush)

    ONYX_FUNC(__spawn_thread)
    ONYX_FUNC(__kill_thread)

    ONYX_FUNC(__process_spawn)
    ONYX_FUNC(__process_read)
    ONYX_FUNC(__process_write)
    ONYX_FUNC(__process_kill)
    ONYX_FUNC(__process_wait)
    ONYX_FUNC(__process_destroy)

    NULL
};