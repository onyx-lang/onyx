
#define BH_DEFINE
#include "bh.h"

#define ONYX_LIBRARY_NAME onyx_runtime
#define ONYX_NO_SHORT_NAMES
#include "onyx_library.h"

#ifdef _BH_LINUX
    #include <pthread.h>
    #include <signal.h>
    #include <sys/wait.h>
    #include <sys/types.h>
    #include <dlfcn.h>
    #include <dirent.h>
    #include <arpa/inet.h>
    #include <netdb.h>
    #include <netinet/in.h>
    #include <sys/socket.h>
    #include <poll.h>
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
    path_len = bh_min(path_len, 511);
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
    path_len = bh_min(path_len, 511);
    strncpy(path, path_ptr, path_len);
    path[path_len] = 0;

    results->data[0] = WASM_I32_VAL(bh_file_exists(path));
    return NULL;
}

ONYX_DEF(__file_remove, (WASM_I32, WASM_I32), (WASM_I32)) {
    char *path_ptr = ONYX_PTR(params->data[0].of.i32);
    int   path_len = params->data[1].of.i32;

    char path[512] = {0};
    path_len = bh_min(path_len, 511);
    strncpy(path, path_ptr, path_len);
    path[path_len] = 0;

    results->data[0] = WASM_I32_VAL(bh_file_remove(path));
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

    i32 curr_pos = bh_file_tell(&file);
    b32 success = bh_file_read_at(&file,
            bh_file_tell(&file),
            ONYX_PTR(params->data[1].of.i32),
            params->data[2].of.i32,
            (i64 *) ONYX_PTR(params->data[3].of.i32));

    bh_file_seek_to(&file, curr_pos + *(i32 *) ONYX_PTR(params->data[3].of.i32));

    results->data[0] = WASM_I32_VAL(0);
    if (!success) results->data[0] = WASM_I32_VAL(2);
    return NULL;
}

ONYX_DEF(__file_write, (WASM_I64, WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {
    i64 fd = params->data[0].of.i64;
    bh_file file = { (bh_file_descriptor) fd };

    i32 curr_pos = bh_file_tell(&file);
    b32 success = bh_file_write_at(&file,
            bh_file_tell(&file),
            ONYX_PTR(params->data[1].of.i32),
            params->data[2].of.i32,
            (i64 *) ONYX_PTR(params->data[3].of.i32));

    bh_file_seek_to(&file, curr_pos + *(i32 *) ONYX_PTR(params->data[3].of.i32));

    results->data[0] = WASM_I32_VAL(0);
    if (!success) results->data[0] = WASM_I32_VAL(2);
    return NULL;
}

ONYX_DEF(__file_flush, (WASM_I64), (WASM_I32)) {
    i64 fd = params->data[0].of.i64;
    bh_file file = { (bh_file_descriptor) fd };
    bh_file_flush(&file);
    results->data[0] = WASM_I32_VAL(0);
    return NULL;
}

ONYX_DEF(__file_size, (WASM_I64), (WASM_I32)) {
    i64 fd = params->data[0].of.i64;
    bh_file file = { (bh_file_descriptor) fd };
    results->data[0] = WASM_I32_VAL(bh_file_size(&file));
    return NULL;
}

ONYX_DEF(__file_get_standard, (WASM_I32, WASM_I32), (WASM_I32)) {
    bh_file_standard standard = (bh_file_standard) params->data[0].of.i32;

    bh_file file;
    bh_file_error error = bh_file_get_standard(&file, standard);
    if (error == BH_FILE_ERROR_NONE) {
        *(u64 *) ONYX_PTR(params->data[1].of.i32) = (u64) file.fd;
    }

    results->data[0] = WASM_I32_VAL(error == BH_FILE_ERROR_NONE);
    return NULL;
}

ONYX_DEF(__file_rename, (WASM_I32, WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {
    char *old_path_ptr = ONYX_PTR(params->data[0].of.i32);
    int   old_path_len = params->data[1].of.i32;

    char old_path[512] = {0};
    old_path_len = bh_min(old_path_len, 511);
    strncpy(old_path, old_path_ptr, old_path_len);
    old_path[old_path_len] = 0;

    char *new_path_ptr = ONYX_PTR(params->data[2].of.i32);
    int   new_path_len = params->data[3].of.i32;

    char new_path[512] = {0};
    new_path_len = bh_min(new_path_len, 511);
    strncpy(new_path, new_path_ptr, new_path_len);
    new_path[new_path_len] = 0;

#ifdef _BH_WINDOWS
    results->data[0] = WASM_I32_VAL(MoveFileA(old_path, new_path));
    return NULL;
#endif

#ifdef _BH_LINUX
    results->data[0] = WASM_I32_VAL(rename(old_path, new_path) == 0);
    return NULL;
#endif
}

//
// Directories
//

ONYX_DEF(__dir_open, (WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {
    char *path_ptr = ONYX_PTR(params->data[0].of.i32);
    int   path_len = params->data[1].of.i32;

    char path[512] = {0};
    path_len = bh_min(path_len, 511);
    strncpy(path, path_ptr, path_len);
    path[path_len] = 0;

#ifdef _BH_WINDOWS
    for (int i=0; i<path_len; i++) if (path[i] == '/') path[i] = '\\';
    strncat(path, "\\*.*", 511);

    Windows_Directory_Opened* dir = malloc(sizeof(Windows_Directory_Opened));
    dir->hndl = FindFirstFileA(path, &dir->found_file);
    if (dir->hndl == INVALID_HANDLE_VALUE) {
        results->data[0] = WASM_I32_VAL(0);
        return NULL;
    }

    *(u64 *) ONYX_PTR(params->data[2].of.i32) = (u64) dir;

    results->data[0] = WASM_I32_VAL(1);
    return NULL;
#endif

#ifdef _BH_LINUX
    DIR* dir = opendir(path);
    *(u64 *) ONYX_PTR(params->data[2].of.i32) = (u64) dir;
    results->data[0] = WASM_I32_VAL(dir != NULL);
    return NULL;
#endif
}

// (DIR*, PTR<DIRENT>) -> BOOL
ONYX_DEF(__dir_read, (WASM_I64, WASM_I32), (WASM_I32)) {
#ifdef _BH_WINDOWS
    Windows_Directory_Opened* dir = (Windows_Directory_Opened *) params->data[0].of.i64;
    if (dir == NULL) {
        results->data[0] = WASM_I32_VAL(0);
        return NULL;
    }

    do {
        BOOL success = FindNextFileA(dir->hndl, &dir->found_file);
        if (!success) {
            results->data[0] = WASM_I32_VAL(0);
            return NULL;
        }
    } while (!strcmp(dir->found_file.cFileName, ".") || !strcmp(dir->found_file.cFileName, ".."));

    u32 out = params->data[1].of.i32;
    assert(out != 0);

    *(u32 *) ONYX_PTR(out + 0) = (dir->found_file.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) ? 3 : 4;
    *(u32 *) ONYX_PTR(out + 4) = 0;
    *(u32 *) ONYX_PTR(out + 8) = strlen(dir->found_file.cFileName);
    strncpy(ONYX_PTR(out + 12), dir->found_file.cFileName, 256);

    results->data[0] = WASM_I32_VAL(1);
    return NULL;
#endif

#ifdef _BH_LINUX
    DIR* dir = (DIR *) params->data[0].of.i64;
    if (dir == NULL) {
        results->data[0] = WASM_I32_VAL(0);
        return NULL;
    }

    struct dirent *ent;
    while (1) {
        ent = readdir(dir);
        if (ent == NULL) {
            results->data[0] = WASM_I32_VAL(0);
            return NULL;
        }

        // Skip the current directory and parent directory
        if (strcmp(ent->d_name, ".") && strcmp(ent->d_name, "..")) break;
    }

    u32 type = 0;
    switch (ent->d_type) {
        case DT_UNKNOWN: break;
        case DT_BLK: type = 1; break;
        case DT_CHR: type = 2; break;
        case DT_DIR: type = 3; break;
        case DT_LNK: type = 5; break;
        case DT_REG: type = 4; break;
        default: type = 6; break;
    }

    u32 out = params->data[1].of.i32;
    assert(out != 0);

    *(u32 *) ONYX_PTR(out + 0) = type;
    *(u32 *) ONYX_PTR(out + 4) = (u32) ent->d_ino;
    *(u32 *) ONYX_PTR(out + 8) = strlen(ent->d_name);
    strncpy(ONYX_PTR(out + 12), ent->d_name, 256);

    results->data[0] = WASM_I32_VAL(1);
    return NULL;
#endif
}

ONYX_DEF(__dir_close, (WASM_I64), ()) {
#ifdef _BH_WINDOWS
    Windows_Directory_Opened* dir = (Windows_Directory_Opened *) params->data[0].of.i64;

    FindClose(dir->hndl);
    free(dir);

    return NULL;
#endif

#ifdef _BH_LINUX
    DIR* dir = (DIR *) params->data[0].of.i64;
    if (dir == NULL) return NULL;

    closedir(dir);
#endif
    return NULL;
}

ONYX_DEF(__dir_create, (WASM_I32, WASM_I32), (WASM_I32)) {
    char *path_ptr = ONYX_PTR(params->data[0].of.i32);
    int   path_len = params->data[1].of.i32;

    char path[512] = {0};
    path_len = bh_min(path_len, 511);
    strncpy(path, path_ptr, path_len);
    path[path_len] = 0;

#ifdef _BH_WINDOWS
    results->data[0] = WASM_I32_VAL(CreateDirectoryA(path, NULL));
    return NULL;
#endif

#ifdef _BH_LINUX
    results->data[0] = WASM_I32_VAL(mkdir(path, 0777) == 0);
    return NULL;
#endif
}

ONYX_DEF(__dir_remove, (WASM_I32, WASM_I32), (WASM_I32)) {
    char *path_ptr = ONYX_PTR(params->data[0].of.i32);
    int   path_len = params->data[1].of.i32;

    char path[512] = {0};
    path_len = bh_min(path_len, 511);
    strncpy(path, path_ptr, path_len);
    path[path_len] = 0;

#ifdef _BH_WINDOWS
    results->data[0] = WASM_I32_VAL(RemoveDirectoryA(path));
    return NULL;
#endif

#ifdef _BH_LINUX
    results->data[0] = WASM_I32_VAL(rmdir(path) == 0);
    return NULL;
#endif
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

    wasm_store_t *wasm_store = runtime->wasm_store_new(runtime->wasm_engine);

    wasm_trap_t* traps = NULL;
    thread->instance = runtime->wasm_instance_new(wasm_store, runtime->wasm_module, &runtime->wasm_imports, &traps);

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
        wasm_val_vec_t results = { 0, 0 };
        wasm_val_vec_t args_array = WASM_ARRAY_VEC(args);

        trap = runtime->wasm_func_call(start_func, &args_array, &results);
        if (trap != NULL) {
            bh_printf("THREAD: %d\n", thread_id);
            runtime->onyx_print_trap(trap);
        }
    }

    { // Call the _thread_exit procedure
        wasm_val_t args[]    = { WASM_I32_VAL(thread_id) };
        wasm_val_vec_t results = { 0, 0 };
        wasm_val_vec_t args_array = WASM_ARRAY_VEC(args);

        trap = runtime->wasm_func_call(exit_func, &args_array, &results);
    }

    runtime->wasm_store_delete(wasm_store);

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
            // This leads to some weirdness and bugs...
            //
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

ONYX_DEF(__process_spawn, (WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32), (WASM_I64)) {
    char* process_str = ONYX_PTR(params->data[0].of.i32);
    i32   process_len = params->data[1].of.i32;
    i32   args_ptr    = params->data[2].of.i32;
    i32   args_len    = params->data[3].of.i32;
    b32   blocking_io = !params->data[4].of.i32;
    char *cwd_str     = ONYX_PTR(params->data[5].of.i32);
    i32   cwd_len     = params->data[6].of.i32;

    char process_path[1024];
    process_len = bh_min(1023, process_len);
    memcpy(process_path, process_str, process_len);
    process_path[process_len] = '\0';

    char starting_dir[1024];
    if (cwd_len > 0) {
        cwd_len = bh_min(1023, cwd_len);
        memcpy(starting_dir, cwd_str, cwd_len);
        starting_dir[cwd_len] = '\0';
    }

    OnyxProcess *process = malloc(sizeof(OnyxProcess));
    memset(process, 0, sizeof(*process));
    process->magic_number = ONYX_PROCESS_MAGIC_NUMBER;

    #ifdef _BH_LINUX
        // :Security - alloca a user controlled string.
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

                if (cwd_len > 0) {
                    chdir(starting_dir); // Switch current working directory.
                }

                execvp(process_path, process_args);
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
        strncat(cmdLine, "\"", 2047);
        strncat(cmdLine, process_path, 2047);
        strncat(cmdLine, "\"", 2047);

        byte_t* array_loc = ONYX_PTR(args_ptr);
        fori (i, 0, args_len) {
            char *arg_str = ONYX_PTR(*(i32 *) (array_loc + i * 2 * POINTER_SIZE));
            i32   arg_len = *(i32 *) (array_loc + i * 2 * POINTER_SIZE + 4);

            strncat(cmdLine, " \"", 2047);
            strncat(cmdLine, arg_str, arg_len);
            strncat(cmdLine, "\"", 2047);
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

        char *working_dir = NULL;
        if (cwd_len > 0) {
            working_dir = starting_dir;
        }

        success = CreateProcessA(process_path, cmdLine, &saAttr, &saAttr, 1, 0, NULL, working_dir, &startup, &process->proc_info);
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
        if (bytes_read < 0) {
            switch (errno) {
                case EAGAIN: bytes_read =  0; break;
                case EBADF:  bytes_read = -1; break;
                default:     bytes_read = -2; break;
            }
        }
    #endif

    #ifdef _BH_WINDOWS
        i32 success = ReadFile(process->proc_to_host_read, buffer, output_len, &bytes_read, NULL);
        if (!success) {
            bytes_read = -1;
        }
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

ONYX_DEF(__args_get, (WASM_I32, WASM_I32), ()) {
    if (runtime->argc == 0 || runtime->argv == NULL) {
        return NULL;
    }

    i32 buffer_base = params->data[1].of.i32;

    for (int i=0; i<runtime->argc; i++) {
        // Should this be strncpy? What would the length be?
        strcpy(ONYX_PTR(buffer_base), runtime->argv[i]);
        *(i32 *) ONYX_PTR(params->data[0].of.i32 + i * 4) = buffer_base;
        buffer_base += strlen(runtime->argv[i]) + 1;
    }

    return NULL;
}

ONYX_DEF(__args_sizes_get, (WASM_I32, WASM_I32), ()) {
    *(i32 *) ONYX_PTR(params->data[0].of.i32) = runtime->argc;

    i32 argv_size = 0;
    for (int i=0; i<runtime->argc; i++) {
        argv_size += strlen(runtime->argv[i]) + 1;
    }

    *(i32 *) ONYX_PTR(params->data[1].of.i32) = argv_size;
    return NULL;
}

ONYX_DEF(__exit, (WASM_I32), ()) {
    exit(params->data[0].of.i32);
    return NULL;
}

ONYX_DEF(__sleep, (WASM_I32), ()) {
    #ifdef _BH_LINUX
    usleep(params->data[0].of.i32 * 1000);
    #endif

    #ifdef _BH_WINDOWS
    Sleep(params->data[0].of.i32);
    #endif
    return NULL;
}

ONYX_DEF(__time, (), (WASM_I64)) {
    results->data[0] = WASM_I64_VAL(bh_time_curr());
    return NULL;
}



//
// Networking
//
struct onyx_socket_addr {
    unsigned short family;
    unsigned short port;
    unsigned int   addr;
};

ONYX_DEF(__net_create_socket, (WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {

    #ifdef _BH_LINUX
    int domain = 0;
    switch (params->data[1].of.i32) {    // :EnumDependent
        case 0: domain = AF_UNIX;  break;
        case 1: domain = AF_INET;  break;
        case 2: domain = AF_INET6; break;
        default: goto bad_settings;
    }

    int type = 0;
    switch (params->data[2].of.i32) {    // :EnumDependent
        case 0: type = SOCK_STREAM; break;
        case 1: type = SOCK_DGRAM;  break;
        default: goto bad_settings;
    }

    *((int *) ONYX_PTR(params->data[0].of.i32)) = socket(domain, type, 0);

    results->data[0] = WASM_I32_VAL(0);
    return NULL;
    #endif 

    #ifdef _BH_WINDOWS
    #endif

bad_settings:
    results->data[0] = WASM_I32_VAL(1); // :EnumDependent
    return NULL;
}

ONYX_DEF(__net_close_socket, (WASM_I32), ()) {
    #ifdef _BH_LINUX
    shutdown(params->data[0].of.i32, SHUT_RDWR);
    close(params->data[0].of.i32);
    #endif

    #ifdef _BH_WINDOWS
    #endif

    return NULL;
}

ONYX_DEF(__net_setting, (WASM_I32, WASM_I32, WASM_I32), ()) {
    #ifdef _BH_LINUX
    switch (params->data[1].of.i32) {
        case 1: { // :EnumDependent  Non-Blocking
            int s = params->data[0].of.i32;
            int flags = fcntl(s, F_GETFL, 0);
            if (params->data[2].of.i32) {
                flags |= O_NONBLOCK;
            } else {
                flags &= ~O_NONBLOCK;
            }

            fcntl(s, F_SETFL, flags);
            break;
        }

        case 2: { // :EnumDependent  Broadcast
            int s = params->data[0].of.i32;
            setsockopt(s, SOL_SOCKET, SO_BROADCAST, (void *) &params->data[2].of.i32, sizeof(int));
            break;
        }
    }
    #endif

    return NULL;
}

ONYX_DEF(__net_bind, (WASM_I32, WASM_I32), (WASM_I32)) {

    #ifdef _BH_LINUX
    struct sockaddr_in bind_addr;
    memset(&bind_addr, 0, sizeof(bind_addr));

    bind_addr.sin_family = AF_INET; // Should this be configurable? Or is binding only ever done for INET? INET6?
    bind_addr.sin_addr.s_addr = htonl(INADDR_ANY);
    bind_addr.sin_port = htons(params->data[1].of.i32);

    int res = bind(params->data[0].of.i32, &bind_addr, sizeof(bind_addr));
    results->data[0] = WASM_I32_VAL(res >= 0);
    #endif

    #ifdef _BH_WINDOWS
    #endif

    return NULL;
}

ONYX_DEF(__net_listen, (WASM_I32, WASM_I32), ()) {
    #ifdef _BH_LINUX
    listen(params->data[0].of.i32, params->data[1].of.i32);
    #endif

    #ifdef _BH_WINDOWS
    #endif

    return NULL;
}

ONYX_DEF(__net_accept, (WASM_I32, WASM_I32), (WASM_I32)) {
    #ifdef _BH_LINUX
    struct sockaddr_in client_addr;
    int client_len = sizeof(client_addr);
    memset(&client_addr, 0, client_len);

    int client_socket = accept(params->data[0].of.i32, &client_addr, &client_len);

    struct onyx_socket_addr* out_addr = (struct onyx_socket_addr *) ONYX_PTR(params->data[1].of.i32); 
    out_addr->family = client_addr.sin_family;
    out_addr->port   = ntohs(client_addr.sin_port);
    out_addr->addr   = ntohl(client_addr.sin_addr.s_addr);

    results->data[0] = WASM_I32_VAL(client_socket);
    #endif

    return NULL;
}

ONYX_DEF(__net_connect, (WASM_I32, WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {
    #ifdef _BH_LINUX
    int   hostlen  = params->data[2].of.i32;
    char *hostname = alloca(hostlen + 1);
    memcpy(hostname, ONYX_PTR(params->data[1].of.i32), hostlen);
    hostname[hostlen] = '\0';

    struct hostent *host;
    host = gethostbyname(hostname);
    if (host == NULL) {
        results->data[0] = WASM_I32_VAL(2);  // :EnumDependent
        return NULL;
    }

    struct sockaddr_in server_addr;
    memset(&server_addr, 0, sizeof(server_addr));

    server_addr.sin_family = AF_INET; // See comment above
    memcpy((char *)&server_addr.sin_addr.s_addr, (char *)host->h_addr, host->h_length);
    server_addr.sin_port = htons(params->data[3].of.i32);

    int result = connect(params->data[0].of.i32, &server_addr, sizeof(server_addr));
    if (result == 0) results->data[0] = WASM_I32_VAL(0);
    else             results->data[0] = WASM_I32_VAL(3); // :EnumDependent

    return NULL;
    #endif

    #ifdef _BH_WINDOWS
    #endif
}

ONYX_DEF(__net_send, (WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {
    #ifdef _BH_LINUX
    // TODO: The flags at the end should be controllable.
    int sent = send(params->data[0].of.i32, ONYX_PTR(params->data[1].of.i32), params->data[2].of.i32, MSG_NOSIGNAL);
    results->data[0] = WASM_I32_VAL(sent);
    #endif
    
    return NULL;
}

ONYX_DEF(__net_sendto, (WASM_I32, WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {
    #ifdef _BH_LINUX
    struct sockaddr_in dest_addr;
    int dest_addr_len = sizeof(dest_addr);
    memset(&dest_addr, 0, dest_addr_len);

    struct onyx_socket_addr *o_addr = (struct onyx_socket_addr *) ONYX_PTR(params->data[3].of.i32);
    dest_addr.sin_family = AF_INET; // TODO: See other comments related to AF_NET above.
    dest_addr.sin_port = htons(o_addr->port);
    dest_addr.sin_addr.s_addr = htonl(o_addr->addr);

    // TODO: The flags at the end should be controllable.
    int sent = sendto(params->data[0].of.i32, ONYX_PTR(params->data[1].of.i32), params->data[2].of.i32, MSG_NOSIGNAL, &dest_addr, dest_addr_len);
    results->data[0] = WASM_I32_VAL(sent);
    #endif
    
    return NULL;
}

ONYX_DEF(__net_recv, (WASM_I32, WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {
    *(i32 *) ONYX_PTR(params->data[3].of.i32) = 0;

    #ifdef _BH_LINUX
    // TODO: The flags at the end should be controllable.
    int received = recv(params->data[0].of.i32, ONYX_PTR(params->data[1].of.i32), params->data[2].of.i32, 0);
    results->data[0] = WASM_I32_VAL(received);

    if (received < 0) {
        if (errno == EAGAIN || errno == EWOULDBLOCK) {
            *(i32 *) ONYX_PTR(params->data[3].of.i32) = 1;
        }
    }
    #endif

    return NULL;
}

ONYX_DEF(__net_recvfrom, (WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {
    *(i32 *) ONYX_PTR(params->data[4].of.i32) = 0;

    #ifdef _BH_LINUX
    struct onyx_socket_addr *out_addr = (struct onyx_socket_addr *) ONYX_PTR(params->data[3].of.i32);

    struct sockaddr_in client_addr;
    int socket_len = sizeof(client_addr);
    memset(&client_addr, 0, socket_len);

    int received = recvfrom(params->data[0].of.i32, ONYX_PTR(params->data[1].of.i32), params->data[2].of.i32, 0, &client_addr, &socket_len);
    out_addr->family = client_addr.sin_family;
    out_addr->port   = ntohs(client_addr.sin_port);
    out_addr->addr   = ntohl(client_addr.sin_addr.s_addr);

    results->data[0] = WASM_I32_VAL(received);

    if (received < 0) {
        if (errno == EAGAIN || errno == EWOULDBLOCK) {
            *(i32 *) ONYX_PTR(params->data[3].of.i32) = 1;
        }
    }
    #endif

    return NULL;
}

ONYX_DEF(__net_poll_recv, (WASM_I32, WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {
    #ifdef _BH_LINUX
    int i, res, cursor;
    struct pollfd* fds;

    fds = alloca(params->data[1].of.i32 * 8); // Guessed size of pollfd

    for (i=0; i<params->data[1].of.i32; i++) {
        fds[i].fd = *(i32 *) ONYX_PTR(params->data[0].of.i32 + 4 * i);
        fds[i].events = POLLIN;
        fds[i].revents = 0;
    }

    res = poll(fds, params->data[1].of.i32, params->data[2].of.i32);

    cursor = 0;
    for (i=0; i<params->data[1].of.i32; i++) {
        if (fds[i].revents & POLLIN) {
            *(i32 *) ONYX_PTR(params->data[3].of.i32 + 4 * cursor) = i;
            cursor++;
        }
    }

    results->data[0] = WASM_I32_VAL(cursor);
    #endif

    return NULL;
}

ONYX_DEF(__net_host_to_net_s, (WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(htons(params->data[0].of.i32));
    return NULL;
}

ONYX_DEF(__net_host_to_net_l, (WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(htonl(params->data[0].of.i32));
    return NULL;
}

ONYX_DEF(__net_net_to_host_s, (WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(ntohs(params->data[0].of.i32));
    return NULL;
}

ONYX_DEF(__net_net_to_host_l, (WASM_I32), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(ntohl(params->data[0].of.i32));
    return NULL;
}


//
// C-Pointers
//
// These are wildly unsafe and break the core principles of the security
// of WebAssembly, so there should be a way to turn them off!
//
ONYX_DEF(__cptr_make, (WASM_I32), (WASM_I64)) {
    wasm_val_init_ptr(&results->data[0], ONYX_PTR(params->data[0].of.i32));
    return NULL;
}

ONYX_DEF(__cptr_read, (WASM_I64, WASM_I32, WASM_I32), ()) {
    memcpy(ONYX_PTR(params->data[1].of.i32), (void *) params->data[0].of.i64, params->data[2].of.i32);
    return NULL;
}

ONYX_DEF(__cptr_read_u8, (WASM_I64), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(*(u8 *) params->data[0].of.i64);
    return NULL;
}

ONYX_DEF(__cptr_read_u16, (WASM_I64), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(*(u16 *) params->data[0].of.i64);
    return NULL;
}

ONYX_DEF(__cptr_read_u32, (WASM_I64), (WASM_I32)) {
    results->data[0] = WASM_I32_VAL(*(u32 *) params->data[0].of.i64);
    return NULL;
}

ONYX_DEF(__cptr_read_u64, (WASM_I64), (WASM_I64)) {
    results->data[0] = WASM_I64_VAL(*(u64 *) params->data[0].of.i64);
    return NULL;
}


ONYX_LIBRARY {
    ONYX_FUNC(__file_open_impl)
    ONYX_FUNC(__file_close)
    ONYX_FUNC(__file_exists)
    ONYX_FUNC(__file_remove)
    ONYX_FUNC(__file_seek)
    ONYX_FUNC(__file_tell)
    ONYX_FUNC(__file_read)
    ONYX_FUNC(__file_write)
    ONYX_FUNC(__file_flush)
    ONYX_FUNC(__file_size)
    ONYX_FUNC(__file_get_standard)
    ONYX_FUNC(__file_rename)

    ONYX_FUNC(__dir_open)
    ONYX_FUNC(__dir_read)
    ONYX_FUNC(__dir_close)
    ONYX_FUNC(__dir_create)
    ONYX_FUNC(__dir_remove)

    ONYX_FUNC(__spawn_thread)
    ONYX_FUNC(__kill_thread)

    ONYX_FUNC(__process_spawn)
    ONYX_FUNC(__process_read)
    ONYX_FUNC(__process_write)
    ONYX_FUNC(__process_kill)
    ONYX_FUNC(__process_wait)
    ONYX_FUNC(__process_destroy)

    ONYX_FUNC(__args_get)
    ONYX_FUNC(__args_sizes_get)

    ONYX_FUNC(__exit)
    ONYX_FUNC(__sleep)
    ONYX_FUNC(__time)

    ONYX_FUNC(__net_create_socket)
    ONYX_FUNC(__net_close_socket)
    ONYX_FUNC(__net_setting)
    ONYX_FUNC(__net_bind)
    ONYX_FUNC(__net_listen)
    ONYX_FUNC(__net_accept)
    ONYX_FUNC(__net_connect)
    ONYX_FUNC(__net_send)
    ONYX_FUNC(__net_sendto)
    ONYX_FUNC(__net_recv)
    ONYX_FUNC(__net_recvfrom)
    ONYX_FUNC(__net_poll_recv)
    ONYX_FUNC(__net_host_to_net_s)
    ONYX_FUNC(__net_host_to_net_l)
    ONYX_FUNC(__net_net_to_host_s)
    ONYX_FUNC(__net_net_to_host_l)

    ONYX_FUNC(__cptr_make)
    ONYX_FUNC(__cptr_read)
    ONYX_FUNC(__cptr_read_u8)
    ONYX_FUNC(__cptr_read_u16)
    ONYX_FUNC(__cptr_read_u32)
    ONYX_FUNC(__cptr_read_u64)

    NULL
};
