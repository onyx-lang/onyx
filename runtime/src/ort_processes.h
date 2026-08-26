
//
// PROCESS
//

#define ONYX_PARAM(index, otype, type, var) \
    type var = params->data[index].of.otype

#define ONYX_PARAM_PTR(index, type, var) \
    type var = ONYX_PTR(params->data[index].of.i32)

#define COPY_TO_BUF(bufname, maxlen, tocopy, copylen) \
    char bufname[maxlen]; \
    memcpy(bufname, tocopy, copylen); \
    bufname[copylen] = '\0'

#define ONYX_PROCESS_MAGIC_NUMBER 0xdeadfadebabecafe
typedef struct OnyxProcess {
    u64 magic_number;

#if defined(_BH_LINUX) || defined(_BH_DARWIN)
    // Pipes
    i32 proc_to_host[2];
    i32 host_to_proc[2];
    i32 proc_err_to_host[2];

    pid_t pid;
#endif

#ifdef _BH_WINDOWS
    HANDLE proc_to_host_read;
    HANDLE proc_to_host_write;
    HANDLE host_to_proc_read;
    HANDLE host_to_proc_write;
    HANDLE proc_err_to_host_read;
    HANDLE proc_err_to_host_write;

    PROCESS_INFORMATION proc_info;
#endif
} OnyxProcess;

typedef struct ProcessSpawnOpts {
    u8 capture_io;
    u8 non_blocking_io;
    u8 detach;
    i32 dir_ptr;
    i32 dir_len;
} ProcessSpawnOpts;

ONYX_DEF(__process_spawn, (WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32), (WASM_I64)) {
    ONYX_PARAM_PTR(0, char *, process_str);
    ONYX_PARAM    (1, i32, i32, process_len);
    ONYX_PARAM    (2, i32, i32, args_ptr);
    ONYX_PARAM    (3, i32, i32, args_len);
    ONYX_PARAM_PTR(4, char **, env);
    ONYX_PARAM_PTR(5, ProcessSpawnOpts *, spawn_opts);

    char *cwd_str = ONYX_PTR(spawn_opts->dir_ptr);
    i32   cwd_len = spawn_opts->dir_len;

    COPY_TO_BUF(process_path, 1024, process_str, bh_min(1023, process_len));
    COPY_TO_BUF(starting_dir, 1024, cwd_str, bh_min(1023, cwd_len));

    OnyxProcess *process = malloc(sizeof(OnyxProcess));
    memset(process, 0, sizeof(*process));
    process->magic_number = ONYX_PROCESS_MAGIC_NUMBER;
#if defined(_BH_LINUX) || defined(_BH_DARWIN)
    process->proc_to_host[0] = process->proc_to_host[1] = -1;
    process->host_to_proc[0] = process->host_to_proc[1] = -1;
    process->proc_err_to_host[0] = process->proc_err_to_host[1] = -1;
#endif
#ifdef _BH_WINDOWS
    process->proc_to_host_read = process->proc_to_host_write = INVALID_HANDLE_VALUE;
    process->host_to_proc_read = process->host_to_proc_write = INVALID_HANDLE_VALUE;
    process->proc_err_to_host_read = process->proc_err_to_host_write = INVALID_HANDLE_VALUE;
#endif

    #if defined(_BH_LINUX) || defined(_BH_DARWIN)
        // :Security - alloca a user controlled string.
        char **process_args = alloca(sizeof(char *) * (args_len + 2));

        byte_t* array_loc = ONYX_PTR(args_ptr);
        fori (i, 0, args_len) {
            i32 *arg_str_loc = array_loc + i * 2 * POINTER_SIZE;
            i32 *arg_len_loc = arg_str_loc + 1;

            char *arg_str = ONYX_PTR(*arg_str_loc);
            i32   arg_len = *arg_len_loc;

            char *arg = alloca(sizeof(char) * (arg_len + 1));
            memcpy(arg, arg_str, arg_len);
            arg[arg_len] = '\0';
            process_args[i + 1] = arg;
        }

        process_args[0] = process_path;
        process_args[args_len + 1] = NULL;

        if (spawn_opts->capture_io) {
            if (pipe(process->proc_to_host) || pipe(process->host_to_proc) || pipe(process->proc_err_to_host)) {
                wasm_val_init_ptr(&results->data[0], NULL); // Failed to run
                return NULL;
            }
        }

        pid_t pid;
        switch (pid = fork()) {
            case -1: // Bad fork
                wasm_val_init_ptr(&results->data[0], NULL); // Failed to run
                
                if (spawn_opts->capture_io) {
                    close(process->proc_to_host[0]);
                    close(process->proc_to_host[1]);
                    close(process->host_to_proc[0]);
                    close(process->host_to_proc[1]);
                    close(process->proc_err_to_host[0]);
                    close(process->proc_err_to_host[1]);
                }
                break;

            case 0: // Child process
                if (spawn_opts->detach) {
                    setsid();

                } else {
                    if (spawn_opts->capture_io) {
                        close(process->proc_to_host[0]);
                        close(process->host_to_proc[1]);
                        close(process->proc_err_to_host[0]);

                        dup2(process->host_to_proc[0], 0);
                        dup2(process->proc_to_host[1], 1);
                        dup2(process->proc_err_to_host[1], 2);
                    }

                    if (spawn_opts->non_blocking_io && spawn_opts->capture_io) {
                        fcntl(0, F_SETFL, O_NONBLOCK);
                        fcntl(1, F_SETFL, O_NONBLOCK);
                    }
                }

                if (cwd_len > 0) {
                    chdir(starting_dir); // Switch current working directory.
                }

                if (env) {
                    char **env_walker = env;

                    while (*env_walker) {
                        // We have to duplicate the string here
                        // because there is disagreance on whether
                        // putenv copies its input or not. To be safe
                        // we copy it into the heap, which may be copied
                        // again by putenv :|
                        char *envvar = bh_strdup(bh_heap_allocator(), *env_walker);
                        putenv(envvar);

                        env_walker++;
                    }
                }

                execvp(process_path, process_args);
                exit(-1);
                break;

            default: {
                process->pid = pid;

                if (spawn_opts->capture_io) {
                    close(process->host_to_proc[0]);
                    close(process->proc_to_host[1]);
                    close(process->proc_err_to_host[1]);
                }

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
        startup.dwFlags |= STARTF_USESHOWWINDOW;
        startup.wShowWindow = 0;

        SECURITY_ATTRIBUTES saAttr;
        saAttr.nLength = sizeof(SECURITY_ATTRIBUTES);
        saAttr.lpSecurityDescriptor = NULL;
        saAttr.bInheritHandle = 1;

        i32 success = 1;
        if (spawn_opts->capture_io) {
            success = success && CreatePipe(&process->host_to_proc_read, &process->host_to_proc_write, &saAttr, 4096);
            success = success && CreatePipe(&process->proc_to_host_read, &process->proc_to_host_write, &saAttr, 4096);
            success = success && CreatePipe(&process->proc_err_to_host_read, &process->proc_err_to_host_write, &saAttr, 4096);
            if (!success) {
                wasm_val_init_ptr(&results->data[0], NULL);
                return NULL;
            }

            success = SetHandleInformation(process->proc_to_host_read, 1 /* HANDLE_FLAG_INHERIT */, 0);
            success = success && SetHandleInformation(process->host_to_proc_write, 1 /* HANDLE_FLAG_INHERIT */, 0);
            success = success && SetHandleInformation(process->proc_err_to_host_read, 1 /* HANDLE_FLAG_INHERIT */, 0);
            if (!success) {
                wasm_val_init_ptr(&results->data[0], NULL);
                return NULL;
            }

            startup.hStdInput  = process->host_to_proc_read;
            startup.hStdOutput = process->proc_to_host_write;
            startup.hStdError  = process->proc_err_to_host_write;

            startup.dwFlags |= STARTF_USESTDHANDLES;
        }

        memset(&process->proc_info, 0, sizeof process->proc_info);

        char *working_dir = NULL;
        if (cwd_len > 0) {
            working_dir = starting_dir;
        }

        success = CreateProcessA(NULL, cmdLine, &saAttr, &saAttr, 1, 0, env, working_dir, &startup, &process->proc_info);
        if (!success) {
            wasm_val_init_ptr(&results->data[0], NULL); // Failed to run @LEAK
            return NULL;
        }

        if (spawn_opts->capture_io) {
            CloseHandle(process->proc_to_host_write);
            CloseHandle(process->host_to_proc_read);
            CloseHandle(process->proc_err_to_host_write);
            process->proc_to_host_write = INVALID_HANDLE_VALUE;
            process->host_to_proc_read = INVALID_HANDLE_VALUE;
            process->proc_err_to_host_write = INVALID_HANDLE_VALUE;
        }

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
    #if defined(_BH_LINUX) || defined(_BH_DARWIN)
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

ONYX_DEF(__process_read_stderr, (WASM_I64, WASM_I32, WASM_I32), (WASM_I32)) {
    OnyxProcess *process = (OnyxProcess *) params->data[0].of.i64;
    if (process == NULL || process->magic_number != ONYX_PROCESS_MAGIC_NUMBER) {
        results->data[0] = WASM_I32_VAL(0);
        return NULL;
    }

    i32 output_ptr = params->data[1].of.i32;
    i32 output_len = params->data[2].of.i32;
    u8 *buffer = ONYX_PTR(output_ptr);

    i32 bytes_read;
    #if defined(_BH_LINUX) || defined(_BH_DARWIN)
        if (process->proc_err_to_host[0] < 0) {
            results->data[0] = WASM_I32_VAL(-1);
            return NULL;
        }
        bytes_read = read(process->proc_err_to_host[0], buffer, output_len);
        if (bytes_read < 0) {
            switch (errno) {
                case EAGAIN: bytes_read =  0; break;
                case EBADF:  bytes_read = -1; break;
                default:     bytes_read = -2; break;
            }
        }
    #endif

    #ifdef _BH_WINDOWS
        if (process->proc_err_to_host_read == INVALID_HANDLE_VALUE) {
            results->data[0] = WASM_I32_VAL(-1);
            return NULL;
        }
        i32 success = ReadFile(process->proc_err_to_host_read, buffer, output_len, &bytes_read, NULL);
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
    #if defined(_BH_LINUX) || defined(_BH_DARWIN)
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

    #if defined(_BH_LINUX) || defined(_BH_DARWIN)
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
        results->data[0] = WASM_I32_VAL(-1); // FailedToRun
        return NULL;
    }

    #if defined(_BH_LINUX) || defined(_BH_DARWIN)
        i32 status;
        waitpid(process->pid, &status, 0);

        i32 exit_code = 1;
        if (WIFEXITED(status)) {
            exit_code = WEXITSTATUS(status);
        } else if (WIFSIGNALED(status)) {
            exit_code = 128 + WTERMSIG(status);
        }
        results->data[0] = WASM_I32_VAL(exit_code);
    #endif

    #ifdef _BH_WINDOWS
        DWORD exitCode;
        while (1) {
            WaitForSingleObject(process->proc_info.hProcess, INFINITE);

            if (!GetExitCodeProcess(process->proc_info.hProcess, &exitCode)) {
                if (GetLastError() == 6) {
                    exitCode = 0;
                    break;
                }

                results->data[0] = WASM_I32_VAL(-2); // InternalErr
                return NULL;
            }

            // 259 is STILL_ACTIVE (aka STATUS_PENDING)
            if (exitCode != 259) break;
        }

        results->data[0] = WASM_I32_VAL((i32) exitCode);
    #endif

    return NULL;
}

ONYX_DEF(__process_destroy, (WASM_I64), ()) {
    OnyxProcess *process = (OnyxProcess *) params->data[0].of.i64;
    if (process == NULL || process->magic_number != ONYX_PROCESS_MAGIC_NUMBER) {
        return NULL;
    }

    #if defined(_BH_LINUX) || defined(_BH_DARWIN)
        if (process->proc_to_host[0] >= 0) close(process->proc_to_host[0]);
        if (process->host_to_proc[1] >= 0) close(process->host_to_proc[1]);
        if (process->proc_err_to_host[0] >= 0) close(process->proc_err_to_host[0]);
    #endif

    #ifdef _BH_WINDOWS
        if (process->proc_to_host_read != INVALID_HANDLE_VALUE) CloseHandle(process->proc_to_host_read);
        if (process->host_to_proc_write != INVALID_HANDLE_VALUE) CloseHandle(process->host_to_proc_write);
        if (process->proc_err_to_host_read != INVALID_HANDLE_VALUE) CloseHandle(process->proc_err_to_host_read);
        if (process->proc_info.hThread) CloseHandle(process->proc_info.hThread);
        if (process->proc_info.hProcess) CloseHandle(process->proc_info.hProcess);
    #endif

    free(process);

    return NULL;
}
