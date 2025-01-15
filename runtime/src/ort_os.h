
//
// OS Things
//

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
    #if defined(_BH_LINUX) || defined(_BH_DARWIN)
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

// ([] PollDescription, timeout: i32) -> void
// PollDescription :: struct { fd: i64; in_event: PollEvent; out_event: PollEvent; }
ONYX_DEF(__poll, (WASM_I32, WASM_I32, WASM_I32), ()) {
    #if defined(_BH_LINUX) || defined(_BH_DARWIN)
    struct pollfd* fds = alloca(params->data[1].of.i32 * sizeof(struct pollfd));

    for (int i=0; i < params->data[1].of.i32; i++) {
        fds[i].fd = *(i64 *) ONYX_PTR(params->data[0].of.i32 + 16 * i);
        fds[i].revents = 0;
        fds[i].events = 0;

        switch (*(i32 *) ONYX_PTR(params->data[0].of.i32 + 16 * i + 8)) {
            case 0x01: // Read
                fds[i].events = POLLIN;
                break;
            
            case 0x02: // Write
                fds[i].events = POLLOUT;
                break;
        }
    }

    int res = poll(fds, params->data[1].of.i32, params->data[2].of.i32);

    for (int i=0; i<params->data[1].of.i32; i++) {
        *(i32 *) ONYX_PTR(params->data[0].of.i32 + 16 * i + 12) = 0; // NO_CHANGE

        if (fds[i].revents & POLLIN) {
            *(i32 *) ONYX_PTR(params->data[0].of.i32 + 16 * i + 12) = 1; // READABLE
        }

        if (fds[i].revents & POLLOUT) {
            *(i32 *) ONYX_PTR(params->data[0].of.i32 + 16 * i + 12) = 2; // WRITABLE
        }

        if ((fds[i].revents & POLLHUP)
            || (fds[i].revents & POLLNVAL)
            || (fds[i].revents & POLLERR)) {
            *(i32 *) ONYX_PTR(params->data[0].of.i32 + 16 * i + 12) = 3; // CLOSED
        }
    }
    #endif

    return NULL;
}


ONYX_DEF(__lookup_env, (WASM_I32, WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {

    #if defined(_BH_LINUX) || defined(_BH_DARWIN)
    char *key_ptr = ONYX_PTR(params->data[0].of.i32);
    int   key_len = params->data[1].of.i32;
    char *out_ptr = ONYX_PTR(params->data[2].of.i32);
    int   out_len = params->data[3].of.i32;

    char key[512] = {0};
    key_len = bh_min(key_len, 511);
    strncpy(key, key_ptr, key_len);
    key[key_len] = 0;

    char * value = getenv(key);
    if (!value) {
        results->data[0] = WASM_I32_VAL(0);
    } else {
        out_len = bh_min(out_len, strlen(value));
        memcpy(out_ptr, value, out_len);
        results->data[0] = WASM_I32_VAL(out_len);
    }
    #endif

    #ifdef _BH_WINDOWS
    results->data[0] = WASM_I32_VAL(0);
    #endif
    return NULL;
}


ONYX_DEF(__random_get, (WASM_PTR, WASM_I32), ()) {
    #if defined(_BH_LINUX)
    getrandom(ONYX_PTR(params->data[0].of.i32), params->data[1].of.i32, 0);
    #endif

    #if defined(_BH_DARWIN)
    SecRandomCopyBytes(NULL, params->data[1].of.i32, ONYX_PTR(params->data[0].of.i32));
    #endif

    #ifdef _BH_WINDOWS
    BCRYPT_ALG_HANDLE alg;
    BCryptOpenAlgorithmProvider(&alg, L"SHA256", NULL, 0);
    BCryptGenRandom(alg, ONYX_PTR(params->data[0].of.i32), params->data[1].of.i32, 0);
    BCryptCloseAlgorithmProvider(alg, 0);
    #endif

    return NULL;
}


ONYX_DEF(__futex_wait, (WASM_PTR, WASM_I32, WASM_I32), (WASM_I32)) {
    int *addr = ONYX_PTR(params->data[0].of.i32);

    #if defined(_BH_LINUX)
    struct timespec delay;

    struct timespec *t = NULL;
    if (params->data[2].of.i32 >= 0) {
        delay.tv_sec  = params->data[2].of.i32 / 1000;
        delay.tv_nsec = (params->data[2].of.i32 % 1000) * 1000000;
        t = &delay;
    }

    int res = syscall(SYS_futex, addr, FUTEX_WAIT | FUTEX_PRIVATE_FLAG, params->data[1].of.i32, t, NULL, 0);

    if (res == 0) {
        if (*addr == params->data[1].of.i32) results->data[0] = WASM_I32_VAL(0);
        else                                 results->data[0] = WASM_I32_VAL(1);
    }
    if (res == -1) {
        if (errno == EAGAIN) results->data[0] = WASM_I32_VAL(2);
        else                 results->data[0] = WASM_I32_VAL(1);
    }
    #endif

    #ifdef _BH_WINDOWS
    results->data[0] = WASM_I32_VAL(WaitOnAddress(addr, &params->data[1].of.i32, 4, params->data[2].of.i32));
    #endif

    #ifdef _BH_DARWIN
    int64_t timeout = params->data[2].of.i32;
    if (timeout >= 0) timeout *= 1000;
    else              timeout  = -1;

    extern int __ulock_wait(int operation, void *addr, long long value, int64_t timeout);
    int res = __ulock_wait(1 /* UL_COMPARE_AND_WAIT */, addr, params->data[1].of.i32, timeout);

    if (res == 0) {
        if (*addr == params->data[1].of.i32) results->data[0] = WASM_I32_VAL(0);
        else                                 results->data[0] = WASM_I32_VAL(1);

    } else {
        results->data[0] = WASM_I32_VAL(2);
    }
    #endif

    return NULL;
}

ONYX_DEF(__futex_wake, (WASM_PTR, WASM_I32), (WASM_I32)) {
    int *addr = ONYX_PTR(params->data[0].of.i32);

    #if defined(_BH_LINUX)
    int res = syscall(SYS_futex, addr, FUTEX_WAKE | FUTEX_PRIVATE_FLAG, params->data[1].of.i32, NULL, NULL, 0);

    results->data[0] = WASM_I32_VAL(res);
    #endif

    #ifdef _BH_WINDOWS
    for (int i=0; i<params->data[1].of.i32; i++) {
        WakeByAddressSingle(addr);
    }

    results->data[0] = WASM_I32_VAL(params->data[1].of.i32);
    #endif

    #ifdef _BH_DARWIN
    int op = 1 /* UL_COMPARE_AND_WAIT */;
    if (params->data[1].of.i32 > 1) {
        op |= 0x100 /* UL_WAKE_ALL */;
    }

    extern int __ulock_wake(int operation, void *addr, long long value);
    int res = __ulock_wake(op, addr, 0);

    results->data[0] = WASM_I32_VAL(res);
    #endif

    return NULL;
}



#if defined(_BH_LINUX)
static wasm_func_t *wasm_cleanup_func;

static void unix_signal_handler(int signo, siginfo_t *info, void *context) {
    wasm_val_vec_t args = WASM_EMPTY_VEC;
    wasm_val_vec_t results = WASM_EMPTY_VEC;
    runtime->wasm_func_call(wasm_cleanup_func, &args, &results);
}
#endif

ONYX_DEF(__register_cleanup, (WASM_I32, WASM_I32), (WASM_I32)) {
    #if defined(_BH_LINUX)

    int len = (127 < params->data[1].of.i32 ? 127 : params->data[1].of.i32);
    char name[128];
    memcpy(name, ONYX_PTR(params->data[0].of.i32), len);
    name[len] = '\0';

    wasm_extern_t *signal_extern = runtime->wasm_extern_lookup_by_name(
                runtime->wasm_module,
                runtime->wasm_instance,
                name);

    wasm_cleanup_func = runtime->wasm_extern_as_func(signal_extern);
    if (!wasm_cleanup_func) {
        results->data[0] = WASM_I32_VAL(0);
        return NULL;
    }

    // This is probably not the most complete list, but seems
    // sufficient for now.
    if (
        (signal(SIGSEGV, &unix_signal_handler) == SIG_ERR) || 
        (signal(SIGQUIT, &unix_signal_handler) == SIG_ERR) || 
        (signal(SIGINT, &unix_signal_handler) == SIG_ERR) || 
        (signal(SIGPIPE, &unix_signal_handler) == SIG_ERR) || 
        (signal(SIGTERM, &unix_signal_handler) == SIG_ERR) || 
        (signal(SIGHUP, &unix_signal_handler) == SIG_ERR) || 
        (signal(SIGFPE, &unix_signal_handler) == SIG_ERR))
    {
        results->data[0] = WASM_I32_VAL(0);
        return NULL;
    }

    results->data[0] = WASM_I32_VAL(1);
    return NULL;
    #endif

    #ifdef _BH_WINDOWS
    #endif
}

