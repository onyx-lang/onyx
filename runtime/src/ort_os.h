
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


ONYX_DEF(__lookup_env, (WASM_I32, WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {

    #ifdef _BH_LINUX
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





#ifdef _BH_LINUX
static wasm_func_t *wasm_cleanup_func;

static void unix_signal_handler(int signo, siginfo_t *info, void *context) {
    wasm_val_vec_t args = WASM_EMPTY_VEC;
    wasm_val_vec_t results = WASM_EMPTY_VEC;
    runtime->wasm_func_call(wasm_cleanup_func, &args, &results);
}
#endif

ONYX_DEF(__register_cleanup, (WASM_I32, WASM_I32), (WASM_I32)) {
    #ifdef _BH_LINUX

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

