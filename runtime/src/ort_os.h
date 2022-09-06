
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

