
//
// THREADS
//
typedef struct OnyxThread {
    i32 id;
    i32 tls_base;
    i32 stack_base;
    i32 funcidx;
    i32 closureptr;
    i32 dataptr;
    wasm_instance_t* instance;

    #if defined(_BH_LINUX) || defined(_BH_DARWIN)
        pthread_t thread;
    #endif

    #ifdef _BH_WINDOWS
        HANDLE thread_handle;
        i32    thread_id;
    #endif
} OnyxThread;

static bh_arr(OnyxThread) threads = NULL;

#if defined(_BH_LINUX) || defined(_BH_DARWIN)
static void *onyx_run_thread(void *data) {
#endif
#ifdef _BH_WINDOWS
static i32 onyx_run_thread(void *data) {
#endif
    OnyxThread *thread = (OnyxThread *) data;

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
        wasm_val_t args[6]    = {
            WASM_I32_VAL(thread_id),
            WASM_I32_VAL(thread->tls_base),
            WASM_I32_VAL(thread->stack_base),
            WASM_I32_VAL(thread->funcidx),
            WASM_I32_VAL(thread->closureptr),
            WASM_I32_VAL(thread->dataptr)
        };
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

    runtime->wasm_instance_delete(thread->instance);

    return 0;
}

ONYX_DEF(__spawn_thread, (WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {
    if (threads == NULL) bh_arr_new(bh_heap_allocator(), threads, 128);
    bh_arr_insert_end(threads, 1);
    OnyxThread *thread = &bh_arr_last(threads);

    thread->id         = params->data[0].of.i32;
    thread->tls_base   = params->data[1].of.i32;
    thread->stack_base = params->data[2].of.i32;
    thread->funcidx    = params->data[3].of.i32;
    thread->closureptr = params->data[4].of.i32;
    thread->dataptr    = params->data[5].of.i32;

    wasm_trap_t* traps = NULL;
    thread->instance = runtime->wasm_instance_new(runtime->wasm_store, runtime->wasm_module, &runtime->wasm_imports, &traps);
    assert(thread->instance);

    #if defined(_BH_LINUX) || defined(_BH_DARWIN)
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
            #if defined(_BH_LINUX) || defined(_BH_DARWIN)
            // This leads to some weirdness and bugs...
            //
            // pthread_kill(thread->thread, SIGKILL);
            pthread_cancel(thread->thread);
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

