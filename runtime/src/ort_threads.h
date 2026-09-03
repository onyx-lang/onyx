
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

// Pointers, not inline structs: workers keep an OnyxThread* for their
// whole life. Joining used to bh_arr_deleten a packed array and move
// the remaining records, which segfaulted other workers.
static bh_arr(OnyxThread *) threads = NULL;

#if defined(_BH_LINUX) || defined(_BH_DARWIN)
static pthread_mutex_t threads_lock = PTHREAD_MUTEX_INITIALIZER;
#endif
#ifdef _BH_WINDOWS
static CRITICAL_SECTION threads_lock;
static b32 threads_lock_inited = 0;
static void threads_lock_ensure(void) {
    if (!threads_lock_inited) {
        InitializeCriticalSection(&threads_lock);
        threads_lock_inited = 1;
    }
}
#endif

static void threads_lock_acquire(void) {
    #if defined(_BH_LINUX) || defined(_BH_DARWIN)
        pthread_mutex_lock(&threads_lock);
    #endif
    #ifdef _BH_WINDOWS
        threads_lock_ensure();
        EnterCriticalSection(&threads_lock);
    #endif
}

static void threads_lock_release(void) {
    #if defined(_BH_LINUX) || defined(_BH_DARWIN)
        pthread_mutex_unlock(&threads_lock);
    #endif
    #ifdef _BH_WINDOWS
        LeaveCriticalSection(&threads_lock);
    #endif
}

static OnyxThread *threads_find(i32 thread_id, i32 *out_index) {
    i32 i = 0;
    bh_arr_each(OnyxThread *, slot, threads) {
        if ((*slot)->id == thread_id) {
            if (out_index) *out_index = i;
            return *slot;
        }
        i++;
    }
    return NULL;
}

static void threads_remove_id(i32 thread_id) {
    i32 i = 0;
    bh_arr_each(OnyxThread *, slot, threads) {
        if ((*slot)->id == thread_id) {
            bh_free(bh_heap_allocator(), *slot);
            bh_arr_deleten(threads, i, 1);
            return;
        }
        i++;
    }
}

#if defined(_BH_LINUX) || defined(_BH_DARWIN)
static void *onyx_run_thread(void *data) {
#endif
#ifdef _BH_WINDOWS
static i32 onyx_run_thread(void *data) {
#endif
    OnyxThread *thread = (OnyxThread *) data;

    wasm_instance_t *instance = thread->instance;
    i32 thread_id   = thread->id;
    i32 tls_base    = thread->tls_base;
    i32 stack_base  = thread->stack_base;
    i32 funcidx     = thread->funcidx;
    i32 closureptr  = thread->closureptr;
    i32 dataptr     = thread->dataptr;

    wasm_extern_t* start_extern = runtime->wasm_extern_lookup_by_name(runtime->wasm_module, instance, "_thread_start");
    wasm_func_t*   start_func   = runtime->wasm_extern_as_func(start_extern);

    wasm_extern_t* exit_extern = runtime->wasm_extern_lookup_by_name(runtime->wasm_module, instance, "_thread_exit");
    wasm_func_t*   exit_func   = runtime->wasm_extern_as_func(exit_extern);

    wasm_trap_t* trap=NULL;

    { // Call the _thread_start procedure
        wasm_val_t args[6]    = {
            WASM_I32_VAL(thread_id),
            WASM_I32_VAL(tls_base),
            WASM_I32_VAL(stack_base),
            WASM_I32_VAL(funcidx),
            WASM_I32_VAL(closureptr),
            WASM_I32_VAL(dataptr)
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

    runtime->wasm_instance_delete(instance);

    return 0;
}

ONYX_DEF(__spawn_thread, (WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32, WASM_I32), (WASM_I32)) {
    threads_lock_acquire();

    if (threads == NULL) bh_arr_new(bh_heap_allocator(), threads, 128);

    OnyxThread *thread = bh_alloc_item(bh_heap_allocator(), OnyxThread);
    memset(thread, 0, sizeof *thread);

    thread->id         = params->data[0].of.i32;
    thread->tls_base   = params->data[1].of.i32;
    thread->stack_base = params->data[2].of.i32;
    thread->funcidx    = params->data[3].of.i32;
    thread->closureptr = params->data[4].of.i32;
    thread->dataptr    = params->data[5].of.i32;

    wasm_trap_t* traps = NULL;
    thread->instance = runtime->wasm_instance_new(runtime->wasm_store, runtime->wasm_module, &runtime->wasm_imports, &traps);
    assert(thread->instance);

    bh_arr_push(threads, thread);

    // Interpreter call frames live on the heap; JIT frames live on the OS stack
    // (prologue plus a C helper per CALL). macOS secondary threads default to
    // 512KiB, which overflows depending on which worker draws a deep call path.
    #define ONYX_THREAD_STACK_SIZE (8u * 1024u * 1024u)

    #if defined(_BH_LINUX) || defined(_BH_DARWIN)
        pthread_attr_t attr;
        pthread_attr_init(&attr);
        pthread_attr_setstacksize(&attr, ONYX_THREAD_STACK_SIZE);
        pthread_create(&thread->thread, &attr, onyx_run_thread, thread);
        pthread_attr_destroy(&attr);
    #endif

    #ifdef _BH_WINDOWS
        thread->thread_handle = (HANDLE) _beginthreadex(NULL, ONYX_THREAD_STACK_SIZE, onyx_run_thread, thread, 0, &thread->thread_id);
    #endif

    #undef ONYX_THREAD_STACK_SIZE

    threads_lock_release();

    results->data[0] = WASM_I32_VAL(1);
    return NULL;
}

ONYX_DEF(__join_thread, (WASM_I32), (WASM_I32)) {
    i32 thread_id = params->data[0].of.i32;

    threads_lock_acquire();
    OnyxThread *thread = threads_find(thread_id, NULL);
    #if defined(_BH_LINUX) || defined(_BH_DARWIN)
        pthread_t handle = thread ? thread->thread : 0;
    #endif
    #ifdef _BH_WINDOWS
        HANDLE handle = thread ? thread->thread_handle : NULL;
    #endif
    threads_lock_release();

    if (!thread) {
        results->data[0] = WASM_I32_VAL(0);
        return NULL;
    }

    #if defined(_BH_LINUX) || defined(_BH_DARWIN)
        pthread_join(handle, NULL);
    #endif
    #ifdef _BH_WINDOWS
        if (handle) {
            WaitForSingleObject(handle, INFINITE);
            CloseHandle(handle);
        }
    #endif

    threads_lock_acquire();
    threads_remove_id(thread_id);
    threads_lock_release();

    results->data[0] = WASM_I32_VAL(1);
    return NULL;
}

ONYX_DEF(__kill_thread, (WASM_I32), (WASM_I32)) {
    i32 thread_id = params->data[0].of.i32;

    threads_lock_acquire();

    i32 i = 0;
    OnyxThread *thread = threads_find(thread_id, &i);
    if (!thread) {
        threads_lock_release();
        results->data[0] = WASM_I32_VAL(0);
        return NULL;
    }

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

    bh_free(bh_heap_allocator(), thread);
    bh_arr_deleten(threads, i, 1);
    threads_lock_release();

    results->data[0] = WASM_I32_VAL(1);
    return NULL;
}
