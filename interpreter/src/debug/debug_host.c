
#include "ovm_debug.h"
#include "vm.h"

void debug_host_init(debug_state_t *debug, struct ovm_engine_t *ovm_engine) {
    memset(debug, 0, sizeof(*debug));
    debug->alloc = bh_heap_allocator();
    debug->ovm_engine = ovm_engine;

    bh_arena_init(&debug->tmp_arena, bh_heap_allocator(), 16 * 1024);
    debug->tmp_alloc = bh_arena_allocator(&debug->tmp_arena);

    debug->info = NULL;

    debug->threads = NULL;
    debug->next_thread_id = 1;
    bh_arr_new(debug->alloc, debug->threads, 4);

    debug->listen_socket_fd = 0;
    debug->client_fd = 0;

    int *pipes = (int *)debug->state_change_pipes;
    if (pipe(pipes) != 0) {
        printf("[ERROR] Failed to create thread notification pipes.\n");
    }
}

void debug_host_start(debug_state_t *debug) {
    if (debug->debug_thread_running) return;

    pthread_create(&debug->debug_thread, NULL, __debug_thread_entry, debug);
}

void debug_host_stop(debug_state_t *debug) {
    debug->debug_thread_running = false;
    pthread_join(debug->debug_thread, NULL);
}

u32 debug_host_register_thread(debug_state_t *debug, ovm_state_t *ovm_state) {
    debug_thread_state_t *new_thread = bh_alloc(debug->alloc, sizeof(*new_thread));
    memset(new_thread, 0, sizeof(*new_thread));

    new_thread->state = debug_state_starting;
    new_thread->ovm_state = ovm_state;
    new_thread->run_count = 0;                    // Start threads in stopped state.

    u32 id = debug->next_thread_id++;
    new_thread->id = id;

    char name_buf[256];
    bh_bprintf(name_buf, 256, "/ovm_thread_%d", new_thread->id);
    new_thread->wait_semaphore = semaphore_create(name_buf, O_CREAT, 0664, 0);

    new_thread->state_change_write_fd = debug->state_change_pipes[1];

    bh_arr_push(debug->threads, new_thread);
    return id;
}

debug_thread_state_t *debug_host_lookup_thread(debug_state_t *debug, u32 id) {
    bh_arr_each(debug_thread_state_t *, pthread, debug->threads) {
        if ((*pthread)->id == id) return *pthread;
    }
    return NULL;
}

