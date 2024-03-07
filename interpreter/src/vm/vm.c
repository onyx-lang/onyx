#define _GNU_SOURCE

#include "vm.h"

#include <sys/mman.h>
#include <signal.h>

#if defined(__arm64__)
    #include <arm_neon.h>
#elif defined(__x86_64__)
    #include <x86intrin.h>
#elif defined(__aarch64__)
#else
    #error "Unsupported architecture"
#endif

// @todo(judah): this may need to change in the future.
#define __ovm_clz(v)        __builtin_clz(v)
#define __ovm_clzll(v)      __builtin_clzll(v)
#define __ovm_ctz(v)        __builtin_ctz(v)
#define __ovm_ctzll(v)      __builtin_ctzll(v)
#define __ovm_popcount(v)   __builtin_popcount(v)
#define __ovm_popcountll(v) __builtin_popcount(v)

#include <math.h> // REMOVE THIS!!!  only needed for sqrt
#include <pthread.h>

#ifdef OVM_DEBUG
#define ovm_assert(c) assert((c))
#else
#define ovm_assert(c)
#endif


static inline void ovm_print_val(ovm_value_t val) {
    switch (val.type) {
        case OVM_TYPE_I32: printf("i32[%d]",   val.i32); break;
        case OVM_TYPE_I64: printf("i64[%lld]", val.i64); break;
        case OVM_TYPE_F32: printf("f32[%f]",   val.f32); break;
        case OVM_TYPE_F64: printf("f64[%lf]",  val.f64); break;
    }
}


//
// Store
ovm_store_t *ovm_store_new() {
    ovm_store_t *store = malloc(sizeof(*store));

    store->heap_allocator = bh_heap_allocator();

    bh_atomic_arena_init(&store->arena, store->heap_allocator, 1 << 20);
    store->arena_allocator = bh_atomic_arena_allocator(&store->arena);

    return store;
}

void ovm_store_delete(ovm_store_t *store) {
    bh_atomic_arena_free(&store->arena);
    free(store);
}


//
// Program
ovm_program_t *ovm_program_new(ovm_store_t *store) {
    ovm_program_t *program = bh_alloc_item(store->heap_allocator, ovm_program_t);
    program->store = store;
    program->register_count = 0;

    program->funcs = NULL;
    program->code  = NULL;
    program->static_integers = NULL;
    program->static_data  = NULL;
    bh_arr_new(store->heap_allocator, program->funcs, 16);
    bh_arr_new(store->heap_allocator, program->code, 1024);
    bh_arr_new(store->heap_allocator, program->static_integers, 128);
    bh_arr_new(store->heap_allocator, program->static_data, 128);

    return program;
}

void ovm_program_delete(ovm_program_t *program) {
    bh_arr_free(program->funcs);
    bh_arr_free(program->code);
    bh_arr_free(program->static_integers);
    bh_arr_free(program->static_data);

    bh_free(program->store->heap_allocator, program);
}

int ovm_program_register_static_ints(ovm_program_t *program, int len, int *data) {
    ovm_static_integer_array_t new_entry;
    new_entry.start_idx = bh_arr_length(program->static_integers);
    new_entry.len = len;

    fori (i, 0, (int) len) {
        bh_arr_push(program->static_integers, data[i]);
    }

    bh_arr_push(program->static_data, new_entry);
    return bh_arr_length(program->static_data) - 1;
}

void ovm_program_modify_static_int(ovm_program_t *program, int arr, int idx, int new_value) {
    if (arr >= bh_arr_length(program->static_data)) return;

    ovm_static_integer_array_t array = program->static_data[arr];
    if (idx >= array.len) return;

    program->static_integers[array.start_idx + idx] = new_value;
}

int ovm_program_register_func(ovm_program_t *program, char *name, i32 instr, i32 param_count, i32 value_number_count) {
    ovm_func_t func;
    func.kind = OVM_FUNC_INTERNAL;
    func.id = bh_arr_length(program->funcs);
    func.name = name;
    func.start_instr = instr;
    func.param_count = param_count;
    func.value_number_count = value_number_count;

    bh_arr_push(program->funcs, func);
    return func.id;
}

int ovm_program_register_external_func(ovm_program_t *program, char *name, i32 param_count, i32 external_func_idx) {
    ovm_func_t func;
    func.kind = OVM_FUNC_EXTERNAL;
    func.id = bh_arr_length(program->funcs);
    func.name = name;
    func.param_count = param_count;
    func.external_func_idx = external_func_idx;
    func.value_number_count = param_count;

    bh_arr_push(program->funcs, func);
    return func.id;
}

void ovm_program_begin_func(ovm_program_t *program, char *name, i32 param_count, i32 value_number_count) {
    ovm_func_t func;
    func.id = bh_arr_length(program->funcs);
    func.name = name;
    func.start_instr = bh_arr_length(program->code);
    func.param_count = param_count;
    func.value_number_count = value_number_count;

    bh_arr_push(program->funcs, func);
}

void ovm_program_add_instructions(ovm_program_t *program, i32 instr_count, ovm_instr_t *instrs) {
    fori (i, 0, instr_count) {
        bh_arr_push(program->code, instrs[i]);
    }
}


//
// Engine
ovm_engine_t *ovm_engine_new(ovm_store_t *store) {
    ovm_engine_t *engine = bh_alloc_item(store->heap_allocator, ovm_engine_t);

    engine->store = store;
    engine->memory_size = 0;
    engine->memory = NULL;
    engine->debug = NULL;
    pthread_mutex_init(&engine->atomic_mutex, NULL);

    //
    // HACK: This should not be necessary, but because moving the memory around
    // causes issues with other standard libraries (like STBTT and STBI), it is
    // better to allocate a large amount here and avoid needing to reallocate.
    // A solution should be possible, because other runtimes like Wasmer support
    // this.
    i64 attempt_to_allocate = 1ull << 32;
    while (!ovm_engine_memory_ensure_capacity(engine, attempt_to_allocate)) {
        attempt_to_allocate /= 2;
    }

    return engine;
}

void ovm_engine_delete(ovm_engine_t *engine) {
    ovm_store_t *store = engine->store;

    if (engine->memory) {
        munmap(engine->memory, engine->memory_size);
    }

    bh_free(store->heap_allocator, engine);
}

static i32 hit_signaled_exception = 0;
static void signal_handler(int signo, siginfo_t *info, void *context) {
    hit_signaled_exception = 1;
}

void ovm_engine_enable_debug(ovm_engine_t *engine, debug_state_t *debug) {
    engine->debug = debug;

    struct sigaction sa;
    sa.sa_sigaction = signal_handler;
    sigemptyset(&sa.sa_mask);
    sa.sa_flags = SA_RESTART | SA_SIGINFO;

    sigaction(SIGQUIT, &sa, NULL);
    sigaction(SIGPIPE, &sa, NULL);
    sigaction(SIGTERM, &sa, NULL);
    sigaction(SIGHUP, &sa, NULL);

    // sigaction(SIGSEGV, &sa, NULL);
    // sigaction(SIGFPE, &sa, NULL);
    // sigaction(SIGINT, &sa, NULL);   Don't overload Ctrl+C
}

bool ovm_engine_memory_ensure_capacity(ovm_engine_t *engine, i64 minimum_size) {
    if (engine->memory_size >= minimum_size) return true;

    void *new_addr;

    if (engine->memory == NULL) {
        new_addr = mmap(NULL, minimum_size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    } else {
        #ifdef _BH_DARWIN // Darwin doesn't support mremap so we need to map and copy it manually
            new_addr = mmap(engine->memory, minimum_size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
            if (new_addr == MAP_FAILED) return false;

            memcpy(new_addr, engine->memory, engine->memory_size);
            munmap(engine->memory, engine->memory_size);
        #elif defined(_BH_LINUX)
            new_addr = mremap(engine->memory, engine->memory_size, minimum_size, MREMAP_MAYMOVE);
        #else
            #error "Unhandled platform."
        #endif
    }

    if (new_addr == MAP_FAILED) {
        // Something went wrong...
        // At this point the program should probably horifically crash.

        return false;
    }

    engine->memory_size = minimum_size;
    engine->memory = new_addr;

    return true;
}

void ovm_engine_memory_copy(ovm_engine_t *engine, i64 target, void *data, i64 size) {
    ovm_assert(engine);
    ovm_assert(engine->memory);
    ovm_assert(data);
    ovm_assert(size + target < engine->memory_size);
    memcpy(((u8 *) engine->memory) + target, data, size);
}



//
// State
//
// This takes in a program because it needs to know how many registers to allocate.
// Should there be another mechanism for this? or is this the most concise way?
ovm_state_t *ovm_state_new(ovm_engine_t *engine, ovm_program_t *program) {
    ovm_store_t *store = engine->store;
    ovm_state_t *state = bh_alloc_item(store->arena_allocator, ovm_state_t);

    state->store = store;
    state->engine = engine;
    state->program = program;
    state->pc = 0;
    state->value_number_offset = 0;

    state->numbered_values = NULL;
    state->stack_frames = NULL;
    state->registers = NULL;
    bh_arr_new(store->heap_allocator, state->numbered_values, 128);
    bh_arr_new(store->heap_allocator, state->stack_frames, 32);
    bh_arr_new(store->heap_allocator, state->registers, program->register_count);
    bh_arr_insert_end(state->registers, program->register_count);

    state->param_buf = bh_alloc_array(store->arena_allocator, ovm_value_t, OVM_MAX_PARAM_COUNT);
    state->param_count = 0;

    state->external_funcs = NULL;
    bh_arr_new(store->heap_allocator, state->external_funcs, 8);

    if (engine->debug) {
        u32 thread_id = debug_host_register_thread(engine->debug, state);
        state->debug = debug_host_lookup_thread(engine->debug, thread_id);
    }

    return state;
}

void ovm_state_delete(ovm_state_t *state) {
    ovm_store_t *store = state->store;

    bh_arr_free(state->numbered_values);
    bh_arr_free(state->stack_frames);
    bh_arr_free(state->registers);
    bh_arr_free(state->external_funcs);
}

void ovm_state_register_external_func(ovm_state_t *state, i32 idx, void (*func)(void *, ovm_value_t *, ovm_value_t *), void *data) {
    ovm_external_func_t external_func;
    external_func.native_func = func;
    external_func.userdata = data;

    bh_arr_set_at(state->external_funcs, idx, external_func);
}

ovm_value_t ovm_state_register_get(ovm_state_t *state, i32 idx) {
    ovm_assert(idx < bh_arr_length(state->registers));

    return state->registers[idx];
}

void ovm_state_register_set(ovm_state_t *state, i32 idx, ovm_value_t val) {
    if (idx >= bh_arr_length(state->registers)) return;

    state->registers[idx] = val;
}

//
// Function calling

static void ovm__func_setup_stack_frame(ovm_state_t *state, ovm_func_t *func, i32 result_number) {
    //
    // Push a stack frame
    ovm_stack_frame_t frame;
    frame.func = func;
    frame.value_number_count = func->value_number_count;
    frame.value_number_base  = bh_arr_length(state->numbered_values);
    frame.return_address = state->pc;
    frame.return_number_value = result_number;
    bh_arr_push(state->stack_frames, frame);

    //
    // Move the base pointer to the value numbers.
    state->value_number_offset = frame.value_number_base;

    //
    // Setup value numbers
    bh_arr_insert_end(state->numbered_values, func->value_number_count);

    state->__frame_values = &state->numbered_values[state->value_number_offset];

    //
    // Modify debug state so step over works
    if (state->debug) {
        state->debug->extra_frames_since_last_pause++;
    }
}

static ovm_stack_frame_t ovm__func_teardown_stack_frame(ovm_state_t *state) {
    ovm_stack_frame_t frame = bh_arr_pop(state->stack_frames);
    bh_arr_fastdeleten(state->numbered_values, frame.value_number_count);

    if (bh_arr_length(state->stack_frames) == 0) {
        state->value_number_offset = 0;
    } else {
        state->value_number_offset = bh_arr_last(state->stack_frames).value_number_base;
    }

    state->__frame_values = &state->numbered_values[state->value_number_offset];

    if (state->debug) {
        state->debug->extra_frames_since_last_pause--;
        if (state->debug->extra_frames_since_last_pause < 0) {
            state->debug->pause_within = -1;
        }
    }

    return frame;
}

ovm_value_t ovm_func_call(ovm_engine_t *engine, ovm_state_t *state, ovm_program_t *program, i32 func_idx, i32 param_count, ovm_value_t *params) {
    ovm_func_t *func = &program->funcs[func_idx];
    ovm_assert(func->value_number_count >= func->param_count);

    state->call_depth += 1;

    switch (func->kind) {
        case OVM_FUNC_INTERNAL: {
            ovm__func_setup_stack_frame(state, func, 0);

            fori (i, 0, param_count) {
                state->numbered_values[i + state->value_number_offset] = params[i];
            }

            state->pc = func->start_instr;
            ovm_value_t result = ovm_run_code(engine, state, program);

            state->call_depth -= 1;
            return result;
        }

        case OVM_FUNC_EXTERNAL: {
            ovm__func_setup_stack_frame(state, func, 0);

            ovm_value_t result = {0};
            ovm_external_func_t external_func = state->external_funcs[func->external_func_idx];
            external_func.native_func(external_func.userdata, params, &result);

            ovm__func_teardown_stack_frame(state);

            state->call_depth -= 1;
            return result;
        }

        default: return (ovm_value_t) {};
    }
}

static inline double __ovm_abs(double f) {
    return f >= 0 ? f : -f;
}

static inline double __ovm_floor(double f) {
    if (f < 0) {
        return (double) (((long long) f) - 1);
    } else {
        return (double) (long long) f;
    }
}

static inline double __ovm_ceil(double f) {
    if (f - (long long) f == 0) {
        return (double) (long long) f;
    } else {
        return __ovm_floor(f) + 1;
    }
}

static inline double __ovm_trunc(double f) {
    return (double) (long long) f;
}

static inline double __ovm_nearest(double f) {
    if (f > 0 && f <= 0.5) {
        return +0;
    }

    if (f >= -0.5 && f < 0) {
        return -0;
    }

    if (f - __ovm_floor(f) < 0.5) return __ovm_floor(f);
    else                          return __ovm_ceil(f);
}

static inline double __ovm_copysign(double a, double b) {
    if ((a > 0 && b > 0) || (a < 0 && b < 0)) return a;
    return -a;
}

static void __ovm_trigger_exception(ovm_state_t *state) {
    if (state->debug) {
        state->debug->state = debug_state_pausing;
        state->debug->pause_reason = debug_pause_exception;

        assert(write(state->debug->state_change_write_fd, "1", 1));
        semaphore_wait(state->debug->wait_semaphore);
    }
}

static void __ovm_debug_hook(ovm_engine_t *engine, ovm_state_t *state) {
    if (!state->debug) return;

    if (hit_signaled_exception) {
        __ovm_trigger_exception(state);
        hit_signaled_exception = 0;
        return;
    }

    if (state->debug->run_count == 0) {
        state->debug->state = debug_state_pausing;

        if (state->debug->started) {
            state->debug->pause_reason = debug_pause_step;
        } else {
            state->debug->pause_reason = debug_pause_entry;
            state->debug->started = 1;
        }

        goto should_wait;
    }

    if (state->debug->pause_at_next_line) {
        if (state->debug->pause_within == -1 || state->debug->pause_within == bh_arr_last(state->stack_frames).func->id) {

            debug_loc_info_t l1, l2;
            debug_info_lookup_location(engine->debug->info, state->pc - 1, &l1);
            debug_info_lookup_location(engine->debug->info, state->pc,     &l2);

            if (l1.file_id != l2.file_id || l1.line != l2.line) {
                state->debug->pause_at_next_line = false;
                state->debug->pause_reason = debug_pause_step;
                state->debug->state = debug_state_pausing;
                goto should_wait;
            }
        }
    }

    ovm_assert(engine->debug);
    bh_arr_each(debug_breakpoint_t, bp, engine->debug->breakpoints) {
        if (bp->instr == (u32) state->pc) {
            state->debug->state = debug_state_hit_breakpoint;
            state->debug->last_breakpoint_hit = bp->id;
            goto should_wait;
        }
    }

    goto shouldnt_wait;

    should_wait:
    assert(write(state->debug->state_change_write_fd, "1", 1));
    semaphore_wait(state->debug->wait_semaphore);
    state->debug->state = debug_state_running;

    shouldnt_wait:
    if (state->debug->run_count > 0) state->debug->run_count--;
}

#define OVMI_FUNC_NAME(n) ovmi_exec_##n
#define OVMI_DISPATCH_NAME ovmi_dispatch
#define OVMI_DEBUG_HOOK ((void)0)
#define OVMI_EXCEPTION_HOOK ((void)0)
#define OVMI_DIVIDE_CHECK_HOOK(_) ((void)0)
#include "./vm_instrs.h"

#define OVMI_FUNC_NAME(n) ovmi_exec_debug_##n
#define OVMI_DISPATCH_NAME ovmi_debug_dispatch
#define OVMI_DEBUG_HOOK __ovm_debug_hook(state->engine, state)
#define OVMI_EXCEPTION_HOOK __ovm_trigger_exception(state)
#define OVMI_DIVIDE_CHECK_HOOK(ctype) if (VAL(instr->b).ctype == 0) __ovm_trigger_exception(state)
#include "./vm_instrs.h"

ovm_value_t ovm_run_code(ovm_engine_t *engine, ovm_state_t *state, ovm_program_t *program) {
    ovm_assert(engine);
    ovm_assert(state);
    ovm_assert(program);

    ovmi_instr_exec_t *exec_table = ovmi_dispatch;
    if (state->debug) {
        exec_table = ovmi_debug_dispatch;
    }

    ovm_instr_t *code = program->code;
    u8 *memory = engine->memory;
    ovm_value_t *values = state->__frame_values;
    ovm_instr_t *instr = &code[state->pc];

    return exec_table[instr->full_instr & 0x7ff](instr, state, values, memory, code);
}


void ovm_print_stack_trace(ovm_engine_t *engine, ovm_state_t *state, ovm_program_t *program) {
    int i = 0;
    bh_arr_rev_each(ovm_stack_frame_t, frame, state->stack_frames) {
        ovm_func_t *func = frame->func;
        printf("[%03d] %s\n", i++, func->name);
    }
}

