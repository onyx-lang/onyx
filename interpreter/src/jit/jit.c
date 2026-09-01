#define _GNU_SOURCE

#include "emit.h"

#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <unistd.h>

#if defined(__APPLE__)
#include <libkern/OSCacheControl.h>
#endif

_Static_assert(sizeof(ovm_value_t) == 16, "ovm_value_t must be 16 bytes for PARAM stores");

void ovm_engine_set_jit_enabled(ovm_engine_t *engine, b32 enabled) {
    if (engine) engine->jit_enabled = enabled;
}

u64 ovm_jit_return(ovm_state_t *state, u64 val) {
    ovm_stack_frame_t frame = ovm__func_teardown_stack_frame(state);

    if (bh_arr_length(state->stack_frames) == 0) {
        return val;
    }

    ovm_func_t *caller = bh_arr_last(state->stack_frames).func;
    if (caller->kind == OVM_FUNC_EXTERNAL) {
        return val;
    }

    if (frame.return_number_value >= 0) {
        state->__frame_values[frame.return_number_value].u64 = val;
    }

    return val;
}

void ovm_jit_call(ovm_state_t *state, i32 func_idx, i32 result_vn) {
    ovm_func_t *func = &state->program->funcs[func_idx];
    i32 extra_params = (i32) state->param_count - func->param_count;
    if (extra_params < 0) extra_params = 0;

    ovm__func_setup_stack_frame(state, func, result_vn);
    state->param_count -= (u32) func->param_count;

    if (func->kind == OVM_FUNC_INTERNAL) {
        for (i32 pi = 0; pi < func->param_count; pi++) {
            state->__frame_values[pi].u64 = state->param_buf[extra_params + pi].u64;
        }

        if (func->native_entry && !state->debug) {
            bh_arr_last(state->stack_frames).compiled = 1;
            func->native_entry(state->engine, state, state->program);
        } else {
            state->pc = func->start_instr;
            ovm_run_code(state->engine, state, state->program);
        }
    } else {
        ovm_external_func_t external_func = state->external_funcs[func->external_func_idx];
        external_func.native_func(external_func.userdata, &state->param_buf[extra_params], &state->__tmp_value);
        ovm__func_teardown_stack_frame(state);
        if (result_vn >= 0) {
            state->__frame_values[result_vn].u64 = state->__tmp_value.u64;
        }
    }
}

void ovm_jit_calli(ovm_state_t *state, i32 func_slot, i32 result_vn) {
    i32 fidx = state->__frame_values[func_slot].i32;
    ovm_jit_call(state, fidx, result_vn);
}

static inline double jit_abs(double f) { return f >= 0 ? f : -f; }

static inline double jit_floor(double f) {
    if (f < 0) return (double) (((long long) f) - 1);
    return (double) (long long) f;
}

static inline double jit_ceil(double f) {
    if (f - (long long) f == 0) return (double) (long long) f;
    return jit_floor(f) + 1;
}

static inline double jit_trunc(double f) { return (double) (long long) f; }

static inline double jit_nearest(double f) {
    if (f > 0 && f <= 0.5) return +0;
    if (f >= -0.5 && f < 0) return -0;
    if (f - jit_floor(f) < 0.5) return jit_floor(f);
    return jit_ceil(f);
}

static inline double jit_copysign(double a, double b) {
    if ((a > 0 && b > 0) || (a < 0 && b < 0)) return a;
    return -a;
}

void ovm_jit_slow(ovm_state_t *state, ovm_instr_t *instr) {
    ovm_slot_t *values = state->__frame_values;
    u8 *memory = (u8 *) state->engine->memory;
    u32 op = OVM_INSTR_INSTR(*instr);
    u32 ty = OVM_INSTR_TYPE(*instr);

#define V(loc) values[loc]
#define BIN(ctype, expr) do { V(instr->r).ctype = (expr); } while (0)

    switch (op) {
        case OVMI_ADD:
            if (ty == OVM_TYPE_I32) BIN(i32, V(instr->a).i32 + V(instr->b).i32);
            else if (ty == OVM_TYPE_I64) BIN(i64, V(instr->a).i64 + V(instr->b).i64);
            else if (ty == OVM_TYPE_F32) BIN(f32, V(instr->a).f32 + V(instr->b).f32);
            else if (ty == OVM_TYPE_F64) BIN(f64, V(instr->a).f64 + V(instr->b).f64);
            break;
        case OVMI_SUB:
            if (ty == OVM_TYPE_I32) BIN(i32, V(instr->a).i32 - V(instr->b).i32);
            else if (ty == OVM_TYPE_I64) BIN(i64, V(instr->a).i64 - V(instr->b).i64);
            else if (ty == OVM_TYPE_F32) BIN(f32, V(instr->a).f32 - V(instr->b).f32);
            else if (ty == OVM_TYPE_F64) BIN(f64, V(instr->a).f64 - V(instr->b).f64);
            break;
        case OVMI_MUL:
            if (ty == OVM_TYPE_I32) BIN(i32, V(instr->a).i32 * V(instr->b).i32);
            else if (ty == OVM_TYPE_I64) BIN(i64, V(instr->a).i64 * V(instr->b).i64);
            else if (ty == OVM_TYPE_F32) BIN(f32, V(instr->a).f32 * V(instr->b).f32);
            else if (ty == OVM_TYPE_F64) BIN(f64, V(instr->a).f64 * V(instr->b).f64);
            break;

        case OVMI_EQ: case OVMI_NE:
        case OVMI_LT: case OVMI_LT_S: case OVMI_LE: case OVMI_LE_S:
        case OVMI_GT: case OVMI_GT_S: case OVMI_GE: case OVMI_GE_S: {
            i32 res = 0;
            if (ty == OVM_TYPE_I32 || ty == OVM_TYPE_F32) {
                if (op == OVMI_EQ) res = (ty == OVM_TYPE_F32) ? (V(instr->a).f32 == V(instr->b).f32) : (V(instr->a).i32 == V(instr->b).i32);
                else if (op == OVMI_NE) res = (ty == OVM_TYPE_F32) ? (V(instr->a).f32 != V(instr->b).f32) : (V(instr->a).i32 != V(instr->b).i32);
                else if (op == OVMI_LT) res = (ty == OVM_TYPE_F32) ? (V(instr->a).f32 < V(instr->b).f32) : (V(instr->a).u32 < V(instr->b).u32);
                else if (op == OVMI_LT_S) res = V(instr->a).i32 < V(instr->b).i32;
                else if (op == OVMI_LE) res = (ty == OVM_TYPE_F32) ? (V(instr->a).f32 <= V(instr->b).f32) : (V(instr->a).u32 <= V(instr->b).u32);
                else if (op == OVMI_LE_S) res = V(instr->a).i32 <= V(instr->b).i32;
                else if (op == OVMI_GT) res = (ty == OVM_TYPE_F32) ? (V(instr->a).f32 > V(instr->b).f32) : (V(instr->a).u32 > V(instr->b).u32);
                else if (op == OVMI_GT_S) res = V(instr->a).i32 > V(instr->b).i32;
                else if (op == OVMI_GE) res = (ty == OVM_TYPE_F32) ? (V(instr->a).f32 >= V(instr->b).f32) : (V(instr->a).u32 >= V(instr->b).u32);
                else if (op == OVMI_GE_S) res = V(instr->a).i32 >= V(instr->b).i32;
            } else if (ty == OVM_TYPE_I64 || ty == OVM_TYPE_F64) {
                if (op == OVMI_EQ) res = (ty == OVM_TYPE_F64) ? (V(instr->a).f64 == V(instr->b).f64) : (V(instr->a).i64 == V(instr->b).i64);
                else if (op == OVMI_NE) res = (ty == OVM_TYPE_F64) ? (V(instr->a).f64 != V(instr->b).f64) : (V(instr->a).i64 != V(instr->b).i64);
                else if (op == OVMI_LT) res = (ty == OVM_TYPE_F64) ? (V(instr->a).f64 < V(instr->b).f64) : (V(instr->a).u64 < V(instr->b).u64);
                else if (op == OVMI_LT_S) res = V(instr->a).i64 < V(instr->b).i64;
                else if (op == OVMI_LE) res = (ty == OVM_TYPE_F64) ? (V(instr->a).f64 <= V(instr->b).f64) : (V(instr->a).u64 <= V(instr->b).u64);
                else if (op == OVMI_LE_S) res = V(instr->a).i64 <= V(instr->b).i64;
                else if (op == OVMI_GT) res = (ty == OVM_TYPE_F64) ? (V(instr->a).f64 > V(instr->b).f64) : (V(instr->a).u64 > V(instr->b).u64);
                else if (op == OVMI_GT_S) res = V(instr->a).i64 > V(instr->b).i64;
                else if (op == OVMI_GE) res = (ty == OVM_TYPE_F64) ? (V(instr->a).f64 >= V(instr->b).f64) : (V(instr->a).u64 >= V(instr->b).u64);
                else if (op == OVMI_GE_S) res = V(instr->a).i64 >= V(instr->b).i64;
            }
            V(instr->r).i32 = res ? 1 : 0;
            break;
        }

        case OVMI_DIV:
            if (ty == OVM_TYPE_I32) BIN(u32, V(instr->a).u32 / V(instr->b).u32);
            else if (ty == OVM_TYPE_I64) BIN(u64, V(instr->a).u64 / V(instr->b).u64);
            else if (ty == OVM_TYPE_F32) BIN(f32, V(instr->a).f32 / V(instr->b).f32);
            else if (ty == OVM_TYPE_F64) BIN(f64, V(instr->a).f64 / V(instr->b).f64);
            break;
        case OVMI_DIV_S:
            if (ty == OVM_TYPE_I32) BIN(i32, V(instr->a).i32 / V(instr->b).i32);
            else if (ty == OVM_TYPE_I64) BIN(i64, V(instr->a).i64 / V(instr->b).i64);
            else if (ty == OVM_TYPE_F32) BIN(f32, V(instr->a).f32 / V(instr->b).f32);
            else if (ty == OVM_TYPE_F64) BIN(f64, V(instr->a).f64 / V(instr->b).f64);
            break;
        case OVMI_REM:
            if (ty == OVM_TYPE_I32) BIN(u32, V(instr->a).u32 % V(instr->b).u32);
            else if (ty == OVM_TYPE_I64) BIN(u64, V(instr->a).u64 % V(instr->b).u64);
            break;
        case OVMI_REM_S:
            if (ty == OVM_TYPE_I32) BIN(i32, V(instr->a).i32 % V(instr->b).i32);
            else if (ty == OVM_TYPE_I64) BIN(i64, V(instr->a).i64 % V(instr->b).i64);
            break;

        case OVMI_COPY: {
            u32 dest  = V(instr->r).u32;
            u32 src   = V(instr->a).u32;
            u32 count = V(instr->b).u32;
            if (dest && src) memmove(&memory[dest], &memory[src], count);
            break;
        }
        case OVMI_FILL: {
            u32 dest  = V(instr->r).u32;
            u8  byte  = V(instr->a).u8;
            u32 count = V(instr->b).u32;
            if (dest) memset(&memory[dest], byte, count);
            break;
        }
        case OVMI_IDX_ARR: {
            ovm_static_integer_array_t data_elem = state->program->static_data[instr->a];
            u32 idx = V(instr->b).u32;
            if (idx < (u32) data_elem.len) {
                V(instr->r).i32 = state->program->static_integers[data_elem.start_idx + (i32) idx];
            }
            break;
        }
        case OVMI_CMPXCHG: {
            pthread_mutex_lock(&state->engine->atomic_mutex);
            if (ty == OVM_TYPE_I32) {
                i32 *addr = (i32 *) &memory[V(instr->r).u32];
                i32 old = *addr;
                if (old == V(instr->a).i32) *addr = V(instr->b).i32;
                V(instr->r).u64 = 0;
                V(instr->r).i32 = old;
            } else if (ty == OVM_TYPE_I64) {
                i64 *addr = (i64 *) &memory[V(instr->r).u32];
                i64 old = *addr;
                if (old == V(instr->a).i64) *addr = V(instr->b).i64;
                V(instr->r).u64 = 0;
                V(instr->r).i64 = old;
            }
            pthread_mutex_unlock(&state->engine->atomic_mutex);
            break;
        }

        case OVMI_ABS:
            if (ty == OVM_TYPE_F32) V(instr->r).f32 = (f32) jit_abs(V(instr->a).f32);
            else if (ty == OVM_TYPE_F64) V(instr->r).f64 = jit_abs(V(instr->a).f64);
            break;
        case OVMI_NEG:
            if (ty == OVM_TYPE_F32) V(instr->r).f32 = -V(instr->a).f32;
            else if (ty == OVM_TYPE_F64) V(instr->r).f64 = -V(instr->a).f64;
            break;
        case OVMI_CEIL:
            if (ty == OVM_TYPE_F32) V(instr->r).f32 = (f32) jit_ceil(V(instr->a).f32);
            else if (ty == OVM_TYPE_F64) V(instr->r).f64 = jit_ceil(V(instr->a).f64);
            break;
        case OVMI_FLOOR:
            if (ty == OVM_TYPE_F32) V(instr->r).f32 = (f32) jit_floor(V(instr->a).f32);
            else if (ty == OVM_TYPE_F64) V(instr->r).f64 = jit_floor(V(instr->a).f64);
            break;
        case OVMI_TRUNC:
            if (ty == OVM_TYPE_F32) V(instr->r).f32 = (f32) jit_trunc(V(instr->a).f32);
            else if (ty == OVM_TYPE_F64) V(instr->r).f64 = jit_trunc(V(instr->a).f64);
            break;
        case OVMI_NEAREST:
            if (ty == OVM_TYPE_F32) V(instr->r).f32 = (f32) jit_nearest(V(instr->a).f32);
            else if (ty == OVM_TYPE_F64) V(instr->r).f64 = jit_nearest(V(instr->a).f64);
            break;
        case OVMI_SQRT:
            if (ty == OVM_TYPE_F32) V(instr->r).f32 = sqrtf(V(instr->a).f32);
            else if (ty == OVM_TYPE_F64) V(instr->r).f64 = sqrt(V(instr->a).f64);
            break;
        case OVMI_MIN:
            if (ty == OVM_TYPE_F32) V(instr->r).f32 = V(instr->a).f32 < V(instr->b).f32 ? V(instr->a).f32 : V(instr->b).f32;
            else if (ty == OVM_TYPE_F64) V(instr->r).f64 = V(instr->a).f64 < V(instr->b).f64 ? V(instr->a).f64 : V(instr->b).f64;
            break;
        case OVMI_MAX:
            if (ty == OVM_TYPE_F32) V(instr->r).f32 = V(instr->a).f32 > V(instr->b).f32 ? V(instr->a).f32 : V(instr->b).f32;
            else if (ty == OVM_TYPE_F64) V(instr->r).f64 = V(instr->a).f64 > V(instr->b).f64 ? V(instr->a).f64 : V(instr->b).f64;
            break;
        case OVMI_COPYSIGN:
            if (ty == OVM_TYPE_F32) V(instr->r).f32 = (f32) jit_copysign(V(instr->a).f32, V(instr->b).f32);
            else if (ty == OVM_TYPE_F64) V(instr->r).f64 = jit_copysign(V(instr->a).f64, V(instr->b).f64);
            break;

        case OVMI_CVT_F32:
            if (ty == OVM_TYPE_I32) V(instr->r).u32 = (u32) V(instr->a).f32;
            else if (ty == OVM_TYPE_I64) V(instr->r).u64 = (u64) V(instr->a).f32;
            else if (ty == OVM_TYPE_F64) V(instr->r).f64 = (f64) V(instr->a).f32;
            break;
        case OVMI_CVT_F32_S:
            if (ty == OVM_TYPE_I32) V(instr->r).i32 = (i32) V(instr->a).f32;
            else if (ty == OVM_TYPE_I64) V(instr->r).i64 = (i64) V(instr->a).f32;
            else if (ty == OVM_TYPE_F64) V(instr->r).f64 = (f64) V(instr->a).f32;
            break;
        case OVMI_CVT_F64:
            if (ty == OVM_TYPE_I32) V(instr->r).u32 = (u32) V(instr->a).f64;
            else if (ty == OVM_TYPE_I64) V(instr->r).u64 = (u64) V(instr->a).f64;
            else if (ty == OVM_TYPE_F32) V(instr->r).f32 = (f32) V(instr->a).f64;
            break;
        case OVMI_CVT_F64_S:
            if (ty == OVM_TYPE_I32) V(instr->r).i32 = (i32) V(instr->a).f64;
            else if (ty == OVM_TYPE_I64) V(instr->r).i64 = (i64) V(instr->a).f64;
            else if (ty == OVM_TYPE_F32) V(instr->r).f32 = (f32) V(instr->a).f64;
            break;
        case OVMI_CVT_I32:
            if (ty == OVM_TYPE_F32) V(instr->r).f32 = (f32) V(instr->a).u32;
            else if (ty == OVM_TYPE_F64) V(instr->r).f64 = (f64) V(instr->a).u32;
            break;
        case OVMI_CVT_I32_S:
            if (ty == OVM_TYPE_F32) V(instr->r).f32 = (f32) V(instr->a).i32;
            else if (ty == OVM_TYPE_F64) V(instr->r).f64 = (f64) V(instr->a).i32;
            break;
        case OVMI_CVT_I64:
            if (ty == OVM_TYPE_F32) V(instr->r).f32 = (f32) V(instr->a).u64;
            else if (ty == OVM_TYPE_F64) V(instr->r).f64 = (f64) V(instr->a).u64;
            break;
        case OVMI_CVT_I64_S:
            if (ty == OVM_TYPE_F32) V(instr->r).f32 = (f32) V(instr->a).i64;
            else if (ty == OVM_TYPE_F64) V(instr->r).f64 = (f64) V(instr->a).i64;
            break;

        case OVMI_MEM_SIZE:
            V(instr->r).u32 = (u32) (state->engine->memory_size / 65536);
            break;
        case OVMI_MEM_GROW: {
            V(instr->r).u32 = (u32) (state->engine->memory_size / 65536);
            i64 new_size = state->engine->memory_size + (i64) V(instr->a).u32 * 65536;
            if (!ovm_engine_memory_ensure_capacity(state->engine, new_size)) {
                V(instr->r).i32 = -1;
            } else {
                state->engine->memory_size = new_size;
            }
            break;
        }

        case OVMI_PARAM:
            state->param_buf[state->param_count++].u64 = V(instr->a).u64;
            break;
        case OVMI_CALL:
            ovm_jit_call(state, instr->a, instr->r);
            break;
        case OVMI_CALLI:
            ovm_jit_calli(state, instr->a, instr->r);
            break;

        default:
            break;
    }

#undef V
#undef BIN
}

static i64 page_align(i64 n) {
    long page = sysconf(_SC_PAGESIZE);
    if (page <= 0) page = 4096;
    return (n + page - 1) & ~(page - 1);
}

void ovm_jit_compile_program(ovm_engine_t *engine, ovm_program_t *program) {
    if (!engine || !program) return;
    if (!engine->jit_enabled) return;
    if (engine->debug) return;
    if (program->jit_exec) return;

#if !defined(__aarch64__) && !defined(__arm64__) && !defined(__x86_64__)
    return;
#endif

    i32 nfuncs = bh_arr_length(program->funcs);
    if (nfuncs <= 0 || bh_arr_length(program->code) <= 0) return;

    ovm_emit_buf_t buf;
    ovm_emit_init(&buf);

    i32 *func_off = calloc((size_t) nfuncs, sizeof(i32));
    u8 *compiled = calloc((size_t) nfuncs, 1);
    if (!func_off || !compiled) {
        free(func_off);
        free(compiled);
        ovm_emit_free(&buf);
        return;
    }

    for (i32 fi = 0; fi < nfuncs; fi++) {
        func_off[fi] = -1;
        ovm_func_t *f = &program->funcs[fi];
        if (f->kind != OVM_FUNC_INTERNAL) continue;

        i32 start = f->start_instr;
        i32 end = ovm_func_code_end(program, fi);
        if (start < 0 || end <= start) continue;
        if (!ovm_jit_should_compile(program, start, end)) continue;

        ovm_emit_align(&buf, 16);
        ovm_emit_reset_func(&buf, start, end);
        func_off[fi] = buf.len;
        compiled[fi] = 1;

#if defined(__aarch64__) || defined(__arm64__)
        ovm_jit_emit_arm64(&buf, program, start, end);
        ovm_jit_patch_arm64(&buf);
#elif defined(__x86_64__)
        ovm_jit_emit_x64(&buf, program, start, end);
        ovm_jit_patch_x64(&buf);
#endif
    }

    if (buf.len <= 0) {
        free(func_off);
        free(compiled);
        ovm_emit_free(&buf);
        return;
    }

    i64 map_size = page_align(buf.len);
    void *mem = mmap(NULL, (size_t) map_size, PROT_READ | PROT_WRITE,
                     MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (mem == MAP_FAILED) {
        free(func_off);
        free(compiled);
        ovm_emit_free(&buf);
        return;
    }

    memcpy(mem, buf.data, (size_t) buf.len);

#if defined(__GNUC__) || defined(__clang__)
    __builtin___clear_cache((char *) mem, (char *) mem + buf.len);
#endif
#if defined(__APPLE__)
    sys_icache_invalidate(mem, (size_t) buf.len);
#endif

    if (mprotect(mem, (size_t) map_size, PROT_READ | PROT_EXEC) != 0) {
        munmap(mem, (size_t) map_size);
        free(func_off);
        free(compiled);
        ovm_emit_free(&buf);
        return;
    }

    program->jit_exec = mem;
    program->jit_exec_size = map_size;

    for (i32 fi = 0; fi < nfuncs; fi++) {
        if (!compiled[fi] || func_off[fi] < 0) continue;
        program->funcs[fi].native_entry =
            (u64 (*)(ovm_engine_t *, ovm_state_t *, ovm_program_t *))
            ((u8 *) mem + func_off[fi]);
    }

    free(func_off);
    free(compiled);
    ovm_emit_free(&buf);
}
