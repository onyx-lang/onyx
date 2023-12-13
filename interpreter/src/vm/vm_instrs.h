

//
// Running code with Continuation-Passing Style
//
// The way OVM code is run now uses "threaded code" or "Continuation-Passing" code.
// The main idea is that GCC/Clang/MSVC are "smart" and will perform a tail call-elimination
// on all `ovmi_exec_...` functions listed below. This in effect means that each function
// ends with a `jmp rax` to the next function, instead of a `call ...`. This prevents the
// stack from growing until it overflows. Many of the `ovmi_exec_...` functions were
// designed so no stack based allocation has to happen.
//
// There are several things that break this however. Chief among which is the __ovm_debug_hook
// function call which must be present in ALL instructions for debugging to work properly.
// When this hook is present, the compiler MUST emit many `push` and `pop` instructions to preserve
// the needed registers because it cannot know what will happen in the function. This more than
// doubles the instruction count of all operations and slows down the runner by at least 20 percent.
//


#define OVMI_INSTR_PROTO(name) \
    ovm_value_t name(ovm_instr_t *instr, ovm_state_t *state, ovm_value_t *values, u8 *memory, ovm_instr_t *code)

#define OVMI_INSTR_EXEC(name) \
    static OVMI_INSTR_PROTO(OVMI_FUNC_NAME(name))

#if _BH_DARWIN
    #define FORCE_TAILCALL __attribute__((musttail))
#else
    #define FORCE_TAILCALL
#endif

#define NEXT_OP \
    OVMI_DEBUG_HOOK; \
    instr = &code[state->pc++]; \
    FORCE_TAILCALL return OVMI_DISPATCH_NAME[instr->full_instr & OVM_INSTR_MASK](instr, state, values, memory, code);

#define VAL(loc) values[loc]

typedef OVMI_INSTR_PROTO((* ovmi_instr_exec_t));

static ovmi_instr_exec_t OVMI_DISPATCH_NAME[];


//
// Special Operations
//

OVMI_INSTR_EXEC(nop) {
    NEXT_OP;
}


//
// Binary Operations
//

#define OVM_OP_EXEC(name, op) \
    OVMI_INSTR_EXEC(name##_i32) { OVM_OP(OVM_TYPE_I32, op, i32); NEXT_OP; } \
    OVMI_INSTR_EXEC(name##_i64) { OVM_OP(OVM_TYPE_I64, op, i64); NEXT_OP; } \
    OVMI_INSTR_EXEC(name##_f32) { OVM_OP(OVM_TYPE_F32, op, f32); NEXT_OP; } \
    OVMI_INSTR_EXEC(name##_f64) { OVM_OP(OVM_TYPE_F64, op, f64); NEXT_OP; }

#define OVM_OP_UNSIGNED_EXEC(name, op) \
    OVMI_INSTR_EXEC(name##_i32) { OVM_OP(OVM_TYPE_I32, op, u32); NEXT_OP; } \
    OVMI_INSTR_EXEC(name##_i64) { OVM_OP(OVM_TYPE_I64, op, u64); NEXT_OP; } \
    OVMI_INSTR_EXEC(name##_f32) { OVM_OP(OVM_TYPE_F32, op, f32); NEXT_OP; } \
    OVMI_INSTR_EXEC(name##_f64) { OVM_OP(OVM_TYPE_F64, op, f64); NEXT_OP; }

#define OVM_OP_INTEGER_EXEC(name, op) \
    OVMI_INSTR_EXEC(name##_i32) { OVM_OP(OVM_TYPE_I32, op, i32); NEXT_OP; } \
    OVMI_INSTR_EXEC(name##_i64) { OVM_OP(OVM_TYPE_I64, op, i64); NEXT_OP; }

#define OVM_OP_INTEGER_UNSIGNED_EXEC(name, op) \
    OVMI_INSTR_EXEC(name##_i32) { OVM_OP(OVM_TYPE_I32, op, u32); NEXT_OP; } \
    OVMI_INSTR_EXEC(name##_i64) { OVM_OP(OVM_TYPE_I64, op, u64); NEXT_OP; }

#define OVM_OP_FLOAT_EXEC(name, op) \
    OVMI_INSTR_EXEC(name##_f32) { OVM_OP(OVM_TYPE_F32, op, f32); NEXT_OP; } \
    OVMI_INSTR_EXEC(name##_f64) { OVM_OP(OVM_TYPE_F64, op, f64); NEXT_OP; }


#define OVM_OP(t, op, ctype) \
    ovm_assert(VAL(instr->a).type == t && VAL(instr->b).type == t); \
    VAL(instr->r).ctype = VAL(instr->a).ctype op VAL(instr->b).ctype; \
    VAL(instr->r).type = t;

OVM_OP_EXEC(add, +)
OVM_OP_EXEC(sub, -)
OVM_OP_EXEC(mul, *)
OVM_OP_INTEGER_UNSIGNED_EXEC(and, &)
OVM_OP_INTEGER_UNSIGNED_EXEC(or, |)
OVM_OP_INTEGER_UNSIGNED_EXEC(xor, ^)
OVM_OP_INTEGER_UNSIGNED_EXEC(shl, <<)
OVM_OP_INTEGER_UNSIGNED_EXEC(shr, >>)
OVM_OP_INTEGER_EXEC(sar, >>)

#undef OVM_OP


#define OVM_OP(t, op, ctype) \
    ovm_assert(VAL(instr->a).type == t && VAL(instr->b).type == t); \
    OVMI_DIVIDE_CHECK_HOOK(ctype); \
    VAL(instr->r).ctype = VAL(instr->a).ctype op VAL(instr->b).ctype; \
    VAL(instr->r).type = t;

OVM_OP_EXEC(div_s, /)
OVM_OP_UNSIGNED_EXEC(div, /)
OVM_OP_INTEGER_UNSIGNED_EXEC(rem, %)
OVM_OP_INTEGER_EXEC(rem_s, %)

#undef OVM_OP

#define OVM_OP(t, func, ctype) \
    ovm_assert(VAL(instr->a).type == t && VAL(instr->b).type == t); \
    VAL(instr->r).ctype = func( VAL(instr->a).ctype, VAL(instr->b).ctype ); \
    VAL(instr->r).type = t;

#ifndef ROTATION_FUNCTIONS
#define ROTATION_FUNCTIONS

static inline u32 rotl32(u32 value, u32 count) {
    const unsigned int mask = 0xFF;
    count &= mask;
    return (value << count) | (value >> (-count & mask));
}

static inline u64 rotl64(u64 value, u32 count) {
    const unsigned int mask = 0xFF;
    count &= mask;
    return (value << count) | (value >> (-count & mask));
}

static inline u32 rotr32(u32 value, u32 count) {
    const unsigned int mask = 0xFF;
    count &= mask;
    return (value >> count) | (value << (-count & mask));
}

static inline u64 rotr64(u64 value, u32 count) {
    const unsigned int mask = 0xFF;
    count &= mask;
    return (value >> count) | (value << (-count & mask));
}

#endif

OVMI_INSTR_EXEC(rotl_i32) { OVM_OP(OVM_TYPE_I32, rotl32, u32); NEXT_OP; }
OVMI_INSTR_EXEC(rotl_i64) { OVM_OP(OVM_TYPE_I64, rotl64, u64); NEXT_OP; }
OVMI_INSTR_EXEC(rotr_i32) { OVM_OP(OVM_TYPE_I32, rotr32, u32); NEXT_OP; }
OVMI_INSTR_EXEC(rotr_i64) { OVM_OP(OVM_TYPE_I64, rotr64, u64); NEXT_OP; }

OVM_OP_FLOAT_EXEC(min, bh_min)
OVM_OP_FLOAT_EXEC(max, bh_max)
OVM_OP_FLOAT_EXEC(copysign, __ovm_copysign)

#undef OVM_OP


#define OVM_OP(t, op, ctype) \
    ovm_assert(VAL(instr->a).type == t); \
    VAL(instr->r).type = t; \
    VAL(instr->r).ctype = (ctype) op (VAL(instr->a).ctype);

OVMI_INSTR_EXEC(clz_i32) { OVM_OP(OVM_TYPE_I32, __ovm_clz, u32);   NEXT_OP; }
OVMI_INSTR_EXEC(clz_i64) { OVM_OP(OVM_TYPE_I64, __ovm_clzll, u64); NEXT_OP; }
OVMI_INSTR_EXEC(ctz_i32) { OVM_OP(OVM_TYPE_I32, __ovm_ctz, u32);   NEXT_OP; }
OVMI_INSTR_EXEC(ctz_i64) { OVM_OP(OVM_TYPE_I64, __ovm_ctzll, u64); NEXT_OP; }
OVMI_INSTR_EXEC(popcount_i32) { OVM_OP(OVM_TYPE_I32, __ovm_popcount, u32);   NEXT_OP; }
OVMI_INSTR_EXEC(popcount_i64) { OVM_OP(OVM_TYPE_I64, __ovm_popcountll, u64); NEXT_OP; }

OVM_OP_FLOAT_EXEC(abs,     __ovm_abs)
OVM_OP_FLOAT_EXEC(neg,     -)
OVM_OP_FLOAT_EXEC(ceil,    __ovm_ceil)
OVM_OP_FLOAT_EXEC(floor,   __ovm_floor)
OVM_OP_FLOAT_EXEC(trunc,   __ovm_trunc)
OVM_OP_FLOAT_EXEC(nearest, __ovm_nearest)
OVM_OP_FLOAT_EXEC(sqrt,    sqrt)

#undef OVM_OP


#define OVM_OP(t, op, ctype) \
    ovm_assert(VAL(instr->a).type == t && VAL(instr->b).type == t); \
    VAL(instr->r).type = OVM_TYPE_I32; \
    VAL(instr->r).i32 = ((VAL(instr->a).ctype op VAL(instr->b).ctype)) ? 1 : 0;

OVM_OP_EXEC(eq, ==)
OVM_OP_EXEC(ne, !=)
OVM_OP_UNSIGNED_EXEC(lt, <)
OVM_OP_UNSIGNED_EXEC(le, <=)
OVM_OP_UNSIGNED_EXEC(gt, >)
OVM_OP_UNSIGNED_EXEC(ge, >=)
OVM_OP_EXEC(lt_s, <)
OVM_OP_EXEC(le_s, <=)
OVM_OP_EXEC(gt_s, >)
OVM_OP_EXEC(ge_s, >=)

#undef OVM_OP



//
// Memory / register operations
//

#define OVM_IMM(t, dtype, stype) \
    VAL(instr->r).type = t; \
    VAL(instr->r).u64 = 0; \
    VAL(instr->r).dtype = instr->stype;


OVMI_INSTR_EXEC(imm_i32) { OVM_IMM(OVM_TYPE_I32, u32, i); NEXT_OP; }
OVMI_INSTR_EXEC(imm_i64) { OVM_IMM(OVM_TYPE_I64, u64, l); NEXT_OP; }
OVMI_INSTR_EXEC(imm_f32) { OVM_IMM(OVM_TYPE_F32, f32, f); NEXT_OP; }
OVMI_INSTR_EXEC(imm_f64) { OVM_IMM(OVM_TYPE_F64, f64, d); NEXT_OP; }

#undef OVM_IMM

OVMI_INSTR_EXEC(mov) {
    VAL(instr->r) = VAL(instr->a);
    NEXT_OP;
}

#define OVM_LOAD(otype, type_, stype) \
    OVMI_INSTR_EXEC(load_##otype) { \
        ovm_assert(VAL(instr->a).type == OVM_TYPE_I32); \
        u32 dest = VAL(instr->a).u32 + (u32) instr->b; \
        if (dest == 0) OVMI_EXCEPTION_HOOK; \
        VAL(instr->r).stype = * (stype *) &memory[dest]; \
        VAL(instr->r).type = type_; \
        NEXT_OP; \
    }

OVM_LOAD(i8,  OVM_TYPE_I8,  u8)
OVM_LOAD(i16, OVM_TYPE_I16, u16)
OVM_LOAD(i32, OVM_TYPE_I32, u32)
OVM_LOAD(i64, OVM_TYPE_I64, u64)
OVM_LOAD(f32, OVM_TYPE_F32, f32)
OVM_LOAD(f64, OVM_TYPE_F64, f64)

#undef OVM_LOAD

#define OVM_STORE(otype, type_, stype) \
    OVMI_INSTR_EXEC(store_##otype) { \
        ovm_assert(VAL(instr->r).type == OVM_TYPE_I32); \
        u32 dest = VAL(instr->r).u32 + (u32) instr->b; \
        if (dest == 0) OVMI_EXCEPTION_HOOK; \
        *(stype *) &memory[dest] = VAL(instr->a).stype; \
        NEXT_OP; \
    }


OVM_STORE(i8,  OVM_TYPE_I8,  u8)
OVM_STORE(i16, OVM_TYPE_I16, u16)
OVM_STORE(i32, OVM_TYPE_I32, u32)
OVM_STORE(i64, OVM_TYPE_I64, u64)
OVM_STORE(f32, OVM_TYPE_F32, f32)
OVM_STORE(f64, OVM_TYPE_F64, f64)

#undef OVM_STORE

OVMI_INSTR_EXEC(copy) {
    u32 dest  = VAL(instr->r).u32;
    u32 src   = VAL(instr->a).u32;
    u32 count = VAL(instr->b).u32;

    if (!dest || !src) OVMI_EXCEPTION_HOOK;

    memmove(&memory[dest], &memory[src], count);

    NEXT_OP;
}

OVMI_INSTR_EXEC(fill) {
    i32 dest  = VAL(instr->r).i32;
    u8  byte  = VAL(instr->a).u8;
    i32 count = VAL(instr->b).i32;

    if (!dest) OVMI_EXCEPTION_HOOK;

    memset(&memory[dest], byte, count);

    NEXT_OP;
}

OVMI_INSTR_EXEC(reg_get) {
    VAL(instr->r) = state->registers[instr->a];

    NEXT_OP;
}

OVMI_INSTR_EXEC(reg_set) {
    state->registers[instr->r] = VAL(instr->a);

    NEXT_OP;
}

OVMI_INSTR_EXEC(idx_arr) {
    ovm_static_integer_array_t data_elem = state->program->static_data[instr->a];
    if (VAL(instr->b).u32 >= (u32) data_elem.len) {
        OVMI_EXCEPTION_HOOK;
        ovm_value_t bad_val;
        bad_val.type = OVM_TYPE_ERR;
        return bad_val;
    }

    VAL(instr->r).i32 = state->program->static_integers[data_elem.start_idx + VAL(instr->b).u32];
    VAL(instr->r).type = OVM_TYPE_I32;

    NEXT_OP;
}


//
// Function calling
//

OVMI_INSTR_EXEC(param) {
    // bh_arr_push(state->params, VAL(instr->a));
    ovm_assert((state->param_count <= OVM_MAX_PARAM_COUNT));
    state->param_buf[state->param_count++] = VAL(instr->a);

    NEXT_OP;
}

OVMI_INSTR_EXEC(return) {
    ovm_value_t val = VAL(instr->a);
    ovm_stack_frame_t frame = ovm__func_teardown_stack_frame(state);
    state->pc = frame.return_address;
    values = state->__frame_values;

    if (bh_arr_length(state->stack_frames) == 0) {
        return val;
    }

    ovm_func_t *new_func = bh_arr_last(state->stack_frames).func;
    if (new_func->kind == OVM_FUNC_EXTERNAL) {
        return val;
    }

    if (frame.return_number_value >= 0) {
        VAL(frame.return_number_value) = val;
    }

#ifdef OVM_VERBOSE
    printf("Returning from %s to %s: ", frame.func->name, bh_arr_last(state->stack_frames).func->name);
    ovm_print_val(val);
    printf("\n\n");
#endif

    NEXT_OP;
}


#define OVM_CALL_CODE(func_idx) \
    i32 fidx = func_idx; \
    ovm_func_t *func = &state->program->funcs[fidx]; \
    i32 extra_params = state->param_count - func->param_count; \
    ovm_assert(extra_params >= 0); \
    ovm__func_setup_stack_frame(state, func, instr->r); \
    state->param_count -= func->param_count; \
    if (func->kind == OVM_FUNC_INTERNAL) { \
        values = state->__frame_values; \
        memcpy(&VAL(0), &state->param_buf[extra_params], func->param_count * sizeof(ovm_value_t)); \
        state->pc = func->start_instr; \
    } else { \
        ovm_external_func_t external_func = state->external_funcs[func->external_func_idx]; \
        external_func.native_func(external_func.userdata, &state->param_buf[extra_params], &state->__tmp_value); \
        memory = state->engine->memory; \
\
        ovm__func_teardown_stack_frame(state); \
\
        if (instr->r >= 0) { \
            VAL(instr->r) = state->__tmp_value; \
        } \
    }


OVMI_INSTR_EXEC(call) {
    OVM_CALL_CODE(instr->a);
    NEXT_OP;
}

OVMI_INSTR_EXEC(calli) {
    OVM_CALL_CODE(VAL(instr->a).i32);
    NEXT_OP;
}

#undef OVM_CALL_CODE



//
// Branching Instructions
//

OVMI_INSTR_EXEC(br)     { state->pc += instr->a; NEXT_OP; }
OVMI_INSTR_EXEC(bri)    { state->pc += VAL(instr->a).i32; NEXT_OP; }
OVMI_INSTR_EXEC(br_nz)  { if (VAL(instr->b).i32 != 0) state->pc += instr->a; NEXT_OP; }
OVMI_INSTR_EXEC(bri_nz) { if (VAL(instr->b).i32 != 0) state->pc += VAL(instr->a).i32; NEXT_OP; }
OVMI_INSTR_EXEC(br_z)   { if (VAL(instr->b).i32 == 0) state->pc += instr->a; NEXT_OP; }
OVMI_INSTR_EXEC(bri_z)  { if (VAL(instr->b).i32 == 0) state->pc += VAL(instr->a).i32; NEXT_OP; }


//
// Conversion
//

#define OVM_CVT(n1, n2, stype, dtype, otype, ctype) \
    OVMI_INSTR_EXEC(cvt_##n1##_##n2) { \
        state->__tmp_value.dtype = (ctype) VAL(instr->a).stype; \
        state->__tmp_value.type = otype; \
        VAL(instr->r) = state->__tmp_value; \
        NEXT_OP; \
    }

OVM_CVT(i8, i16,   u8, u16, OVM_TYPE_I16, u16);
OVM_CVT(i8, i32,   u8, u32, OVM_TYPE_I32, u32);
OVM_CVT(i8, i64,   u8, u64, OVM_TYPE_I64, u64);
OVM_CVT(i8_s, i16, i8, i16, OVM_TYPE_I16, i16);
OVM_CVT(i8_s, i32, i8, i32, OVM_TYPE_I32, i32);
OVM_CVT(i8_s, i64, i8, i64, OVM_TYPE_I64, i64);

OVM_CVT(i16,   i8,  u16, u8,  OVM_TYPE_I8, u8);
OVM_CVT(i16,   i32, u16, u32, OVM_TYPE_I32, u32);
OVM_CVT(i16,   i64, u16, u64, OVM_TYPE_I64, u64);
OVM_CVT(i16_s, i8,  i16, i8,  OVM_TYPE_I8, i8);
OVM_CVT(i16_s, i32, i16, i32, OVM_TYPE_I32, i32);
OVM_CVT(i16_s, i64, i16, i64, OVM_TYPE_I64, i64);

OVM_CVT(i32,   i8,  u32, u8,  OVM_TYPE_I8,  u8);
OVM_CVT(i32,   i16, u32, u16, OVM_TYPE_I16, u16);
OVM_CVT(i32,   i64, u32, u64, OVM_TYPE_I64, u64);
OVM_CVT(i32_s, i8,  i32, i8,  OVM_TYPE_I8,  i8);
OVM_CVT(i32_s, i16, i32, i16, OVM_TYPE_I16, i16);
OVM_CVT(i32_s, i64, i32, i64, OVM_TYPE_I64, i64);
OVM_CVT(i32,   f32, u32, f32, OVM_TYPE_F32, f32);
OVM_CVT(i32_s, f32, i32, f32, OVM_TYPE_F32, f32);
OVM_CVT(i32,   f64, u32, f64, OVM_TYPE_F64, f64);
OVM_CVT(i32_s, f64, i32, f64, OVM_TYPE_F64, f64);

OVM_CVT(i64,   i8,  u64, u8,  OVM_TYPE_I8,  u8);
OVM_CVT(i64,   i16, u64, u16, OVM_TYPE_I16, u16);
OVM_CVT(i64,   i32, u64, u32, OVM_TYPE_I32, u32);
OVM_CVT(i64_s, i8,  i64, i8,  OVM_TYPE_I8,  i8);
OVM_CVT(i64_s, i16, i64, i16, OVM_TYPE_I16, i16);
OVM_CVT(i64_s, i32, i64, i32, OVM_TYPE_I32, i32);
OVM_CVT(i64,   f32, u64, f32, OVM_TYPE_F32, f32);
OVM_CVT(i64_s, f32, i64, f32, OVM_TYPE_F32, f32);
OVM_CVT(i64,   f64, u64, f64, OVM_TYPE_F64, f64);
OVM_CVT(i64_s, f64, i64, f64, OVM_TYPE_F64, f64);

OVM_CVT(f32,   i32, f32, u32, OVM_TYPE_I32, u32);
OVM_CVT(f32,   i64, f32, u64, OVM_TYPE_I64, u64);
OVM_CVT(f32,   f64, f32, f64, OVM_TYPE_F64, f64);
OVM_CVT(f32_s, i32, f32, i32, OVM_TYPE_I32, i32);
OVM_CVT(f32_s, i64, f32, i64, OVM_TYPE_I64, i64);
OVM_CVT(f32_s, f64, f32, f64, OVM_TYPE_F64, f64);

OVM_CVT(f64,   i32, f64, u32, OVM_TYPE_I32, u32);
OVM_CVT(f64,   i64, f64, u64, OVM_TYPE_I64, u64);
OVM_CVT(f64,   f32, f64, f32, OVM_TYPE_F32, f32);
OVM_CVT(f64_s, i32, f64, i32, OVM_TYPE_I32, i32);
OVM_CVT(f64_s, i64, f64, i64, OVM_TYPE_I64, i64);
OVM_CVT(f64_s, f32, f64, f32, OVM_TYPE_F32, f32);

#undef OVM_CVT

#define OVM_CVT(n1, n2, stype, dtype, otype, ctype) \
    OVMI_INSTR_EXEC(transmute_##n1##_##n2) { \
        ovm_value_t tmp_val; \
        tmp_val.dtype = *(ctype *) &VAL(instr->a).stype; \
        tmp_val.type = otype; \
        VAL(instr->r) = tmp_val; \
        NEXT_OP; \
    }

OVM_CVT(i32, f32, u32, f32, OVM_TYPE_F32, f32);
OVM_CVT(i64, f64, u64, f64, OVM_TYPE_F64, f64);
OVM_CVT(f32, i32, f32, u32, OVM_TYPE_I32, u32);
OVM_CVT(f64, i64, f64, u64, OVM_TYPE_I64, u64);

#undef OVM_CVT


//
// Compare exchange
//

#define CMPXCHG(otype, ctype) \
    OVMI_INSTR_EXEC(cmpxchg_##ctype) { \
        pthread_mutex_lock(&state->engine->atomic_mutex); \
        if (VAL(instr->r).u32 == 0) OVMI_EXCEPTION_HOOK; \
        ctype *addr = (ctype *) &memory[VAL(instr->r).u32]; \
 \
        VAL(instr->r).u64 = 0; \
        VAL(instr->r).type = otype; \
        VAL(instr->r).ctype = *addr; \
 \
        if (*addr == VAL(instr->a).ctype) { \
            *addr = VAL(instr->b).ctype ; \
        } \
 \
        pthread_mutex_unlock(&state->engine->atomic_mutex); \
        NEXT_OP; \
    }

CMPXCHG(OVM_TYPE_I32, i32)
CMPXCHG(OVM_TYPE_I64, i64)

#undef CMPXCHG


//
// Memory
//

OVMI_INSTR_EXEC(mem_size) {
    VAL(instr->r).u32 = (u32) (state->engine->memory_size / 65536);
    VAL(instr->r).type = OVM_TYPE_I32;
    NEXT_OP;
}

OVMI_INSTR_EXEC(mem_grow) {
    ovm_assert(VAL(instr->a).type == OVM_TYPE_I32);
    VAL(instr->r).type = OVM_TYPE_I32;
    VAL(instr->r).u32 = (u32) (state->engine->memory_size / 65536);

    if (!ovm_engine_memory_ensure_capacity(state->engine,
            state->engine->memory_size + VAL(instr->a).u32 * 65536)) {
        VAL(instr->r).i32 = -1;
    }

    memory = state->engine->memory;
    NEXT_OP;
}


OVMI_INSTR_EXEC(illegal) {
    OVMI_EXCEPTION_HOOK;
    return ((ovm_value_t) {0});
}

//
// Dispatch table
//

#define D(n) OVMI_FUNC_NAME(n)

#define IROW_UNTYPED(name) D(name), NULL, NULL, NULL, NULL, NULL, NULL, NULL,
#define IROW_TYPED(name)   NULL, D(name##_i8), D(name##_i16), D(name##_i32), D(name##_i64), D(name##_f32), D(name##_f64), NULL,
#define IROW_PARTIAL(name) NULL, NULL, NULL, D(name##_i32), D(name##_i64), D(name##_f32), D(name##_f64), NULL,
#define IROW_INT(name)     NULL, NULL, NULL, D(name##_i32), D(name##_i64), NULL, NULL, NULL,
#define IROW_FLOAT(name)   NULL, NULL, NULL, NULL, NULL, D(name##_f32), D(name##_f64), NULL,
#define IROW_SAME(name)    D(name),D(name),D(name),D(name),D(name),D(name),D(name),NULL,

static ovmi_instr_exec_t OVMI_DISPATCH_NAME[] = {
    IROW_UNTYPED(nop) // 0x00
    IROW_PARTIAL(add)
    IROW_PARTIAL(sub)
    IROW_PARTIAL(mul)
    IROW_PARTIAL(div)
    IROW_PARTIAL(div_s)
    IROW_INT(rem)
    IROW_INT(rem_s)
    IROW_INT(and)
    IROW_INT(or)
    IROW_INT(xor)
    IROW_INT(shl)
    IROW_INT(shr)
    IROW_INT(sar)
    IROW_SAME(illegal)
    IROW_SAME(illegal)
    IROW_PARTIAL(imm) // 0x10
    IROW_UNTYPED(mov)
    IROW_TYPED(load)
    IROW_TYPED(store)
    IROW_UNTYPED(copy)
    IROW_UNTYPED(fill)
    IROW_UNTYPED(reg_get)
    IROW_UNTYPED(reg_set)
    IROW_UNTYPED(idx_arr)
    IROW_PARTIAL(lt)
    IROW_PARTIAL(lt_s)
    IROW_PARTIAL(le)
    IROW_PARTIAL(le_s)
    IROW_PARTIAL(eq)
    IROW_PARTIAL(ge)
    IROW_PARTIAL(ge_s)
    IROW_PARTIAL(gt)  // 0x20
    IROW_PARTIAL(gt_s)
    IROW_PARTIAL(ne)
    IROW_UNTYPED(param)
    IROW_UNTYPED(return)
    IROW_UNTYPED(call)
    IROW_UNTYPED(calli)
    IROW_UNTYPED(br)
    IROW_UNTYPED(br_z)
    IROW_UNTYPED(br_nz)
    IROW_UNTYPED(bri)
    IROW_UNTYPED(bri_z)
    IROW_UNTYPED(bri_nz)
    IROW_INT(clz)
    IROW_INT(ctz)
    IROW_INT(popcount)
    IROW_INT(rotl)  // 0x30
    IROW_INT(rotr)
    IROW_FLOAT(abs)
    IROW_FLOAT(neg)
    IROW_FLOAT(ceil)
    IROW_FLOAT(floor)
    IROW_FLOAT(trunc)
    IROW_FLOAT(nearest)
    IROW_FLOAT(sqrt)
    IROW_FLOAT(min)
    IROW_FLOAT(max)
    IROW_FLOAT(copysign)
    NULL, NULL, D(cvt_i8_i16), D(cvt_i8_i32), D(cvt_i8_i64), NULL, NULL, NULL,
    NULL, NULL, D(cvt_i8_s_i16), D(cvt_i8_s_i32), D(cvt_i8_s_i64), NULL, NULL, NULL,
    NULL, D(cvt_i16_i8),   NULL, D(cvt_i16_i32), D(cvt_i16_i64), NULL, NULL, NULL,
    NULL, D(cvt_i16_s_i8), NULL, D(cvt_i16_s_i32), D(cvt_i16_s_i64), NULL, NULL, NULL,
    NULL, D(cvt_i32_i8),   D(cvt_i32_i16),   NULL, D(cvt_i32_i64), D(cvt_i32_f32), D(cvt_i32_f64), NULL,  // 0x40
    NULL, D(cvt_i32_s_i8), D(cvt_i32_s_i16), NULL, D(cvt_i32_s_i64), D(cvt_i32_s_f32), D(cvt_i32_s_f64), NULL,
    NULL, D(cvt_i64_i8),   D(cvt_i64_i16),   D(cvt_i64_i32),   NULL, D(cvt_i64_f32), D(cvt_i64_f64), NULL,
    NULL, D(cvt_i64_s_i8), D(cvt_i64_s_i16), D(cvt_i64_s_i32), NULL, D(cvt_i64_s_f32), D(cvt_i64_s_f64), NULL,
    NULL, NULL, NULL, D(cvt_f32_i32), D(cvt_f32_i64), NULL, D(cvt_f32_f64), NULL,
    NULL, NULL, NULL, D(cvt_f32_s_i32), D(cvt_f32_s_i64), NULL, D(cvt_f32_s_f64), NULL,
    NULL, NULL, NULL, D(cvt_f64_i32), D(cvt_f64_i64), D(cvt_f64_f32), NULL, NULL,
    NULL, NULL, NULL, D(cvt_f64_s_i32), D(cvt_f64_s_i64), D(cvt_f64_s_f32), NULL, NULL,
    NULL, NULL, NULL, D(transmute_i32_f32), NULL, NULL, NULL, NULL,
    NULL, NULL, NULL, NULL, D(transmute_i64_f64), NULL, NULL, NULL,
    NULL, NULL, NULL, NULL, NULL, D(transmute_f32_i32), NULL, NULL,
    NULL, NULL, NULL, NULL, NULL, NULL, D(transmute_f64_i64), NULL,
    IROW_INT(cmpxchg)
    IROW_SAME(illegal)
    IROW_UNTYPED(mem_size)
    IROW_UNTYPED(mem_grow)
};

#undef D
#undef IROW_UNTYPED
#undef IROW_TYPED
#undef IROW_PARTIAL
#undef IROW_INT
#undef IROW_FLOAT
#undef IROW_SAME

#undef OVM_OP_EXEC
#undef OVM_OP_UNSIGNED_EXEC
#undef OVM_OP_INTEGER_EXEC
#undef OVM_OP_INTEGER_UNSIGNED_EXEC
#undef OVM_OP_FLOAT_EXEC
#undef OVMI_INSTR_PROTO
#undef OVMI_INSTR_EXEC
#undef NEXT_OP
#undef VAL

#undef OVMI_FUNC_NAME
#undef OVMI_DISPATCH_NAME
#undef OVMI_DEBUG_HOOK
#undef OVMI_EXCEPTION_HOOK
#undef OVMI_DIVIDE_CHECK_HOOK

