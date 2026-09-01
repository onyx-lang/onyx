
//
// Computed-goto interpreter
//
// Handlers are labels in one function so `values`, `memory`, and `code` stay
// in callee-saved registers. NEXT advances an instruction pointer and jumps
// through a table of &&labels. The debug include of this file injects
// __ovm_debug_hook; the release include does not.
//

#define OVMI_LABEL(name) OVMI_FUNC_NAME(name)
#define OVMI_INSTR_EXEC(name) OVMI_LABEL(name):
#define VAL(loc) values[loc]
#define DISPATCH goto *OVMI_DISPATCH_NAME[(instr)->full_instr & OVM_INSTR_MASK]

#define NEXT_OP \
    do { \
        OVMI_DEBUG_HOOK; \
        instr++; \
        DISPATCH; \
    } while (0)

static ovm_value_t OVM_RUN_NAME(ovm_engine_t *engine, ovm_state_t *state, ovm_program_t *program) {
    ovm_instr_t *code = program->code;
    ovm_instr_t *instr = &code[state->pc];
    u8 *memory = engine->memory;
    ovm_slot_t *values = state->__frame_values;

#define D(n) &&OVMI_LABEL(n)
#define IL   D(illegal)
#define IROW_UNTYPED(name) D(name), IL, IL, IL, IL, IL, IL, IL,
#define IROW_TYPED(name)   IL, D(name##_i8), D(name##_i16), D(name##_i32), D(name##_i64), D(name##_f32), D(name##_f64), IL,
#define IROW_PARTIAL(name) IL, IL, IL, D(name##_i32), D(name##_i64), D(name##_f32), D(name##_f64), IL,
#define IROW_INT(name)     IL, IL, IL, D(name##_i32), D(name##_i64), IL, IL, IL,
#define IROW_FLOAT(name)   IL, IL, IL, IL, IL, D(name##_f32), D(name##_f64), IL,
#define IROW_SAME(name)    D(name),D(name),D(name),D(name),D(name),D(name),D(name),IL,

    static const void *OVMI_DISPATCH_NAME[] = {
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
        IL, IL, D(cvt_i8_i16), D(cvt_i8_i32), D(cvt_i8_i64), IL, IL, IL,
        IL, IL, D(cvt_i8_s_i16), D(cvt_i8_s_i32), D(cvt_i8_s_i64), IL, IL, IL,
        IL, D(cvt_i16_i8),   IL, D(cvt_i16_i32), D(cvt_i16_i64), IL, IL, IL,
        IL, D(cvt_i16_s_i8), IL, D(cvt_i16_s_i32), D(cvt_i16_s_i64), IL, IL, IL,
        IL, D(cvt_i32_i8),   D(cvt_i32_i16),   IL, D(cvt_i32_i64), D(cvt_i32_f32), D(cvt_i32_f64), IL,  // 0x40
        IL, D(cvt_i32_s_i8), D(cvt_i32_s_i16), IL, D(cvt_i32_s_i64), D(cvt_i32_s_f32), D(cvt_i32_s_f64), IL,
        IL, D(cvt_i64_i8),   D(cvt_i64_i16),   D(cvt_i64_i32),   IL, D(cvt_i64_f32), D(cvt_i64_f64), IL,
        IL, D(cvt_i64_s_i8), D(cvt_i64_s_i16), D(cvt_i64_s_i32), IL, D(cvt_i64_s_f32), D(cvt_i64_s_f64), IL,
        IL, IL, IL, D(cvt_f32_i32), D(cvt_f32_i64), IL, D(cvt_f32_f64), IL,
        IL, IL, IL, D(cvt_f32_s_i32), D(cvt_f32_s_i64), IL, D(cvt_f32_s_f64), IL,
        IL, IL, IL, D(cvt_f64_i32), D(cvt_f64_i64), D(cvt_f64_f32), IL, IL,
        IL, IL, IL, D(cvt_f64_s_i32), D(cvt_f64_s_i64), D(cvt_f64_s_f32), IL, IL,
        IL, IL, IL, D(transmute_i32_f32), IL, IL, IL, IL,
        IL, IL, IL, IL, D(transmute_i64_f64), IL, IL, IL,
        IL, IL, IL, IL, IL, D(transmute_f32_i32), IL, IL,
        IL, IL, IL, IL, IL, IL, D(transmute_f64_i64), IL,
        IROW_INT(cmpxchg)
        IROW_SAME(illegal)
        IROW_UNTYPED(mem_size)
        IROW_UNTYPED(mem_grow)
        IROW_INT(add_imm) // 0x50
        IROW_INT(sub_imm)
        IROW_INT(mul_imm)
        IROW_INT(and_imm)
        IROW_INT(or_imm)
        IROW_INT(xor_imm)
        IROW_INT(shl_imm)
        IROW_INT(shr_imm)
        IROW_INT(sar_imm)
        IROW_PARTIAL(br_eq)
        IROW_PARTIAL(br_ne)
        IROW_PARTIAL(br_lt)
        IROW_PARTIAL(br_lt_s)
        IROW_PARTIAL(br_le)
        IROW_PARTIAL(br_le_s)
        IROW_PARTIAL(br_gt)
        IROW_PARTIAL(br_gt_s)
        IROW_PARTIAL(br_ge)
        IROW_PARTIAL(br_ge_s)
    };

#undef D
#undef IL
#undef IROW_UNTYPED
#undef IROW_TYPED
#undef IROW_PARTIAL
#undef IROW_INT
#undef IROW_FLOAT
#undef IROW_SAME

    DISPATCH;

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
    OVMI_INSTR_EXEC(name##_i32) { OVM_OP(op, i32); NEXT_OP; } \
    OVMI_INSTR_EXEC(name##_i64) { OVM_OP(op, i64); NEXT_OP; } \
    OVMI_INSTR_EXEC(name##_f32) { OVM_OP(op, f32); NEXT_OP; } \
    OVMI_INSTR_EXEC(name##_f64) { OVM_OP(op, f64); NEXT_OP; }

#define OVM_OP_UNSIGNED_EXEC(name, op) \
    OVMI_INSTR_EXEC(name##_i32) { OVM_OP(op, u32); NEXT_OP; } \
    OVMI_INSTR_EXEC(name##_i64) { OVM_OP(op, u64); NEXT_OP; } \
    OVMI_INSTR_EXEC(name##_f32) { OVM_OP(op, f32); NEXT_OP; } \
    OVMI_INSTR_EXEC(name##_f64) { OVM_OP(op, f64); NEXT_OP; }

#define OVM_OP_INTEGER_EXEC(name, op) \
    OVMI_INSTR_EXEC(name##_i32) { OVM_OP(op, i32); NEXT_OP; } \
    OVMI_INSTR_EXEC(name##_i64) { OVM_OP(op, i64); NEXT_OP; }

#define OVM_OP_INTEGER_UNSIGNED_EXEC(name, op) \
    OVMI_INSTR_EXEC(name##_i32) { OVM_OP(op, u32); NEXT_OP; } \
    OVMI_INSTR_EXEC(name##_i64) { OVM_OP(op, u64); NEXT_OP; }

#define OVM_OP_FLOAT_EXEC(name, op) \
    OVMI_INSTR_EXEC(name##_f32) { OVM_OP(op, f32); NEXT_OP; } \
    OVMI_INSTR_EXEC(name##_f64) { OVM_OP(op, f64); NEXT_OP; }


#define OVM_OP(op, ctype) \
    VAL(instr->r).ctype = VAL(instr->a).ctype op VAL(instr->b).ctype;

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


#define OVM_OP(op, ctype) \
    OVMI_DIVIDE_CHECK_HOOK(ctype); \
    VAL(instr->r).ctype = VAL(instr->a).ctype op VAL(instr->b).ctype;

OVM_OP_EXEC(div_s, /)
OVM_OP_UNSIGNED_EXEC(div, /)
OVM_OP_INTEGER_UNSIGNED_EXEC(rem, %)
OVM_OP_INTEGER_EXEC(rem_s, %)

#undef OVM_OP

#define OVM_OP(func, ctype) \
    VAL(instr->r).ctype = func( VAL(instr->a).ctype, VAL(instr->b).ctype );

OVMI_INSTR_EXEC(rotl_i32) { OVM_OP(rotl32, u32); NEXT_OP; }
OVMI_INSTR_EXEC(rotl_i64) { OVM_OP(rotl64, u64); NEXT_OP; }
OVMI_INSTR_EXEC(rotr_i32) { OVM_OP(rotr32, u32); NEXT_OP; }
OVMI_INSTR_EXEC(rotr_i64) { OVM_OP(rotr64, u64); NEXT_OP; }

OVM_OP_FLOAT_EXEC(min, bh_min)
OVM_OP_FLOAT_EXEC(max, bh_max)
OVM_OP_FLOAT_EXEC(copysign, __ovm_copysign)

#undef OVM_OP


#define OVM_OP(op, ctype) \
    VAL(instr->r).ctype = (ctype) op (VAL(instr->a).ctype);

OVMI_INSTR_EXEC(clz_i32) { OVM_OP(__ovm_clz, u32);   NEXT_OP; }
OVMI_INSTR_EXEC(clz_i64) { OVM_OP(__ovm_clzll, u64); NEXT_OP; }
OVMI_INSTR_EXEC(ctz_i32) { OVM_OP(__ovm_ctz, u32);   NEXT_OP; }
OVMI_INSTR_EXEC(ctz_i64) { OVM_OP(__ovm_ctzll, u64); NEXT_OP; }
OVMI_INSTR_EXEC(popcount_i32) { OVM_OP(__ovm_popcount, u32);   NEXT_OP; }
OVMI_INSTR_EXEC(popcount_i64) { OVM_OP(__ovm_popcountll, u64); NEXT_OP; }

OVM_OP_FLOAT_EXEC(abs,     __ovm_abs)
OVM_OP_FLOAT_EXEC(neg,     -)
OVM_OP_FLOAT_EXEC(ceil,    __ovm_ceil)
OVM_OP_FLOAT_EXEC(floor,   __ovm_floor)
OVM_OP_FLOAT_EXEC(trunc,   __ovm_trunc)
OVM_OP_FLOAT_EXEC(nearest, __ovm_nearest)
OVM_OP_FLOAT_EXEC(sqrt,    sqrt)

#undef OVM_OP


#define OVM_OP(op, ctype) \
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
// Immediate / fused binary
//

#define OVM_OP_IMM(op, ctype, icast) \
    VAL(instr->r).ctype = VAL(instr->a).ctype op (ctype) (icast) instr->b;

OVMI_INSTR_EXEC(add_imm_i32) { OVM_OP_IMM(+, i32, i32); NEXT_OP; }
OVMI_INSTR_EXEC(add_imm_i64) { OVM_OP_IMM(+, i64, i32); NEXT_OP; }
OVMI_INSTR_EXEC(sub_imm_i32) { OVM_OP_IMM(-, i32, i32); NEXT_OP; }
OVMI_INSTR_EXEC(sub_imm_i64) { OVM_OP_IMM(-, i64, i32); NEXT_OP; }
OVMI_INSTR_EXEC(mul_imm_i32) { OVM_OP_IMM(*, i32, i32); NEXT_OP; }
OVMI_INSTR_EXEC(mul_imm_i64) { OVM_OP_IMM(*, i64, i32); NEXT_OP; }
OVMI_INSTR_EXEC(and_imm_i32) { OVM_OP_IMM(&, u32, u32); NEXT_OP; }
OVMI_INSTR_EXEC(and_imm_i64) { OVM_OP_IMM(&, u64, i32); NEXT_OP; }
OVMI_INSTR_EXEC(or_imm_i32)  { OVM_OP_IMM(|, u32, u32); NEXT_OP; }
OVMI_INSTR_EXEC(or_imm_i64)  { OVM_OP_IMM(|, u64, i32); NEXT_OP; }
OVMI_INSTR_EXEC(xor_imm_i32) { OVM_OP_IMM(^, u32, u32); NEXT_OP; }
OVMI_INSTR_EXEC(xor_imm_i64) { OVM_OP_IMM(^, u64, i32); NEXT_OP; }
OVMI_INSTR_EXEC(shl_imm_i32) { OVM_OP_IMM(<<, u32, u32); NEXT_OP; }
OVMI_INSTR_EXEC(shl_imm_i64) { OVM_OP_IMM(<<, u64, u32); NEXT_OP; }
OVMI_INSTR_EXEC(shr_imm_i32) { OVM_OP_IMM(>>, u32, u32); NEXT_OP; }
OVMI_INSTR_EXEC(shr_imm_i64) { OVM_OP_IMM(>>, u64, u32); NEXT_OP; }
OVMI_INSTR_EXEC(sar_imm_i32) { OVM_OP_IMM(>>, i32, i32); NEXT_OP; }
OVMI_INSTR_EXEC(sar_imm_i64) { OVM_OP_IMM(>>, i64, i32); NEXT_OP; }

#undef OVM_OP_IMM


//
// Memory / register operations
//

#define OVM_IMM(dtype, stype) \
    VAL(instr->r).u64 = 0; \
    VAL(instr->r).dtype = instr->stype;


OVMI_INSTR_EXEC(imm_i32) { OVM_IMM(u32, i); NEXT_OP; }
OVMI_INSTR_EXEC(imm_i64) { OVM_IMM(u64, l); NEXT_OP; }
OVMI_INSTR_EXEC(imm_f32) { OVM_IMM(f32, f); NEXT_OP; }
OVMI_INSTR_EXEC(imm_f64) { OVM_IMM(f64, d); NEXT_OP; }

#undef OVM_IMM

OVMI_INSTR_EXEC(mov) {
    VAL(instr->r) = VAL(instr->a);
    NEXT_OP;
}

#define OVM_LOAD(otype, stype) \
    OVMI_INSTR_EXEC(load_##otype) { \
        u32 dest = VAL(instr->a).u32 + (u32) instr->b; \
        if (dest == 0) OVMI_EXCEPTION_HOOK; \
        VAL(instr->r).stype = * (stype *) &memory[dest]; \
        NEXT_OP; \
    }

OVM_LOAD(i8,  u8)
OVM_LOAD(i16, u16)
OVM_LOAD(i32, u32)
OVM_LOAD(i64, u64)
OVM_LOAD(f32, f32)
OVM_LOAD(f64, f64)

#undef OVM_LOAD

#define OVM_STORE(otype, stype) \
    OVMI_INSTR_EXEC(store_##otype) { \
        u32 dest = VAL(instr->r).u32 + (u32) instr->b; \
        if (dest == 0) OVMI_EXCEPTION_HOOK; \
        *(stype *) &memory[dest] = VAL(instr->a).stype; \
        NEXT_OP; \
    }

OVM_STORE(i8,  u8)
OVM_STORE(i16, u16)
OVM_STORE(i32, u32)
OVM_STORE(i64, u64)
OVM_STORE(f32, f32)
OVM_STORE(f64, f64)

#undef OVM_STORE

OVMI_INSTR_EXEC(copy) {
    u32 dest  = VAL(instr->r).u32;
    u32 src   = VAL(instr->a).u32;
    u32 count = VAL(instr->b).u32;

    if (!dest || !src) OVMI_EXCEPTION_HOOK;

    if ((u64) dest + count > (u64) state->engine->memory_size
        || (u64) src + count > (u64) state->engine->memory_size) {
        OVMI_EXCEPTION_HOOK;
        ovm_value_t bad_val;
        bad_val.type = OVM_TYPE_ERR;
        return bad_val;
    }

    memmove(&memory[dest], &memory[src], count);

    NEXT_OP;
}

OVMI_INSTR_EXEC(fill) {
    u32 dest  = VAL(instr->r).u32;
    u8  byte  = VAL(instr->a).u8;
    u32 count = VAL(instr->b).u32;

    if (!dest) OVMI_EXCEPTION_HOOK;

    if ((u64) dest + count > (u64) state->engine->memory_size) {
        OVMI_EXCEPTION_HOOK;
        ovm_value_t bad_val;
        bad_val.type = OVM_TYPE_ERR;
        return bad_val;
    }

    memset(&memory[dest], byte, count);

    NEXT_OP;
}

OVMI_INSTR_EXEC(reg_get) {
    VAL(instr->r).u64 = state->registers[instr->a].u64;
    NEXT_OP;
}

OVMI_INSTR_EXEC(reg_set) {
    state->registers[instr->r].u64 = VAL(instr->a).u64;
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
    NEXT_OP;
}


//
// Function calling
//

OVMI_INSTR_EXEC(param) {
    ovm_assert((state->param_count <= OVM_MAX_PARAM_COUNT));
    state->param_buf[state->param_count++].u64 = VAL(instr->a).u64;
    NEXT_OP;
}

OVMI_INSTR_EXEC(return) {
    ovm_slot_t val = VAL(instr->a);
    ovm_stack_frame_t frame = ovm__func_teardown_stack_frame(state);
    values = state->__frame_values;

    if (bh_arr_length(state->stack_frames) == 0) {
        ovm_value_t out = {0};
        out.u64 = val.u64;
        return out;
    }

    ovm_func_t *new_func = bh_arr_last(state->stack_frames).func;
    if (new_func->kind == OVM_FUNC_EXTERNAL) {
        ovm_value_t out = {0};
        out.u64 = val.u64;
        return out;
    }

    if (frame.return_number_value >= 0) {
        VAL(frame.return_number_value) = val;
    }

    if (bh_arr_last(state->stack_frames).compiled) {
        ovm_value_t out = {0};
        out.u64 = val.u64;
        return out;
    }

#ifdef OVM_VERBOSE
    printf("Returning from %s to %s: ", frame.func->name, bh_arr_last(state->stack_frames).func->name);
    ovm_print_val((ovm_value_t) { .u64 = val.u64 });
    printf("\n\n");
#endif

    instr = &code[frame.return_address];
    DISPATCH;
}


#define OVM_CALL_CODE(func_idx) \
    i32 fidx = func_idx; \
    ovm_func_t *func = &state->program->funcs[fidx]; \
    i32 extra_params = state->param_count - func->param_count; \
    ovm_assert(extra_params >= 0); \
    state->pc = (i32)(instr - code) + 1; \
    ovm__func_setup_stack_frame(state, func, instr->r); \
    state->param_count -= func->param_count; \
    if (func->kind == OVM_FUNC_INTERNAL) { \
        values = state->__frame_values; \
        for (i32 pi = 0; pi < func->param_count; pi++) { \
            VAL(pi).u64 = state->param_buf[extra_params + pi].u64; \
        } \
        if (func->native_entry) { \
            bh_arr_last(state->stack_frames).compiled = 1; \
            func->native_entry(state->engine, state, state->program); \
            values = state->__frame_values; \
            memory = state->engine->memory; \
        } else { \
            instr = &code[func->start_instr]; \
            DISPATCH; \
        } \
    } else { \
        ovm_external_func_t external_func = state->external_funcs[func->external_func_idx]; \
        external_func.native_func(external_func.userdata, &state->param_buf[extra_params], &state->__tmp_value); \
        memory = state->engine->memory; \
\
        ovm__func_teardown_stack_frame(state); \
\
        if (instr->r >= 0) { \
            VAL(instr->r).u64 = state->__tmp_value.u64; \
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

OVMI_INSTR_EXEC(br)     { instr += instr->a; NEXT_OP; }
OVMI_INSTR_EXEC(bri)    { instr += VAL(instr->a).i32; NEXT_OP; }
OVMI_INSTR_EXEC(br_nz)  { if (VAL(instr->b).i32 != 0) instr += instr->a; NEXT_OP; }
OVMI_INSTR_EXEC(bri_nz) { if (VAL(instr->b).i32 != 0) instr += VAL(instr->a).i32; NEXT_OP; }
OVMI_INSTR_EXEC(br_z)   { if (VAL(instr->b).i32 == 0) instr += instr->a; NEXT_OP; }
OVMI_INSTR_EXEC(bri_z)  { if (VAL(instr->b).i32 == 0) instr += VAL(instr->a).i32; NEXT_OP; }

#define OVM_BR_CMP_EXEC(name, op) \
    OVMI_INSTR_EXEC(name##_i32) { if (VAL(instr->a).i32 op VAL(instr->b).i32) instr += instr->r; NEXT_OP; } \
    OVMI_INSTR_EXEC(name##_i64) { if (VAL(instr->a).i64 op VAL(instr->b).i64) instr += instr->r; NEXT_OP; } \
    OVMI_INSTR_EXEC(name##_f32) { if (VAL(instr->a).f32 op VAL(instr->b).f32) instr += instr->r; NEXT_OP; } \
    OVMI_INSTR_EXEC(name##_f64) { if (VAL(instr->a).f64 op VAL(instr->b).f64) instr += instr->r; NEXT_OP; }

#define OVM_BR_CMP_UNSIGNED_EXEC(name, op) \
    OVMI_INSTR_EXEC(name##_i32) { if (VAL(instr->a).u32 op VAL(instr->b).u32) instr += instr->r; NEXT_OP; } \
    OVMI_INSTR_EXEC(name##_i64) { if (VAL(instr->a).u64 op VAL(instr->b).u64) instr += instr->r; NEXT_OP; } \
    OVMI_INSTR_EXEC(name##_f32) { if (VAL(instr->a).f32 op VAL(instr->b).f32) instr += instr->r; NEXT_OP; } \
    OVMI_INSTR_EXEC(name##_f64) { if (VAL(instr->a).f64 op VAL(instr->b).f64) instr += instr->r; NEXT_OP; }

OVM_BR_CMP_EXEC(br_eq, ==)
OVM_BR_CMP_EXEC(br_ne, !=)
OVM_BR_CMP_UNSIGNED_EXEC(br_lt, <)
OVM_BR_CMP_EXEC(br_lt_s, <)
OVM_BR_CMP_UNSIGNED_EXEC(br_le, <=)
OVM_BR_CMP_EXEC(br_le_s, <=)
OVM_BR_CMP_UNSIGNED_EXEC(br_gt, >)
OVM_BR_CMP_EXEC(br_gt_s, >)
OVM_BR_CMP_UNSIGNED_EXEC(br_ge, >=)
OVM_BR_CMP_EXEC(br_ge_s, >=)

#undef OVM_BR_CMP_EXEC
#undef OVM_BR_CMP_UNSIGNED_EXEC


//
// Conversion
//

#define OVM_CVT(n1, n2, stype, dtype, ctype) \
    OVMI_INSTR_EXEC(cvt_##n1##_##n2) { \
        VAL(instr->r).dtype = (ctype) VAL(instr->a).stype; \
        NEXT_OP; \
    }

OVM_CVT(i8, i16,   u8, u16, u16);
OVM_CVT(i8, i32,   u8, u32, u32);
OVM_CVT(i8, i64,   u8, u64, u64);
OVM_CVT(i8_s, i16, i8, i16, i16);
OVM_CVT(i8_s, i32, i8, i32, i32);
OVM_CVT(i8_s, i64, i8, i64, i64);

OVM_CVT(i16,   i8,  u16, u8,  u8);
OVM_CVT(i16,   i32, u16, u32, u32);
OVM_CVT(i16,   i64, u16, u64, u64);
OVM_CVT(i16_s, i8,  i16, i8,  i8);
OVM_CVT(i16_s, i32, i16, i32, i32);
OVM_CVT(i16_s, i64, i16, i64, i64);

OVM_CVT(i32,   i8,  u32, u8,  u8);
OVM_CVT(i32,   i16, u32, u16, u16);
OVM_CVT(i32,   i64, u32, u64, u64);
OVM_CVT(i32_s, i8,  i32, i8,  i8);
OVM_CVT(i32_s, i16, i32, i16, i16);
OVM_CVT(i32_s, i64, i32, i64, i64);
OVM_CVT(i32,   f32, u32, f32, f32);
OVM_CVT(i32_s, f32, i32, f32, f32);
OVM_CVT(i32,   f64, u32, f64, f64);
OVM_CVT(i32_s, f64, i32, f64, f64);

OVM_CVT(i64,   i8,  u64, u8,  u8);
OVM_CVT(i64,   i16, u64, u16, u16);
OVM_CVT(i64,   i32, u64, u32, u32);
OVM_CVT(i64_s, i8,  i64, i8,  i8);
OVM_CVT(i64_s, i16, i64, i16, i16);
OVM_CVT(i64_s, i32, i64, i32, i32);
OVM_CVT(i64,   f32, u64, f32, f32);
OVM_CVT(i64_s, f32, i64, f32, f32);
OVM_CVT(i64,   f64, u64, f64, f64);
OVM_CVT(i64_s, f64, i64, f64, f64);

OVM_CVT(f32,   i32, f32, u32, u32);
OVM_CVT(f32,   i64, f32, u64, u64);
OVM_CVT(f32,   f64, f32, f64, f64);
OVM_CVT(f32_s, i32, f32, i32, i32);
OVM_CVT(f32_s, i64, f32, i64, i64);
OVM_CVT(f32_s, f64, f32, f64, f64);

OVM_CVT(f64,   i32, f64, u32, u32);
OVM_CVT(f64,   i64, f64, u64, u64);
OVM_CVT(f64,   f32, f64, f32, f32);
OVM_CVT(f64_s, i32, f64, i32, i32);
OVM_CVT(f64_s, i64, f64, i64, i64);
OVM_CVT(f64_s, f32, f64, f32, f32);

#undef OVM_CVT

#define OVM_CVT(n1, n2, stype, dtype, ctype) \
    OVMI_INSTR_EXEC(transmute_##n1##_##n2) { \
        VAL(instr->r).dtype = *(ctype *) &VAL(instr->a).stype; \
        NEXT_OP; \
    }

OVM_CVT(i32, f32, u32, f32, f32);
OVM_CVT(i64, f64, u64, f64, f64);
OVM_CVT(f32, i32, f32, u32, u32);
OVM_CVT(f64, i64, f64, u64, u64);

#undef OVM_CVT


//
// Compare exchange
//

#define CMPXCHG(ctype) \
    OVMI_INSTR_EXEC(cmpxchg_##ctype) { \
        pthread_mutex_lock(&state->engine->atomic_mutex); \
        if (VAL(instr->r).u32 == 0) OVMI_EXCEPTION_HOOK; \
        ctype *addr = (ctype *) &memory[VAL(instr->r).u32]; \
 \
        VAL(instr->r).u64 = 0; \
        VAL(instr->r).ctype = *addr; \
 \
        if (*addr == VAL(instr->a).ctype) { \
            *addr = VAL(instr->b).ctype ; \
        } \
 \
        pthread_mutex_unlock(&state->engine->atomic_mutex); \
        NEXT_OP; \
    }

CMPXCHG(i32)
CMPXCHG(i64)

#undef CMPXCHG


//
// Memory
//

OVMI_INSTR_EXEC(mem_size) {
    VAL(instr->r).u32 = (u32) (state->engine->memory_size / 65536);
    NEXT_OP;
}

OVMI_INSTR_EXEC(mem_grow) {
    VAL(instr->r).u32 = (u32) (state->engine->memory_size / 65536);

    i64 new_size = state->engine->memory_size + (i64) VAL(instr->a).u32 * 65536;
    if (!ovm_engine_memory_ensure_capacity(state->engine, new_size)) {
        VAL(instr->r).i32 = -1;
    } else {
        state->engine->memory_size = new_size;
    }

    memory = state->engine->memory;
    NEXT_OP;
}


OVMI_INSTR_EXEC(illegal) {
    OVMI_EXCEPTION_HOOK;
    return ((ovm_value_t) {0});
}

}

#undef OVM_OP_EXEC
#undef OVM_OP_UNSIGNED_EXEC
#undef OVM_OP_INTEGER_EXEC
#undef OVM_OP_INTEGER_UNSIGNED_EXEC
#undef OVM_OP_FLOAT_EXEC
#undef OVMI_INSTR_EXEC
#undef OVMI_LABEL
#undef NEXT_OP
#undef DISPATCH
#undef VAL

#undef OVMI_FUNC_NAME
#undef OVMI_DISPATCH_NAME
#undef OVM_RUN_NAME
#undef OVMI_DEBUG_HOOK
#undef OVMI_EXCEPTION_HOOK
#undef OVMI_DIVIDE_CHECK_HOOK
