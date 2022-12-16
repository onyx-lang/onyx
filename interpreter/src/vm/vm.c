#include "vm.h"

#include <sys/mman.h>
#include <x86intrin.h>
#include <math.h> // REMOVE THIS!!!  only needed for sqrt
#include <pthread.h>

#ifdef OVM_DEBUG
#define ovm_assert(c) assert((c))
#else
#define ovm_assert(c)
#endif


static inline void ovm_print_val(ovm_value_t val) {
    switch (val.type) {
        case OVM_TYPE_I32: printf("i32[%d]", val.i32); break;
        case OVM_TYPE_I64: printf("i64[%ld]", val.i64); break;
        case OVM_TYPE_F32: printf("f32[%f]", val.f32); break;
        case OVM_TYPE_F64: printf("f64[%lf]", val.f64); break;
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


static char *ovm_instr_name(i32 full_instr) {
    return "";
}

#if 0
static char *ovm_instr_name(i32 full_instr) {
#define C(...) \
    case __VA_ARGS__: return #__VA_ARGS__; \
    case __VA_ARGS__ | OVMI_ATOMIC: return "ATOMIC_" #__VA_ARGS__;

    static char buf[64];

    switch (full_instr) {
        C(OVMI_NOP)

        C(OVM_TYPED_INSTR(OVMI_ADD, OVM_TYPE_I8))
        C(OVM_TYPED_INSTR(OVMI_ADD, OVM_TYPE_I16))
        C(OVM_TYPED_INSTR(OVMI_ADD, OVM_TYPE_I32))
        C(OVM_TYPED_INSTR(OVMI_ADD, OVM_TYPE_I64))
        C(OVM_TYPED_INSTR(OVMI_ADD, OVM_TYPE_F32))
        C(OVM_TYPED_INSTR(OVMI_ADD, OVM_TYPE_F64))

        C(OVM_TYPED_INSTR(OVMI_SUB, OVM_TYPE_I8))
        C(OVM_TYPED_INSTR(OVMI_SUB, OVM_TYPE_I16))
        C(OVM_TYPED_INSTR(OVMI_SUB, OVM_TYPE_I32))
        C(OVM_TYPED_INSTR(OVMI_SUB, OVM_TYPE_I64))
        C(OVM_TYPED_INSTR(OVMI_SUB, OVM_TYPE_F32))
        C(OVM_TYPED_INSTR(OVMI_SUB, OVM_TYPE_F64))

        C(OVM_TYPED_INSTR(OVMI_MUL, OVM_TYPE_I8))
        C(OVM_TYPED_INSTR(OVMI_MUL, OVM_TYPE_I16))
        C(OVM_TYPED_INSTR(OVMI_MUL, OVM_TYPE_I32))
        C(OVM_TYPED_INSTR(OVMI_MUL, OVM_TYPE_I64))
        C(OVM_TYPED_INSTR(OVMI_MUL, OVM_TYPE_F32))
        C(OVM_TYPED_INSTR(OVMI_MUL, OVM_TYPE_F64))

        C(OVM_TYPED_INSTR(OVMI_DIV, OVM_TYPE_I8))
        C(OVM_TYPED_INSTR(OVMI_DIV, OVM_TYPE_I16))
        C(OVM_TYPED_INSTR(OVMI_DIV, OVM_TYPE_I32))
        C(OVM_TYPED_INSTR(OVMI_DIV, OVM_TYPE_I64))
        C(OVM_TYPED_INSTR(OVMI_DIV, OVM_TYPE_F32))
        C(OVM_TYPED_INSTR(OVMI_DIV, OVM_TYPE_F64))

        C(OVM_TYPED_INSTR(OVMI_DIV_S, OVM_TYPE_I8))
        C(OVM_TYPED_INSTR(OVMI_DIV_S, OVM_TYPE_I16))
        C(OVM_TYPED_INSTR(OVMI_DIV_S, OVM_TYPE_I32))
        C(OVM_TYPED_INSTR(OVMI_DIV_S, OVM_TYPE_I64))
        C(OVM_TYPED_INSTR(OVMI_DIV_S, OVM_TYPE_F32))
        C(OVM_TYPED_INSTR(OVMI_DIV_S, OVM_TYPE_F64))

        C(OVM_TYPED_INSTR(OVMI_REM, OVM_TYPE_I8))
        C(OVM_TYPED_INSTR(OVMI_REM, OVM_TYPE_I16))
        C(OVM_TYPED_INSTR(OVMI_REM, OVM_TYPE_I32))
        C(OVM_TYPED_INSTR(OVMI_REM, OVM_TYPE_I64))

        C(OVM_TYPED_INSTR(OVMI_REM_S, OVM_TYPE_I8))
        C(OVM_TYPED_INSTR(OVMI_REM_S, OVM_TYPE_I16))
        C(OVM_TYPED_INSTR(OVMI_REM_S, OVM_TYPE_I32))
        C(OVM_TYPED_INSTR(OVMI_REM_S, OVM_TYPE_I64))

        C(OVM_TYPED_INSTR(OVMI_AND, OVM_TYPE_I8))
        C(OVM_TYPED_INSTR(OVMI_AND, OVM_TYPE_I16))
        C(OVM_TYPED_INSTR(OVMI_AND, OVM_TYPE_I32))
        C(OVM_TYPED_INSTR(OVMI_AND, OVM_TYPE_I64))

        C(OVM_TYPED_INSTR(OVMI_OR, OVM_TYPE_I8))
        C(OVM_TYPED_INSTR(OVMI_OR, OVM_TYPE_I16))
        C(OVM_TYPED_INSTR(OVMI_OR, OVM_TYPE_I32))
        C(OVM_TYPED_INSTR(OVMI_OR, OVM_TYPE_I64))

        C(OVM_TYPED_INSTR(OVMI_XOR, OVM_TYPE_I8))
        C(OVM_TYPED_INSTR(OVMI_XOR, OVM_TYPE_I16))
        C(OVM_TYPED_INSTR(OVMI_XOR, OVM_TYPE_I32))
        C(OVM_TYPED_INSTR(OVMI_XOR, OVM_TYPE_I64))

        C(OVM_TYPED_INSTR(OVMI_SHL, OVM_TYPE_I8))
        C(OVM_TYPED_INSTR(OVMI_SHL, OVM_TYPE_I16))
        C(OVM_TYPED_INSTR(OVMI_SHL, OVM_TYPE_I32))
        C(OVM_TYPED_INSTR(OVMI_SHL, OVM_TYPE_I64))

        C(OVM_TYPED_INSTR(OVMI_SHR, OVM_TYPE_I8))
        C(OVM_TYPED_INSTR(OVMI_SHR, OVM_TYPE_I16))
        C(OVM_TYPED_INSTR(OVMI_SHR, OVM_TYPE_I32))
        C(OVM_TYPED_INSTR(OVMI_SHR, OVM_TYPE_I64))

        C(OVM_TYPED_INSTR(OVMI_SAR, OVM_TYPE_I8))
        C(OVM_TYPED_INSTR(OVMI_SAR, OVM_TYPE_I16))
        C(OVM_TYPED_INSTR(OVMI_SAR, OVM_TYPE_I32))
        C(OVM_TYPED_INSTR(OVMI_SAR, OVM_TYPE_I64))

        C(OVM_TYPED_INSTR(OVMI_IMM, OVM_TYPE_I8))
        C(OVM_TYPED_INSTR(OVMI_IMM, OVM_TYPE_I16))
        C(OVM_TYPED_INSTR(OVMI_IMM, OVM_TYPE_I32))
        C(OVM_TYPED_INSTR(OVMI_IMM, OVM_TYPE_I64))
        C(OVM_TYPED_INSTR(OVMI_IMM, OVM_TYPE_F32))
        C(OVM_TYPED_INSTR(OVMI_IMM, OVM_TYPE_F64))

        C(OVMI_MOV)
        C(OVM_TYPED_INSTR(OVMI_LOAD, OVM_TYPE_I8))
        C(OVM_TYPED_INSTR(OVMI_LOAD, OVM_TYPE_I16))
        C(OVM_TYPED_INSTR(OVMI_LOAD, OVM_TYPE_I32))
        C(OVM_TYPED_INSTR(OVMI_LOAD, OVM_TYPE_I64))
        C(OVM_TYPED_INSTR(OVMI_LOAD, OVM_TYPE_F32))
        C(OVM_TYPED_INSTR(OVMI_LOAD, OVM_TYPE_F64))
        C(OVM_TYPED_INSTR(OVMI_STORE, OVM_TYPE_I8))
        C(OVM_TYPED_INSTR(OVMI_STORE, OVM_TYPE_I16))
        C(OVM_TYPED_INSTR(OVMI_STORE, OVM_TYPE_I32))
        C(OVM_TYPED_INSTR(OVMI_STORE, OVM_TYPE_I64))
        C(OVM_TYPED_INSTR(OVMI_STORE, OVM_TYPE_F32))
        C(OVM_TYPED_INSTR(OVMI_STORE, OVM_TYPE_F64))
        C(OVMI_COPY)
        C(OVMI_FILL)

        C(OVMI_REG_GET)
        C(OVMI_REG_SET)

        C(OVMI_IDX_ARR)

        C(OVM_TYPED_INSTR(OVMI_LT, OVM_TYPE_I8))
        C(OVM_TYPED_INSTR(OVMI_LT, OVM_TYPE_I16))
        C(OVM_TYPED_INSTR(OVMI_LT, OVM_TYPE_I32))
        C(OVM_TYPED_INSTR(OVMI_LT, OVM_TYPE_I64))
        C(OVM_TYPED_INSTR(OVMI_LT, OVM_TYPE_F32))
        C(OVM_TYPED_INSTR(OVMI_LT, OVM_TYPE_F64))

        C(OVM_TYPED_INSTR(OVMI_LT_S, OVM_TYPE_I8))
        C(OVM_TYPED_INSTR(OVMI_LT_S, OVM_TYPE_I16))
        C(OVM_TYPED_INSTR(OVMI_LT_S, OVM_TYPE_I32))
        C(OVM_TYPED_INSTR(OVMI_LT_S, OVM_TYPE_I64))
        C(OVM_TYPED_INSTR(OVMI_LT_S, OVM_TYPE_F32))
        C(OVM_TYPED_INSTR(OVMI_LT_S, OVM_TYPE_F64))

        C(OVM_TYPED_INSTR(OVMI_LE, OVM_TYPE_I8))
        C(OVM_TYPED_INSTR(OVMI_LE, OVM_TYPE_I16))
        C(OVM_TYPED_INSTR(OVMI_LE, OVM_TYPE_I32))
        C(OVM_TYPED_INSTR(OVMI_LE, OVM_TYPE_I64))
        C(OVM_TYPED_INSTR(OVMI_LE, OVM_TYPE_F32))
        C(OVM_TYPED_INSTR(OVMI_LE, OVM_TYPE_F64))

        C(OVM_TYPED_INSTR(OVMI_LE_S, OVM_TYPE_I8))
        C(OVM_TYPED_INSTR(OVMI_LE_S, OVM_TYPE_I16))
        C(OVM_TYPED_INSTR(OVMI_LE_S, OVM_TYPE_I32))
        C(OVM_TYPED_INSTR(OVMI_LE_S, OVM_TYPE_I64))
        C(OVM_TYPED_INSTR(OVMI_LE_S, OVM_TYPE_F32))
        C(OVM_TYPED_INSTR(OVMI_LE_S, OVM_TYPE_F64))

        C(OVM_TYPED_INSTR(OVMI_EQ, OVM_TYPE_I8))
        C(OVM_TYPED_INSTR(OVMI_EQ, OVM_TYPE_I16))
        C(OVM_TYPED_INSTR(OVMI_EQ, OVM_TYPE_I32))
        C(OVM_TYPED_INSTR(OVMI_EQ, OVM_TYPE_I64))
        C(OVM_TYPED_INSTR(OVMI_EQ, OVM_TYPE_F32))
        C(OVM_TYPED_INSTR(OVMI_EQ, OVM_TYPE_F64))

        C(OVM_TYPED_INSTR(OVMI_GE, OVM_TYPE_I8))
        C(OVM_TYPED_INSTR(OVMI_GE, OVM_TYPE_I16))
        C(OVM_TYPED_INSTR(OVMI_GE, OVM_TYPE_I32))
        C(OVM_TYPED_INSTR(OVMI_GE, OVM_TYPE_I64))
        C(OVM_TYPED_INSTR(OVMI_GE, OVM_TYPE_F32))
        C(OVM_TYPED_INSTR(OVMI_GE, OVM_TYPE_F64))

        C(OVM_TYPED_INSTR(OVMI_GE_S, OVM_TYPE_I8))
        C(OVM_TYPED_INSTR(OVMI_GE_S, OVM_TYPE_I16))
        C(OVM_TYPED_INSTR(OVMI_GE_S, OVM_TYPE_I32))
        C(OVM_TYPED_INSTR(OVMI_GE_S, OVM_TYPE_I64))
        C(OVM_TYPED_INSTR(OVMI_GE_S, OVM_TYPE_F32))
        C(OVM_TYPED_INSTR(OVMI_GE_S, OVM_TYPE_F64))

        C(OVM_TYPED_INSTR(OVMI_GT, OVM_TYPE_I8))
        C(OVM_TYPED_INSTR(OVMI_GT, OVM_TYPE_I16))
        C(OVM_TYPED_INSTR(OVMI_GT, OVM_TYPE_I32))
        C(OVM_TYPED_INSTR(OVMI_GT, OVM_TYPE_I64))
        C(OVM_TYPED_INSTR(OVMI_GT, OVM_TYPE_F32))
        C(OVM_TYPED_INSTR(OVMI_GT, OVM_TYPE_F64))

        C(OVM_TYPED_INSTR(OVMI_GT_S, OVM_TYPE_I8))
        C(OVM_TYPED_INSTR(OVMI_GT_S, OVM_TYPE_I16))
        C(OVM_TYPED_INSTR(OVMI_GT_S, OVM_TYPE_I32))
        C(OVM_TYPED_INSTR(OVMI_GT_S, OVM_TYPE_I64))
        C(OVM_TYPED_INSTR(OVMI_GT_S, OVM_TYPE_F32))
        C(OVM_TYPED_INSTR(OVMI_GT_S, OVM_TYPE_F64))

        C(OVM_TYPED_INSTR(OVMI_NE, OVM_TYPE_I8))
        C(OVM_TYPED_INSTR(OVMI_NE, OVM_TYPE_I16))
        C(OVM_TYPED_INSTR(OVMI_NE, OVM_TYPE_I32))
        C(OVM_TYPED_INSTR(OVMI_NE, OVM_TYPE_I64))
        C(OVM_TYPED_INSTR(OVMI_NE, OVM_TYPE_F32))
        C(OVM_TYPED_INSTR(OVMI_NE, OVM_TYPE_F64))

        C(OVMI_PARAM)
        C(OVMI_RETURN)
        C(OVMI_CALL)
        C(OVMI_CALLI)

        C(OVMI_BR)
        C(OVMI_BR_Z)
        C(OVMI_BR_NZ)
        C(OVMI_BRI)
        C(OVMI_BRI_Z)
        C(OVMI_BRI_NZ)

        C(OVM_TYPED_INSTR(OVMI_CLZ, OVM_TYPE_I8))
        C(OVM_TYPED_INSTR(OVMI_CLZ, OVM_TYPE_I16))
        C(OVM_TYPED_INSTR(OVMI_CLZ, OVM_TYPE_I32))
        C(OVM_TYPED_INSTR(OVMI_CLZ, OVM_TYPE_I64))
        C(OVM_TYPED_INSTR(OVMI_CTZ, OVM_TYPE_I8))
        C(OVM_TYPED_INSTR(OVMI_CTZ, OVM_TYPE_I16))
        C(OVM_TYPED_INSTR(OVMI_CTZ, OVM_TYPE_I32))
        C(OVM_TYPED_INSTR(OVMI_CTZ, OVM_TYPE_I64))
        C(OVM_TYPED_INSTR(OVMI_POPCNT, OVM_TYPE_I8))
        C(OVM_TYPED_INSTR(OVMI_POPCNT, OVM_TYPE_I16))
        C(OVM_TYPED_INSTR(OVMI_POPCNT, OVM_TYPE_I32))
        C(OVM_TYPED_INSTR(OVMI_POPCNT, OVM_TYPE_I64))
        C(OVM_TYPED_INSTR(OVMI_ROTL, OVM_TYPE_I8))
        C(OVM_TYPED_INSTR(OVMI_ROTL, OVM_TYPE_I16))
        C(OVM_TYPED_INSTR(OVMI_ROTL, OVM_TYPE_I32))
        C(OVM_TYPED_INSTR(OVMI_ROTL, OVM_TYPE_I64))
        C(OVM_TYPED_INSTR(OVMI_ROTR, OVM_TYPE_I8))
        C(OVM_TYPED_INSTR(OVMI_ROTR, OVM_TYPE_I16))
        C(OVM_TYPED_INSTR(OVMI_ROTR, OVM_TYPE_I32))
        C(OVM_TYPED_INSTR(OVMI_ROTR, OVM_TYPE_I64))

        C(OVM_TYPED_INSTR(OVMI_ABS, OVM_TYPE_F32))
        C(OVM_TYPED_INSTR(OVMI_NEG, OVM_TYPE_F32))
        C(OVM_TYPED_INSTR(OVMI_CEIL, OVM_TYPE_F32))
        C(OVM_TYPED_INSTR(OVMI_FLOOR, OVM_TYPE_F32))
        C(OVM_TYPED_INSTR(OVMI_TRUNC, OVM_TYPE_F32))
        C(OVM_TYPED_INSTR(OVMI_NEAREST, OVM_TYPE_F32))
        C(OVM_TYPED_INSTR(OVMI_SQRT, OVM_TYPE_F32))
        C(OVM_TYPED_INSTR(OVMI_MIN, OVM_TYPE_F32))
        C(OVM_TYPED_INSTR(OVMI_MAX, OVM_TYPE_F32))
        C(OVM_TYPED_INSTR(OVMI_COPYSIGN, OVM_TYPE_F32))

        C(OVM_TYPED_INSTR(OVMI_ABS, OVM_TYPE_F64))
        C(OVM_TYPED_INSTR(OVMI_NEG, OVM_TYPE_F64))
        C(OVM_TYPED_INSTR(OVMI_CEIL, OVM_TYPE_F64))
        C(OVM_TYPED_INSTR(OVMI_FLOOR, OVM_TYPE_F64))
        C(OVM_TYPED_INSTR(OVMI_TRUNC, OVM_TYPE_F64))
        C(OVM_TYPED_INSTR(OVMI_NEAREST, OVM_TYPE_F64))
        C(OVM_TYPED_INSTR(OVMI_SQRT, OVM_TYPE_F64))
        C(OVM_TYPED_INSTR(OVMI_MIN, OVM_TYPE_F64))
        C(OVM_TYPED_INSTR(OVMI_MAX, OVM_TYPE_F64))
        C(OVM_TYPED_INSTR(OVMI_COPYSIGN, OVM_TYPE_F64))

        C(OVM_TYPED_INSTR(OVMI_CVT_I8, OVM_TYPE_I16))
        C(OVM_TYPED_INSTR(OVMI_CVT_I8, OVM_TYPE_I32))
        C(OVM_TYPED_INSTR(OVMI_CVT_I8, OVM_TYPE_I64))

        C(OVM_TYPED_INSTR(OVMI_CVT_I16, OVM_TYPE_I8))
        C(OVM_TYPED_INSTR(OVMI_CVT_I16, OVM_TYPE_I32))
        C(OVM_TYPED_INSTR(OVMI_CVT_I16, OVM_TYPE_I64))

        C(OVM_TYPED_INSTR(OVMI_CVT_I32, OVM_TYPE_I8))
        C(OVM_TYPED_INSTR(OVMI_CVT_I32, OVM_TYPE_I16))
        C(OVM_TYPED_INSTR(OVMI_CVT_I32, OVM_TYPE_I64))
        C(OVM_TYPED_INSTR(OVMI_CVT_I32, OVM_TYPE_F32))
        C(OVM_TYPED_INSTR(OVMI_CVT_I32, OVM_TYPE_F64))

        C(OVM_TYPED_INSTR(OVMI_CVT_I64, OVM_TYPE_I8))
        C(OVM_TYPED_INSTR(OVMI_CVT_I64, OVM_TYPE_I16))
        C(OVM_TYPED_INSTR(OVMI_CVT_I64, OVM_TYPE_I32))
        C(OVM_TYPED_INSTR(OVMI_CVT_I64, OVM_TYPE_F32))
        C(OVM_TYPED_INSTR(OVMI_CVT_I64, OVM_TYPE_F64))

        C(OVM_TYPED_INSTR(OVMI_CVT_F32, OVM_TYPE_I32))
        C(OVM_TYPED_INSTR(OVMI_CVT_F32, OVM_TYPE_I64))
        C(OVM_TYPED_INSTR(OVMI_CVT_F32, OVM_TYPE_F64))

        C(OVM_TYPED_INSTR(OVMI_CVT_F64, OVM_TYPE_I32))
        C(OVM_TYPED_INSTR(OVMI_CVT_F64, OVM_TYPE_I64))
        C(OVM_TYPED_INSTR(OVMI_CVT_F64, OVM_TYPE_F32))

        C(OVM_TYPED_INSTR(OVMI_CMPXCHG, OVM_TYPE_I8))
        C(OVM_TYPED_INSTR(OVMI_CMPXCHG, OVM_TYPE_I16))
        C(OVM_TYPED_INSTR(OVMI_CMPXCHG, OVM_TYPE_I32))
        C(OVM_TYPED_INSTR(OVMI_CMPXCHG, OVM_TYPE_I64))

        default:
            snprintf(buf, 64, "unknown (%d)", full_instr);
            return buf;
    }
}
#endif

void ovm_program_print_instructions(ovm_program_t *program, i32 start_instr, i32 instr_count) {
    fori (i, start_instr, start_instr + instr_count) {
        //
        // Horribly inefficient way of checking to see if this instruction
        // is the start of a function, but for now, it'll do. -brendanfh 06/13/2022
        bh_arr_each(ovm_func_t, func, program->funcs) {
            if (i == func->start_instr && func->kind == OVM_FUNC_INTERNAL) {
                printf("\n[%d] %s  values=%d:\n", func->id, func->name, func->value_number_count);
            }
        }

        ovm_instr_t instr = program->code[i];
        printf("%6lx  %50s | r=%02d a=%02d b=%02d  i=%d f=%f l=%ld d=%lf\n", i, ovm_instr_name(instr.full_instr), instr.r, instr.a, instr.b, instr.i, instr.f, instr.l, instr.d);
    }
}

void ovm_raw_print_instructions(i32 instr_count, ovm_instr_t *instrs) {
    fori (i, 0, instr_count) {
        ovm_instr_t instr = instrs[i];
        printf("%6lx  %50s | r=%02d a=%02d b=%02d  i=%d f=%f l=%ld d=%lf\n", i, ovm_instr_name(instr.full_instr), instr.r, instr.a, instr.b, instr.i, instr.f, instr.l, instr.d);
    }
}

//
// Engine
ovm_engine_t *ovm_engine_new(ovm_store_t *store) {
    ovm_engine_t *engine = bh_alloc_item(store->heap_allocator, ovm_engine_t);

    engine->store = store;
    engine->memory_size = 1ull << 32;
    engine->memory = mmap(NULL, engine->memory_size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    pthread_mutex_init(&engine->atomic_mutex, NULL);

    engine->debug = NULL;

    return engine;
}

void ovm_engine_delete(ovm_engine_t *engine) {
    ovm_store_t *store = engine->store;

    munmap(engine->memory, engine->memory_size);
    bh_free(store->heap_allocator, engine);
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
    state->params = NULL;
    state->stack_frames = NULL;
    state->registers = NULL;
    bh_arr_new(store->heap_allocator, state->numbered_values, 128);
    bh_arr_new(store->heap_allocator, state->params, 16);
    bh_arr_new(store->heap_allocator, state->stack_frames, 32);
    bh_arr_new(store->heap_allocator, state->registers, program->register_count);
    bh_arr_insert_end(state->registers, program->register_count);

    state->external_funcs = NULL;
    bh_arr_new(store->heap_allocator, state->external_funcs, 8);

    if (engine->debug) {
        u32 thread_id = debug_host_register_thread(engine->debug, state);
        state->debug = debug_host_lookup_thread(engine->debug, thread_id);
    }

#ifdef OVM_VERBOSE
    ovm_program_print_instructions(program, 0, bh_arr_length(program->code));
#endif

    return state;
}

void ovm_state_delete(ovm_state_t *state) {
    ovm_store_t *store = state->store;

    bh_arr_free(state->numbered_values);
    bh_arr_free(state->params);
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

static inline void ovm__func_setup_stack_frame(ovm_state_t *state, ovm_func_t *func, i32 result_number) {
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

    //
    // Modify debug state so step over works
    if (state->debug) {
        state->debug->extra_frames_since_last_pause++;
    }
}

static inline ovm_stack_frame_t ovm__func_teardown_stack_frame(ovm_state_t *state) {
    ovm_stack_frame_t frame = bh_arr_pop(state->stack_frames);
    bh_arr_fastdeleten(state->numbered_values, frame.value_number_count);

    if (bh_arr_length(state->stack_frames) == 0) {
        state->value_number_offset = 0;
    } else {
        state->value_number_offset = bh_arr_last(state->stack_frames).value_number_base;
    }

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
    ovm_assert(func.value_number_count >= func.param_count);

    switch (func->kind) {
        case OVM_FUNC_INTERNAL: {
            ovm__func_setup_stack_frame(state, func, 0);

            fori (i, 0, param_count) {
                state->numbered_values[i + state->value_number_offset] = params[i];
            }

            state->pc = func->start_instr;
            ovm_value_t result = ovm_run_code(engine, state, program);

            return result;
        }

        case OVM_FUNC_EXTERNAL: {
            ovm__func_setup_stack_frame(state, func, 0);

            ovm_value_t result = {0};
            ovm_external_func_t external_func = state->external_funcs[func->external_func_idx];
            external_func.native_func(external_func.userdata, params, &result);

            ovm__func_teardown_stack_frame(state);
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

static inline double __ovm_copysign(a, b) double a, b; {
    if ((a > 0 && b > 0) || (a < 0 && b < 0)) return a;
    return -a;
}

static inline void __ovm_trigger_exception(ovm_state_t *state) {
    if (state->debug) {
        state->debug->state = debug_state_pausing;
        state->debug->pause_reason = debug_pause_exception;

        assert(write(state->debug->state_change_write_fd, "1", 1));
        sem_wait(&state->debug->wait_semaphore);
    }
}

static inline void __ovm_debug_hook(ovm_engine_t *engine, ovm_state_t *state) {
    if (!state->debug) return;

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
    sem_wait(&state->debug->wait_semaphore);
    state->debug->state = debug_state_running;

    shouldnt_wait:
    if (state->debug->run_count > 0) state->debug->run_count--;
}



//
// Running code with Continuation-Passing Style
//



#define OVMI_INSTR_PROTO(name) \
    ovm_value_t name(ovm_instr_t *instr, ovm_state_t *state, u8 *memory, ovm_instr_t *code)

#define OVMI_INSTR_EXEC(name) \
    static OVMI_INSTR_PROTO(name)

#define NEXT_OP \
    if (instr->full_instr & OVMI_ATOMIC) pthread_mutex_unlock(&state->engine->atomic_mutex); \
    if (state->debug) __ovm_debug_hook(state->engine, state); \
    instr = &code[state->pc++]; \
    if (instr->full_instr & OVMI_ATOMIC) pthread_mutex_lock(&state->engine->atomic_mutex); \
    return ovmi_dispatch[instr->full_instr & OVM_INSTR_MASK](instr, state, memory, code);

#define VAL(loc) state->numbered_values[(u32) (loc + state->value_number_offset)]


typedef OVMI_INSTR_PROTO((* ovmi_instr_exec_t));

static ovmi_instr_exec_t ovmi_dispatch[];


//
// Special Operations
//

OVMI_INSTR_EXEC(ovmi_exec_nop) {
    NEXT_OP;
}


//
// Binary Operations
//

#define OVM_OP_EXEC(name, op) \
    OVMI_INSTR_EXEC(ovmi_exec_##name##_i32) { OVM_OP(OVM_TYPE_I32, op, i32); NEXT_OP; } \
    OVMI_INSTR_EXEC(ovmi_exec_##name##_i64) { OVM_OP(OVM_TYPE_I64, op, i64); NEXT_OP; } \
    OVMI_INSTR_EXEC(ovmi_exec_##name##_f32) { OVM_OP(OVM_TYPE_F32, op, f32); NEXT_OP; } \
    OVMI_INSTR_EXEC(ovmi_exec_##name##_f64) { OVM_OP(OVM_TYPE_F64, op, f64); NEXT_OP; }

#define OVM_OP_UNSIGNED_EXEC(name, op) \
    OVMI_INSTR_EXEC(ovmi_exec_##name##_i32) { OVM_OP(OVM_TYPE_I32, op, u32); NEXT_OP; } \
    OVMI_INSTR_EXEC(ovmi_exec_##name##_i64) { OVM_OP(OVM_TYPE_I64, op, u64); NEXT_OP; } \
    OVMI_INSTR_EXEC(ovmi_exec_##name##_f32) { OVM_OP(OVM_TYPE_F32, op, f32); NEXT_OP; } \
    OVMI_INSTR_EXEC(ovmi_exec_##name##_f64) { OVM_OP(OVM_TYPE_F64, op, f64); NEXT_OP; }

#define OVM_OP_INTEGER_EXEC(name, op) \
    OVMI_INSTR_EXEC(ovmi_exec_##name##_i32) { OVM_OP(OVM_TYPE_I32, op, i32); NEXT_OP; } \
    OVMI_INSTR_EXEC(ovmi_exec_##name##_i64) { OVM_OP(OVM_TYPE_I64, op, i64); NEXT_OP; }

#define OVM_OP_INTEGER_UNSIGNED_EXEC(name, op) \
    OVMI_INSTR_EXEC(ovmi_exec_##name##_i32) { OVM_OP(OVM_TYPE_I32, op, u32); NEXT_OP; } \
    OVMI_INSTR_EXEC(ovmi_exec_##name##_i64) { OVM_OP(OVM_TYPE_I64, op, u64); NEXT_OP; }

#define OVM_OP_FLOAT_EXEC(name, op) \
    OVMI_INSTR_EXEC(ovmi_exec_##name##_f32) { OVM_OP(OVM_TYPE_F32, op, f32); NEXT_OP; } \
    OVMI_INSTR_EXEC(ovmi_exec_##name##_f64) { OVM_OP(OVM_TYPE_F64, op, f64); NEXT_OP; }


#define OVM_OP(t, op, ctype) \
    ovm_assert(VAL(instr->a).type == t && VAL(instr->b).type == t); \
    VAL(instr->r).ctype = VAL(instr->a).ctype op VAL(instr->b).ctype; \
    VAL(instr->r).type = t;

OVM_OP_EXEC(add, +)
OVM_OP_EXEC(sub, -)
OVM_OP_EXEC(mul, *)
OVM_OP_EXEC(div_s, /)
OVM_OP_UNSIGNED_EXEC(div, /)
OVM_OP_INTEGER_UNSIGNED_EXEC(rem, %)
OVM_OP_INTEGER_EXEC(rem_s, %)
OVM_OP_INTEGER_UNSIGNED_EXEC(and, &)
OVM_OP_INTEGER_UNSIGNED_EXEC(or, |)
OVM_OP_INTEGER_UNSIGNED_EXEC(xor, ^)
OVM_OP_INTEGER_UNSIGNED_EXEC(shl, <<)
OVM_OP_INTEGER_UNSIGNED_EXEC(shr, >>)
OVM_OP_INTEGER_EXEC(sar, >>)

#undef OVM_OP

#define OVM_OP(t, func, ctype) \
    ovm_assert(VAL(instr->a).type == t && VAL(instr->b).type == t); \
    VAL(instr->r).ctype = func( VAL(instr->a).ctype, VAL(instr->b).ctype ); \
    VAL(instr->r).type = t;

OVMI_INSTR_EXEC(ovmi_exec_rotl_i32) { OVM_OP(OVM_TYPE_I32, __rold, u32); NEXT_OP; }
OVMI_INSTR_EXEC(ovmi_exec_rotl_i64) { OVM_OP(OVM_TYPE_I64, __rolq, u64); NEXT_OP; }
OVMI_INSTR_EXEC(ovmi_exec_rotr_i32) { OVM_OP(OVM_TYPE_I32, __rord, u32); NEXT_OP; }
OVMI_INSTR_EXEC(ovmi_exec_rotr_i64) { OVM_OP(OVM_TYPE_I64, __rorq, u64); NEXT_OP; }

OVM_OP_FLOAT_EXEC(min, bh_min)
OVM_OP_FLOAT_EXEC(max, bh_max)
OVM_OP_FLOAT_EXEC(copysign, __ovm_copysign)

#undef OVM_OP


#define OVM_OP(t, op, ctype) \
    ovm_assert(VAL(instr->a).type == t); \
    VAL(instr->r).type = t; \
    VAL(instr->r).ctype = (ctype) op (VAL(instr->a).ctype);

OVMI_INSTR_EXEC(ovmi_exec_clz_i32) { OVM_OP(OVM_TYPE_I32, __builtin_clz, u32);   NEXT_OP; }
OVMI_INSTR_EXEC(ovmi_exec_clz_i64) { OVM_OP(OVM_TYPE_I64, __builtin_clzll, u64); NEXT_OP; }
OVMI_INSTR_EXEC(ovmi_exec_ctz_i32) { OVM_OP(OVM_TYPE_I32, __builtin_ctz, u32);   NEXT_OP; }
OVMI_INSTR_EXEC(ovmi_exec_ctz_i64) { OVM_OP(OVM_TYPE_I64, __builtin_ctzll, u64); NEXT_OP; }
OVMI_INSTR_EXEC(ovmi_exec_popcount_i32) { OVM_OP(OVM_TYPE_I32, __builtin_popcount, u32);   NEXT_OP; }
OVMI_INSTR_EXEC(ovmi_exec_popcount_i64) { OVM_OP(OVM_TYPE_I64, __builtin_popcountll, u64); NEXT_OP; }

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


OVMI_INSTR_EXEC(ovmi_exec_imm_i32) { OVM_IMM(OVM_TYPE_I32, u32, i); NEXT_OP; }
OVMI_INSTR_EXEC(ovmi_exec_imm_i64) { OVM_IMM(OVM_TYPE_I64, u64, l); NEXT_OP; }
OVMI_INSTR_EXEC(ovmi_exec_imm_f32) { OVM_IMM(OVM_TYPE_F32, f32, f); NEXT_OP; }
OVMI_INSTR_EXEC(ovmi_exec_imm_f64) { OVM_IMM(OVM_TYPE_F64, f64, d); NEXT_OP; }

#undef OVM_IMM

OVMI_INSTR_EXEC(ovmi_exec_mov) {
    VAL(instr->r) = VAL(instr->a);
    NEXT_OP;
}

#define OVM_LOAD(otype, type_, stype) \
    OVMI_INSTR_EXEC(ovmi_exec_load_##otype) { \
        ovm_assert(VAL(instr->a).type == OVM_TYPE_I32); \
        u32 dest = VAL(instr->a).u32 + (u32) instr->b; \
        if (dest == 0) __ovm_trigger_exception(state); \
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
    OVMI_INSTR_EXEC(ovmi_exec_store_##otype) { \
        ovm_assert(VAL(instr->r).type == OVM_TYPE_I32); \
        u32 dest = VAL(instr->r).u32 + (u32) instr->b; \
        if (dest == 0) __ovm_trigger_exception(state); \
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

OVMI_INSTR_EXEC(ovmi_exec_copy) {
    u32 dest  = VAL(instr->r).u32;
    u32 src   = VAL(instr->a).u32;
    u32 count = VAL(instr->b).u32;

    if (!dest || !src) __ovm_trigger_exception(state);

    memmove(&memory[dest], &memory[src], count);

    NEXT_OP;
}

OVMI_INSTR_EXEC(ovmi_exec_fill) {
    i32 dest  = VAL(instr->r).i32;
    u8  byte  = VAL(instr->a).u8;
    i32 count = VAL(instr->b).i32;

    if (!dest) __ovm_trigger_exception(state);

    memset(&memory[dest], byte, count);

    NEXT_OP;
}

OVMI_INSTR_EXEC(ovmi_exec_reg_get) {
    VAL(instr->r) = state->registers[instr->a];

    NEXT_OP;
}

OVMI_INSTR_EXEC(ovmi_exec_reg_set) {
    state->registers[instr->r] = VAL(instr->a);

    NEXT_OP;
}

OVMI_INSTR_EXEC(ovmi_exec_idx_arr) {
    ovm_static_integer_array_t data_elem = state->program->static_data[instr->a];
    ovm_assert(VAL(instr->b).u32 < (u32) data_elem.len);

    VAL(instr->r).i32 = state->program->static_integers[data_elem.start_idx + VAL(instr->b).u32];
    VAL(instr->r).type = OVM_TYPE_I32;

    NEXT_OP;
}


//
// Function calling
//

OVMI_INSTR_EXEC(ovmi_exec_param) {
    bh_arr_push(state->params, VAL(instr->a));

    NEXT_OP;
}

OVMI_INSTR_EXEC(ovmi_exec_return) {
    ovm_value_t val = VAL(instr->a);
    ovm_stack_frame_t frame = ovm__func_teardown_stack_frame(state);
    state->pc = frame.return_address;

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
    i32 extra_params = bh_arr_length(state->params) - func->param_count; \
    ovm_assert(extra_params >= 0); \
    ovm__func_setup_stack_frame(state, func, instr->r); \
    if (func->kind == OVM_FUNC_INTERNAL) { \
        fori (i, 0, func->param_count) { \
            VAL(i) = state->params[i + extra_params]; \
        } \
        bh_arr_fastdeleten(state->params, func->param_count); \
\
        state->pc = func->start_instr; \
    } else { \
        ovm_value_t result = {0}; \
        ovm_external_func_t external_func = state->external_funcs[func->external_func_idx]; \
        external_func.native_func(external_func.userdata, &state->params[extra_params], &result); \
        bh_arr_fastdeleten(state->params, func->param_count); \
\
        ovm__func_teardown_stack_frame(state); \
\
        if (instr->r >= 0) { \
            VAL(instr->r) = result; \
        } \
    }


OVMI_INSTR_EXEC(ovmi_exec_call) {
    OVM_CALL_CODE(instr->a);
    NEXT_OP;
}

OVMI_INSTR_EXEC(ovmi_exec_calli) {
    OVM_CALL_CODE(VAL(instr->a).i32);
    NEXT_OP;
}

#undef OVM_CALL_CODE



//
// Branching Instructions
//

OVMI_INSTR_EXEC(ovmi_exec_br)     { state->pc += instr->a; NEXT_OP; }
OVMI_INSTR_EXEC(ovmi_exec_bri)    { state->pc += VAL(instr->a).i32; NEXT_OP; }
OVMI_INSTR_EXEC(ovmi_exec_br_nz)  { if (VAL(instr->b).i32 != 0) state->pc += instr->a; NEXT_OP; }
OVMI_INSTR_EXEC(ovmi_exec_bri_nz) { if (VAL(instr->b).i32 != 0) state->pc += VAL(instr->a).i32; NEXT_OP; }
OVMI_INSTR_EXEC(ovmi_exec_br_z)   { if (VAL(instr->b).i32 == 0) state->pc += instr->a; NEXT_OP; }
OVMI_INSTR_EXEC(ovmi_exec_bri_z)  { if (VAL(instr->b).i32 == 0) state->pc += VAL(instr->a).i32; NEXT_OP; }


//
// Conversion
//

#define OVM_CVT(n1, n2, stype, dtype, otype, ctype) \
    OVMI_INSTR_EXEC(ovmi_exec_cvt_##n1##_##n2) { \
        ovm_value_t tmp_val; \
        tmp_val.dtype = (ctype) VAL(instr->a).stype; \
        tmp_val.type = otype; \
        VAL(instr->r) = tmp_val; \
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
    OVMI_INSTR_EXEC(ovmi_exec_transmute_##n1##_##n2) { \
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

#undef CVT


//
// Compare exchange
//

#define CMPXCHG(otype, ctype) \
    OVMI_INSTR_EXEC(ovmi_exec_cmpxchg_##ctype) { \
        if (VAL(instr->r).u32 == 0) __ovm_trigger_exception(state); \
        ctype *addr = (ctype *) &memory[VAL(instr->r).u32]; \
 \
        VAL(instr->r).u64 = 0; \
        VAL(instr->r).type = otype; \
        VAL(instr->r).ctype = *addr; \
 \
        if (*addr == VAL(instr->a).ctype) { \
            *addr = VAL(instr->b).ctype ; \
        } \
        NEXT_OP; \
    }

CMPXCHG(OVM_TYPE_I32, i32)
CMPXCHG(OVM_TYPE_I64, i64)

#undef CMPXCHG


OVMI_INSTR_EXEC(ovmi_exec_illegal) {
    __ovm_trigger_exception(state);
    return ((ovm_value_t) {0});
}

//
// Dispatch table
//

#define IROW_UNTYPED(name) ovmi_exec_##name, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
#define IROW_TYPED(name)   NULL, ovmi_exec_##name##_i8, ovmi_exec_##name##_i16, ovmi_exec_##name##_i32, ovmi_exec_##name##_i64, ovmi_exec_##name##_f32, ovmi_exec_##name##_f64, NULL,
#define IROW_PARTIAL(name) NULL, NULL, NULL, ovmi_exec_##name##_i32, ovmi_exec_##name##_i64, ovmi_exec_##name##_f32, ovmi_exec_##name##_f64, NULL,
#define IROW_INT(name)     NULL, NULL, NULL, ovmi_exec_##name##_i32, ovmi_exec_##name##_i64, NULL, NULL, NULL,
#define IROW_FLOAT(name)   NULL, NULL, NULL, NULL, NULL, ovmi_exec_##name##_f32, ovmi_exec_##name##_f64, NULL,
#define IROW_SAME(name)    ovmi_exec_##name,ovmi_exec_##name,ovmi_exec_##name,ovmi_exec_##name,ovmi_exec_##name,ovmi_exec_##name,ovmi_exec_##name,NULL,

static ovmi_instr_exec_t ovmi_dispatch[] = {
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
    NULL, NULL, ovmi_exec_cvt_i8_i16, ovmi_exec_cvt_i8_i32, ovmi_exec_cvt_i8_i64, NULL, NULL, NULL,
    NULL, NULL, ovmi_exec_cvt_i8_s_i16, ovmi_exec_cvt_i8_s_i32, ovmi_exec_cvt_i8_s_i64, NULL, NULL, NULL,
    NULL, ovmi_exec_cvt_i16_i8,   NULL, ovmi_exec_cvt_i16_i32, ovmi_exec_cvt_i16_i64, NULL, NULL, NULL,
    NULL, ovmi_exec_cvt_i16_s_i8, NULL, ovmi_exec_cvt_i16_s_i32, ovmi_exec_cvt_i16_s_i64, NULL, NULL, NULL,
    NULL, ovmi_exec_cvt_i32_i8,   ovmi_exec_cvt_i32_i16,   NULL, ovmi_exec_cvt_i32_i64, ovmi_exec_cvt_i32_f32, ovmi_exec_cvt_i32_f64, NULL,  // 0x40
    NULL, ovmi_exec_cvt_i32_s_i8, ovmi_exec_cvt_i32_s_i16, NULL, ovmi_exec_cvt_i32_s_i64, ovmi_exec_cvt_i32_s_f32, ovmi_exec_cvt_i32_s_f64, NULL,
    NULL, ovmi_exec_cvt_i64_i8,   ovmi_exec_cvt_i64_i16,   ovmi_exec_cvt_i64_i32,   NULL, ovmi_exec_cvt_i64_f32, ovmi_exec_cvt_i64_f64, NULL,
    NULL, ovmi_exec_cvt_i64_s_i8, ovmi_exec_cvt_i64_s_i16, ovmi_exec_cvt_i64_s_i32, NULL, ovmi_exec_cvt_i64_s_f32, ovmi_exec_cvt_i64_s_f64, NULL,
    NULL, NULL, NULL, ovmi_exec_cvt_f32_i32, ovmi_exec_cvt_f32_i64, NULL, ovmi_exec_cvt_f32_f64, NULL,
    NULL, NULL, NULL, ovmi_exec_cvt_f32_s_i32, ovmi_exec_cvt_f32_s_i64, NULL, ovmi_exec_cvt_f32_s_f64, NULL,
    NULL, NULL, NULL, ovmi_exec_cvt_f64_i32, ovmi_exec_cvt_f64_i64, ovmi_exec_cvt_f64_f32, NULL, NULL,
    NULL, NULL, NULL, ovmi_exec_cvt_f64_s_i32, ovmi_exec_cvt_f64_s_i64, ovmi_exec_cvt_f64_s_f32, NULL, NULL,
    NULL, NULL, NULL, ovmi_exec_transmute_i32_f32, NULL, NULL, NULL, NULL,
    NULL, NULL, NULL, NULL, ovmi_exec_transmute_i64_f64, NULL, NULL, NULL,
    NULL, NULL, NULL, NULL, NULL, ovmi_exec_transmute_f32_i32, NULL, NULL,
    NULL, NULL, NULL, NULL, NULL, NULL, ovmi_exec_transmute_f64_i64, NULL,
    IROW_INT(cmpxchg)
    IROW_SAME(illegal)
};


ovm_value_t ovm_run_code(ovm_engine_t *engine, ovm_state_t *state, ovm_program_t *program) {
    ovm_assert(engine);
    ovm_assert(state);
    ovm_assert(program);

    ovm_instr_t *code = program->code;
    u8 *memory = engine->memory;

    ovm_instr_t *instr = &code[state->pc];
    NEXT_OP;
}


void ovm_print_stack_trace(ovm_engine_t *engine, ovm_state_t *state, ovm_program_t *program) {
    int i = 0;
    bh_arr_rev_each(ovm_stack_frame_t, frame, state->stack_frames) {
        ovm_func_t *func = frame->func;
        printf("[%03d] %s\n", i++, func->name);
    }
}

