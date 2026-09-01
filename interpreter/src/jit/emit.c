#include "emit.h"
#include <stdlib.h>
#include <string.h>

void ovm_emit_init(ovm_emit_buf_t *buf) {
    memset(buf, 0, sizeof(*buf));
    buf->jumptable_off = -1;
    bh_arr_new(bh_heap_allocator(), buf->relocs, 32);
}

void ovm_emit_reloc(ovm_emit_buf_t *buf, i32 at, i32 target_ovmi, i32 kind, i32 cond) {
    ovm_reloc_t r;
    r.at = at;
    r.target_ovmi = target_ovmi;
    r.kind = kind;
    r.cond = cond;
    bh_arr_push(buf->relocs, r);
}

void ovm_emit_reset_func(ovm_emit_buf_t *buf, i32 start, i32 end) {
    bh_arr_clear(buf->relocs);
    free(buf->ovmi_off);
    buf->ninstr = end - start;
    buf->ovmi_off = calloc((size_t) buf->ninstr, sizeof(i32));
    buf->func_start_ovmi = start;
    buf->has_indirect_br = 0;
    buf->jumptable_off = -1;
}

void ovm_emit_free(ovm_emit_buf_t *buf) {
    free(buf->data);
    free(buf->ovmi_off);
    bh_arr_free(buf->relocs);
    memset(buf, 0, sizeof(*buf));
}

void ovm_emit_ensure(ovm_emit_buf_t *buf, i32 extra) {
    i32 need = buf->len + extra;
    if (need <= buf->cap) return;
    i32 cap = buf->cap ? buf->cap : 4096;
    while (cap < need) cap *= 2;
    buf->data = realloc(buf->data, (size_t) cap);
    buf->cap = cap;
}

void ovm_emit_u8(ovm_emit_buf_t *buf, u8 b) {
    ovm_emit_ensure(buf, 1);
    buf->data[buf->len++] = b;
}

void ovm_emit_u32(ovm_emit_buf_t *buf, u32 w) {
    ovm_emit_ensure(buf, 4);
    memcpy(buf->data + buf->len, &w, 4);
    buf->len += 4;
}

void ovm_emit_u64(ovm_emit_buf_t *buf, u64 w) {
    ovm_emit_ensure(buf, 8);
    memcpy(buf->data + buf->len, &w, 8);
    buf->len += 8;
}

void ovm_emit_bytes(ovm_emit_buf_t *buf, const void *p, i32 n) {
    ovm_emit_ensure(buf, n);
    memcpy(buf->data + buf->len, p, (size_t) n);
    buf->len += n;
}

void ovm_emit_align(ovm_emit_buf_t *buf, i32 align) {
    while (buf->len & (align - 1)) ovm_emit_u8(buf, 0);
}

i32 ovm_func_code_end(ovm_program_t *program, i32 func_idx) {
    i32 start = program->funcs[func_idx].start_instr;
    i32 end = bh_arr_length(program->code);
    for (i32 i = 0; i < bh_arr_length(program->funcs); i++) {
        if (program->funcs[i].kind != OVM_FUNC_INTERNAL) continue;
        i32 s = program->funcs[i].start_instr;
        if (s > start && s < end) end = s;
    }
    return end;
}

static b32 instr_is_indirect_br(u32 op) {
    return op == OVMI_BRI || op == OVMI_BRI_Z || op == OVMI_BRI_NZ;
}

static b32 instr_is_emit_worthy(u32 op) {
    switch (op) {
        case OVMI_NOP:
        case OVMI_ADD: case OVMI_SUB: case OVMI_MUL:
        case OVMI_AND: case OVMI_OR: case OVMI_XOR:
        case OVMI_SHL: case OVMI_SHR: case OVMI_SAR:
        case OVMI_IMM: case OVMI_MOV:
        case OVMI_LOAD: case OVMI_STORE:
        case OVMI_REG_GET: case OVMI_REG_SET:
        case OVMI_LT: case OVMI_LT_S: case OVMI_LE: case OVMI_LE_S:
        case OVMI_EQ: case OVMI_GE: case OVMI_GE_S:
        case OVMI_GT: case OVMI_GT_S: case OVMI_NE:
        case OVMI_BR: case OVMI_BR_Z: case OVMI_BR_NZ:
        case OVMI_CLZ: case OVMI_CTZ: case OVMI_POPCNT:
        case OVMI_ROTL: case OVMI_ROTR:
        case OVMI_CVT_I8: case OVMI_CVT_I8_S:
        case OVMI_CVT_I16: case OVMI_CVT_I16_S:
        case OVMI_CVT_I32: case OVMI_CVT_I32_S:
        case OVMI_CVT_I64: case OVMI_CVT_I64_S:
        case OVMI_TRANSMUTE_I32: case OVMI_TRANSMUTE_I64:
        case OVMI_TRANSMUTE_F32: case OVMI_TRANSMUTE_F64:
        case OVMI_ADD_IMM: case OVMI_SUB_IMM: case OVMI_MUL_IMM:
        case OVMI_AND_IMM: case OVMI_OR_IMM: case OVMI_XOR_IMM:
        case OVMI_SHL_IMM: case OVMI_SHR_IMM: case OVMI_SAR_IMM:
        case OVMI_BR_EQ: case OVMI_BR_NE:
        case OVMI_BR_LT: case OVMI_BR_LT_S:
        case OVMI_BR_LE: case OVMI_BR_LE_S:
        case OVMI_BR_GT: case OVMI_BR_GT_S:
        case OVMI_BR_GE: case OVMI_BR_GE_S:
            return 1;
        default:
            return 0;
    }
}

b32 ovm_jit_should_compile(ovm_program_t *program, i32 start, i32 end) {
    b32 any_native = 0;
    for (i32 i = start; i < end; i++) {
        u32 op = OVM_INSTR_INSTR(program->code[i]);
        u32 ty = OVM_INSTR_TYPE(program->code[i]);
        if (instr_is_indirect_br(op)) return 0;
#if defined(__x86_64__)
        if (op >= OVMI_BR_EQ && op <= OVMI_BR_GE_S &&
            (ty == OVM_TYPE_F32 || ty == OVM_TYPE_F64)) {
            return 0;
        }
#else
        (void) ty;
#endif
        if (instr_is_emit_worthy(op)) any_native = 1;
    }
    return any_native;
}
