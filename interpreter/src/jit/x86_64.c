#include "emit.h"

#include <stddef.h>
#include <stdint.h>
#include <string.h>

#if defined(__x86_64__)
// native x86_64 backend


enum {
    RAX = 0, RCX = 1, RDX = 2, RBX = 3,
    RSP = 4, RBP = 5, RSI = 6, RDI = 7,
    R8 = 8, R9 = 9, R10 = 10, R11 = 11,
    R12 = 12, R13 = 13, R14 = 14, R15 = 15
};

enum {
    CC_B = 0x2, CC_AE = 0x3, CC_E = 0x4, CC_NE = 0x5,
    CC_BE = 0x6, CC_A = 0x7, CC_P = 0xA, CC_NP = 0xB,
    CC_L = 0xC, CC_GE = 0xD, CC_LE = 0xE, CC_G = 0xF
};

#define OFF_FRAME   ((i32) offsetof(ovm_state_t, __frame_values))
#define OFF_MEMORY  ((i32) offsetof(ovm_engine_t, memory))
#define OFF_PBUF    ((i32) offsetof(ovm_state_t, param_buf))
#define OFF_PCNT    ((i32) offsetof(ovm_state_t, param_count))
#define OFF_REGS    ((i32) offsetof(ovm_state_t, registers))

static void x64_u8(ovm_emit_buf_t *b, u8 v) { ovm_emit_u8(b, v); }
static void x64_u32(ovm_emit_buf_t *b, u32 v) { ovm_emit_u32(b, v); }
static void x64_u64(ovm_emit_buf_t *b, u64 v) { ovm_emit_u64(b, v); }

static int rex_bit(int r) { return (r >= 8) ? 1 : 0; }
static int reg7(int r) { return r & 7; }

static void x64_rex(ovm_emit_buf_t *b, int w, int r, int x, int rb) {
    u8 rex = 0x40 | (w ? 8 : 0) | (r ? 4 : 0) | (x ? 2 : 0) | (rb ? 1 : 0);
    if (rex != 0x40 || w) x64_u8(b, rex);
}

static void x64_modrm(ovm_emit_buf_t *b, int mod, int reg, int rm) {
    x64_u8(b, (u8) ((mod << 6) | ((reg & 7) << 3) | (rm & 7)));
}

static void x64_sib(ovm_emit_buf_t *b, int scale, int index, int base) {
    x64_u8(b, (u8) ((scale << 6) | ((index & 7) << 3) | (base & 7)));
}

static void x64_push(ovm_emit_buf_t *b, int r) {
    if (r >= 8) x64_u8(b, 0x41);
    x64_u8(b, (u8) (0x50 + reg7(r)));
}

static void x64_pop(ovm_emit_buf_t *b, int r) {
    if (r >= 8) x64_u8(b, 0x41);
    x64_u8(b, (u8) (0x58 + reg7(r)));
}

static void x64_mov_rr(ovm_emit_buf_t *b, int dst, int src) {
    x64_rex(b, 1, rex_bit(src), 0, rex_bit(dst));
    x64_u8(b, 0x89);
    x64_modrm(b, 3, src, dst);
}

static void x64_mov_imm64(ovm_emit_buf_t *b, int dst, u64 imm) {
    x64_rex(b, 1, 0, 0, rex_bit(dst));
    x64_u8(b, (u8) (0xB8 + reg7(dst)));
    x64_u64(b, imm);
}

static void x64_mov_imm32(ovm_emit_buf_t *b, int dst, u32 imm) {
    if (dst >= 8) x64_u8(b, 0x41);
    x64_u8(b, (u8) (0xB8 + reg7(dst)));
    x64_u32(b, imm);
}

static void x64_load_disp(ovm_emit_buf_t *b, int dst, int base, i32 disp, int wide) {
    x64_rex(b, wide, rex_bit(dst), 0, rex_bit(base));
    x64_u8(b, 0x8B);
    int need_sib = (reg7(base) == 4);
    int mod = (disp == 0 && reg7(base) != 5) ? 0 : (disp >= -128 && disp <= 127) ? 1 : 2;
    if (reg7(base) == 5 && disp == 0) mod = 1;
    x64_modrm(b, mod == 2 ? 2 : (mod == 1 ? 1 : 0), dst, need_sib ? 4 : base);
    if (need_sib) x64_sib(b, 0, 4, base);
    if (mod == 1) x64_u8(b, (u8) disp);
    else if (mod == 2) x64_u32(b, (u32) disp);
}

static void x64_store_disp(ovm_emit_buf_t *b, int base, i32 disp, int src, int wide) {
    x64_rex(b, wide, rex_bit(src), 0, rex_bit(base));
    x64_u8(b, 0x89);
    int need_sib = (reg7(base) == 4);
    int mod = (disp == 0 && reg7(base) != 5) ? 0 : (disp >= -128 && disp <= 127) ? 1 : 2;
    if (reg7(base) == 5 && disp == 0) mod = 1;
    x64_modrm(b, mod == 2 ? 2 : (mod == 1 ? 1 : 0), src, need_sib ? 4 : base);
    if (need_sib) x64_sib(b, 0, 4, base);
    if (mod == 1) x64_u8(b, (u8) disp);
    else if (mod == 2) x64_u32(b, (u32) disp);
}

static void x64_slot_ld(ovm_emit_buf_t *b, int dst, i32 vn, int wide) {
    x64_load_disp(b, dst, RBX, vn * 8, wide);
}

static void x64_slot_st(ovm_emit_buf_t *b, i32 vn, int src, int wide) {
    x64_store_disp(b, RBX, vn * 8, src, wide);
}

static void x64_alu_rr(ovm_emit_buf_t *b, u8 opc, int dst, int src, int wide) {
    x64_rex(b, wide, rex_bit(src), 0, rex_bit(dst));
    x64_u8(b, opc);
    x64_modrm(b, 3, src, dst);
}

static void x64_alu_imm32(ovm_emit_buf_t *b, int dst, i32 imm, int ext, int wide) {
    x64_rex(b, wide, 0, 0, rex_bit(dst));
    x64_u8(b, 0x81);
    x64_modrm(b, 3, ext, dst);
    x64_u32(b, (u32) imm);
}

static void x64_imul(ovm_emit_buf_t *b, int dst, int src, int wide) {
    x64_rex(b, wide, rex_bit(dst), 0, rex_bit(src));
    x64_u8(b, 0x0F);
    x64_u8(b, 0xAF);
    x64_modrm(b, 3, dst, src);
}

static void x64_shift_cl(ovm_emit_buf_t *b, int dst, int ext, int wide) {
    x64_rex(b, wide, 0, 0, rex_bit(dst));
    x64_u8(b, 0xD3);
    x64_modrm(b, 3, ext, dst);
}

static void x64_shl_imm(ovm_emit_buf_t *b, int dst, int amount, int wide) {
    x64_rex(b, wide, 0, 0, rex_bit(dst));
    x64_u8(b, 0xC1);
    x64_modrm(b, 3, 4, dst);
    x64_u8(b, (u8) amount);
}

static void x64_cmp_rr(ovm_emit_buf_t *b, int a, int rb, int wide) {
    x64_alu_rr(b, 0x39, a, rb, wide);
}

static void x64_setcc(ovm_emit_buf_t *b, int dst, int cc) {
    if (dst >= 4) x64_rex(b, 0, 0, 0, rex_bit(dst));
    x64_u8(b, 0x0F);
    x64_u8(b, (u8) (0x90 + cc));
    x64_modrm(b, 3, 0, dst);
    x64_rex(b, 1, 0, 0, rex_bit(dst));
    x64_u8(b, 0x0F);
    x64_u8(b, 0xB6);
    x64_modrm(b, 3, dst, dst);
}

static void x64_jcc_reloc(ovm_emit_buf_t *b, int cc, i32 target) {
    i32 at = b->len;
    x64_u8(b, 0x0F);
    x64_u8(b, (u8) (0x80 + cc));
    x64_u32(b, 0);
    ovm_emit_reloc(b, at, target, OVM_RELOC_X64_REL32, cc);
}

static void x64_jmp_reloc(ovm_emit_buf_t *b, i32 target) {
    i32 at = b->len;
    x64_u8(b, 0xE9);
    x64_u32(b, 0);
    ovm_emit_reloc(b, at, target, OVM_RELOC_X64_REL32, -1);
}

static void x64_call_rax(ovm_emit_buf_t *b) {
    x64_u8(b, 0xFF);
    x64_u8(b, 0xD0);
}

static void x64_reload_pins(ovm_emit_buf_t *b) {
    x64_load_disp(b, RBX, R13, OFF_FRAME, 1);
    x64_load_disp(b, R12, R14, OFF_MEMORY, 1);
}

static void x64_call_abs(ovm_emit_buf_t *b, u64 addr) {
    x64_mov_imm64(b, RAX, addr);
    x64_call_rax(b);
}

static void x64_call_slow(ovm_emit_buf_t *b, ovm_program_t *program, i32 idx) {
    x64_mov_rr(b, RDI, R13);
    x64_mov_imm64(b, RSI, (u64)(uintptr_t) &program->code[idx]);
    x64_call_abs(b, (u64)(uintptr_t) ovm_jit_slow);
    x64_reload_pins(b);
}

static void x64_prologue(ovm_emit_buf_t *b) {
    x64_push(b, RBP);
    x64_mov_rr(b, RBP, RSP);
    x64_push(b, RBX);
    x64_push(b, R12);
    x64_push(b, R13);
    x64_push(b, R14);
    x64_mov_rr(b, R14, RDI);
    x64_mov_rr(b, R13, RSI);
    x64_reload_pins(b);
}

static void x64_epilogue(ovm_emit_buf_t *b) {
    x64_pop(b, R14);
    x64_pop(b, R13);
    x64_pop(b, R12);
    x64_pop(b, RBX);
    x64_pop(b, RBP);
    x64_u8(b, 0xC3);
}

static int cc_for_cmp(u32 op, int signed_int) {
    switch (op) {
        case OVMI_EQ: case OVMI_BR_EQ: return CC_E;
        case OVMI_NE: case OVMI_BR_NE: return CC_NE;
        case OVMI_LT: case OVMI_BR_LT: return signed_int ? CC_L : CC_B;
        case OVMI_LT_S: case OVMI_BR_LT_S: return CC_L;
        case OVMI_LE: case OVMI_BR_LE: return signed_int ? CC_LE : CC_BE;
        case OVMI_LE_S: case OVMI_BR_LE_S: return CC_LE;
        case OVMI_GT: case OVMI_BR_GT: return signed_int ? CC_G : CC_A;
        case OVMI_GT_S: case OVMI_BR_GT_S: return CC_G;
        case OVMI_GE: case OVMI_BR_GE: return signed_int ? CC_GE : CC_AE;
        case OVMI_GE_S: case OVMI_BR_GE_S: return CC_GE;
        default: return CC_E;
    }
}

static void x64_emit_alu(ovm_emit_buf_t *b, u8 opc, int wide, i32 r, i32 a, i32 ba) {
    x64_slot_ld(b, RAX, a, wide);
    x64_slot_ld(b, RCX, ba, wide);
    x64_alu_rr(b, opc, RAX, RCX, wide);
    x64_slot_st(b, r, RAX, wide);
}

static void x64_emit_alu_imm(ovm_emit_buf_t *b, u32 kind, int wide, i32 r, i32 a, i32 imm_i32) {
    x64_slot_ld(b, RAX, a, wide);
    i64 imm = wide ? (i64) imm_i32 : (i64)(i32) imm_i32;
    switch (kind) {
        case OVMI_ADD_IMM: x64_alu_imm32(b, RAX, (i32) imm, 0, wide); break;
        case OVMI_SUB_IMM: x64_alu_imm32(b, RAX, (i32) imm, 5, wide); break;
        case OVMI_MUL_IMM:
            x64_mov_imm64(b, RCX, (u64) imm);
            x64_imul(b, RAX, RCX, wide);
            break;
        case OVMI_AND_IMM:
            x64_alu_imm32(b, RAX, wide ? (i32) imm : imm_i32, 4, wide);
            break;
        case OVMI_OR_IMM:
            x64_alu_imm32(b, RAX, wide ? (i32) imm : imm_i32, 1, wide);
            break;
        case OVMI_XOR_IMM:
            x64_alu_imm32(b, RAX, wide ? (i32) imm : imm_i32, 6, wide);
            break;
        case OVMI_SHL_IMM:
            x64_mov_imm32(b, RCX, (u32) imm_i32);
            x64_shift_cl(b, RAX, 4, wide);
            break;
        case OVMI_SHR_IMM:
            x64_mov_imm32(b, RCX, (u32) imm_i32);
            x64_shift_cl(b, RAX, 5, wide);
            break;
        case OVMI_SAR_IMM:
            x64_mov_imm32(b, RCX, (u32) imm_i32);
            x64_shift_cl(b, RAX, 7, wide);
            break;
    }
    x64_slot_st(b, r, RAX, wide);
}

static void x64_lea_mem(ovm_emit_buf_t *b, i32 base_vn, i32 off) {
    x64_slot_ld(b, RAX, base_vn, 0);
    if (off) x64_alu_imm32(b, RAX, off, 0, 0);
    x64_rex(b, 1, 0, rex_bit(RAX), rex_bit(R12));
    x64_u8(b, 0x8D);
    x64_modrm(b, 0, RAX, 4);
    x64_sib(b, 0, RAX, R12);
}

static void x64_movd_xmm_from_gpr(ovm_emit_buf_t *b, int xmm, int gpr, int dbl) {
    x64_u8(b, 0x66);
    if (dbl) x64_u8(b, (u8) (0x48 | rex_bit(gpr)));
    else if (gpr >= 8) x64_u8(b, 0x41);
    x64_u8(b, 0x0F);
    x64_u8(b, 0x6E);
    x64_modrm(b, 3, xmm, gpr);
}

static void x64_movd_gpr_from_xmm(ovm_emit_buf_t *b, int gpr, int xmm, int dbl) {
    x64_u8(b, 0x66);
    if (dbl) x64_u8(b, (u8) (0x48 | rex_bit(gpr)));
    else if (gpr >= 8) x64_u8(b, 0x41);
    x64_u8(b, 0x0F);
    x64_u8(b, 0x7E);
    x64_modrm(b, 3, xmm, gpr);
}

static void x64_sse_bin(ovm_emit_buf_t *b, u8 opc, int dbl, int dst, int src) {
    x64_u8(b, dbl ? 0xF2 : 0xF3);
    x64_u8(b, 0x0F);
    x64_u8(b, opc);
    x64_modrm(b, 3, dst, src);
}

static void x64_emit_fp_bin(ovm_emit_buf_t *b, u8 sse_opc, int dbl, i32 r, i32 a, i32 ba) {
    x64_slot_ld(b, RAX, a, dbl);
    x64_movd_xmm_from_gpr(b, 0, RAX, dbl);
    x64_slot_ld(b, RCX, ba, dbl);
    x64_movd_xmm_from_gpr(b, 1, RCX, dbl);
    x64_sse_bin(b, sse_opc, dbl, 0, 1);
    x64_movd_gpr_from_xmm(b, RAX, 0, dbl);
    x64_slot_st(b, r, RAX, dbl);
}

static void x64_load_fp_pair(ovm_emit_buf_t *b, int dbl, i32 a, i32 ba) {
    x64_slot_ld(b, RAX, a, dbl);
    x64_movd_xmm_from_gpr(b, 0, RAX, dbl);
    x64_slot_ld(b, RCX, ba, dbl);
    x64_movd_xmm_from_gpr(b, 1, RCX, dbl);
}

static void x64_ucomi(ovm_emit_buf_t *b, int dbl) {
    if (dbl) x64_u8(b, 0x66);
    x64_u8(b, 0x0F);
    x64_u8(b, 0x2E);
    x64_modrm(b, 3, 0, 1);
}

/* IEEE/WASM: NaN makes EQ/LT/LE/GT/GE false and NE true.
   ucomiss sets PF on unordered; JE/JB/JBE are also true on NaN. */
static void x64_emit_fcmpset(ovm_emit_buf_t *b, int dbl, u32 op, i32 r, i32 a, i32 ba) {
    int cc = CC_E;
    int and_np = 0;
    int or_p = 0;
    switch (op) {
        case OVMI_EQ: cc = CC_E; and_np = 1; break;
        case OVMI_NE: cc = CC_NE; or_p = 1; break;
        case OVMI_LT: case OVMI_LT_S: cc = CC_B; and_np = 1; break;
        case OVMI_LE: case OVMI_LE_S: cc = CC_BE; and_np = 1; break;
        case OVMI_GT: case OVMI_GT_S: cc = CC_A; break;
        case OVMI_GE: case OVMI_GE_S: cc = CC_AE; break;
        default: break;
    }
    x64_load_fp_pair(b, dbl, a, ba);
    x64_ucomi(b, dbl);
    x64_setcc(b, RAX, cc);
    if (and_np) {
        x64_setcc(b, RCX, CC_NP);
        x64_alu_rr(b, 0x21, RAX, RCX, 0);
    } else if (or_p) {
        x64_setcc(b, RCX, CC_P);
        x64_alu_rr(b, 0x09, RAX, RCX, 0);
    }
    x64_slot_st(b, r, RAX, 0);
}

static void x64_jp_skip_jcc(ovm_emit_buf_t *b) {
    x64_u8(b, 0x7A);
    x64_u8(b, 0x06);
}

static void x64_emit_fp_br(ovm_emit_buf_t *b, int dbl, u32 op, i32 a, i32 ba, i32 target) {
    x64_load_fp_pair(b, dbl, a, ba);
    x64_ucomi(b, dbl);
    switch (op) {
        case OVMI_BR_EQ:
            x64_jp_skip_jcc(b);
            x64_jcc_reloc(b, CC_E, target);
            break;
        case OVMI_BR_NE:
            x64_jcc_reloc(b, CC_P, target);
            x64_jcc_reloc(b, CC_NE, target);
            break;
        case OVMI_BR_LT: case OVMI_BR_LT_S:
            x64_jp_skip_jcc(b);
            x64_jcc_reloc(b, CC_B, target);
            break;
        case OVMI_BR_LE: case OVMI_BR_LE_S:
            x64_jp_skip_jcc(b);
            x64_jcc_reloc(b, CC_BE, target);
            break;
        case OVMI_BR_GT: case OVMI_BR_GT_S:
            x64_jcc_reloc(b, CC_A, target);
            break;
        case OVMI_BR_GE: case OVMI_BR_GE_S:
            x64_jcc_reloc(b, CC_AE, target);
            break;
        default:
            x64_jp_skip_jcc(b);
            x64_jcc_reloc(b, CC_E, target);
            break;
    }
}

static void x64_sse_rex_opc(ovm_emit_buf_t *b, u8 pref, int rex_w, int reg, int rm, u8 opc) {
    x64_u8(b, pref);
    u8 rex = (u8) (0x40 | (rex_w ? 8 : 0) | (rex_bit(reg) ? 4 : 0) | rex_bit(rm));
    if (rex != 0x40) x64_u8(b, rex);
    x64_u8(b, 0x0F);
    x64_u8(b, opc);
    x64_modrm(b, 3, reg, rm);
}

static void x64_emit_fp_unop(ovm_emit_buf_t *b, u8 sse_opc, int dbl, i32 r, i32 a) {
    x64_slot_ld(b, RAX, a, dbl);
    x64_movd_xmm_from_gpr(b, 0, RAX, dbl);
    x64_sse_bin(b, sse_opc, dbl, 0, 0);
    x64_movd_gpr_from_xmm(b, RAX, 0, dbl);
    x64_slot_st(b, r, RAX, dbl);
}

static void x64_emit_fp_sign(ovm_emit_buf_t *b, int dbl, int abs, i32 r, i32 a) {
    x64_slot_ld(b, RAX, a, dbl);
    if (dbl) {
        x64_mov_imm64(b, RCX, abs ? 0x7FFFFFFFFFFFFFFFull : 0x8000000000000000ull);
        x64_alu_rr(b, abs ? 0x21 : 0x31, RAX, RCX, 1);
    } else {
        x64_alu_imm32(b, RAX, abs ? (i32) 0x7FFFFFFF : (i32) 0x80000000, abs ? 4 : 6, 0);
    }
    x64_slot_st(b, r, RAX, dbl);
}

static void x64_emit_fp_cvt(ovm_emit_buf_t *b, ovm_program_t *program, i32 idx, u32 op, u32 ty, i32 r, i32 a) {
    int to_f32 = (ty == OVM_TYPE_F32);
    int to_f64 = (ty == OVM_TYPE_F64);
    int to_i32 = (ty == OVM_TYPE_I32);
    int to_i64 = (ty == OVM_TYPE_I64);

    if ((op == OVMI_CVT_I32 || op == OVMI_CVT_I32_S || op == OVMI_CVT_I64 || op == OVMI_CVT_I64_S) &&
        (to_f32 || to_f64)) {
        int src64 = (op == OVMI_CVT_I64 || op == OVMI_CVT_I64_S);
        int uns = (op == OVMI_CVT_I32 || op == OVMI_CVT_I64);
        if (uns && src64) {
            x64_call_slow(b, program, idx);
            return;
        }
        x64_slot_ld(b, RAX, a, src64);
        /* u32 → float: zero-extend then convert as i64 (exact). */
        x64_sse_rex_opc(b, to_f64 ? 0xF2 : 0xF3, uns || src64, 0, RAX, 0x2A);
        x64_movd_gpr_from_xmm(b, RAX, 0, to_f64);
        x64_slot_st(b, r, RAX, to_f64);
        return;
    }

    if ((op == OVMI_CVT_F32 || op == OVMI_CVT_F32_S || op == OVMI_CVT_F64 || op == OVMI_CVT_F64_S) &&
        (to_i32 || to_i64)) {
        int src64 = (op == OVMI_CVT_F64 || op == OVMI_CVT_F64_S);
        int uns = (op == OVMI_CVT_F32 || op == OVMI_CVT_F64);
        if (uns) {
            x64_call_slow(b, program, idx);
            return;
        }
        x64_slot_ld(b, RAX, a, src64);
        x64_movd_xmm_from_gpr(b, 0, RAX, src64);
        x64_sse_rex_opc(b, src64 ? 0xF2 : 0xF3, to_i64, RAX, 0, 0x2C);
        x64_slot_st(b, r, RAX, to_i64);
        return;
    }

    if (op == OVMI_CVT_F32 && to_f64) {
        x64_slot_ld(b, RAX, a, 0);
        x64_movd_xmm_from_gpr(b, 0, RAX, 0);
        x64_sse_rex_opc(b, 0xF3, 0, 0, 0, 0x5A);
        x64_movd_gpr_from_xmm(b, RAX, 0, 1);
        x64_slot_st(b, r, RAX, 1);
        return;
    }
    if (op == OVMI_CVT_F64 && to_f32) {
        x64_slot_ld(b, RAX, a, 1);
        x64_movd_xmm_from_gpr(b, 0, RAX, 1);
        x64_sse_rex_opc(b, 0xF2, 0, 0, 0, 0x5A);
        x64_movd_gpr_from_xmm(b, RAX, 0, 0);
        x64_slot_st(b, r, RAX, 0);
        return;
    }

    x64_call_slow(b, program, idx);
}

static void x64_patch_rel8(ovm_emit_buf_t *b, i32 at) {
    b->data[at + 1] = (u8) (b->len - (at + 2));
}

static i32 x64_jcc8(ovm_emit_buf_t *b, int cc) {
    i32 at = b->len;
    x64_u8(b, (u8) (0x70 + cc));
    x64_u8(b, 0);
    return at;
}

static i32 x64_jmp8(ovm_emit_buf_t *b) {
    i32 at = b->len;
    x64_u8(b, 0xEB);
    x64_u8(b, 0);
    return at;
}

static void x64_idiv(ovm_emit_buf_t *b, int src, int wide) {
    x64_rex(b, wide, 0, 0, rex_bit(src));
    x64_u8(b, 0xF7);
    x64_modrm(b, 3, 7, src);
}

static void x64_div(ovm_emit_buf_t *b, int src, int wide) {
    x64_rex(b, wide, 0, 0, rex_bit(src));
    x64_u8(b, 0xF7);
    x64_modrm(b, 3, 6, src);
}

/* Skip #DE: divisor 0 leaves dest unchanged; INT_MIN/-1 is rem=0, quot=INT_MIN. */
static void x64_emit_divmod(ovm_emit_buf_t *b, int wide, int is_signed, int want_rem, i32 r, i32 a, i32 ba) {
    x64_slot_ld(b, RAX, a, wide);
    x64_slot_ld(b, RCX, ba, wide);
    x64_alu_rr(b, 0x85, RCX, RCX, wide);
    i32 jz_done = x64_jcc8(b, CC_E);

    i32 jmp_ovf_done = -1;
    if (is_signed) {
        x64_alu_imm32(b, RCX, -1, 7, wide);
        i32 jne_div = x64_jcc8(b, CC_NE);
        if (wide) {
            x64_mov_imm64(b, RDX, 0x8000000000000000ull);
            x64_cmp_rr(b, RAX, RDX, 1);
        } else {
            x64_alu_imm32(b, RAX, (i32) 0x80000000, 7, 0);
        }
        i32 jne_div2 = x64_jcc8(b, CC_NE);
        if (want_rem) {
            x64_u8(b, 0x31);
            x64_modrm(b, 3, RAX, RAX);
        }
        x64_slot_st(b, r, RAX, wide);
        jmp_ovf_done = x64_jmp8(b);
        x64_patch_rel8(b, jne_div);
        x64_patch_rel8(b, jne_div2);
        if (wide) x64_u8(b, 0x48);
        x64_u8(b, 0x99);
        x64_idiv(b, RCX, wide);
    } else {
        if (wide) x64_rex(b, 1, 0, 0, 0);
        x64_u8(b, 0x31);
        x64_modrm(b, 3, RDX, RDX);
        x64_div(b, RCX, wide);
    }
    if (want_rem) x64_mov_rr(b, RAX, RDX);
    x64_slot_st(b, r, RAX, wide);
    x64_patch_rel8(b, jz_done);
    if (jmp_ovf_done >= 0) x64_patch_rel8(b, jmp_ovf_done);
}

static void x64_emit_copy(ovm_emit_buf_t *b, i32 dest, i32 src, i32 count) {
    x64_slot_ld(b, RDI, dest, 0);
    x64_slot_ld(b, RSI, src, 0);
    x64_slot_ld(b, RDX, count, 0);
    x64_alu_rr(b, 0x85, RDI, RDI, 0);
    i32 z1 = x64_jcc8(b, CC_E);
    x64_alu_rr(b, 0x85, RSI, RSI, 0);
    i32 z2 = x64_jcc8(b, CC_E);
    x64_alu_rr(b, 0x01, RDI, R12, 1);
    x64_alu_rr(b, 0x01, RSI, R12, 1);
    x64_call_abs(b, (u64)(uintptr_t) memmove);
    x64_reload_pins(b);
    x64_patch_rel8(b, z1);
    x64_patch_rel8(b, z2);
}

static void x64_emit_fill(ovm_emit_buf_t *b, i32 dest, i32 byte, i32 count) {
    x64_slot_ld(b, RDI, dest, 0);
    x64_slot_ld(b, RSI, byte, 0);
    x64_slot_ld(b, RDX, count, 0);
    x64_alu_rr(b, 0x85, RDI, RDI, 0);
    i32 z = x64_jcc8(b, CC_E);
    x64_alu_rr(b, 0x01, RDI, R12, 1);
    x64_call_abs(b, (u64)(uintptr_t) memset);
    x64_reload_pins(b);
    x64_patch_rel8(b, z);
}

static void x64_emit_copysign(ovm_emit_buf_t *b, int dbl, i32 r, i32 a, i32 ba) {
    x64_slot_ld(b, RAX, a, dbl);
    x64_slot_ld(b, RCX, ba, dbl);
    if (dbl) {
        x64_mov_imm64(b, RDX, 0x8000000000000000ull);
        x64_alu_rr(b, 0x21, RCX, RDX, 1);
        x64_mov_imm64(b, RDX, 0x7FFFFFFFFFFFFFFFull);
        x64_alu_rr(b, 0x21, RAX, RDX, 1);
        x64_alu_rr(b, 0x09, RAX, RCX, 1);
    } else {
        x64_alu_imm32(b, RCX, (i32) 0x80000000, 4, 0);
        x64_alu_imm32(b, RAX, (i32) 0x7FFFFFFF, 4, 0);
        x64_alu_rr(b, 0x09, RAX, RCX, 0);
    }
    x64_slot_st(b, r, RAX, dbl);
}

static void x64_emit_round(ovm_emit_buf_t *b, int dbl, u8 mode, i32 r, i32 a) {
    x64_slot_ld(b, RAX, a, dbl);
    x64_movd_xmm_from_gpr(b, 0, RAX, dbl);
    x64_u8(b, 0x66);
    x64_u8(b, 0x0F);
    x64_u8(b, 0x3A);
    x64_u8(b, dbl ? 0x0B : 0x0A);
    x64_modrm(b, 3, 0, 0);
    /* bits 1:0 = mode; bit 2 = 1 would ignore those and use MXCSR.RC */
    x64_u8(b, (u8) (mode | 8));
    x64_movd_gpr_from_xmm(b, RAX, 0, dbl);
    x64_slot_st(b, r, RAX, dbl);
}

static void x64_emit_load(ovm_emit_buf_t *b, u32 ty, i32 r, i32 a, i32 off) {
    x64_lea_mem(b, a, off);
    switch (ty) {
        case OVM_TYPE_I8:
            x64_u8(b, 0x0F);
            x64_u8(b, 0xB6);
            x64_modrm(b, 0, RAX, RAX);
            x64_slot_st(b, r, RAX, 0);
            break;
        case OVM_TYPE_I16:
            x64_u8(b, 0x0F);
            x64_u8(b, 0xB7);
            x64_modrm(b, 0, RAX, RAX);
            x64_slot_st(b, r, RAX, 0);
            break;
        case OVM_TYPE_I32:
        case OVM_TYPE_F32:
            x64_u8(b, 0x8B);
            x64_modrm(b, 0, RAX, RAX);
            x64_slot_st(b, r, RAX, 0);
            break;
        default:
            x64_rex(b, 1, 0, 0, 0);
            x64_u8(b, 0x8B);
            x64_modrm(b, 0, RAX, RAX);
            x64_slot_st(b, r, RAX, 1);
            break;
    }
}

static void x64_emit_store(ovm_emit_buf_t *b, u32 ty, i32 r, i32 a, i32 off) {
    x64_lea_mem(b, r, off);
    x64_mov_rr(b, RDX, RAX);
    switch (ty) {
        case OVM_TYPE_I8:
            x64_slot_ld(b, RAX, a, 0);
            x64_u8(b, 0x88);
            x64_modrm(b, 0, RAX, RDX);
            break;
        case OVM_TYPE_I16:
            x64_slot_ld(b, RAX, a, 0);
            x64_u8(b, 0x66);
            x64_u8(b, 0x89);
            x64_modrm(b, 0, RAX, RDX);
            break;
        case OVM_TYPE_I32:
        case OVM_TYPE_F32:
            x64_slot_ld(b, RAX, a, 0);
            x64_u8(b, 0x89);
            x64_modrm(b, 0, RAX, RDX);
            break;
        default:
            x64_slot_ld(b, RAX, a, 1);
            x64_rex(b, 1, 0, 0, rex_bit(RDX));
            x64_u8(b, 0x89);
            x64_modrm(b, 0, RAX, RDX);
            break;
    }
}

static void x64_emit_cmpset(ovm_emit_buf_t *b, int wide, int cc, i32 r, i32 a, i32 ba) {
    x64_slot_ld(b, RAX, a, wide);
    x64_slot_ld(b, RCX, ba, wide);
    x64_cmp_rr(b, RAX, RCX, wide);
    x64_setcc(b, RAX, cc);
    x64_slot_st(b, r, RAX, 0);
}

static void x64_emit_one(ovm_emit_buf_t *b, ovm_program_t *program, i32 idx, ovm_instr_t *ins) {
    u32 op = OVM_INSTR_INSTR(*ins);
    u32 ty = OVM_INSTR_TYPE(*ins);
    int wide = (ty == OVM_TYPE_I64);

    switch (op) {
        case OVMI_NOP:
            break;
        case OVMI_MOV:
            x64_slot_ld(b, RAX, ins->a, 1);
            x64_slot_st(b, ins->r, RAX, 1);
            break;
        case OVMI_IMM:
            if (ty == OVM_TYPE_I32) x64_mov_imm64(b, RAX, (u64)(u32) ins->i);
            else if (ty == OVM_TYPE_I64) x64_mov_imm64(b, RAX, (u64) ins->l);
            else if (ty == OVM_TYPE_F32) {
                u32 bits = 0;
                memcpy(&bits, &ins->f, 4);
                x64_mov_imm64(b, RAX, bits);
            } else {
                u64 bits = 0;
                memcpy(&bits, &ins->d, 8);
                x64_mov_imm64(b, RAX, bits);
            }
            x64_slot_st(b, ins->r, RAX, 1);
            break;

        case OVMI_ADD:
            if (ty == OVM_TYPE_F32 || ty == OVM_TYPE_F64) {
                x64_emit_fp_bin(b, 0x58, ty == OVM_TYPE_F64, ins->r, ins->a, ins->b);
                break;
            }
            x64_emit_alu(b, 0x01, wide, ins->r, ins->a, ins->b);
            break;
        case OVMI_SUB:
            if (ty == OVM_TYPE_F32 || ty == OVM_TYPE_F64) {
                x64_emit_fp_bin(b, 0x5C, ty == OVM_TYPE_F64, ins->r, ins->a, ins->b);
                break;
            }
            x64_emit_alu(b, 0x29, wide, ins->r, ins->a, ins->b);
            break;
        case OVMI_MUL:
            if (ty == OVM_TYPE_F32 || ty == OVM_TYPE_F64) {
                x64_emit_fp_bin(b, 0x59, ty == OVM_TYPE_F64, ins->r, ins->a, ins->b);
                break;
            }
            x64_slot_ld(b, RAX, ins->a, wide);
            x64_slot_ld(b, RCX, ins->b, wide);
            x64_imul(b, RAX, RCX, wide);
            x64_slot_st(b, ins->r, RAX, wide);
            break;
        case OVMI_DIV: case OVMI_DIV_S:
            if (ty == OVM_TYPE_F32 || ty == OVM_TYPE_F64) {
                x64_emit_fp_bin(b, 0x5E, ty == OVM_TYPE_F64, ins->r, ins->a, ins->b);
                break;
            }
            x64_emit_divmod(b, wide, op == OVMI_DIV_S, 0, ins->r, ins->a, ins->b);
            break;
        case OVMI_REM: case OVMI_REM_S:
            x64_emit_divmod(b, wide, op == OVMI_REM_S, 1, ins->r, ins->a, ins->b);
            break;
        case OVMI_COPY:
            x64_emit_copy(b, ins->r, ins->a, ins->b);
            break;
        case OVMI_FILL:
            x64_emit_fill(b, ins->r, ins->a, ins->b);
            break;
        case OVMI_COPYSIGN:
            x64_emit_copysign(b, ty == OVM_TYPE_F64, ins->r, ins->a, ins->b);
            break;
        case OVMI_NEAREST:
            x64_emit_round(b, ty == OVM_TYPE_F64, 0, ins->r, ins->a);
            break;
        case OVMI_FLOOR:
            x64_emit_round(b, ty == OVM_TYPE_F64, 1, ins->r, ins->a);
            break;
        case OVMI_CEIL:
            x64_emit_round(b, ty == OVM_TYPE_F64, 2, ins->r, ins->a);
            break;
        case OVMI_TRUNC:
            x64_emit_round(b, ty == OVM_TYPE_F64, 3, ins->r, ins->a);
            break;
        case OVMI_NEG:
            x64_emit_fp_sign(b, ty == OVM_TYPE_F64, 0, ins->r, ins->a);
            break;
        case OVMI_ABS:
            x64_emit_fp_sign(b, ty == OVM_TYPE_F64, 1, ins->r, ins->a);
            break;
        case OVMI_SQRT:
            x64_emit_fp_unop(b, 0x51, ty == OVM_TYPE_F64, ins->r, ins->a);
            break;
        case OVMI_AND: x64_emit_alu(b, 0x21, wide, ins->r, ins->a, ins->b); break;
        case OVMI_OR:  x64_emit_alu(b, 0x09, wide, ins->r, ins->a, ins->b); break;
        case OVMI_XOR: x64_emit_alu(b, 0x31, wide, ins->r, ins->a, ins->b); break;
        case OVMI_SHL:
            x64_slot_ld(b, RAX, ins->a, wide);
            x64_slot_ld(b, RCX, ins->b, wide);
            x64_shift_cl(b, RAX, 4, wide);
            x64_slot_st(b, ins->r, RAX, wide);
            break;
        case OVMI_SHR:
            x64_slot_ld(b, RAX, ins->a, wide);
            x64_slot_ld(b, RCX, ins->b, wide);
            x64_shift_cl(b, RAX, 5, wide);
            x64_slot_st(b, ins->r, RAX, wide);
            break;
        case OVMI_SAR:
            x64_slot_ld(b, RAX, ins->a, wide);
            x64_slot_ld(b, RCX, ins->b, wide);
            x64_shift_cl(b, RAX, 7, wide);
            x64_slot_st(b, ins->r, RAX, wide);
            break;

        case OVMI_ADD_IMM: case OVMI_SUB_IMM: case OVMI_MUL_IMM:
        case OVMI_AND_IMM: case OVMI_OR_IMM: case OVMI_XOR_IMM:
        case OVMI_SHL_IMM: case OVMI_SHR_IMM: case OVMI_SAR_IMM:
            x64_emit_alu_imm(b, op, wide, ins->r, ins->a, ins->b);
            break;

        case OVMI_LOAD:  x64_emit_load(b, ty, ins->r, ins->a, ins->b); break;
        case OVMI_STORE: x64_emit_store(b, ty, ins->r, ins->a, ins->b); break;

        case OVMI_EQ: case OVMI_NE:
        case OVMI_LT: case OVMI_LT_S: case OVMI_LE: case OVMI_LE_S:
        case OVMI_GT: case OVMI_GT_S: case OVMI_GE: case OVMI_GE_S:
            if (ty == OVM_TYPE_F32 || ty == OVM_TYPE_F64) {
                x64_emit_fcmpset(b, ty == OVM_TYPE_F64, op, ins->r, ins->a, ins->b);
            } else {
                int s = (op == OVMI_LT_S || op == OVMI_LE_S || op == OVMI_GT_S || op == OVMI_GE_S);
                x64_emit_cmpset(b, wide, cc_for_cmp(op, s), ins->r, ins->a, ins->b);
            }
            break;

        case OVMI_BR:
            x64_jmp_reloc(b, idx + ins->a + 1);
            break;
        case OVMI_BR_Z:
            x64_slot_ld(b, RAX, ins->b, 0);
            x64_alu_rr(b, 0x85, RAX, RAX, 0); // test eax, eax
            x64_jcc_reloc(b, CC_E, idx + ins->a + 1);
            break;
        case OVMI_BR_NZ:
            x64_slot_ld(b, RAX, ins->b, 0);
            x64_alu_rr(b, 0x85, RAX, RAX, 0);
            x64_jcc_reloc(b, CC_NE, idx + ins->a + 1);
            break;

        case OVMI_BR_EQ: case OVMI_BR_NE:
        case OVMI_BR_LT: case OVMI_BR_LT_S:
        case OVMI_BR_LE: case OVMI_BR_LE_S:
        case OVMI_BR_GT: case OVMI_BR_GT_S:
        case OVMI_BR_GE: case OVMI_BR_GE_S:
            if (ty == OVM_TYPE_F32 || ty == OVM_TYPE_F64) {
                x64_emit_fp_br(b, ty == OVM_TYPE_F64, op, ins->a, ins->b, idx + ins->r + 1);
            } else {
                int s = (op == OVMI_BR_LT_S || op == OVMI_BR_LE_S || op == OVMI_BR_GT_S || op == OVMI_BR_GE_S);
                x64_slot_ld(b, RAX, ins->a, wide);
                x64_slot_ld(b, RCX, ins->b, wide);
                x64_cmp_rr(b, RAX, RCX, wide);
                x64_jcc_reloc(b, cc_for_cmp(op, s), idx + ins->r + 1);
            }
            break;

        case OVMI_REG_GET:
            x64_load_disp(b, RAX, R13, OFF_REGS, 1);
            x64_load_disp(b, RAX, RAX, ins->a * 16, 1);
            x64_slot_st(b, ins->r, RAX, 1);
            break;
        case OVMI_REG_SET:
            x64_load_disp(b, RAX, R13, OFF_REGS, 1);
            x64_slot_ld(b, RCX, ins->a, 1);
            x64_store_disp(b, RAX, ins->r * 16, RCX, 1);
            break;

        case OVMI_PARAM:
            // param_buf[count] is 16-byte ovm_value_t; SIB cannot scale by 16.
            x64_load_disp(b, RAX, R13, OFF_PCNT, 0);
            x64_load_disp(b, RCX, R13, OFF_PBUF, 1);
            x64_mov_rr(b, RDX, RAX);
            x64_shl_imm(b, RDX, 4, 1);
            x64_alu_rr(b, 0x01, RCX, RDX, 1);
            x64_slot_ld(b, RDX, ins->a, 1);
            x64_store_disp(b, RCX, 0, RDX, 1);
            x64_alu_imm32(b, RAX, 1, 0, 0);
            x64_store_disp(b, R13, OFF_PCNT, RAX, 0);
            break;

        case OVMI_RETURN:
            x64_slot_ld(b, RSI, ins->a, 1);
            x64_mov_rr(b, RDI, R13);
            x64_call_abs(b, (u64)(uintptr_t) ovm_jit_return);
            x64_epilogue(b);
            break;

        case OVMI_CALL:
            x64_mov_rr(b, RDI, R13);
            x64_mov_imm32(b, RSI, (u32) ins->a);
            x64_mov_imm64(b, RDX, (u64)(i64) ins->r);
            x64_call_abs(b, (u64)(uintptr_t) ovm_jit_call);
            x64_reload_pins(b);
            break;
        case OVMI_CALLI:
            x64_mov_rr(b, RDI, R13);
            x64_mov_imm32(b, RSI, (u32) ins->a);
            x64_mov_imm64(b, RDX, (u64)(i64) ins->r);
            x64_call_abs(b, (u64)(uintptr_t) ovm_jit_calli);
            x64_reload_pins(b);
            break;

        case OVMI_CLZ:
            x64_slot_ld(b, RAX, ins->a, wide);
            x64_rex(b, wide, 0, 0, 0);
            x64_u8(b, 0x0F);
            x64_u8(b, 0xBD);
            x64_modrm(b, 3, RAX, RAX); // bsr
            x64_mov_imm32(b, RCX, wide ? 63 : 31);
            x64_alu_rr(b, 0x29, RCX, RAX, wide); // ecx - eax
            x64_slot_st(b, ins->r, RCX, wide);
            break;
        case OVMI_CTZ:
            x64_slot_ld(b, RAX, ins->a, wide);
            x64_rex(b, wide, 0, 0, 0);
            x64_u8(b, 0x0F);
            x64_u8(b, 0xBC);
            x64_modrm(b, 3, RAX, RAX); // bsf
            x64_slot_st(b, ins->r, RAX, wide);
            break;
        case OVMI_POPCNT:
            x64_slot_ld(b, RAX, ins->a, wide);
            x64_u8(b, 0xF3);
            if (wide) x64_u8(b, 0x48);
            x64_u8(b, 0x0F);
            x64_u8(b, 0xB8);
            x64_modrm(b, 3, RAX, RAX);
            x64_slot_st(b, ins->r, RAX, wide);
            break;
        case OVMI_ROTL:
            x64_slot_ld(b, RAX, ins->a, wide);
            x64_slot_ld(b, RCX, ins->b, wide);
            x64_shift_cl(b, RAX, 0, wide); // rol
            x64_slot_st(b, ins->r, RAX, wide);
            break;
        case OVMI_ROTR:
            x64_slot_ld(b, RAX, ins->a, wide);
            x64_slot_ld(b, RCX, ins->b, wide);
            x64_shift_cl(b, RAX, 1, wide); // ror
            x64_slot_st(b, ins->r, RAX, wide);
            break;

        case OVMI_TRANSMUTE_I32:
        case OVMI_TRANSMUTE_I64:
        case OVMI_TRANSMUTE_F32:
        case OVMI_TRANSMUTE_F64:
            x64_slot_ld(b, RAX, ins->a, 1);
            x64_slot_st(b, ins->r, RAX, 1);
            break;

        case OVMI_CVT_I8: case OVMI_CVT_I8_S:
        case OVMI_CVT_I16: case OVMI_CVT_I16_S:
        case OVMI_CVT_I32: case OVMI_CVT_I32_S:
        case OVMI_CVT_I64: case OVMI_CVT_I64_S:
        case OVMI_CVT_F32: case OVMI_CVT_F32_S:
        case OVMI_CVT_F64: case OVMI_CVT_F64_S:
            if (ty == OVM_TYPE_F32 || ty == OVM_TYPE_F64 ||
                op == OVMI_CVT_F32 || op == OVMI_CVT_F32_S ||
                op == OVMI_CVT_F64 || op == OVMI_CVT_F64_S) {
                x64_emit_fp_cvt(b, program, idx, op, ty, ins->r, ins->a);
            } else {
                x64_slot_ld(b, RAX, ins->a, 1);
                if (op == OVMI_CVT_I8) {
                    x64_rex(b, 1, 0, 0, 0);
                    x64_u8(b, 0x0F); x64_u8(b, 0xB6); x64_modrm(b, 3, RAX, RAX);
                } else if (op == OVMI_CVT_I8_S) {
                    x64_rex(b, 1, 0, 0, 0);
                    x64_u8(b, 0x0F); x64_u8(b, 0xBE); x64_modrm(b, 3, RAX, RAX);
                } else if (op == OVMI_CVT_I16) {
                    x64_rex(b, 1, 0, 0, 0);
                    x64_u8(b, 0x0F); x64_u8(b, 0xB7); x64_modrm(b, 3, RAX, RAX);
                } else if (op == OVMI_CVT_I16_S) {
                    x64_rex(b, 1, 0, 0, 0);
                    x64_u8(b, 0x0F); x64_u8(b, 0xBF); x64_modrm(b, 3, RAX, RAX);
                } else if (op == OVMI_CVT_I32 && ty == OVM_TYPE_I64) {
                    x64_u8(b, 0x89); x64_modrm(b, 3, RAX, RAX); // mov eax, eax
                } else if (op == OVMI_CVT_I32_S && ty == OVM_TYPE_I64) {
                    x64_rex(b, 1, 0, 0, 0);
                    x64_u8(b, 0x63); x64_modrm(b, 3, RAX, RAX); // movsxd
                }
                x64_slot_st(b, ins->r, RAX, ty == OVM_TYPE_I64);
            }
            break;

        default:
            x64_call_slow(b, program, idx);
            break;
    }
}

void ovm_jit_emit_x64(ovm_emit_buf_t *buf, ovm_program_t *program, i32 start, i32 end) {
    x64_prologue(buf);
    for (i32 i = start; i < end; i++) {
        buf->ovmi_off[i - start] = buf->len;
        x64_emit_one(buf, program, i, &program->code[i]);
    }
    x64_mov_imm64(buf, RAX, 0);
    x64_epilogue(buf);
}

void ovm_jit_patch_x64(ovm_emit_buf_t *buf) {
    i32 n = bh_arr_length(buf->relocs);
    for (i32 i = 0; i < n; i++) {
        ovm_reloc_t r = buf->relocs[i];
        i32 local = r.target_ovmi - buf->func_start_ovmi;
        if (local < 0 || local >= buf->ninstr) continue;
        i32 target = buf->ovmi_off[local];
        i32 insn_len = (r.cond < 0) ? 5 : 6;
        i32 rel = target - (r.at + insn_len);
        memcpy(buf->data + r.at + (insn_len - 4), &rel, 4);
    }
}

void ovm_jit_fill_x64_jumptable(ovm_emit_buf_t *buf, u8 *exec_base) {
    (void) buf;
    (void) exec_base;
}

#else
typedef int ovm_jit_x64_unused;
#endif
