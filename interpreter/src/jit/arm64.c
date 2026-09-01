#include "emit.h"

#include <stddef.h>
#include <stdint.h>
#include <string.h>

#if defined(__aarch64__) || defined(__arm64__)

enum {
    X0 = 0, X1 = 1, X2 = 2, X8 = 8, X9 = 9, X10 = 10,
    X19 = 19, X20 = 20, X21 = 21, X22 = 22,
    X29 = 29, X30 = 30, SP = 31, XZR = 31
};

enum {
    COND_EQ = 0, COND_NE = 1, COND_HS = 2, COND_LO = 3,
    COND_HI = 8, COND_LS = 9, COND_GE = 10, COND_LT = 11,
    COND_GT = 12, COND_LE = 13
};

#define OFF_FRAME   ((i32) offsetof(ovm_state_t, __frame_values))
#define OFF_MEMORY  ((i32) offsetof(ovm_engine_t, memory))
#define OFF_PBUF    ((i32) offsetof(ovm_state_t, param_buf))
#define OFF_PCNT    ((i32) offsetof(ovm_state_t, param_count))
#define OFF_REGS    ((i32) offsetof(ovm_state_t, registers))

static void a64_u32(ovm_emit_buf_t *b, u32 w) { ovm_emit_u32(b, w); }

static void a64_mov64(ovm_emit_buf_t *b, int rd, u64 imm) {
    a64_u32(b, 0xD2800000u | ((u32)(imm & 0xffff) << 5) | (u32) rd);
    u16 p1 = (u16) (imm >> 16);
    u16 p2 = (u16) (imm >> 32);
    u16 p3 = (u16) (imm >> 48);
    if (p1) a64_u32(b, 0xF2800000u | (1u << 21) | ((u32) p1 << 5) | (u32) rd);
    if (p2) a64_u32(b, 0xF2800000u | (2u << 21) | ((u32) p2 << 5) | (u32) rd);
    if (p3) a64_u32(b, 0xF2800000u | (3u << 21) | ((u32) p3 << 5) | (u32) rd);
}

static void a64_add_imm(ovm_emit_buf_t *b, int sf, int rd, int rn, u32 imm) {
    if (imm <= 0xfff) {
        a64_u32(b, 0x11000000u | ((u32) sf << 31) | (imm << 10) | ((u32) rn << 5) | (u32) rd);
    } else if ((imm & 0xfff) == 0 && (imm >> 12) <= 0xfff) {
        a64_u32(b, 0x11000000u | ((u32) sf << 31) | (1u << 22) | ((imm >> 12) << 10) | ((u32) rn << 5) | (u32) rd);
    } else {
        a64_mov64(b, X10, imm);
        a64_u32(b, 0x0B000000u | ((u32) sf << 31) | (X10 << 16) | ((u32) rn << 5) | (u32) rd);
    }
}

static void a64_sub_imm(ovm_emit_buf_t *b, int sf, int rd, int rn, u32 imm) {
    if (imm <= 0xfff) {
        a64_u32(b, 0x51000000u | ((u32) sf << 31) | (imm << 10) | ((u32) rn << 5) | (u32) rd);
    } else {
        a64_mov64(b, X10, imm);
        a64_u32(b, 0x4B000000u | ((u32) sf << 31) | (X10 << 16) | ((u32) rn << 5) | (u32) rd);
    }
}

static void a64_ldr_off64(ovm_emit_buf_t *b, int rt, int rn, i32 off) {
    if (off >= 0 && (off & 7) == 0 && (off / 8) <= 4095) {
        a64_u32(b, 0xF9400000u | ((u32)(off / 8) << 10) | ((u32) rn << 5) | (u32) rt);
    } else {
        a64_mov64(b, X10, (u64)(u32) off);
        a64_u32(b, 0xF8606800u | (X10 << 16) | ((u32) rn << 5) | (u32) rt);
    }
}

static void a64_ldr_off32(ovm_emit_buf_t *b, int rt, int rn, i32 off) {
    if (off >= 0 && (off & 3) == 0 && (off / 4) <= 4095) {
        a64_u32(b, 0xB9400000u | ((u32)(off / 4) << 10) | ((u32) rn << 5) | (u32) rt);
    } else {
        a64_mov64(b, X10, (u64)(u32) off);
        a64_u32(b, 0xB8606800u | (X10 << 16) | ((u32) rn << 5) | (u32) rt);
    }
}

static void a64_str_off32(ovm_emit_buf_t *b, int rt, int rn, i32 off) {
    if (off >= 0 && (off & 3) == 0 && (off / 4) <= 4095) {
        a64_u32(b, 0xB9000000u | ((u32)(off / 4) << 10) | ((u32) rn << 5) | (u32) rt);
    } else {
        a64_mov64(b, X10, (u64)(u32) off);
        a64_u32(b, 0xB8206800u | (X10 << 16) | ((u32) rn << 5) | (u32) rt);
    }
}

static void a64_slot_ld64(ovm_emit_buf_t *b, int rt, i32 vn) {
    if (vn >= 0 && vn <= 4095) {
        a64_u32(b, 0xF9400000u | ((u32) vn << 10) | (X19 << 5) | (u32) rt);
    } else {
        a64_mov64(b, X10, (u64)(u32) vn);
        a64_u32(b, 0xF8607800u | (X10 << 16) | (X19 << 5) | (u32) rt);
    }
}

static void a64_slot_st64(ovm_emit_buf_t *b, int rt, i32 vn) {
    if (vn >= 0 && vn <= 4095) {
        a64_u32(b, 0xF9000000u | ((u32) vn << 10) | (X19 << 5) | (u32) rt);
    } else {
        a64_mov64(b, X10, (u64)(u32) vn);
        a64_u32(b, 0xF8207800u | (X10 << 16) | (X19 << 5) | (u32) rt);
    }
}

static void a64_slot_ld32(ovm_emit_buf_t *b, int rt, i32 vn) {
    if (vn >= 0 && vn * 2 <= 4095) {
        a64_u32(b, 0xB9400000u | ((u32)(vn * 2) << 10) | (X19 << 5) | (u32) rt);
    } else {
        a64_mov64(b, X10, (u64)(u32) vn * 8);
        a64_u32(b, 0xB8606800u | (X10 << 16) | (X19 << 5) | (u32) rt);
    }
}

static void a64_slot_st32(ovm_emit_buf_t *b, int rt, i32 vn) {
    if (vn >= 0 && vn * 2 <= 4095) {
        a64_u32(b, 0xB9000000u | ((u32)(vn * 2) << 10) | (X19 << 5) | (u32) rt);
    } else {
        a64_mov64(b, X10, (u64)(u32) vn * 8);
        a64_u32(b, 0xB8206800u | (X10 << 16) | (X19 << 5) | (u32) rt);
    }
}

static void a64_ldrsw_slot(ovm_emit_buf_t *b, int rt, i32 vn) {
    if (vn >= 0 && vn * 2 <= 4095) {
        a64_u32(b, 0xB9800000u | ((u32)(vn * 2) << 10) | (X19 << 5) | (u32) rt);
    } else {
        a64_mov64(b, X10, (u64)(u32) vn * 8);
        a64_u32(b, 0xB8A06800u | (X10 << 16) | (X19 << 5) | (u32) rt);
    }
}

static void a64_reload_pins(ovm_emit_buf_t *b) {
    a64_ldr_off64(b, X19, X21, OFF_FRAME);
    a64_ldr_off64(b, X20, X22, OFF_MEMORY);
}

static void a64_blr_abs(ovm_emit_buf_t *b, u64 addr) {
    a64_mov64(b, X8, addr);
    a64_u32(b, 0xD63F0000u | (X8 << 5));
}

static void a64_call_slow(ovm_emit_buf_t *b, ovm_program_t *program, i32 idx) {
    a64_u32(b, 0xAA1503E0u); // mov x0, x21
    a64_mov64(b, X1, (u64)(uintptr_t) &program->code[idx]);
    a64_blr_abs(b, (u64)(uintptr_t) ovm_jit_slow);
    a64_reload_pins(b);
}

static void a64_prologue(ovm_emit_buf_t *b) {
    a64_sub_imm(b, 1, SP, SP, 64);
    a64_u32(b, 0xA9007BFDu); // stp x29, x30, [sp]
    a64_u32(b, 0xA90153F3u); // stp x19, x20, [sp, #16]
    a64_u32(b, 0xA9025BF5u); // stp x21, x22, [sp, #32]
    a64_u32(b, 0xAA0003F6u); // mov x22, x0  engine
    a64_u32(b, 0xAA0103F5u); // mov x21, x1  state
    a64_reload_pins(b);
}

static void a64_epilogue(ovm_emit_buf_t *b) {
    a64_u32(b, 0xA94153F3u); // ldp x19, x20, [sp, #16]
    a64_u32(b, 0xA9425BF5u); // ldp x21, x22, [sp, #32]
    a64_u32(b, 0xA9407BFDu); // ldp x29, x30, [sp]
    a64_add_imm(b, 1, SP, SP, 64);
    a64_u32(b, 0xD65F03C0u); // ret
}

static void a64_b_reloc(ovm_emit_buf_t *b, i32 target) {
    i32 at = b->len;
    a64_u32(b, 0x14000000u);
    ovm_emit_reloc(b, at, target, OVM_RELOC_ARM64_B, 0);
}

static void a64_bcond_reloc(ovm_emit_buf_t *b, int cond, i32 target) {
    i32 at = b->len;
    a64_u32(b, 0x54000000u | (u32) cond);
    ovm_emit_reloc(b, at, target, OVM_RELOC_ARM64_BCOND, cond);
}

static void a64_cmp(ovm_emit_buf_t *b, int sf, int rn, int rm) {
    a64_u32(b, 0x6B00001Fu | ((u32) sf << 31) | ((u32) rm << 16) | ((u32) rn << 5));
}

static void a64_cset(ovm_emit_buf_t *b, int rd, int cond) {
    a64_u32(b, 0x1A9F07E0u | ((u32)(cond ^ 1) << 12) | (u32) rd);
}

static void a64_alu_reg(ovm_emit_buf_t *b, u32 base, int sf, int rd, int rn, int rm) {
    a64_u32(b, base | ((u32) sf << 31) | ((u32) rm << 16) | ((u32) rn << 5) | (u32) rd);
}

static void a64_emit_alu(ovm_emit_buf_t *b, u32 opbase, int wide, i32 r, i32 a, i32 srcb) {
    if (wide) {
        a64_slot_ld64(b, X8, a);
        a64_slot_ld64(b, X9, srcb);
        a64_alu_reg(b, opbase, 1, X8, X8, X9);
        a64_slot_st64(b, X8, r);
    } else {
        a64_slot_ld32(b, X8, a);
        a64_slot_ld32(b, X9, srcb);
        a64_alu_reg(b, opbase, 0, X8, X8, X9);
        a64_slot_st32(b, X8, r);
    }
}

static void a64_emit_alu_imm(ovm_emit_buf_t *b, u32 kind, int wide, i32 r, i32 a, i32 imm_i32) {
    if (wide) a64_slot_ld64(b, X8, a);
    else a64_slot_ld32(b, X8, a);

    i64 imm = wide ? (i64) imm_i32 : (i64)(i32) imm_i32;
    int sf = wide ? 1 : 0;

    switch (kind) {
        case OVMI_ADD_IMM:
            if (imm >= 0) a64_add_imm(b, sf, X8, X8, (u32) imm);
            else a64_sub_imm(b, sf, X8, X8, (u32)(-imm));
            break;
        case OVMI_SUB_IMM:
            if (imm >= 0) a64_sub_imm(b, sf, X8, X8, (u32) imm);
            else a64_add_imm(b, sf, X8, X8, (u32)(-imm));
            break;
        case OVMI_MUL_IMM:
            a64_mov64(b, X9, (u64) imm);
            a64_u32(b, 0x1B007C00u | ((u32) sf << 31) | (X9 << 16) | (X8 << 5) | X8);
            break;
        case OVMI_AND_IMM:
            a64_mov64(b, X9, wide ? (u64) imm : (u64)(u32) imm_i32);
            a64_alu_reg(b, 0x0A000000u, sf, X8, X8, X9);
            break;
        case OVMI_OR_IMM:
            a64_mov64(b, X9, wide ? (u64) imm : (u64)(u32) imm_i32);
            a64_alu_reg(b, 0x2A000000u, sf, X8, X8, X9);
            break;
        case OVMI_XOR_IMM:
            a64_mov64(b, X9, wide ? (u64) imm : (u64)(u32) imm_i32);
            a64_alu_reg(b, 0x4A000000u, sf, X8, X8, X9);
            break;
        case OVMI_SHL_IMM:
            a64_mov64(b, X9, (u64)(u32) imm_i32);
            a64_u32(b, 0x1AC02000u | ((u32) sf << 31) | (X9 << 16) | (X8 << 5) | X8);
            break;
        case OVMI_SHR_IMM:
            a64_mov64(b, X9, (u64)(u32) imm_i32);
            a64_u32(b, 0x1AC02400u | ((u32) sf << 31) | (X9 << 16) | (X8 << 5) | X8);
            break;
        case OVMI_SAR_IMM:
            a64_mov64(b, X9, (u64)(u32) imm_i32);
            a64_u32(b, 0x1AC02800u | ((u32) sf << 31) | (X9 << 16) | (X8 << 5) | X8);
            break;
    }

    if (wide) a64_slot_st64(b, X8, r);
    else a64_slot_st32(b, X8, r);
}

static void a64_fadd(ovm_emit_buf_t *b, int dbl, int rd, int rn, int rm) {
    a64_u32(b, (dbl ? 0x1E602800u : 0x1E202800u) | ((u32) rm << 16) | ((u32) rn << 5) | (u32) rd);
}
static void a64_fsub(ovm_emit_buf_t *b, int dbl, int rd, int rn, int rm) {
    a64_u32(b, (dbl ? 0x1E603800u : 0x1E203800u) | ((u32) rm << 16) | ((u32) rn << 5) | (u32) rd);
}
static void a64_fmul(ovm_emit_buf_t *b, int dbl, int rd, int rn, int rm) {
    a64_u32(b, (dbl ? 0x1E600800u : 0x1E200800u) | ((u32) rm << 16) | ((u32) rn << 5) | (u32) rd);
}
static void a64_fdiv(ovm_emit_buf_t *b, int dbl, int rd, int rn, int rm) {
    a64_u32(b, (dbl ? 0x1E601800u : 0x1E201800u) | ((u32) rm << 16) | ((u32) rn << 5) | (u32) rd);
}
static void a64_fcmp(ovm_emit_buf_t *b, int dbl, int rn, int rm) {
    a64_u32(b, (dbl ? 0x1E602000u : 0x1E202000u) | ((u32) rm << 16) | ((u32) rn << 5));
}
static void a64_fp_1src(ovm_emit_buf_t *b, u32 base_s, u32 base_d, int dbl, int rd, int rn) {
    a64_u32(b, (dbl ? base_d : base_s) | ((u32) rn << 5) | (u32) rd);
}

static void a64_slot_ld_fp(ovm_emit_buf_t *b, int dbl, int vt, i32 vn) {
    if (dbl) {
        if (vn >= 0 && vn <= 4095) {
            a64_u32(b, 0xFD400000u | ((u32) vn << 10) | (X19 << 5) | (u32) vt);
        } else {
            a64_slot_ld64(b, X8, vn);
            a64_u32(b, 0x9E670100u | (u32) vt); // fmov dN, x8  — wait, fmov Dd, Xn
        }
    } else {
        if (vn >= 0 && vn * 2 <= 4095) {
            a64_u32(b, 0xBD400000u | ((u32)(vn * 2) << 10) | (X19 << 5) | (u32) vt);
        } else {
            a64_slot_ld32(b, X8, vn);
            a64_u32(b, 0x1E270100u | (u32) vt); // fmov sN, w8
        }
    }
}

static void a64_slot_st_fp(ovm_emit_buf_t *b, int dbl, int vt, i32 vn) {
    if (dbl) {
        if (vn >= 0 && vn <= 4095) {
            a64_u32(b, 0xFD000000u | ((u32) vn << 10) | (X19 << 5) | (u32) vt);
        } else {
            a64_u32(b, 0x9E660108u); // fmov x8, d0  (vt assumed 0 if we only use v0)
            a64_slot_st64(b, X8, vn);
        }
    } else {
        if (vn >= 0 && vn * 2 <= 4095) {
            a64_u32(b, 0xBD000000u | ((u32)(vn * 2) << 10) | (X19 << 5) | (u32) vt);
        } else {
            a64_u32(b, 0x1E260108u); // fmov w8, s0
            a64_slot_st32(b, X8, vn);
        }
    }
}

// FMOV Dd, Xn: 1001 1110 0110 0111 0000 00 Rn Rd = 0x9E670000 | rn<<5 | rd
// FMOV Xd, Dn: 1001 1110 0110 0110 0000 00 Rn Rd = 0x9E660000 | rn<<5 | rd

static void a64_emit_fp_bin(ovm_emit_buf_t *b, u32 op, int dbl, i32 r, i32 a, i32 ba) {
    a64_slot_ld_fp(b, dbl, 0, a);
    a64_slot_ld_fp(b, dbl, 1, ba);
    if (op == OVMI_ADD) a64_fadd(b, dbl, 0, 0, 1);
    else if (op == OVMI_SUB) a64_fsub(b, dbl, 0, 0, 1);
    else if (op == OVMI_DIV || op == OVMI_DIV_S) a64_fdiv(b, dbl, 0, 0, 1);
    else a64_fmul(b, dbl, 0, 0, 1);
    a64_slot_st_fp(b, dbl, 0, r);
}

static void a64_emit_fp_unop(ovm_emit_buf_t *b, u32 base_s, u32 base_d, int dbl, i32 r, i32 a) {
    a64_slot_ld_fp(b, dbl, 0, a);
    a64_fp_1src(b, base_s, base_d, dbl, 0, 0);
    a64_slot_st_fp(b, dbl, 0, r);
}

static i32 a64_bcond_here(ovm_emit_buf_t *b, int cond) {
    i32 at = b->len;
    a64_u32(b, 0x54000000u | (u32) cond);
    return at;
}

static void a64_patch_bcond(ovm_emit_buf_t *b, i32 at) {
    i32 words = (b->len - at) / 4;
    u32 old = 0;
    memcpy(&old, b->data + at, 4);
    u32 insn = 0x54000000u | (((u32) words & 0x7ffff) << 5) | (old & 0xfu);
    memcpy(b->data + at, &insn, 4);
}

static void a64_emit_divmod(ovm_emit_buf_t *b, int wide, int is_signed, int want_rem, i32 r, i32 a, i32 ba) {
    if (wide) {
        a64_slot_ld64(b, X8, a);
        a64_slot_ld64(b, X9, ba);
    } else {
        a64_slot_ld32(b, X8, a);
        a64_slot_ld32(b, X9, ba);
    }
    u32 div = (is_signed ? 0x1AC00C00u : 0x1AC00800u) | ((u32) wide << 31) | (X9 << 16) | (X8 << 5) | X10;
    a64_u32(b, div);
    if (want_rem) {
        a64_u32(b, 0x1B008000u | ((u32) wide << 31) | (X9 << 16) | (X8 << 10) | (X10 << 5) | X8);
    } else {
        a64_u32(b, 0xAA0A03E8u); // mov x8, x10
    }
    if (wide) a64_slot_st64(b, X8, r);
    else a64_slot_st32(b, X8, r);
}

static void a64_emit_copy(ovm_emit_buf_t *b, i32 dest, i32 src, i32 count) {
    a64_slot_ld32(b, X0, dest);
    a64_slot_ld32(b, X1, src);
    a64_slot_ld32(b, X2, count);
    a64_u32(b, 0x7100001Fu); // cmp w0, #0
    i32 z1 = a64_bcond_here(b, COND_EQ);
    a64_u32(b, 0x7100003Fu); // cmp w1, #0
    i32 z2 = a64_bcond_here(b, COND_EQ);
    a64_u32(b, 0x8B000280u); // add x0, x20, x0
    a64_u32(b, 0x8B010281u); // add x1, x20, x1
    a64_blr_abs(b, (u64)(uintptr_t) memmove);
    a64_reload_pins(b);
    a64_patch_bcond(b, z1);
    a64_patch_bcond(b, z2);
}

static void a64_emit_fill(ovm_emit_buf_t *b, i32 dest, i32 byte, i32 count) {
    a64_slot_ld32(b, X0, dest);
    a64_slot_ld32(b, X1, byte);
    a64_slot_ld32(b, X2, count);
    a64_u32(b, 0x7100001Fu);
    i32 z = a64_bcond_here(b, COND_EQ);
    a64_u32(b, 0x8B000280u);
    a64_blr_abs(b, (u64)(uintptr_t) memset);
    a64_reload_pins(b);
    a64_patch_bcond(b, z);
}

static void a64_emit_copysign(ovm_emit_buf_t *b, int dbl, i32 r, i32 a, i32 ba) {
    if (dbl) {
        a64_slot_ld64(b, X8, a);
        a64_slot_ld64(b, X9, ba);
        a64_mov64(b, X10, 0x8000000000000000ull);
        a64_alu_reg(b, 0x0A000000u, 1, X9, X9, X10);
        a64_mov64(b, X10, 0x7FFFFFFFFFFFFFFFull);
        a64_alu_reg(b, 0x0A000000u, 1, X8, X8, X10);
        a64_alu_reg(b, 0x2A000000u, 1, X8, X8, X9);
        a64_slot_st64(b, X8, r);
    } else {
        a64_slot_ld32(b, X8, a);
        a64_slot_ld32(b, X9, ba);
        a64_mov64(b, X10, 0x80000000ull);
        a64_alu_reg(b, 0x0A000000u, 0, X9, X9, X10);
        a64_mov64(b, X10, 0x7FFFFFFFull);
        a64_alu_reg(b, 0x0A000000u, 0, X8, X8, X10);
        a64_alu_reg(b, 0x2A000000u, 0, X8, X8, X9);
        a64_slot_st32(b, X8, r);
    }
}

static void a64_emit_fp_cvt(ovm_emit_buf_t *b, ovm_program_t *program, i32 idx, u32 op, u32 ty, i32 r, i32 a) {
    int to_f32 = (ty == OVM_TYPE_F32);
    int to_f64 = (ty == OVM_TYPE_F64);
    int to_i32 = (ty == OVM_TYPE_I32);
    int to_i64 = (ty == OVM_TYPE_I64);

    if ((op == OVMI_CVT_I32 || op == OVMI_CVT_I32_S || op == OVMI_CVT_I64 || op == OVMI_CVT_I64_S) &&
        (to_f32 || to_f64)) {
        int src64 = (op == OVMI_CVT_I64 || op == OVMI_CVT_I64_S);
        int uns = (op == OVMI_CVT_I32 || op == OVMI_CVT_I64);
        if (src64) a64_slot_ld64(b, X8, a);
        else a64_slot_ld32(b, X8, a);
        u32 base = uns ? (to_f64 ? 0x1E630000u : 0x1E230000u)
                       : (to_f64 ? 0x1E620000u : 0x1E220000u);
        if (src64) base |= 0x80000000u;
        a64_u32(b, base | (X8 << 5));
        a64_slot_st_fp(b, to_f64, 0, r);
        return;
    }

    if ((op == OVMI_CVT_F32 || op == OVMI_CVT_F32_S || op == OVMI_CVT_F64 || op == OVMI_CVT_F64_S) &&
        (to_i32 || to_i64)) {
        int src64 = (op == OVMI_CVT_F64 || op == OVMI_CVT_F64_S);
        int uns = (op == OVMI_CVT_F32 || op == OVMI_CVT_F64);
        a64_slot_ld_fp(b, src64, 0, a);
        u32 base = uns ? 0x1E390000u : 0x1E380000u;
        if (src64) base |= 0x00400000u;
        if (to_i64) base |= 0x80000000u;
        a64_u32(b, base | X8);
        if (to_i64) a64_slot_st64(b, X8, r);
        else a64_slot_st32(b, X8, r);
        return;
    }

    if (op == OVMI_CVT_F32 && to_f64) {
        a64_slot_ld_fp(b, 0, 0, a);
        a64_u32(b, 0x1E22C000u);
        a64_slot_st_fp(b, 1, 0, r);
        return;
    }
    if (op == OVMI_CVT_F64 && to_f32) {
        a64_slot_ld_fp(b, 1, 0, a);
        a64_u32(b, 0x1E624000u);
        a64_slot_st_fp(b, 0, 0, r);
        return;
    }

    a64_call_slow(b, program, idx);
}

static void a64_mem_addr(ovm_emit_buf_t *b, i32 base_vn, i32 off) {
    a64_slot_ld32(b, X8, base_vn);
    if (off > 0) a64_add_imm(b, 0, X8, X8, (u32) off);
    else if (off < 0) a64_sub_imm(b, 0, X8, X8, (u32)(-off));
}

static void a64_emit_load(ovm_emit_buf_t *b, u32 ty, i32 r, i32 a, i32 off) {
    a64_mem_addr(b, a, off);
    switch (ty) {
        case OVM_TYPE_I8:  a64_u32(b, 0x38604800u | (X8 << 16) | (X20 << 5) | X9); a64_slot_st32(b, X9, r); break;
        case OVM_TYPE_I16: a64_u32(b, 0x78604800u | (X8 << 16) | (X20 << 5) | X9); a64_slot_st32(b, X9, r); break;
        case OVM_TYPE_I32:
        case OVM_TYPE_F32: a64_u32(b, 0xB8604800u | (X8 << 16) | (X20 << 5) | X9); a64_slot_st32(b, X9, r); break;
        case OVM_TYPE_I64:
        case OVM_TYPE_F64: a64_u32(b, 0xF8604800u | (X8 << 16) | (X20 << 5) | X9); a64_slot_st64(b, X9, r); break;
        default: break;
    }
}

static void a64_emit_store(ovm_emit_buf_t *b, u32 ty, i32 r, i32 a, i32 off) {
    a64_mem_addr(b, r, off);
    switch (ty) {
        case OVM_TYPE_I8:
            a64_slot_ld32(b, X9, a);
            a64_u32(b, 0x38204800u | (X8 << 16) | (X20 << 5) | X9);
            break;
        case OVM_TYPE_I16:
            a64_slot_ld32(b, X9, a);
            a64_u32(b, 0x78204800u | (X8 << 16) | (X20 << 5) | X9);
            break;
        case OVM_TYPE_I32:
        case OVM_TYPE_F32:
            a64_slot_ld32(b, X9, a);
            a64_u32(b, 0xB8204800u | (X8 << 16) | (X20 << 5) | X9);
            break;
        case OVM_TYPE_I64:
        case OVM_TYPE_F64:
            a64_slot_ld64(b, X9, a);
            a64_u32(b, 0xF8204800u | (X8 << 16) | (X20 << 5) | X9);
            break;
        default: break;
    }
}

static void a64_emit_cmpset(ovm_emit_buf_t *b, int sf, int cond, i32 r, i32 a, i32 ba) {
    if (sf) {
        a64_slot_ld64(b, X8, a);
        a64_slot_ld64(b, X9, ba);
    } else {
        a64_slot_ld32(b, X8, a);
        a64_slot_ld32(b, X9, ba);
    }
    a64_cmp(b, sf, X8, X9);
    a64_cset(b, X8, cond);
    a64_slot_st32(b, X8, r);
}

static void a64_emit_fcmpset(ovm_emit_buf_t *b, int dbl, int cond, i32 r, i32 a, i32 ba) {
    a64_slot_ld_fp(b, dbl, 0, a);
    a64_slot_ld_fp(b, dbl, 1, ba);
    a64_fcmp(b, dbl, 0, 1);
    a64_cset(b, X8, cond);
    a64_slot_st32(b, X8, r);
}

static int cond_for_cmp(u32 op, int signed_int) {
    switch (op) {
        case OVMI_EQ: case OVMI_BR_EQ: return COND_EQ;
        case OVMI_NE: case OVMI_BR_NE: return COND_NE;
        case OVMI_LT: case OVMI_BR_LT: return signed_int ? COND_LT : COND_LO;
        case OVMI_LT_S: case OVMI_BR_LT_S: return COND_LT;
        case OVMI_LE: case OVMI_BR_LE: return signed_int ? COND_LE : COND_LS;
        case OVMI_LE_S: case OVMI_BR_LE_S: return COND_LE;
        case OVMI_GT: case OVMI_BR_GT: return signed_int ? COND_GT : COND_HI;
        case OVMI_GT_S: case OVMI_BR_GT_S: return COND_GT;
        case OVMI_GE: case OVMI_BR_GE: return signed_int ? COND_GE : COND_HS;
        case OVMI_GE_S: case OVMI_BR_GE_S: return COND_GE;
        default: return COND_EQ;
    }
}

static void a64_emit_cvt_int(ovm_emit_buf_t *b, u32 op, u32 ty, i32 r, i32 a) {
    switch (op) {
        case OVMI_CVT_I8:
            a64_slot_ld32(b, X8, a);
            a64_u32(b, 0x12001D08u); // and w8, w8, #0xff
            if (ty == OVM_TYPE_I64) a64_slot_st64(b, X8, r);
            else a64_slot_st32(b, X8, r);
            break;
        case OVMI_CVT_I8_S:
            a64_slot_ld32(b, X8, a);
            a64_u32(b, 0x13001D08u); // sbfx w8, w8, #0, #8
            if (ty == OVM_TYPE_I64) {
                a64_u32(b, 0x93407D08u); // sxtw x8, w8
                a64_slot_st64(b, X8, r);
            } else a64_slot_st32(b, X8, r);
            break;
        case OVMI_CVT_I16:
            a64_slot_ld32(b, X8, a);
            a64_u32(b, 0x12003D08u); // and w8, w8, #0xffff
            if (ty == OVM_TYPE_I64) a64_slot_st64(b, X8, r);
            else a64_slot_st32(b, X8, r);
            break;
        case OVMI_CVT_I16_S:
            a64_slot_ld32(b, X8, a);
            a64_u32(b, 0x13003D08u); // sbfx w8, w8, #0, #16
            if (ty == OVM_TYPE_I64) {
                a64_u32(b, 0x93407D08u);
                a64_slot_st64(b, X8, r);
            } else a64_slot_st32(b, X8, r);
            break;
        case OVMI_CVT_I32:
            a64_slot_ld32(b, X8, a);
            if (ty == OVM_TYPE_I64) a64_slot_st64(b, X8, r);
            else if (ty == OVM_TYPE_I8) { a64_u32(b, 0x12001D08u); a64_slot_st32(b, X8, r); }
            else if (ty == OVM_TYPE_I16) { a64_u32(b, 0x12003D08u); a64_slot_st32(b, X8, r); }
            else a64_slot_st32(b, X8, r);
            break;
        case OVMI_CVT_I32_S:
            if (ty == OVM_TYPE_I64) {
                a64_ldrsw_slot(b, X8, a);
                a64_slot_st64(b, X8, r);
            } else {
                a64_slot_ld32(b, X8, a);
                if (ty == OVM_TYPE_I8) a64_u32(b, 0x13001D08u);
                else if (ty == OVM_TYPE_I16) a64_u32(b, 0x13003D08u);
                a64_slot_st32(b, X8, r);
            }
            break;
        case OVMI_CVT_I64:
            a64_slot_ld64(b, X8, a);
            if (ty == OVM_TYPE_I64) a64_slot_st64(b, X8, r);
            else a64_slot_st32(b, X8, r);
            break;
        case OVMI_CVT_I64_S:
            a64_slot_ld64(b, X8, a);
            if (ty == OVM_TYPE_I8) a64_u32(b, 0x13001D08u);
            else if (ty == OVM_TYPE_I16) a64_u32(b, 0x13003D08u);
            if (ty == OVM_TYPE_I64) a64_slot_st64(b, X8, r);
            else a64_slot_st32(b, X8, r);
            break;
        default:
            break;
    }
}

static void a64_emit_call_fast(ovm_emit_buf_t *b, u64 helper, u64 a, i32 result_vn) {
    a64_u32(b, 0xAA1503E0u); // mov x0, x21
    a64_mov64(b, X1, a);
    a64_mov64(b, X2, (u64)(i64) result_vn);
    a64_blr_abs(b, helper);
    a64_u32(b, 0xF100001Fu); // cmp x0, #0
    i32 z = a64_bcond_here(b, COND_EQ);
    a64_reload_pins(b);
    a64_u32(b, 0xD63F0000u); // blr x0
    a64_patch_bcond(b, z);
    a64_reload_pins(b);
}

static void a64_emit_one(ovm_emit_buf_t *b, ovm_program_t *program, i32 idx, ovm_instr_t *ins) {
    u32 op = OVM_INSTR_INSTR(*ins);
    u32 ty = OVM_INSTR_TYPE(*ins);
    int wide = (ty == OVM_TYPE_I64);
    int dbl = (ty == OVM_TYPE_F64);

    switch (op) {
        case OVMI_NOP:
            break;

        case OVMI_MOV:
            a64_slot_ld64(b, X8, ins->a);
            a64_slot_st64(b, X8, ins->r);
            break;

        case OVMI_IMM:
            if (ty == OVM_TYPE_I32) a64_mov64(b, X8, (u64)(u32) ins->i);
            else if (ty == OVM_TYPE_I64) a64_mov64(b, X8, (u64) ins->l);
            else if (ty == OVM_TYPE_F32) {
                u32 bits = 0;
                memcpy(&bits, &ins->f, 4);
                a64_mov64(b, X8, bits);
            } else {
                u64 bits = 0;
                memcpy(&bits, &ins->d, 8);
                a64_mov64(b, X8, bits);
            }
            a64_slot_st64(b, X8, ins->r);
            break;

        case OVMI_ADD:
            if (ty == OVM_TYPE_F32 || ty == OVM_TYPE_F64) a64_emit_fp_bin(b, op, dbl, ins->r, ins->a, ins->b);
            else a64_emit_alu(b, 0x0B000000u, wide, ins->r, ins->a, ins->b);
            break;
        case OVMI_SUB:
            if (ty == OVM_TYPE_F32 || ty == OVM_TYPE_F64) a64_emit_fp_bin(b, op, dbl, ins->r, ins->a, ins->b);
            else a64_emit_alu(b, 0x4B000000u, wide, ins->r, ins->a, ins->b);
            break;
        case OVMI_MUL:
            if (ty == OVM_TYPE_F32 || ty == OVM_TYPE_F64) a64_emit_fp_bin(b, op, dbl, ins->r, ins->a, ins->b);
            else {
                if (wide) { a64_slot_ld64(b, X8, ins->a); a64_slot_ld64(b, X9, ins->b); }
                else { a64_slot_ld32(b, X8, ins->a); a64_slot_ld32(b, X9, ins->b); }
                a64_u32(b, 0x1B007C00u | ((u32) wide << 31) | (X9 << 16) | (X8 << 5) | X8);
                if (wide) a64_slot_st64(b, X8, ins->r); else a64_slot_st32(b, X8, ins->r);
            }
            break;
        case OVMI_DIV: case OVMI_DIV_S:
            if (ty == OVM_TYPE_F32 || ty == OVM_TYPE_F64) a64_emit_fp_bin(b, op, dbl, ins->r, ins->a, ins->b);
            else a64_emit_divmod(b, wide, op == OVMI_DIV_S, 0, ins->r, ins->a, ins->b);
            break;
        case OVMI_REM: case OVMI_REM_S:
            a64_emit_divmod(b, wide, op == OVMI_REM_S, 1, ins->r, ins->a, ins->b);
            break;
        case OVMI_COPY:
            a64_emit_copy(b, ins->r, ins->a, ins->b);
            break;
        case OVMI_FILL:
            a64_emit_fill(b, ins->r, ins->a, ins->b);
            break;
        case OVMI_NEG:
            a64_emit_fp_unop(b, 0x1E214000u, 0x1E614000u, dbl, ins->r, ins->a);
            break;
        case OVMI_ABS:
            a64_emit_fp_unop(b, 0x1E20C000u, 0x1E60C000u, dbl, ins->r, ins->a);
            break;
        case OVMI_SQRT:
            a64_emit_fp_unop(b, 0x1E21C000u, 0x1E61C000u, dbl, ins->r, ins->a);
            break;
        case OVMI_COPYSIGN:
            a64_emit_copysign(b, dbl, ins->r, ins->a, ins->b);
            break;
        case OVMI_NEAREST:
            a64_emit_fp_unop(b, 0x1E244000u, 0x1E644000u, dbl, ins->r, ins->a);
            break;
        case OVMI_FLOOR:
            a64_emit_fp_unop(b, 0x1E254000u, 0x1E654000u, dbl, ins->r, ins->a);
            break;
        case OVMI_CEIL:
            a64_emit_fp_unop(b, 0x1E24C000u, 0x1E64C000u, dbl, ins->r, ins->a);
            break;
        case OVMI_TRUNC:
            a64_emit_fp_unop(b, 0x1E25C000u, 0x1E65C000u, dbl, ins->r, ins->a);
            break;
        case OVMI_AND: a64_emit_alu(b, 0x0A000000u, wide, ins->r, ins->a, ins->b); break;
        case OVMI_OR:  a64_emit_alu(b, 0x2A000000u, wide, ins->r, ins->a, ins->b); break;
        case OVMI_XOR: a64_emit_alu(b, 0x4A000000u, wide, ins->r, ins->a, ins->b); break;
        case OVMI_SHL:
            if (wide) { a64_slot_ld64(b, X8, ins->a); a64_slot_ld64(b, X9, ins->b); }
            else { a64_slot_ld32(b, X8, ins->a); a64_slot_ld32(b, X9, ins->b); }
            a64_u32(b, 0x1AC02000u | ((u32) wide << 31) | (X9 << 16) | (X8 << 5) | X8);
            if (wide) a64_slot_st64(b, X8, ins->r); else a64_slot_st32(b, X8, ins->r);
            break;
        case OVMI_SHR:
            if (wide) { a64_slot_ld64(b, X8, ins->a); a64_slot_ld64(b, X9, ins->b); }
            else { a64_slot_ld32(b, X8, ins->a); a64_slot_ld32(b, X9, ins->b); }
            a64_u32(b, 0x1AC02400u | ((u32) wide << 31) | (X9 << 16) | (X8 << 5) | X8);
            if (wide) a64_slot_st64(b, X8, ins->r); else a64_slot_st32(b, X8, ins->r);
            break;
        case OVMI_SAR:
            if (wide) { a64_slot_ld64(b, X8, ins->a); a64_slot_ld64(b, X9, ins->b); }
            else { a64_slot_ld32(b, X8, ins->a); a64_slot_ld32(b, X9, ins->b); }
            a64_u32(b, 0x1AC02800u | ((u32) wide << 31) | (X9 << 16) | (X8 << 5) | X8);
            if (wide) a64_slot_st64(b, X8, ins->r); else a64_slot_st32(b, X8, ins->r);
            break;

        case OVMI_ADD_IMM: case OVMI_SUB_IMM: case OVMI_MUL_IMM:
        case OVMI_AND_IMM: case OVMI_OR_IMM: case OVMI_XOR_IMM:
        case OVMI_SHL_IMM: case OVMI_SHR_IMM: case OVMI_SAR_IMM:
            a64_emit_alu_imm(b, op, wide, ins->r, ins->a, ins->b);
            break;

        case OVMI_LOAD:  a64_emit_load(b, ty, ins->r, ins->a, ins->b); break;
        case OVMI_STORE: a64_emit_store(b, ty, ins->r, ins->a, ins->b); break;

        case OVMI_EQ: case OVMI_NE:
        case OVMI_LT: case OVMI_LT_S: case OVMI_LE: case OVMI_LE_S:
        case OVMI_GT: case OVMI_GT_S: case OVMI_GE: case OVMI_GE_S:
            if (ty == OVM_TYPE_F32 || ty == OVM_TYPE_F64) {
                a64_emit_fcmpset(b, dbl, cond_for_cmp(op, 1), ins->r, ins->a, ins->b);
            } else {
                int s = (op == OVMI_LT_S || op == OVMI_LE_S || op == OVMI_GT_S || op == OVMI_GE_S);
                a64_emit_cmpset(b, wide, cond_for_cmp(op, s), ins->r, ins->a, ins->b);
            }
            break;

        case OVMI_BR:
            a64_b_reloc(b, idx + ins->a + 1);
            break;
        case OVMI_BR_Z:
            a64_slot_ld32(b, X8, ins->b);
            a64_u32(b, 0x7100011Fu); // cmp w8, #0
            a64_bcond_reloc(b, COND_EQ, idx + ins->a + 1);
            break;
        case OVMI_BR_NZ:
            a64_slot_ld32(b, X8, ins->b);
            a64_u32(b, 0x7100011Fu);
            a64_bcond_reloc(b, COND_NE, idx + ins->a + 1);
            break;

        case OVMI_BR_EQ: case OVMI_BR_NE:
        case OVMI_BR_LT: case OVMI_BR_LT_S:
        case OVMI_BR_LE: case OVMI_BR_LE_S:
        case OVMI_BR_GT: case OVMI_BR_GT_S:
        case OVMI_BR_GE: case OVMI_BR_GE_S: {
            int target = idx + ins->r + 1;
            int s = (op == OVMI_BR_LT_S || op == OVMI_BR_LE_S || op == OVMI_BR_GT_S || op == OVMI_BR_GE_S);
            int cond = cond_for_cmp(op, s);
            if (ty == OVM_TYPE_F32 || ty == OVM_TYPE_F64) {
                a64_slot_ld_fp(b, dbl, 0, ins->a);
                a64_slot_ld_fp(b, dbl, 1, ins->b);
                a64_fcmp(b, dbl, 0, 1);
            } else {
                if (wide) { a64_slot_ld64(b, X8, ins->a); a64_slot_ld64(b, X9, ins->b); }
                else { a64_slot_ld32(b, X8, ins->a); a64_slot_ld32(b, X9, ins->b); }
                a64_cmp(b, wide, X8, X9);
            }
            a64_bcond_reloc(b, cond, target);
            break;
        }

        case OVMI_REG_GET:
            a64_ldr_off64(b, X8, X21, OFF_REGS);
            a64_mov64(b, X9, (u64)(u32) ins->a * 16);
            a64_u32(b, 0xF8696908u); // ldr x8, [x8, x9]
            a64_slot_st64(b, X8, ins->r);
            break;
        case OVMI_REG_SET:
            a64_ldr_off64(b, X8, X21, OFF_REGS);
            a64_mov64(b, X9, (u64)(u32) ins->r * 16);
            a64_slot_ld64(b, X10, ins->a);
            a64_u32(b, 0xF829690Au); // str x10, [x8, x9]
            break;

        case OVMI_PARAM:
            a64_ldr_off32(b, X8, X21, OFF_PCNT);
            a64_ldr_off64(b, X9, X21, OFF_PBUF);
            a64_u32(b, 0x8B081129u); // add x9, x9, x8, lsl #4
            a64_slot_ld64(b, X10, ins->a);
            a64_u32(b, 0xF900012Au); // str x10, [x9]
            a64_add_imm(b, 0, X8, X8, 1);
            a64_str_off32(b, X8, X21, OFF_PCNT);
            break;

        case OVMI_RETURN:
            a64_slot_ld64(b, X1, ins->a);
            a64_u32(b, 0xAA1503E0u); // mov x0, x21
            a64_blr_abs(b, (u64)(uintptr_t) ovm_jit_return);
            a64_u32(b, 0xF84107FEu); // ldr x30, [sp], #16
            a64_u32(b, 0xD65F03C0u);
            break;

        case OVMI_CALL:
            a64_emit_call_fast(b, (u64)(uintptr_t) ovm_jit_call_fast, (u64)(i64) ins->a, ins->r);
            break;
        case OVMI_CALLI:
            a64_emit_call_fast(b, (u64)(uintptr_t) ovm_jit_calli_fast, (u64)(i64) ins->a, ins->r);
            break;

        case OVMI_CLZ:
            if (wide) a64_slot_ld64(b, X8, ins->a);
            else a64_slot_ld32(b, X8, ins->a);
            a64_u32(b, 0x5AC01000u | ((u32) wide << 31) | (X8 << 5) | X8);
            if (wide) a64_slot_st64(b, X8, ins->r); else a64_slot_st32(b, X8, ins->r);
            break;
        case OVMI_CTZ:
            if (wide) a64_slot_ld64(b, X8, ins->a);
            else a64_slot_ld32(b, X8, ins->a);
            a64_u32(b, 0x5AC00000u | ((u32) wide << 31) | (X8 << 5) | X8); // rbit
            a64_u32(b, 0x5AC01000u | ((u32) wide << 31) | (X8 << 5) | X8); // clz
            if (wide) a64_slot_st64(b, X8, ins->r); else a64_slot_st32(b, X8, ins->r);
            break;
        case OVMI_POPCNT:
            if (wide) {
                a64_slot_ld64(b, X8, ins->a);
                a64_u32(b, 0x9E670100u); // fmov d0, x8
            } else {
                a64_slot_ld32(b, X8, ins->a);
                a64_u32(b, 0x1E270100u); // fmov s0, w8
            }
            a64_u32(b, 0x0E205800u); // cnt v0.8b, v0.8b
            a64_u32(b, 0x0E31B800u); // addv b0, v0.8b
            a64_u32(b, 0x1E260108u); // fmov w8, s0
            if (wide) a64_slot_st64(b, X8, ins->r); else a64_slot_st32(b, X8, ins->r);
            break;
        case OVMI_ROTL:
            if (wide) { a64_slot_ld64(b, X8, ins->a); a64_slot_ld64(b, X9, ins->b); }
            else { a64_slot_ld32(b, X8, ins->a); a64_slot_ld32(b, X9, ins->b); }
            a64_u32(b, 0x4B0903E9u | ((u32) wide << 31)); // neg x/w9, x/w9
            a64_u32(b, 0x1AC02C00u | ((u32) wide << 31) | (X9 << 16) | (X8 << 5) | X8); // rorv
            if (wide) a64_slot_st64(b, X8, ins->r); else a64_slot_st32(b, X8, ins->r);
            break;
        case OVMI_ROTR:
            if (wide) { a64_slot_ld64(b, X8, ins->a); a64_slot_ld64(b, X9, ins->b); }
            else { a64_slot_ld32(b, X8, ins->a); a64_slot_ld32(b, X9, ins->b); }
            a64_u32(b, 0x1AC02C00u | ((u32) wide << 31) | (X9 << 16) | (X8 << 5) | X8);
            if (wide) a64_slot_st64(b, X8, ins->r); else a64_slot_st32(b, X8, ins->r);
            break;

        case OVMI_CVT_I8: case OVMI_CVT_I8_S:
        case OVMI_CVT_I16: case OVMI_CVT_I16_S:
        case OVMI_CVT_I32: case OVMI_CVT_I32_S:
        case OVMI_CVT_I64: case OVMI_CVT_I64_S:
            if (ty == OVM_TYPE_F32 || ty == OVM_TYPE_F64) {
                a64_emit_fp_cvt(b, program, idx, op, ty, ins->r, ins->a);
            } else {
                a64_emit_cvt_int(b, op, ty, ins->r, ins->a);
            }
            break;
        case OVMI_CVT_F32: case OVMI_CVT_F32_S:
        case OVMI_CVT_F64: case OVMI_CVT_F64_S:
            a64_emit_fp_cvt(b, program, idx, op, ty, ins->r, ins->a);
            break;

        case OVMI_TRANSMUTE_I32:
        case OVMI_TRANSMUTE_I64:
        case OVMI_TRANSMUTE_F32:
        case OVMI_TRANSMUTE_F64:
            a64_slot_ld64(b, X8, ins->a);
            a64_slot_st64(b, X8, ins->r);
            break;

        default:
            a64_call_slow(b, program, idx);
            break;
    }
}

void ovm_jit_emit_arm64(ovm_emit_buf_t *buf, ovm_program_t *program, i32 start, i32 end) {
    i32 func_start = buf->len;
    a64_prologue(buf);
    i32 bl_at = buf->len;
    a64_u32(buf, 0x94000000u);
    a64_epilogue(buf);
    buf->nested_off = buf->len - func_start;
    i32 words = (buf->len - bl_at) / 4;
    u32 bl = 0x94000000u | ((u32) words & 0x03ffffffu);
    memcpy(buf->data + bl_at, &bl, 4);
    /* Helpers use blr and clobber x30; keep the incoming return address. */
    a64_u32(buf, 0xF81F0FFEu); // str x30, [sp, #-16]!
    for (i32 i = start; i < end; i++) {
        buf->ovmi_off[i - start] = buf->len;
        a64_emit_one(buf, program, i, &program->code[i]);
    }
    a64_mov64(buf, X0, 0);
    a64_u32(buf, 0xF84107FEu); // ldr x30, [sp], #16
    a64_u32(buf, 0xD65F03C0u);
}

void ovm_jit_patch_arm64(ovm_emit_buf_t *buf) {
    i32 n = bh_arr_length(buf->relocs);
    for (i32 i = 0; i < n; i++) {
        ovm_reloc_t r = buf->relocs[i];
        i32 local = r.target_ovmi - buf->func_start_ovmi;
        if (local < 0 || local >= buf->ninstr) continue;
        i32 target = buf->ovmi_off[local];
        i32 delta = target - r.at;
        i32 words = delta / 4;
        u32 insn = 0;
        if (r.kind == OVM_RELOC_ARM64_B) {
            insn = 0x14000000u | ((u32) words & 0x03ffffffu);
        } else {
            insn = 0x54000000u | (((u32) words & 0x7ffffu) << 5) | (u32) r.cond;
        }
        memcpy(buf->data + r.at, &insn, 4);
    }
}

void ovm_jit_fill_arm64_jumptable(ovm_emit_buf_t *buf, u8 *exec_base) {
    (void) buf;
    (void) exec_base;
}

#else
typedef int ovm_jit_arm64_unused;
#endif
