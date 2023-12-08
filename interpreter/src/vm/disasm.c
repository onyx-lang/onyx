//
// Disassembler
//

#include "vm.h"

enum instr_format_kind_t {
    instr_format_none,

    instr_format_rab,
    instr_format_ra,
    instr_format_a,

    instr_format_imm,

    instr_format_load,
    instr_format_store,

    instr_format_idx_arr,

    instr_format_br,
    instr_format_br_cond,
    instr_format_bri,
    instr_format_bri_cond,

    instr_format_call,
    instr_format_calli,
};

typedef struct instr_format_t {
    char *instr;
    enum instr_format_kind_t kind;
} instr_format_t;

static instr_format_t instr_formats[] = {
    { "nop",  instr_format_none },
    { "add", instr_format_rab },
    { "sub", instr_format_rab },
    { "mul", instr_format_rab },
    { "div", instr_format_rab },
    { "div_s", instr_format_rab },
    { "rem", instr_format_rab },
    { "rem_s", instr_format_rab },
    { "and", instr_format_rab },
    { "or", instr_format_rab },
    { "xor", instr_format_rab },
    { "shl", instr_format_rab },
    { "shr", instr_format_rab },
    { "sar", instr_format_rab },

    { "illegal", instr_format_none },
    { "illegal", instr_format_none },

    { "imm", instr_format_imm },
    { "mov", instr_format_ra },
    { "load", instr_format_load },
    { "store", instr_format_store },

    { "copy", instr_format_rab },
    { "fill", instr_format_rab },

    { "global_get", instr_format_ra },
    { "global_set", instr_format_ra },

    { "idx_arr", instr_format_idx_arr },

    { "lt", instr_format_rab },
    { "lt_s", instr_format_rab },
    { "le", instr_format_rab },
    { "le_s", instr_format_rab },
    { "eq", instr_format_rab },
    { "ge", instr_format_rab },
    { "ge_s", instr_format_rab },
    { "gt", instr_format_rab },
    { "gt_s", instr_format_rab },
    { "ne", instr_format_rab },

    { "param", instr_format_a },
    { "return", instr_format_a },
    { "call", instr_format_call },
    { "calli", instr_format_calli },

    { "br", instr_format_br },
    { "br_z", instr_format_br_cond },
    { "br_nz", instr_format_br_cond },

    { "bri", instr_format_bri },
    { "bri_z", instr_format_bri_cond },
    { "bri_nz", instr_format_bri_cond },

    { "clz", instr_format_ra },
    { "ctz", instr_format_ra },
    { "popcnt", instr_format_ra },
    { "rotl", instr_format_rab },
    { "rotr", instr_format_rab },

    { "abs", instr_format_ra },
    { "neg", instr_format_ra },
    { "ceil", instr_format_ra },
    { "floor", instr_format_ra },
    { "trunc", instr_format_ra },
    { "nearest", instr_format_ra },
    { "sqrt", instr_format_ra },
    { "min", instr_format_ra },
    { "max", instr_format_ra },
    { "copysign", instr_format_rab },

    { "cvt_i8", instr_format_ra },
    { "cvt_i8_s", instr_format_ra },
    { "cvt_i16", instr_format_ra },
    { "cvt_i16_s", instr_format_ra },
    { "cvt_i32", instr_format_ra },
    { "cvt_i32_s", instr_format_ra },
    { "cvt_i64", instr_format_ra },
    { "cvt_i64_s", instr_format_ra },
    { "cvt_f32", instr_format_ra },
    { "cvt_f32_s", instr_format_ra },
    { "cvt_f64", instr_format_ra },
    { "cvt_f64_s", instr_format_ra },
    { "transmute_i32", instr_format_ra },
    { "transmute_i64", instr_format_ra },
    { "transmute_f32", instr_format_ra },
    { "transmute_f64", instr_format_ra },

    { "cmpxchg", instr_format_rab },

    { "break", instr_format_none },

    { "memory_size", instr_format_none },
    { "memory_grow", instr_format_ra }
};

void ovm_disassemble(ovm_program_t *program, u32 instr_addr, bh_buffer *instr_text) {
    static char buf[256];

    ovm_instr_t *instr = &program->code[instr_addr];
    switch (OVM_INSTR_TYPE(*instr)) {
        case OVM_TYPE_I8: bh_buffer_write_string(instr_text, "i8."); break;
        case OVM_TYPE_I16: bh_buffer_write_string(instr_text, "i16."); break;
        case OVM_TYPE_I32: bh_buffer_write_string(instr_text, "i32."); break;
        case OVM_TYPE_I64: bh_buffer_write_string(instr_text, "i64."); break;
        case OVM_TYPE_F32: bh_buffer_write_string(instr_text, "f32."); break;
        case OVM_TYPE_F64: bh_buffer_write_string(instr_text, "f64."); break;
        case OVM_TYPE_V128: bh_buffer_write_string(instr_text, "v128."); break;
    }

    instr_format_t *format = &instr_formats[OVM_INSTR_INSTR(*instr)];

    bh_buffer_write_string(instr_text, format->instr);

    u32 formatted = 0;
    switch (format->kind) {
        case instr_format_rab: formatted = snprintf(buf, 255, "%%%d, %%%d, %%%d", instr->r, instr->a, instr->b); break;
        case instr_format_ra:  formatted = snprintf(buf, 255, "%%%d, %%%d", instr->r, instr->a); break;
        case instr_format_a:   formatted = snprintf(buf, 255, "%%%d", instr->a); break;

        case instr_format_imm:
            switch (OVM_INSTR_TYPE(*instr)) {
                case OVM_TYPE_I8:   formatted = snprintf(buf, 255, "%%%d, %hhd", instr->r, instr->i); break;
                case OVM_TYPE_I16:  formatted = snprintf(buf, 255, "%%%d, %hd",  instr->r, instr->i); break;
                case OVM_TYPE_I32:  formatted = snprintf(buf, 255, "%%%d, %d",   instr->r, instr->i); break;
                case OVM_TYPE_I64:  formatted = snprintf(buf, 255, "%%%d, %lld", instr->r, instr->l); break;
                case OVM_TYPE_F32:  formatted = snprintf(buf, 255, "%%%d, %f",   instr->r, instr->f); break;
                case OVM_TYPE_F64:  formatted = snprintf(buf, 255, "%%%d, %lf",  instr->r, instr->d); break;
            }
            break;

        case instr_format_load:  formatted = snprintf(buf, 255, "%%%d, [%%%d + %d]", instr->r, instr->a, instr->b); break;
        case instr_format_store: formatted = snprintf(buf, 255, "[%%%d + %d], %%%d", instr->r, instr->b, instr->a); break;

        case instr_format_idx_arr: formatted = snprintf(buf, 255, "%%%d, __global_arr_%d[%%%d]", instr->r, instr->a, instr->b); break;

        case instr_format_br:       formatted = snprintf(buf, 255, "%d", instr_addr + instr->a + 1); break;
        case instr_format_br_cond:  formatted = snprintf(buf, 255, "%d, %%%d", instr_addr + instr->a + 1, instr->b); break;
        case instr_format_bri:      formatted = snprintf(buf, 255, "ip + %%%d", instr->a); break;
        case instr_format_bri_cond: formatted = snprintf(buf, 255, "ip + %%%d, %%%d", instr->a, instr->b); break;

        case instr_format_call:
            if (instr->r >= 0) {
                formatted = snprintf(buf, 255, "%%%d, %d  <%s>", instr->r, instr->a, program->funcs[instr->a].name);
            } else {
                formatted = snprintf(buf, 255, "%d  <%s>", instr->a, program->funcs[instr->a].name);
            }
            break;

        case instr_format_calli:
            if (instr->r >= 0) {
                formatted = snprintf(buf, 255, "%%%d, %%%d", instr->r, instr->a);
            } else {
                formatted = snprintf(buf, 255, "%%%d", instr->a);
            }
            break;

        default: break;
    }

    if (formatted > 0) {
        bh_buffer_write_byte(instr_text, ' ');
        bh_buffer_append(instr_text, buf, formatted);
    }
}


