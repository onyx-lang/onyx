#ifndef _OVM_JIT_EMIT_H
#define _OVM_JIT_EMIT_H

#include "vm.h"
#include "ovm_jit.h"

enum ovm_reloc_kind_t {
    OVM_RELOC_ARM64_B,
    OVM_RELOC_ARM64_BCOND,
    OVM_RELOC_X64_REL32,
};

enum ovm_jit_helper_t {
    OVM_JIT_H_RETURN = 0,
    OVM_JIT_H_CALL_FAST,
    OVM_JIT_H_CALLI_FAST,
    OVM_JIT_H_SLOW,
    OVM_JIT_H_MEMMOVE,
    OVM_JIT_H_MEMSET,
    OVM_JIT_H_COUNT
};

enum ovm_helper_reloc_kind_t {
    OVM_HRELOC_X64_RIP,
    OVM_HRELOC_A64_ADRP,
    OVM_HRELOC_A64_LDR
};

typedef struct ovm_reloc_t {
    i32 at;
    i32 target_ovmi;
    i32 kind;
    i32 cond;
} ovm_reloc_t;

typedef struct ovm_helper_reloc_t {
    i32 at;
    i32 helper;
    i32 kind;
} ovm_helper_reloc_t;

typedef struct ovm_emit_buf_t {
    u8 *data;
    i32 len;
    i32 cap;
    bh_arr(ovm_reloc_t) relocs;
    bh_arr(ovm_helper_reloc_t) helper_relocs;
    i32 *ovmi_off;
    i32 ninstr;
    i32 func_start_ovmi;
    i32 has_indirect_br;
    i32 jumptable_off;
    i32 nested_off;
    i32 aot;
} ovm_emit_buf_t;

void ovm_emit_init(ovm_emit_buf_t *buf);
void ovm_emit_free(ovm_emit_buf_t *buf);
void ovm_emit_ensure(ovm_emit_buf_t *buf, i32 extra);
void ovm_emit_u8(ovm_emit_buf_t *buf, u8 b);
void ovm_emit_u32(ovm_emit_buf_t *buf, u32 w);
void ovm_emit_u64(ovm_emit_buf_t *buf, u64 w);
void ovm_emit_bytes(ovm_emit_buf_t *buf, const void *p, i32 n);
void ovm_emit_align(ovm_emit_buf_t *buf, i32 align);
void ovm_emit_reloc(ovm_emit_buf_t *buf, i32 at, i32 target_ovmi, i32 kind, i32 cond);
void ovm_emit_helper_reloc(ovm_emit_buf_t *buf, i32 at, i32 helper, i32 kind);
void ovm_emit_reset_func(ovm_emit_buf_t *buf, i32 start, i32 end);

i32  ovm_func_code_end(ovm_program_t *program, i32 func_idx);
b32  ovm_jit_should_compile(ovm_program_t *program, i32 start, i32 end);
/* Emit all compile-worthy functions. func_off[i] = text offset or -1;
   body_off[i] = offset of native_body from that function's entry. */
b32  ovm_jit_emit_functions(ovm_program_t *program, ovm_emit_buf_t *buf, i32 *func_off, i32 *body_off);

#if defined(__aarch64__) || defined(__arm64__)
void ovm_jit_emit_arm64(ovm_emit_buf_t *buf, ovm_program_t *program, i32 start, i32 end);
void ovm_jit_patch_arm64(ovm_emit_buf_t *buf);
void ovm_jit_fill_arm64_jumptable(ovm_emit_buf_t *buf, u8 *exec_base);
#endif

#if defined(__x86_64__)
void ovm_jit_emit_x64(ovm_emit_buf_t *buf, ovm_program_t *program, i32 start, i32 end);
void ovm_jit_patch_x64(ovm_emit_buf_t *buf);
void ovm_jit_fill_x64_jumptable(ovm_emit_buf_t *buf, u8 *exec_base);
#endif

#endif
