#ifndef _OVM_JIT_H
#define _OVM_JIT_H

#include "vm.h"

void ovm_engine_set_jit_enabled(ovm_engine_t *engine, b32 enabled);
void ovm_jit_compile_program(ovm_engine_t *engine, ovm_program_t *program);

typedef u64 (*ovm_jit_fn_t)(ovm_engine_t *engine, ovm_state_t *state, ovm_program_t *program);

typedef struct ovm_aot_image_t {
    u8 *text;
    i32 *entry_off;
    i32 *body_off;
    i32 nfuncs;
} ovm_aot_image_t;

void ovm_jit_set_aot_image(ovm_aot_image_t *img);
ovm_aot_image_t *ovm_jit_get_aot_image(void);
void ovm_jit_bind_aot(ovm_program_t *program, ovm_aot_image_t *img);
int  ovm_native_compile_wasm(const void *wasm, u32 wasm_len, const char *obj_path);

u64  ovm_jit_return(ovm_state_t *state, u64 val);
void ovm_jit_call(ovm_state_t *state, i32 func_idx, i32 result_vn);
void ovm_jit_calli(ovm_state_t *state, i32 func_slot, i32 result_vn);
/* Setup + copy params. Returns native_body if the caller should invoke it
   with pins already live; NULL if the call was finished here. */
ovm_jit_fn_t ovm_jit_call_fast(ovm_state_t *state, i32 func_idx, i32 result_vn);
ovm_jit_fn_t ovm_jit_calli_fast(ovm_state_t *state, i32 func_slot, i32 result_vn);
void ovm_jit_slow(ovm_state_t *state, ovm_instr_t *instr);
void ovm_jit_slow_idx(ovm_state_t *state, i32 idx);

#endif
