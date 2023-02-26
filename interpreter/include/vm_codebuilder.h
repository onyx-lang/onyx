#ifndef _OVM_CODE_BUILDER_H
#define _OVM_CODE_BUILDER_H

#include "vm.h"
#include "ovm_debug.h"

typedef struct ovm_code_builder_t ovm_code_builder_t;
typedef struct label_target_t label_target_t;
typedef struct branch_patch_t branch_patch_t;
typedef enum label_kind_t label_kind_t;

//
// A new code builder will be "made" for each function
// being compiled.
struct ovm_code_builder_t {
    bh_arr(i32) execution_stack;

    i32 next_label_idx;
    bh_arr(label_target_t) label_stack;
    bh_arr(branch_patch_t) branch_patches;

    i32 param_count, result_count, local_count;

    ovm_program_t *program;
    i32 start_instr;

    i32 func_table_arr_idx;
    i32 highest_value_number;

    debug_info_builder_t *debug_builder;
};

enum label_kind_t {
    label_kind_func,
    label_kind_block,
    label_kind_loop,
    label_kind_if,
};

struct label_target_t {
    i32 idx;
    label_kind_t kind;
    i32 instr;

    i32 values_on_stack_before_block;
};

enum branch_patch_kind_t {
    branch_patch_instr_a,    // For patching the '.a' register of a branch instruction.
    branch_patch_static_idx, // For patching an integer in the static integers section.
};

struct branch_patch_t {
    enum branch_patch_kind_t kind;
    i32 branch_instr;
    i32 label_idx;
    i32 static_arr;
    i32 static_idx;
    bool targets_else;
};

ovm_code_builder_t ovm_code_builder_new(ovm_program_t *program, debug_info_builder_t *debug, i32 param_count, i32 result_count, i32 local_count);
label_target_t     ovm_code_builder_wasm_target_idx(ovm_code_builder_t *builder, i32 idx);
i32                ovm_code_builder_push_label_target(ovm_code_builder_t *builder, label_kind_t kind);
void               ovm_code_builder_pop_label_target(ovm_code_builder_t *builder);
void               ovm_code_builder_patch_else(ovm_code_builder_t *builder, label_target_t if_target);
void               ovm_code_builder_free(ovm_code_builder_t *builder);
void               ovm_code_builder_add_nop(ovm_code_builder_t *builder);
void               ovm_code_builder_add_break(ovm_code_builder_t *builder);
void               ovm_code_builder_add_binop(ovm_code_builder_t *builder, u32 instr);
void               ovm_code_builder_add_unop(ovm_code_builder_t *builder, u32 instr);
void               ovm_code_builder_add_imm(ovm_code_builder_t *builder, u32 ovm_type, void *imm);
void               ovm_code_builder_add_branch(ovm_code_builder_t *builder, i32 label_idx);
void               ovm_code_builder_add_cond_branch(ovm_code_builder_t *builder, i32 label_idx, bool branch_if_true, bool targets_else);
void               ovm_code_builder_add_branch_table(ovm_code_builder_t *builder, i32 count, i32 *label_indicies, i32 default_label_idx);
void               ovm_code_builder_add_return(ovm_code_builder_t *builder);
void               ovm_code_builder_add_call(ovm_code_builder_t *builder, i32 func_idx, i32 param_count, bool has_return_value);
void               ovm_code_builder_add_indirect_call(ovm_code_builder_t *builder, i32 param_count, bool has_return_value);
void               ovm_code_builder_drop_value(ovm_code_builder_t *builder);
void               ovm_code_builder_add_local_get(ovm_code_builder_t *builder, i32 local_idx);
void               ovm_code_builder_add_local_set(ovm_code_builder_t *builder, i32 local_idx);
void               ovm_code_builder_add_local_tee(ovm_code_builder_t *builder, i32 local_idx);
void               ovm_code_builder_add_register_get(ovm_code_builder_t *builder, i32 local_idx);
void               ovm_code_builder_add_register_set(ovm_code_builder_t *builder, i32 local_idx);
void               ovm_code_builder_add_load(ovm_code_builder_t *builder, u32 ovm_type, i32 offset);
void               ovm_code_builder_add_store(ovm_code_builder_t *builder, u32 ovm_type, i32 offset);
void               ovm_code_builder_add_atomic_load(ovm_code_builder_t *builder, u32 ovm_type, i32 offset);
void               ovm_code_builder_add_atomic_store(ovm_code_builder_t *builder, u32 ovm_type, i32 offset);
void               ovm_code_builder_add_cmpxchg(ovm_code_builder_t *builder, u32 ovm_type, i32 offset);
void               ovm_code_builder_add_memory_copy(ovm_code_builder_t *builder);
void               ovm_code_builder_add_memory_fill(ovm_code_builder_t *builder);
void               ovm_code_builder_add_memory_size(ovm_code_builder_t *builder);
void               ovm_code_builder_add_memory_grow(ovm_code_builder_t *builder);

#endif
