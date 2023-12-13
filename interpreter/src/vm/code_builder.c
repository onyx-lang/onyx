
#include "vm_codebuilder.h"
#include "ovm_debug.h"

// #define BUILDER_DEBUG

#if defined(BUILDER_DEBUG)
    #define POP_VALUE(b)     (bh_arr_length((b)->execution_stack) == 0 ? (assert(0 && "invalid value pop"), 0) : bh_arr_pop((b)->execution_stack))
#else
    #define POP_VALUE(b) bh_arr_pop((b)->execution_stack)
#endif

#define PUSH_VALUE(b, r) (bh_arr_push((b)->execution_stack, r))

#define LAST_VALUE(b) bh_arr_last((b)->execution_stack)

#define IS_TEMPORARY_VALUE(b, r) (r >= (b->param_count + b->local_count))

static inline int NEXT_VALUE(ovm_code_builder_t *b) {
#if defined(BUILDER_DEBUG)
    b->highest_value_number += 1;
    return b->highest_value_number - 1;

#else
    if (bh_arr_length(b->execution_stack) == 0) {
        return b->param_count + b->local_count;
    }

    i32 max = b->param_count + b->local_count;
    bh_arr_each(i32, reg, b->execution_stack) {
        max = bh_max(*reg, max);
    }

    b->highest_value_number = bh_max(b->highest_value_number, max + 1);
    return max + 1;
#endif
}

ovm_code_builder_t ovm_code_builder_new(ovm_program_t *program, debug_info_builder_t *debug, i32 param_count, i32 result_count, i32 local_count) {
    ovm_code_builder_t builder;
    builder.param_count = param_count;
    builder.result_count = result_count;
    builder.local_count = local_count;
    builder.start_instr = bh_arr_length(program->code);
    builder.program = program;

    builder.execution_stack = NULL;
    bh_arr_new(bh_heap_allocator(), builder.execution_stack, 32);

    builder.next_label_idx = 0;
    builder.label_stack = NULL;
    builder.branch_patches = NULL;
    bh_arr_new(bh_heap_allocator(), builder.label_stack, 32);
    bh_arr_new(bh_heap_allocator(), builder.branch_patches, 32);

    builder.highest_value_number = param_count + local_count;

    builder.debug_builder = debug;

    return builder;
}

void ovm_code_builder_free(ovm_code_builder_t *builder) {
    bh_arr_free(builder->execution_stack);
    bh_arr_free(builder->label_stack);
    bh_arr_free(builder->branch_patches);
}

label_target_t ovm_code_builder_wasm_target_idx(ovm_code_builder_t *builder, i32 idx) {
    i32 walker = bh_arr_length(builder->label_stack) - 1 - idx;
    assert(walker >= 0);
    return builder->label_stack[walker];
}

i32 ovm_code_builder_push_label_target(ovm_code_builder_t *builder, label_kind_t kind) {
    label_target_t target;
    target.kind = kind;
    target.idx = builder->next_label_idx++;
    target.instr = -1;
    target.values_on_stack_before_block = bh_arr_length(builder->execution_stack);

    if (kind == label_kind_loop) {
        target.instr = bh_arr_length(builder->program->code);
    }

    bh_arr_push(builder->label_stack, target);

    return target.idx;
}

void ovm_code_builder_pop_label_target(ovm_code_builder_t *builder) {
    label_target_t target = bh_arr_pop(builder->label_stack);
    if (target.instr == -1) {
        target.instr = bh_arr_length(builder->program->code);
    }

    fori (i, 0, bh_arr_length(builder->branch_patches)) {
        branch_patch_t patch = builder->branch_patches[i];
        if (patch.label_idx != target.idx) continue;

        int br_delta = target.instr - patch.branch_instr - 1;

        switch (patch.kind) {
            case branch_patch_instr_a:
                builder->program->code[patch.branch_instr].a = br_delta;
                break;

            case branch_patch_static_idx:
                ovm_program_modify_static_int(builder->program, patch.static_arr, patch.static_idx, br_delta);
                break;
        }

        bh_arr_fastdelete(builder->branch_patches, i);
        i--;
    }

    // i32 values_on_stack = bh_arr_length(builder->execution_stack);
    // if (values_on_stack > target.values_on_stack_before_block) {
    //     bh_arr_set_length(builder->execution_stack, target.values_on_stack_before_block);
    // }
}

void ovm_code_builder_patch_else(ovm_code_builder_t *builder, label_target_t if_target) {
    assert(if_target.kind == label_kind_if);

    fori (i, 0, bh_arr_length(builder->branch_patches)) {
        branch_patch_t patch = builder->branch_patches[i];
        if (patch.label_idx != if_target.idx) continue;
        if (!patch.targets_else) continue;

        int br_delta = bh_arr_length(builder->program->code) - patch.branch_instr - 1;
        assert(patch.kind == branch_patch_instr_a);

        builder->program->code[patch.branch_instr].a = br_delta;

        bh_arr_fastdelete(builder->branch_patches, i);
        return;
    }
}

void ovm_code_builder_add_nop(ovm_code_builder_t *builder) {
    //
    // If debugging info was not present in the binary,
    // do not create NOP instructions. NOP instructions are
    // normally used as placeholders / special instructions
    // to signify something weird happened, such as an expanded
    // macro or operator overload. When this happens, it is
    // nice to have the correct location in the source tree
    // so you don't magically jump from one place to another.
    // HOWEVER, if there is no debug info, then this is not
    // a concern, and NOPs can be ommited.
    if (builder->debug_builder->data == NULL) return;

    ovm_instr_t nop = {0};
    nop.full_instr = OVMI_NOP;

    debug_info_builder_emit_location(builder->debug_builder);
    ovm_program_add_instructions(builder->program, 1, &nop);
}

void ovm_code_builder_add_break(ovm_code_builder_t *builder) {
    ovm_instr_t break_ = {0};
    break_.full_instr = OVM_TYPED_INSTR(OVMI_BREAK, OVM_TYPE_NONE);
    debug_info_builder_emit_location(builder->debug_builder);
    ovm_program_add_instructions(builder->program, 1, &break_);
}

void ovm_code_builder_add_binop(ovm_code_builder_t *builder, u32 instr) {
    i32 right  = POP_VALUE(builder);
    i32 left   = POP_VALUE(builder);
    i32 result = NEXT_VALUE(builder);

    ovm_instr_t binop;
    binop.full_instr = instr;
    binop.r = result;
    binop.a = left;
    binop.b = right;

    debug_info_builder_emit_location(builder->debug_builder);
    ovm_program_add_instructions(builder->program, 1, &binop);
    PUSH_VALUE(builder, result);
}

void ovm_code_builder_add_imm(ovm_code_builder_t *builder, u32 ovm_type, void *imm) {
    ovm_instr_t imm_instr = {0};
    imm_instr.full_instr = OVM_TYPED_INSTR(OVMI_IMM, ovm_type);
    imm_instr.r = NEXT_VALUE(builder);

    switch (ovm_type) {
        case OVM_TYPE_I8:  imm_instr.i = (u32) *(u8 *) imm; break;
        case OVM_TYPE_I16: imm_instr.i = (u32) *(u16 *) imm; break;
        case OVM_TYPE_I32: imm_instr.i =       *(u32 *) imm; break;
        case OVM_TYPE_I64: imm_instr.l =       *(u64 *) imm; break;
        case OVM_TYPE_F32: imm_instr.f =       *(f32 *) imm; break;
        case OVM_TYPE_F64: imm_instr.d =       *(f64 *) imm; break;
        default: assert(0 && "bad ovm type for add_imm");
    }

    debug_info_builder_emit_location(builder->debug_builder);
    ovm_program_add_instructions(builder->program, 1, &imm_instr);
    PUSH_VALUE(builder, imm_instr.r);
}

void ovm_code_builder_add_unop(ovm_code_builder_t *builder, u32 instr) {
    i32 operand = POP_VALUE(builder);

    ovm_instr_t unop = {0};
    unop.full_instr = instr;
    unop.r = NEXT_VALUE(builder);
    unop.a = operand;

    debug_info_builder_emit_location(builder->debug_builder);
    ovm_program_add_instructions(builder->program, 1, &unop);
    PUSH_VALUE(builder, unop.r);
}

void ovm_code_builder_add_branch(ovm_code_builder_t *builder, i32 label_idx) {
    ovm_instr_t branch_instr = {0};
    branch_instr.full_instr = OVM_TYPED_INSTR(OVMI_BR, OVM_TYPE_NONE);
    branch_instr.a = -1;

    branch_patch_t patch;
    patch.kind = branch_patch_instr_a;
    patch.branch_instr = bh_arr_length(builder->program->code);
    patch.label_idx = label_idx;
    patch.targets_else = false;

    bh_arr_push(builder->branch_patches, patch);

    debug_info_builder_emit_location(builder->debug_builder);
    ovm_program_add_instructions(builder->program, 1, &branch_instr);
}

void ovm_code_builder_add_cond_branch(ovm_code_builder_t *builder, i32 label_idx, bool branch_if_true, bool targets_else) {
    ovm_instr_t branch_instr = {0};
    if (branch_if_true) {
        branch_instr.full_instr = OVM_TYPED_INSTR(OVMI_BR_NZ, OVM_TYPE_NONE);
    } else {
        branch_instr.full_instr = OVM_TYPED_INSTR(OVMI_BR_Z, OVM_TYPE_NONE);
    }

    branch_instr.a = -1;
    branch_instr.b = POP_VALUE(builder);

    branch_patch_t patch;
    patch.kind = branch_patch_instr_a;
    patch.branch_instr = bh_arr_length(builder->program->code);
    patch.label_idx = label_idx;
    patch.targets_else = targets_else;

    bh_arr_push(builder->branch_patches, patch);

    debug_info_builder_emit_location(builder->debug_builder);
    ovm_program_add_instructions(builder->program, 1, &branch_instr);
}

void ovm_code_builder_add_branch_table(ovm_code_builder_t *builder, i32 count, i32 *label_indicies, i32 default_label_idx) {
    //
    // Passing label indicies here is a little disingenuous, because that is not
    // what the data will have to be. But since it is already the correct length
    // I am using it as a substitute.
    int table_idx = ovm_program_register_static_ints(builder->program, count, label_indicies);

    ovm_instr_t instrs[5] = {0};
    int tmp_register = NEXT_VALUE(builder);
    int index_register = POP_VALUE(builder);
    PUSH_VALUE(builder, tmp_register);

    instrs[0].full_instr = OVM_TYPED_INSTR(OVMI_IMM, OVM_TYPE_I32);
    instrs[0].r = tmp_register;
    instrs[0].i = count;

    instrs[1].full_instr = OVM_TYPED_INSTR(OVMI_LT, OVM_TYPE_I32);
    instrs[1].r = tmp_register;
    instrs[1].a = index_register;
    instrs[1].b = tmp_register;

    instrs[2].full_instr = OVM_TYPED_INSTR(OVMI_BR_Z, OVM_TYPE_NONE);
    instrs[2].a = -1;
    instrs[2].b = tmp_register;

    instrs[3].full_instr = OVM_TYPED_INSTR(OVMI_IDX_ARR, OVM_TYPE_NONE);
    instrs[3].r = tmp_register;
    instrs[3].a = table_idx;
    instrs[3].b = index_register;

    instrs[4].full_instr = OVM_TYPED_INSTR(OVMI_BRI, OVM_TYPE_NONE);
    instrs[4].a = tmp_register;

    POP_VALUE(builder);

    fori (i, 0, count) {
        branch_patch_t patch;
        patch.kind = branch_patch_static_idx;
        patch.branch_instr = bh_arr_length(builder->program->code) + 4;
        patch.label_idx = label_indicies[i];
        patch.static_arr = table_idx;
        patch.static_idx = i;
        patch.targets_else = false;
        bh_arr_push(builder->branch_patches, patch);
    }

    branch_patch_t default_patch;
    default_patch.kind = branch_patch_instr_a;
    default_patch.branch_instr = bh_arr_length(builder->program->code) + 2;
    default_patch.label_idx = default_label_idx;
    default_patch.targets_else = false;
    bh_arr_push(builder->branch_patches, default_patch);

    debug_info_builder_emit_location(builder->debug_builder);
    debug_info_builder_emit_location(builder->debug_builder);
    debug_info_builder_emit_location(builder->debug_builder);
    debug_info_builder_emit_location(builder->debug_builder);
    debug_info_builder_emit_location(builder->debug_builder);
    ovm_program_add_instructions(builder->program, 5, instrs);
}

void ovm_code_builder_add_return(ovm_code_builder_t *builder) {
    ovm_instr_t instr = {0};
    instr.full_instr = OVM_TYPED_INSTR(OVMI_RETURN, OVM_TYPE_NONE);

    i32 values_on_stack = bh_arr_length(builder->execution_stack);
    if (values_on_stack > 0 && builder->result_count > 0) {
        instr.a = POP_VALUE(builder);
    }

    debug_info_builder_emit_location(builder->debug_builder);
    ovm_program_add_instructions(builder->program, 1, &instr);
}

static void ovm_code_builder_add_params(ovm_code_builder_t *builder, i32 param_count) {
    i32 *flipped_params = alloca(param_count * sizeof(i32));

    fori (i, 0, param_count) {
        flipped_params[i] = POP_VALUE(builder);
    }

    fori (i, 0, param_count) {
        ovm_instr_t param_instr = {0};
        param_instr.full_instr = OVM_TYPED_INSTR(OVMI_PARAM, OVM_TYPE_NONE);
        param_instr.a = flipped_params[param_count - 1 - i];

        debug_info_builder_emit_location(builder->debug_builder);
        ovm_program_add_instructions(builder->program, 1, &param_instr);
    }
}

void ovm_code_builder_add_call(ovm_code_builder_t *builder, i32 func_idx, i32 param_count, bool has_return_value) {
    ovm_code_builder_add_params(builder, param_count);

    ovm_instr_t call_instr = {0};
    call_instr.full_instr = OVM_TYPED_INSTR(OVMI_CALL, OVM_TYPE_NONE);
    call_instr.a = func_idx;
    call_instr.r = -1;

    if (has_return_value) {
        call_instr.r = NEXT_VALUE(builder);
    }

    debug_info_builder_emit_location(builder->debug_builder);
    ovm_program_add_instructions(builder->program, 1, &call_instr);

    if (has_return_value) {
        PUSH_VALUE(builder, call_instr.r);
    }
}

void ovm_code_builder_add_indirect_call(ovm_code_builder_t *builder, i32 param_count, bool has_return_value) {
    ovm_instr_t call_instrs[2] = {0};

    // idxarr %k, table, %j
    call_instrs[0].full_instr = OVM_TYPED_INSTR(OVMI_IDX_ARR, OVM_TYPE_NONE);
    call_instrs[0].r = NEXT_VALUE(builder);
    call_instrs[0].a = builder->func_table_arr_idx;
    call_instrs[0].b = POP_VALUE(builder);

    call_instrs[1].full_instr = OVM_TYPED_INSTR(OVMI_CALLI, OVM_TYPE_NONE);
    call_instrs[1].a = call_instrs[0].r;
    call_instrs[1].r = -1;

    ovm_code_builder_add_params(builder, param_count);

    if (has_return_value) {
        call_instrs[1].r = NEXT_VALUE(builder);
    }

    debug_info_builder_emit_location(builder->debug_builder);
    debug_info_builder_emit_location(builder->debug_builder);
    ovm_program_add_instructions(builder->program, 2, call_instrs);

    if (has_return_value) {
        PUSH_VALUE(builder, call_instrs[1].r);
    }
}

void ovm_code_builder_drop_value(ovm_code_builder_t *builder) {
    POP_VALUE(builder);
}

void ovm_code_builder_add_local_get(ovm_code_builder_t *builder, i32 local_idx) {
    PUSH_VALUE(builder, local_idx);
}

static void maybe_copy_register_if_going_to_be_replaced(ovm_code_builder_t *builder, i32 local_idx) {
    b32 need_to_copy = 0;
    bh_arr_each(i32, reg, builder->execution_stack) {
        if (*reg == local_idx) {
            need_to_copy = 1;
            break;
        }
    }

    if (need_to_copy) {
        i32 new_register = NEXT_VALUE(builder);
        assert(IS_TEMPORARY_VALUE(builder, new_register));
        ovm_instr_t instr = {0};
        instr.full_instr = OVM_TYPED_INSTR(OVMI_MOV, OVM_TYPE_NONE);
        instr.r = new_register;
        instr.a = local_idx;

        debug_info_builder_emit_location(builder->debug_builder);
        ovm_program_add_instructions(builder->program, 1, &instr);

        bh_arr_each(i32, reg, builder->execution_stack) {
            if (*reg == local_idx) {
                *reg = new_register;
            }
        }
    }
}

void ovm_code_builder_add_local_set(ovm_code_builder_t *builder, i32 local_idx) {
    maybe_copy_register_if_going_to_be_replaced(builder, local_idx);

    // :PrimitiveOptimization
    ovm_instr_t *last_instr = &bh_arr_last(builder->program->code);
    if (IS_TEMPORARY_VALUE(builder, last_instr->r) && last_instr->r == LAST_VALUE(builder)) {
        last_instr->r = local_idx;
        POP_VALUE(builder);
        return;
    }

    ovm_instr_t instr = {0};
    instr.full_instr = OVM_TYPED_INSTR(OVMI_MOV, OVM_TYPE_NONE);
    instr.r = local_idx; // This makes the assumption that the params will be in
                         // the lower "address space" of the value numbers. This
                         // will be true for web assembly, because that's how it
                         // it was spec'd; but in the future for other things,
                         // this will be incorrect.
    instr.a = POP_VALUE(builder);

    debug_info_builder_emit_location(builder->debug_builder);
    ovm_program_add_instructions(builder->program, 1, &instr);
}

void ovm_code_builder_add_local_tee(ovm_code_builder_t *builder, i32 local_idx) {
    ovm_code_builder_add_local_set(builder, local_idx);
    PUSH_VALUE(builder, local_idx);
}

void ovm_code_builder_add_register_get(ovm_code_builder_t *builder, i32 reg_idx) {
    ovm_instr_t instr = {0};
    instr.full_instr = OVM_TYPED_INSTR(OVMI_REG_GET, OVM_TYPE_NONE);
    instr.r = NEXT_VALUE(builder);
    instr.a = reg_idx;

    debug_info_builder_emit_location(builder->debug_builder);
    ovm_program_add_instructions(builder->program, 1, &instr);

    PUSH_VALUE(builder, instr.r);
}

void ovm_code_builder_add_register_set(ovm_code_builder_t *builder, i32 reg_idx) {
    // :PrimitiveOptimization
    {
        ovm_instr_t *last_instr = &bh_arr_last(builder->program->code);
        if (OVM_INSTR_INSTR(*last_instr) == OVMI_MOV) {
            if (IS_TEMPORARY_VALUE(builder, last_instr->r) && last_instr->r == LAST_VALUE(builder)) {

                last_instr->full_instr = OVM_TYPED_INSTR(OVMI_REG_SET, OVM_TYPE_NONE);
                last_instr->r = reg_idx;

                POP_VALUE(builder);
                return;
            }
        }
    }

    ovm_instr_t instr = {0};
    instr.full_instr = OVM_TYPED_INSTR(OVMI_REG_SET, OVM_TYPE_NONE);
    instr.r = reg_idx;
    instr.a = POP_VALUE(builder);

    debug_info_builder_emit_location(builder->debug_builder);
    ovm_program_add_instructions(builder->program, 1, &instr);
}

void ovm_code_builder_add_load(ovm_code_builder_t *builder, u32 ovm_type, i32 offset) {
    ovm_instr_t load_instr = {0};
    load_instr.full_instr = OVM_TYPED_INSTR(OVMI_LOAD, ovm_type);
    load_instr.b = offset;
    load_instr.a = POP_VALUE(builder);
    load_instr.r = NEXT_VALUE(builder);

    debug_info_builder_emit_location(builder->debug_builder);
    ovm_program_add_instructions(builder->program, 1, &load_instr);

    PUSH_VALUE(builder, load_instr.r);
}

void ovm_code_builder_add_store(ovm_code_builder_t *builder, u32 ovm_type, i32 offset) {
    ovm_instr_t store_instr = {0};
    store_instr.full_instr = OVM_TYPED_INSTR(OVMI_STORE, ovm_type);
    store_instr.b = offset;
    store_instr.a = POP_VALUE(builder);
    store_instr.r = POP_VALUE(builder);

    debug_info_builder_emit_location(builder->debug_builder);
    ovm_program_add_instructions(builder->program, 1, &store_instr);
    return;
}

void ovm_code_builder_add_cmpxchg(ovm_code_builder_t *builder, u32 ovm_type, i32 offset) {
    // This is an optimization for a 0 offset that could come back, but since CMPXCHG is
    // not a common instruction (I think it is used in one place in the entire standard library),
    // it does not make much of a difference.
    //
    // if (offset == 0) {
    //     ovm_instr_t cmpxchg_instr = {0};
    //     cmpxchg_instr.full_instr = OVMI_ATOMIC | OVM_TYPED_INSTR(OVMI_CMPXCHG, ovm_type);
    //     cmpxchg_instr.b = POP_VALUE(builder);
    //     cmpxchg_instr.a = POP_VALUE(builder);
    //     cmpxchg_instr.r = POP_VALUE(builder);

    //     debug_info_builder_emit_location(builder->debug_builder);
    //     ovm_program_add_instructions(builder->program, 1, &cmpxchg_instr);

    //     PUSH_VALUE(builder, cmpxchg_instr.r);
    //     return;
    // }

    ovm_instr_t instrs[3] = {0};
    // imm.i32 %n, offset
    instrs[0].full_instr = OVM_TYPED_INSTR(OVMI_IMM, OVM_TYPE_I32);
    instrs[0].i = offset;
    instrs[0].r = NEXT_VALUE(builder);

    int value_reg = POP_VALUE(builder);
    int expected_reg = POP_VALUE(builder);
    int addr_reg = POP_VALUE(builder);

    // add.i32 %n, %n, %i
    instrs[1].full_instr = OVM_TYPED_INSTR(OVMI_ADD, OVM_TYPE_I32);
    instrs[1].r = instrs[0].r;
    instrs[1].a = addr_reg;
    instrs[1].b = instrs[0].r;

    // cmpxchg.x %n, %e, %v
    instrs[2].full_instr = OVMI_ATOMIC | OVM_TYPED_INSTR(OVMI_CMPXCHG, ovm_type);
    instrs[2].r = instrs[1].r;
    instrs[2].a = expected_reg;
    instrs[2].b = value_reg;

    debug_info_builder_emit_location(builder->debug_builder);
    debug_info_builder_emit_location(builder->debug_builder);
    debug_info_builder_emit_location(builder->debug_builder);
    ovm_program_add_instructions(builder->program, 3, instrs);

    PUSH_VALUE(builder, instrs[2].r);
}

void ovm_code_builder_add_memory_size(ovm_code_builder_t *builder) {
    ovm_instr_t instr = {0};
    instr.full_instr = OVM_TYPED_INSTR(OVMI_MEM_SIZE, OVM_TYPE_NONE);
    instr.r = NEXT_VALUE(builder);

    debug_info_builder_emit_location(builder->debug_builder);
    ovm_program_add_instructions(builder->program, 1, &instr);

    PUSH_VALUE(builder, instr.r);
}

void ovm_code_builder_add_memory_grow(ovm_code_builder_t *builder) {
    ovm_instr_t instr = {0};
    instr.full_instr = OVM_TYPED_INSTR(OVMI_MEM_GROW, OVM_TYPE_NONE);
    instr.a = POP_VALUE(builder);
    instr.r = NEXT_VALUE(builder);

    debug_info_builder_emit_location(builder->debug_builder);
    ovm_program_add_instructions(builder->program, 1, &instr);

    PUSH_VALUE(builder, instr.r);
}

void ovm_code_builder_add_memory_copy(ovm_code_builder_t *builder) {
    ovm_instr_t instr = {0};
    instr.full_instr = OVM_TYPED_INSTR(OVMI_COPY, OVM_TYPE_NONE);
    instr.b = POP_VALUE(builder);
    instr.a = POP_VALUE(builder);
    instr.r = POP_VALUE(builder);

    debug_info_builder_emit_location(builder->debug_builder);
    ovm_program_add_instructions(builder->program, 1, &instr);
}

void ovm_code_builder_add_memory_fill(ovm_code_builder_t *builder) {
    ovm_instr_t instr = {0};
    instr.full_instr = OVM_TYPED_INSTR(OVMI_FILL, OVM_TYPE_NONE);
    instr.b = POP_VALUE(builder);
    instr.a = POP_VALUE(builder);
    instr.r = POP_VALUE(builder);

    debug_info_builder_emit_location(builder->debug_builder);
    ovm_program_add_instructions(builder->program, 1, &instr);
}

//
// CopyNPaste from _add_load
void ovm_code_builder_add_atomic_load(ovm_code_builder_t *builder, u32 ovm_type, i32 offset) {
    ovm_instr_t load_instr = {0};
    load_instr.full_instr = OVMI_ATOMIC | OVM_TYPED_INSTR(OVMI_LOAD, ovm_type);
    load_instr.b = offset;
    load_instr.a = POP_VALUE(builder);
    load_instr.r = NEXT_VALUE(builder);

    debug_info_builder_emit_location(builder->debug_builder);
    ovm_program_add_instructions(builder->program, 1, &load_instr);

    PUSH_VALUE(builder, load_instr.r);
}

//
// CopyNPaste from _add_store
void ovm_code_builder_add_atomic_store(ovm_code_builder_t *builder, u32 ovm_type, i32 offset) {
    ovm_instr_t store_instr = {0};
    store_instr.full_instr = OVMI_ATOMIC | OVM_TYPED_INSTR(OVMI_STORE, ovm_type);
    store_instr.b = offset;
    store_instr.a = POP_VALUE(builder);
    store_instr.r = POP_VALUE(builder);

    debug_info_builder_emit_location(builder->debug_builder);
    ovm_program_add_instructions(builder->program, 1, &store_instr);
}
