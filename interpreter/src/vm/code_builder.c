
#include "vm_codebuilder.h"
#include "ovm_debug.h"

// #define BUILDER_DEBUG

#define PUSH_VALUE(b, r) (bh_arr_push((b)->execution_stack, r))

#define LAST_VALUE(b) bh_arr_last((b)->execution_stack)

#define IS_TEMPORARY_VALUE(b, r) (r >= (b->param_count + b->local_count))

static i32 pop_value(ovm_code_builder_t *b) {
    if (bh_arr_length(b->execution_stack) > 0) return bh_arr_pop(b->execution_stack);

    // Unreachable wasm is polymorphic: later drops and arithmetic still parse.
    assert(!b->reachable);
    if (b->unreachable_sink < 0) {
        b->unreachable_sink = b->param_count + b->local_count;
        b->highest_value_number = bh_max(b->highest_value_number, b->unreachable_sink);
    }
    return b->unreachable_sink;
}

#if defined(BUILDER_DEBUG)
    #define POP_VALUE(b)     (bh_arr_length((b)->execution_stack) == 0 ? pop_value(b) : bh_arr_pop((b)->execution_stack))
#else
    #define POP_VALUE(b) pop_value(b)
#endif

static inline int NEXT_VALUE(ovm_code_builder_t *b) {
#if defined(BUILDER_DEBUG)
    b->highest_value_number += 1;
    return b->highest_value_number - 1;

#else
    // Locals occupy [0, param_count + local_count). The next temporary is one
    // past every register still live on the stack or pinned by an open block.
    i32 max = b->param_count + b->local_count - 1;
    bh_arr_each(i32, reg, b->execution_stack) {
        max = bh_max(*reg, max);
    }

    fori (li, 0, bh_arr_length(b->label_stack)) {
        label_target_t *label = &b->label_stack[li];
        fori (i, 0, label->param_count) max = bh_max(max, label->param_regs[i]);
        fori (i, 0, label->result_count) max = bh_max(max, label->result_regs[i]);
    }

    i32 next = max + 1;
    b->highest_value_number = bh_max(b->highest_value_number, next);
    return next;
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
    builder.reachable = 1;
    builder.unreachable_sink = -1;

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

static void emit_raw(ovm_code_builder_t *builder, ovm_instr_t instr) {
    debug_info_builder_emit_location(builder->debug_builder);
    ovm_program_add_instructions(builder->program, 1, &instr);
}

static void emit_mov(ovm_code_builder_t *builder, i32 dst, i32 src) {
    if (dst == src) return;
    ovm_instr_t instr = {0};
    instr.full_instr = OVM_TYPED_INSTR(OVMI_MOV, OVM_TYPE_NONE);
    instr.r = dst;
    instr.a = src;
    emit_raw(builder, instr);
}

static label_target_t *label_from_idx(ovm_code_builder_t *builder, i32 idx) {
    fori (i, 0, bh_arr_length(builder->label_stack)) {
        if (builder->label_stack[i].idx == idx) return &builder->label_stack[i];
    }
    assert(0 && "unknown label");
    return NULL;
}

static i32 branch_arity(const label_target_t *label) {
    if (label->kind == label_kind_loop) return label->param_count;
    if (label->kind == label_kind_func) return 0;
    return label->result_count;
}

static i32 *branch_dest(label_target_t *label) {
    if (label->kind == label_kind_loop) return label->param_regs;
    return label->result_regs;
}

// Copy src into dest. A destination that is also another source is staged
// through a temporary so the values cannot clobber each other.
static void emit_safe_moves(ovm_code_builder_t *builder, const i32 *dest, const i32 *src, i32 n) {
    b32 clash = 0;
    fori (i, 0, n) {
        if (dest[i] == src[i]) continue;
        fori (j, 0, n) {
            if (i != j && dest[i] == src[j]) clash = 1;
        }
    }

    if (!clash) {
        fori (i, 0, n) emit_mov(builder, dest[i], src[i]);
        return;
    }

    i32 *tmps = alloca(n * sizeof(i32));
    fori (i, 0, n) {
        tmps[i] = NEXT_VALUE(builder);
        PUSH_VALUE(builder, tmps[i]);
    }
    fori (i, 0, n) emit_mov(builder, tmps[i], src[i]);
    fori (i, 0, n) emit_mov(builder, dest[i], tmps[i]);
    fori (i, 0, n) POP_VALUE(builder);
}

static i32 planned_move_count(const i32 *dest, const i32 *src, i32 n) {
    b32 clash = 0;
    i32 count = 0;
    fori (i, 0, n) {
        if (dest[i] == src[i]) continue;
        count++;
        fori (j, 0, n) {
            if (i != j && dest[i] == src[j]) clash = 1;
        }
    }
    return clash ? n * 2 : count;
}

static void read_top_values(ovm_code_builder_t *builder, i32 n, i32 *out) {
    i32 len = bh_arr_length(builder->execution_stack);
    assert(len >= n);
    fori (i, 0, n) out[i] = builder->execution_stack[len - n + i];
}

static void emit_branch_instr(ovm_code_builder_t *builder, i32 label_idx) {
    ovm_instr_t branch_instr = {0};
    branch_instr.full_instr = OVM_TYPED_INSTR(OVMI_BR, OVM_TYPE_NONE);
    branch_instr.a = -1;

    branch_patch_t patch;
    memset(&patch, 0, sizeof(patch));
    patch.kind = branch_patch_instr_a;
    patch.branch_instr = bh_arr_length(builder->program->code);
    patch.label_idx = label_idx;

    bh_arr_push(builder->branch_patches, patch);
    emit_raw(builder, branch_instr);
}

static void move_branch_values(ovm_code_builder_t *builder, label_target_t *label) {
    i32 arity = branch_arity(label);
    if (arity == 0 || !builder->reachable) return;

    i32 *src = alloca(arity * sizeof(i32));
    read_top_values(builder, arity, src);
    emit_safe_moves(builder, branch_dest(label), src, arity);
}

i32 ovm_code_builder_push_label_target(ovm_code_builder_t *builder, label_kind_t kind) {
    label_target_t target;
    memset(&target, 0, sizeof(target));
    target.kind = kind;
    target.idx = builder->next_label_idx++;
    target.instr = -1;
    target.stack_height = bh_arr_length(builder->execution_stack);
    target.entry_reachable = builder->reachable;

    bh_arr_push(builder->label_stack, target);

    return target.idx;
}

void ovm_code_builder_bind_block(ovm_code_builder_t *builder, i32 param_count, i32 result_count) {
    label_target_t *label = &bh_arr_last(builder->label_stack);
    label->entry_reachable = builder->reachable;

    if (!builder->reachable) {
        label->stack_height = bh_arr_length(builder->execution_stack);
        if (param_count > 0) {
            label->param_regs = malloc(sizeof(i32) * param_count);
            fori (i, 0, param_count) {
                label->param_regs[i] = NEXT_VALUE(builder);
                PUSH_VALUE(builder, label->param_regs[i]);
            }
            fori (i, 0, param_count) POP_VALUE(builder);
            label->param_count = param_count;
        }
        if (result_count > 0) {
            label->result_regs = malloc(sizeof(i32) * result_count);
            fori (i, 0, result_count) {
                label->result_regs[i] = NEXT_VALUE(builder);
                PUSH_VALUE(builder, label->result_regs[i]);
            }
            fori (i, 0, result_count) POP_VALUE(builder);
            label->result_count = result_count;
        }
        if (label->kind == label_kind_loop) {
            label->instr = bh_arr_length(builder->program->code);
        }
        return;
    }

    i32 len = bh_arr_length(builder->execution_stack);
    assert(len >= param_count);
    label->stack_height = len - param_count;

    if (param_count > 0) {
        label->param_regs = malloc(sizeof(i32) * param_count);
        fori (i, 0, param_count) {
            i32 src = builder->execution_stack[label->stack_height + i];
            i32 dst = NEXT_VALUE(builder);
            emit_mov(builder, dst, src);
            builder->execution_stack[label->stack_height + i] = dst;
            label->param_regs[i] = dst;
        }
        label->param_count = param_count;
    }

    if (result_count > 0) {
        label->result_regs = malloc(sizeof(i32) * result_count);
        fori (i, 0, result_count) {
            label->result_regs[i] = NEXT_VALUE(builder);
            PUSH_VALUE(builder, label->result_regs[i]);
        }
        fori (i, 0, result_count) POP_VALUE(builder);
        label->result_count = result_count;
    }

    if (label->kind == label_kind_loop) {
        label->instr = bh_arr_length(builder->program->code);
    }
}

void ovm_code_builder_bind_if(ovm_code_builder_t *builder, i32 param_count, i32 result_count) {
    i32 cond = POP_VALUE(builder);
    ovm_code_builder_bind_block(builder, param_count, result_count);
    PUSH_VALUE(builder, cond);
}

static void rebuild_stack_with_results(ovm_code_builder_t *builder, label_target_t *target) {
    if (target->param_count == 0 && target->result_count == 0) return;

    bh_arr_set_length(builder->execution_stack, target->stack_height);
    fori (i, 0, target->result_count) PUSH_VALUE(builder, target->result_regs[i]);
}

void ovm_code_builder_pop_label_target(ovm_code_builder_t *builder) {
    label_target_t *top = &bh_arr_last(builder->label_stack);
    if (top->kind != label_kind_func && top->result_count > 0 && builder->reachable) {
        i32 *src = alloca(top->result_count * sizeof(i32));
        read_top_values(builder, top->result_count, src);
        emit_safe_moves(builder, top->result_regs, src, top->result_count);
    }

    label_target_t target = bh_arr_pop(builder->label_stack);
    if (target.instr == -1) {
        target.instr = bh_arr_length(builder->program->code);
    }

    b32 else_falls_to_end = 0;
    fori (i, 0, bh_arr_length(builder->branch_patches)) {
        branch_patch_t patch = builder->branch_patches[i];
        if (patch.label_idx != target.idx) continue;
        if (patch.targets_else) else_falls_to_end = 1;

        int br_delta = target.instr - patch.branch_instr - 1;

        switch (patch.kind) {
            case branch_patch_instr_a:
                builder->program->code[patch.branch_instr].a = br_delta;
                break;

            case branch_patch_instr_r:
                builder->program->code[patch.branch_instr].r = br_delta;
                break;

            case branch_patch_static_idx:
                ovm_program_modify_static_int(builder->program, patch.static_arr, patch.static_idx, br_delta);
                break;
        }

        bh_arr_fastdelete(builder->branch_patches, i);
        i--;
    }

    if (target.kind != label_kind_func) {
        b32 fallthrough = builder->reachable;
        if (target.kind == label_kind_loop) {
            builder->reachable = target.entry_reachable && fallthrough;
        } else {
            builder->reachable = target.entry_reachable && (fallthrough || target.targeted || else_falls_to_end);
        }
        rebuild_stack_with_results(builder, &target);
    }

    free(target.param_regs);
    free(target.result_regs);
}

void ovm_code_builder_else(ovm_code_builder_t *builder) {
    label_target_t *label = &bh_arr_last(builder->label_stack);
    assert(label->kind == label_kind_if);

    if (builder->reachable) {
        if (label->result_count > 0) {
            i32 *src = alloca(label->result_count * sizeof(i32));
            read_top_values(builder, label->result_count, src);
            emit_safe_moves(builder, label->result_regs, src, label->result_count);
        }
        label->targeted = 1;
        emit_branch_instr(builder, label->idx);
        builder->reachable = 0;
    }

    ovm_code_builder_patch_else(builder, *label);

    if (label->param_count > 0 || label->result_count > 0) {
        bh_arr_set_length(builder->execution_stack, label->stack_height);
        fori (i, 0, label->param_count) PUSH_VALUE(builder, label->param_regs[i]);
    }
    builder->reachable = label->entry_reachable;
}

void ovm_code_builder_patch_else(ovm_code_builder_t *builder, label_target_t if_target) {
    assert(if_target.kind == label_kind_if);

    fori (i, 0, bh_arr_length(builder->branch_patches)) {
        branch_patch_t patch = builder->branch_patches[i];
        if (patch.label_idx != if_target.idx) continue;
        if (!patch.targets_else) continue;

        int br_delta = bh_arr_length(builder->program->code) - patch.branch_instr - 1;
        if (patch.kind == branch_patch_instr_r) {
            builder->program->code[patch.branch_instr].r = br_delta;
        } else {
            assert(patch.kind == branch_patch_instr_a);
            builder->program->code[patch.branch_instr].a = br_delta;
        }

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
    builder->reachable = 0;
}

static b32 value_still_on_stack(ovm_code_builder_t *builder, i32 val) {
    bh_arr_each(i32, reg, builder->execution_stack) {
        if (*reg == val) return 1;
    }
    return 0;
}

static i32 imm_binop_opcode(u32 op) {
    switch (op) {
        case OVMI_ADD: return OVMI_ADD_IMM;
        case OVMI_SUB: return OVMI_SUB_IMM;
        case OVMI_MUL: return OVMI_MUL_IMM;
        case OVMI_AND: return OVMI_AND_IMM;
        case OVMI_OR:  return OVMI_OR_IMM;
        case OVMI_XOR: return OVMI_XOR_IMM;
        case OVMI_SHL: return OVMI_SHL_IMM;
        case OVMI_SHR: return OVMI_SHR_IMM;
        case OVMI_SAR: return OVMI_SAR_IMM;
        default: return -1;
    }
}

static b32 imm_binop_commutative(u32 op) {
    return op == OVMI_ADD || op == OVMI_MUL || op == OVMI_AND || op == OVMI_OR || op == OVMI_XOR;
}

static b32 try_fold_imm_binop(ovm_code_builder_t *builder, u32 instr, i32 left, i32 right) {
    if (bh_arr_length(builder->program->code) == 0) return 0;

    ovm_instr_t *last = &bh_arr_last(builder->program->code);
    if (OVM_INSTR_INSTR(*last) != OVMI_IMM) return 0;

    u32 type = OVM_INSTR_TYPE(*last);
    if (type != (instr & 0x7)) return 0;
    if (type != OVM_TYPE_I32 && type != OVM_TYPE_I64) return 0;

    u32 op = (instr >> 3) & 0xff;
    i32 fused = imm_binop_opcode(op);
    if (fused < 0) return 0;

    i32 imm_reg = last->r;
    i32 src;
    if (imm_reg == right) {
        src = left;
    } else if (imm_reg == left && imm_binop_commutative(op)) {
        src = right;
    } else {
        return 0;
    }

    if (!IS_TEMPORARY_VALUE(builder, imm_reg)) return 0;
    if (value_still_on_stack(builder, imm_reg)) return 0;

    i32 imm_bits;
    if (type == OVM_TYPE_I32) {
        imm_bits = last->i;
    } else {
        if ((i64)(i32) last->l != last->l) return 0;
        imm_bits = (i32) last->l;
    }

    i32 result = NEXT_VALUE(builder);
    last->full_instr = OVM_TYPED_INSTR((u32) fused, type);
    last->r = result;
    last->a = src;
    last->b = imm_bits;
    PUSH_VALUE(builder, result);
    return 1;
}

void ovm_code_builder_add_binop(ovm_code_builder_t *builder, u32 instr) {
    i32 right  = POP_VALUE(builder);
    i32 left   = POP_VALUE(builder);

    if (try_fold_imm_binop(builder, instr, left, right)) return;

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
    label_target_t *label = label_from_idx(builder, label_idx);
    if (builder->reachable) label->targeted = 1;
    move_branch_values(builder, label);
    emit_branch_instr(builder, label_idx);
    builder->reachable = 0;
}

static i32 cmp_to_br_opcode(u32 cmp, bool branch_if_true) {
    if (branch_if_true) {
        switch (cmp) {
            case OVMI_EQ:   return OVMI_BR_EQ;
            case OVMI_NE:   return OVMI_BR_NE;
            case OVMI_LT:   return OVMI_BR_LT;
            case OVMI_LT_S: return OVMI_BR_LT_S;
            case OVMI_LE:   return OVMI_BR_LE;
            case OVMI_LE_S: return OVMI_BR_LE_S;
            case OVMI_GT:   return OVMI_BR_GT;
            case OVMI_GT_S: return OVMI_BR_GT_S;
            case OVMI_GE:   return OVMI_BR_GE;
            case OVMI_GE_S: return OVMI_BR_GE_S;
            default: return -1;
        }
    }

    switch (cmp) {
        case OVMI_EQ:   return OVMI_BR_NE;
        case OVMI_NE:   return OVMI_BR_EQ;
        case OVMI_LT:   return OVMI_BR_GE;
        case OVMI_LT_S: return OVMI_BR_GE_S;
        case OVMI_LE:   return OVMI_BR_GT;
        case OVMI_LE_S: return OVMI_BR_GT_S;
        case OVMI_GT:   return OVMI_BR_LE;
        case OVMI_GT_S: return OVMI_BR_LE_S;
        case OVMI_GE:   return OVMI_BR_LT;
        case OVMI_GE_S: return OVMI_BR_LT_S;
        default: return -1;
    }
}

void ovm_code_builder_add_cond_branch(ovm_code_builder_t *builder, i32 label_idx, bool branch_if_true, bool targets_else) {
    i32 cond = POP_VALUE(builder);
    label_target_t *label = label_from_idx(builder, label_idx);

    // The if-header branch jumps to the else (or the end, if there is no else).
    // It does not carry block results. br_if does, and those copies must run
    // only when the branch is taken so the fallthrough keeps its own registers.
    i32 arity = targets_else ? 0 : branch_arity(label);
    if (!targets_else && builder->reachable) label->targeted = 1;

    if (arity > 0 && builder->reachable) {
        i32 *src = alloca(arity * sizeof(i32));
        read_top_values(builder, arity, src);
        i32 nmov = planned_move_count(branch_dest(label), src, arity);
        if (nmov > 0) {
            ovm_instr_t skip = {0};
            skip.full_instr = OVM_TYPED_INSTR(branch_if_true ? OVMI_BR_Z : OVMI_BR_NZ, OVM_TYPE_NONE);
            skip.a = nmov + 1;
            skip.b = cond;
            emit_raw(builder, skip);
            emit_safe_moves(builder, branch_dest(label), src, arity);
            emit_branch_instr(builder, label_idx);
            return;
        }
    }

    if (bh_arr_length(builder->program->code) > 0) {
        ovm_instr_t *last = &bh_arr_last(builder->program->code);
        i32 fused = cmp_to_br_opcode(OVM_INSTR_INSTR(*last), branch_if_true);
        if (fused >= 0
            && last->r == cond
            && IS_TEMPORARY_VALUE(builder, cond)
            && !value_still_on_stack(builder, cond)) {
            last->full_instr = OVM_TYPED_INSTR((u32) fused, OVM_INSTR_TYPE(*last));
            last->r = -1;

            branch_patch_t patch;
            patch.kind = branch_patch_instr_r;
            patch.branch_instr = bh_arr_length(builder->program->code) - 1;
            patch.label_idx = label_idx;
            patch.targets_else = targets_else;
            bh_arr_push(builder->branch_patches, patch);
            return;
        }
    }

    ovm_instr_t branch_instr = {0};
    if (branch_if_true) {
        branch_instr.full_instr = OVM_TYPED_INSTR(OVMI_BR_NZ, OVM_TYPE_NONE);
    } else {
        branch_instr.full_instr = OVM_TYPED_INSTR(OVMI_BR_Z, OVM_TYPE_NONE);
    }

    branch_instr.a = -1;
    branch_instr.b = cond;

    branch_patch_t patch;
    patch.kind = branch_patch_instr_a;
    patch.branch_instr = bh_arr_length(builder->program->code);
    patch.label_idx = label_idx;
    patch.targets_else = targets_else;

    bh_arr_push(builder->branch_patches, patch);

    debug_info_builder_emit_location(builder->debug_builder);
    ovm_program_add_instructions(builder->program, 1, &branch_instr);
}

static void mark_branch_target(ovm_code_builder_t *builder, i32 label_idx) {
    if (!builder->reachable) return;
    label_from_idx(builder, label_idx)->targeted = 1;
}

void ovm_code_builder_add_branch_table(ovm_code_builder_t *builder, i32 count, i32 *label_indicies, i32 default_label_idx) {
    label_target_t *def = label_from_idx(builder, default_label_idx);
    i32 arity = branch_arity(def);
    fori (i, 0, count) {
        assert(branch_arity(label_from_idx(builder, label_indicies[i])) == arity);
    }

    // An indirect branch cannot copy a different register set per target.
    // Value-carrying tables become a compare chain so each arm can move first.
    if (arity > 0 && builder->reachable) {
        i32 index = POP_VALUE(builder);
        i32 index_pin = NEXT_VALUE(builder);
        emit_mov(builder, index_pin, index);
        PUSH_VALUE(builder, index_pin);

        i32 *src = alloca(arity * sizeof(i32));
        i32 len = bh_arr_length(builder->execution_stack);
        assert(len >= arity + 1);
        fori (i, 0, arity) src[i] = builder->execution_stack[len - 1 - arity + i];

        mark_branch_target(builder, default_label_idx);
        fori (i, 0, count) mark_branch_target(builder, label_indicies[i]);

        fori (arm, 0, count) {
            label_target_t *arm_label = label_from_idx(builder, label_indicies[arm]);
            i32 *dest = branch_dest(arm_label);
            i32 nmov = planned_move_count(dest, src, arity);

            i32 immreg = NEXT_VALUE(builder);
            ovm_instr_t imm = {0};
            imm.full_instr = OVM_TYPED_INSTR(OVMI_IMM, OVM_TYPE_I32);
            imm.r = immreg;
            imm.i = arm;
            emit_raw(builder, imm);

            ovm_instr_t cmp = {0};
            cmp.full_instr = OVM_TYPED_INSTR(OVMI_BR_NE, OVM_TYPE_I32);
            cmp.a = index_pin;
            cmp.b = immreg;
            cmp.r = nmov + 1;
            emit_raw(builder, cmp);

            emit_safe_moves(builder, dest, src, arity);
            emit_branch_instr(builder, label_indicies[arm]);
        }

        emit_safe_moves(builder, branch_dest(def), src, arity);
        emit_branch_instr(builder, default_label_idx);

        POP_VALUE(builder);
        builder->reachable = 0;
        return;
    }

    mark_branch_target(builder, default_label_idx);
    fori (i, 0, count) mark_branch_target(builder, label_indicies[i]);

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
    builder->reachable = 0;
}

void ovm_code_builder_add_return(ovm_code_builder_t *builder) {
    ovm_instr_t instr = {0};
    instr.full_instr = OVM_TYPED_INSTR(OVMI_RETURN, OVM_TYPE_NONE);

    i32 values_on_stack = bh_arr_length(builder->execution_stack);
    if (values_on_stack > 0 && builder->result_count > 0 && builder->reachable) {
        instr.a = POP_VALUE(builder);
    }

    debug_info_builder_emit_location(builder->debug_builder);
    ovm_program_add_instructions(builder->program, 1, &instr);
    builder->reachable = 0;
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

void ovm_code_builder_add_select(ovm_code_builder_t *builder) {
    // WASM select: result = cond != 0 ? val1 : val2. The condition is i32.
    i32 cond = POP_VALUE(builder);
    i32 val2 = POP_VALUE(builder);
    i32 val1 = POP_VALUE(builder);

    i32 result = NEXT_VALUE(builder);
    if (result == cond || result == val2) {
        result = builder->highest_value_number;
        if (result == cond || result == val2) result++;
        builder->highest_value_number = result + 1;
    }

    ovm_instr_t instrs[3] = {0};
    instrs[0].full_instr = OVM_TYPED_INSTR(OVMI_MOV, OVM_TYPE_NONE);
    instrs[0].r = result;
    instrs[0].a = val1;

    // Skip the fallback move when the condition is true. The VM and JIT both
    // land on instruction (index + a + 1).
    instrs[1].full_instr = OVM_TYPED_INSTR(OVMI_BR_NZ, OVM_TYPE_NONE);
    instrs[1].a = 1;
    instrs[1].b = cond;

    instrs[2].full_instr = OVM_TYPED_INSTR(OVMI_MOV, OVM_TYPE_NONE);
    instrs[2].r = result;
    instrs[2].a = val2;

    debug_info_builder_emit_location(builder->debug_builder);
    debug_info_builder_emit_location(builder->debug_builder);
    debug_info_builder_emit_location(builder->debug_builder);
    ovm_program_add_instructions(builder->program, 3, instrs);
    PUSH_VALUE(builder, result);
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
