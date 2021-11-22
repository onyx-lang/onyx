//
// There are several things I'm seeing in this file that I want to clean up.
// They are:
//  [x] remove the need to know if the stack is needed before generating the function.
//      Just leave 5 nops at the beginning because they will be automatically removed
//      by the WASM outputter.
//  [x] remove the need to have "allocate_exprs" on blocks and in functions. This will
//      be easy once the above is done.
//  [x] there should be a better way to emit pending deferred statements because there
//      is some code duplication between emit_return and emit_structured_jump.
//  [ ] Change the calling convention so it is easier to use from both JS and in the compiler.




#define BH_DEBUG
#include "wasm_emit.h"
#include "utils.h"

#define WASM_TYPE_INT32   0x7F
#define WASM_TYPE_INT64   0x7E
#define WASM_TYPE_FLOAT32 0x7D
#define WASM_TYPE_FLOAT64 0x7C
#define WASM_TYPE_VAR128  0x7B
#define WASM_TYPE_PTR     WASM_TYPE_INT32
#define WASM_TYPE_FUNC    WASM_TYPE_INT32
#define WASM_TYPE_VOID    0x00

static WasmType onyx_type_to_wasm_type(Type* type) {
    if (type->kind == Type_Kind_Struct) {
        return WASM_TYPE_VOID;
    }

    if (type->kind == Type_Kind_Slice) {
        return WASM_TYPE_VOID;
    }

    if (type->kind == Type_Kind_Enum) {
        return onyx_type_to_wasm_type(type->Enum.backing);
    }

    if (type->kind == Type_Kind_Pointer) {
        return WASM_TYPE_PTR;
    }

    if (type->kind == Type_Kind_Array) {
        return WASM_TYPE_PTR;
    }

    if (type->kind == Type_Kind_Function) {
        return WASM_TYPE_FUNC;
    }

    if (type->kind == Type_Kind_Basic) {
        TypeBasic* basic = &type->Basic;
        if (basic->flags & Basic_Flag_Boolean) return WASM_TYPE_INT32;
        if (basic->flags & Basic_Flag_Integer) {
            if (basic->size <= 4) return WASM_TYPE_INT32;
            if (basic->size == 8) return WASM_TYPE_INT64;
        }
        if (basic->flags & Basic_Flag_Pointer) return WASM_TYPE_INT32;
        if (basic->flags & Basic_Flag_Float) {
            if (basic->size <= 4) return WASM_TYPE_FLOAT32;
            if (basic->size == 8) return WASM_TYPE_FLOAT64;
        }
        if (basic->flags & Basic_Flag_SIMD) return WASM_TYPE_VAR128;
        if (basic->flags & Basic_Flag_Type_Index) return WASM_TYPE_INT32;
        if (basic->size == 0) return WASM_TYPE_VOID;
    }

    return WASM_TYPE_VOID;
}

static i32 generate_type_idx(OnyxWasmModule* mod, Type* ft);
static i32 get_element_idx(OnyxWasmModule* mod, AstFunction* func);

#define LOCAL_I32  0x000000000
#define LOCAL_I64  0x100000000
#define LOCAL_F32  0x300000000
#define LOCAL_F64  0x700000000
#define LOCAL_V128 0xf00000000

static b32 local_is_wasm_local(AstTyped* local) {
    if (local->kind == Ast_Kind_Local && local->flags & Ast_Flag_Address_Taken) return 0;
    if (local->type->kind == Type_Kind_Basic) return 1;
    if (local->type->kind == Type_Kind_Enum && local->type->Enum.backing->kind == Type_Kind_Basic) return 1;
    if (local->type->kind == Type_Kind_Pointer) return 1;
    return 0;
}

static u64 local_raw_allocate(LocalAllocator* la, WasmType wt) {
    i32 idx = 0;
    if (wt == WASM_TYPE_INT32)   idx = 0;
    if (wt == WASM_TYPE_INT64)   idx = 1;
    if (wt == WASM_TYPE_FLOAT32) idx = 2;
    if (wt == WASM_TYPE_FLOAT64) idx = 3;
    if (wt == WASM_TYPE_VAR128)  idx = 4;

    u64 flag_bits = LOCAL_IS_WASM;
    if (wt == WASM_TYPE_INT32)   flag_bits |= LOCAL_I32;
    if (wt == WASM_TYPE_INT64)   flag_bits |= LOCAL_I64;
    if (wt == WASM_TYPE_FLOAT32) flag_bits |= LOCAL_F32;
    if (wt == WASM_TYPE_FLOAT64) flag_bits |= LOCAL_F64;
    if (wt == WASM_TYPE_VAR128)  flag_bits |= LOCAL_V128;

    if (la->freed[idx] > 0) {
        la->freed[idx]--;
        return flag_bits | ((u64) (la->allocated[idx] - la->freed[idx] - 1 + la->param_count));

    } else {
        la->allocated[idx]++;
        return flag_bits | ((u64) (la->allocated[idx] - 1 + la->param_count));
    }
}

static void local_raw_free(LocalAllocator* la, WasmType wt) {
    i32 idx = 0;

    if (wt == WASM_TYPE_INT32)   idx = 0;
    if (wt == WASM_TYPE_INT64)   idx = 1;
    if (wt == WASM_TYPE_FLOAT32) idx = 2;
    if (wt == WASM_TYPE_FLOAT64) idx = 3;
    if (wt == WASM_TYPE_VAR128)  idx = 4;

    assert(la->allocated[idx] > 0 && la->freed[idx] < la->allocated[idx]);

    la->freed[idx]++;
}

static u64 local_allocate(LocalAllocator* la, AstTyped* local) {
    if (local_is_wasm_local(local)) {
        WasmType wt = onyx_type_to_wasm_type(local->type);
        return local_raw_allocate(la, wt);

    } else {
        u32 size = type_size_of(local->type);
        u32 alignment = type_alignment_of(local->type);

        bh_align(la->curr_stack, alignment);

        if (la->max_stack < la->curr_stack)
            la->max_stack = la->curr_stack;

        bh_align(size, alignment);

        if (la->max_stack - la->curr_stack >= (i32) size) {
            la->curr_stack += size;

        } else {
            la->max_stack += size - (la->max_stack - la->curr_stack);
            la->curr_stack = la->max_stack;
        }

        return la->curr_stack - size;
    }
}

static void local_free(LocalAllocator* la, AstTyped* local) {
    if (local_is_wasm_local(local)) {
        WasmType wt = onyx_type_to_wasm_type(local->type);
        local_raw_free(la, wt);

    } else {
        u32 size = type_size_of(local->type);
        u32 alignment = type_alignment_of(local->type);
        bh_align(size, alignment);

        la->curr_stack -= size;
    }
}

static u64 local_lookup_idx(LocalAllocator* la, u64 value) {
    assert(value & LOCAL_IS_WASM);

    u32 idx = value & 0xFFFFFFFF;
    if (value & 0x100000000) idx += la->allocated[0];
    if (value & 0x200000000) idx += la->allocated[1];
    if (value & 0x400000000) idx += la->allocated[2];
    if (value & 0x800000000) idx += la->allocated[3];

    return (u64) idx;
}


typedef enum StructuredBlockType StructuredBlockType;
enum StructuredBlockType {
    SBT_Basic_Block,       // Cannot be targeted using jump
    SBT_Breakable_Block,   // Targeted using break
    SBT_Continue_Block,    // Targeted using continue
    SBT_Fallthrough_Block, // Targeted using fallthrough
    SBT_Return_Block,      // Targeted using return, (used for expression macros)

    SBT_Basic_If,          // Cannot be targeted using jump
    SBT_Breakable_If,      // Targeted using break

    SBT_Basic_Loop,        // Cannot be targeted using jump
    SBT_Continue_Loop,     // Targeted using continue

    SBT_Count,
};


#define WI(instr) bh_arr_push(code, ((WasmInstruction){ instr, 0x00 }))
#define WID(instr, data) bh_arr_push(code, ((WasmInstruction){ instr, data }))
#define WIL(instr, data) bh_arr_push(code, ((WasmInstruction){ instr, { .l = data } }))
#define WIP(instr, data) bh_arr_push(code, ((WasmInstruction){ instr, { .p = data } }))
#define EMIT_FUNC(kind, ...) static void emit_ ## kind (OnyxWasmModule* mod, bh_arr(WasmInstruction)* pcode, __VA_ARGS__)
#define EMIT_FUNC_NO_ARGS(kind) static void emit_ ## kind (OnyxWasmModule* mod, bh_arr(WasmInstruction)* pcode)
#define STACK_SWAP(type1, type2) { \
    u64 t0 = local_raw_allocate(mod->local_alloc, type1); \
    u64 t1 = local_raw_allocate(mod->local_alloc, type2); \
                                                          \
    WIL(WI_LOCAL_SET, t0);                                \
    WIL(WI_LOCAL_SET, t1);                                \
    WIL(WI_LOCAL_GET, t0);                                \
    WIL(WI_LOCAL_GET, t1);                                \
                                                          \
    local_raw_free(mod->local_alloc, type1);              \
    local_raw_free(mod->local_alloc, type2);              \
    }
#define SUBMIT_PATCH(patch_arr, offset) bh_arr_push((patch_arr), ((PatchInfo) { bh_arr_length(code) - offset }))

EMIT_FUNC(function_body,                 AstFunction* fd);
EMIT_FUNC(block,                         AstBlock* block, b32 generate_block_headers);
EMIT_FUNC(statement,                     AstNode* stmt);
EMIT_FUNC(local_allocation,              AstTyped* stmt);
EMIT_FUNC_NO_ARGS(free_local_allocations);
EMIT_FUNC(assignment,                    AstBinaryOp* assign);
EMIT_FUNC(assignment_of_array,           AstTyped* left, AstTyped* right);
EMIT_FUNC(compound_assignment,           AstBinaryOp* assign);
EMIT_FUNC(store_instruction,             Type* type, u32 offset);
EMIT_FUNC(load_instruction,              Type* type, u32 offset);
EMIT_FUNC(if,                            AstIfWhile* if_node);
EMIT_FUNC(while,                         AstIfWhile* while_node);
EMIT_FUNC(for,                           AstFor* for_node);
EMIT_FUNC(switch,                        AstSwitch* switch_node);
EMIT_FUNC(defer,                         AstDefer* defer);
EMIT_FUNC(defer_code,                    WasmInstruction* deferred_code, u32 code_count);
EMIT_FUNC(deferred_stmt,                 DeferredStmt deferred_stmt);
EMIT_FUNC_NO_ARGS(deferred_stmts);
EMIT_FUNC(binop,                         AstBinaryOp* binop);
EMIT_FUNC(unaryop,                       AstUnaryOp* unop);
EMIT_FUNC(call,                          AstCall* call);
EMIT_FUNC(intrinsic_call,                AstCall* call);
EMIT_FUNC(subscript_location,            AstSubscript* sub, u64* offset_return);
EMIT_FUNC(field_access_location,         AstFieldAccess* field, u64* offset_return);
EMIT_FUNC(local_location,                AstLocal* local, u64* offset_return);
EMIT_FUNC(memory_reservation_location,   AstMemRes* memres);
EMIT_FUNC(location_return_offset,        AstTyped* expr, u64* offset_return);
EMIT_FUNC(location,                      AstTyped* expr);
EMIT_FUNC(compound_load,                 Type* type, u64 offset);
EMIT_FUNC(struct_lval,                   AstTyped* lval);
EMIT_FUNC(struct_literal,                AstStructLiteral* sl);
EMIT_FUNC(compound_store,                Type* type, u64 offset, b32 location_first);
EMIT_FUNC(array_store,                   Type* type, u32 offset);
EMIT_FUNC(array_literal,                 AstArrayLiteral* al);
EMIT_FUNC(range_literal,                 AstRangeLiteral* range);
EMIT_FUNC(if_expression,                 AstIfExpression* if_expr);
EMIT_FUNC(do_block,                      AstDoBlock* doblock);
EMIT_FUNC(expression,                    AstTyped* expr);
EMIT_FUNC(cast,                          AstUnaryOp* cast);
EMIT_FUNC(return,                        AstReturn* ret);
EMIT_FUNC(stack_enter,                   u64 stacksize);
EMIT_FUNC(zero_value,                    WasmType wt);
EMIT_FUNC(zero_value_for_type,           Type* type, OnyxToken* where);

EMIT_FUNC(enter_structured_block,        StructuredBlockType sbt);
EMIT_FUNC_NO_ARGS(leave_structured_block);

static void emit_raw_data(OnyxWasmModule* mod, ptr data, AstTyped* node);
static b32 emit_raw_data_(OnyxWasmModule* mod, ptr data, AstTyped* node);

#include "wasm_intrinsics.c"
#include "wasm_type_table.c"

EMIT_FUNC(function_body, AstFunction* fd) {
    if (fd->body == NULL) return;

    emit_block(mod, pcode, fd->body, 0);
}

EMIT_FUNC(block, AstBlock* block, b32 generate_block_headers) {
    bh_arr(WasmInstruction) code = *pcode;

    generate_block_headers = generate_block_headers && (block->rules & Block_Rule_Emit_Instructions);

    if (generate_block_headers) {
        emit_enter_structured_block(mod, &code, (block->rules & Block_Rule_Override_Return)
                                                ? SBT_Return_Block
                                                : SBT_Breakable_Block);
    }

    forll (AstNode, stmt, block->body, next) {
        emit_statement(mod, &code, stmt);
    }

    // HACK: I'm not convinced this is the best way to handle this case. Essentially, if
    // a deferred statement uses a local that is freed from the a macro block, then the
    // local won't be valid anymore and could have garbage data. This ensures that the
    // freeing of the local variables and flushing of the deferred statements happen together,
    // so a deferred statement can never try to use a local that has been freed.
    //                                                                   - brendanfh 2021/08/29
    if (block->rules & Block_Rule_Clear_Defer) {
        emit_deferred_stmts(mod, &code);

        // if (block->rules & Block_Rule_New_Scope)
        emit_free_local_allocations(mod, &code);
    }

    if (generate_block_headers)
        emit_leave_structured_block(mod, &code);

    *pcode = code;
}

EMIT_FUNC(enter_structured_block, StructuredBlockType sbt) {
    bh_arr(WasmInstruction) code = *pcode;

    static const StructuredBlockType jump_numbers[SBT_Count] = {
        /* SBT_Basic_Block */       0,
        /* SBT_Breakable_Block */   1,
        /* SBT_Continue_Block */    2,
        /* SBT_Fallthrough_Block */ 3,
        /* SBT_Return_Block */      4,

        /* SBT_Basic_If */          0,
        /* SBT_Breakable_If */      1,

        /* SBT_Basic_Loop */        0,
        /* SBT_Continue_Loop */     2,
    };
    
    static const WasmInstructionType block_instrs[SBT_Count] = {
        /* SBT_Basic_Block */       WI_BLOCK_START,
        /* SBT_Breakable_Block */   WI_BLOCK_START,
        /* SBT_Continue_Block */    WI_BLOCK_START,
        /* SBT_Fallthrough_Block */ WI_BLOCK_START,
        /* SBT_Return_Block */      WI_BLOCK_START,

        /* SBT_Basic_If */          WI_IF_START,
        /* SBT_Breakable_If */      WI_IF_START,

        /* SBT_Basic_Loop */        WI_LOOP_START,
        /* SBT_Continue_Loop */     WI_LOOP_START,
    };


    WID(block_instrs[sbt], 0x40);
    bh_arr_push(mod->structured_jump_target, jump_numbers[sbt]);

    *pcode = code;
}

EMIT_FUNC_NO_ARGS(leave_structured_block) {
    bh_arr(WasmInstruction) code = *pcode;

    WI(WI_BLOCK_END);
    bh_arr_pop(mod->structured_jump_target);

    *pcode = code;
}

i64 get_structured_jump_label(OnyxWasmModule* mod, JumpType jump_type, u32 jump_count) {
    // :CLEANUP These numbers should become constants because they are shared with
    // enter_structured_block's definitions.
    static const u8 wants[Jump_Type_Count] = { 1, 2, 3, 4 };

    i64 labelidx = 0;
    u8 wanted = wants[jump_type];
    b32 success = 0;

    i32 len = bh_arr_length(mod->structured_jump_target) - 1;
    for (u8* t = &bh_arr_last(mod->structured_jump_target); len >= 0; len--, t--) {
        if (*t == wanted) jump_count--;
        if (jump_count == 0) {
            success = 1;
            break;
        }

        labelidx++;
    }

    return (success == 0) ? -1 : labelidx;
}

EMIT_FUNC(structured_jump, AstJump* jump) {
    bh_arr(WasmInstruction) code = *pcode;

    i64 labelidx = get_structured_jump_label(mod, jump->jump, jump->count);

    if (bh_arr_length(mod->deferred_stmts) > 0) {
        i32 i = bh_arr_length(mod->deferred_stmts) - 1;
        i32 d = bh_arr_length(mod->structured_jump_target) - (labelidx + 1);

        while (i >= 0 && (i32) mod->deferred_stmts[i].depth > d) {
            emit_deferred_stmt(mod, &code, mod->deferred_stmts[i]);
            i--;
        }
    }

    if (labelidx >= 0) {
        // NOTE: If the previous instruction was a non conditional jump,
        // don't emit another jump since it will never be reached.
        if (bh_arr_last(code).type != WI_JUMP)
            WID(WI_JUMP, labelidx);
    } else {
        onyx_report_error(jump->token->pos, "Invalid structured jump.");
    }

    *pcode = code;
}

EMIT_FUNC(statement, AstNode* stmt) {
    bh_arr(WasmInstruction) code = *pcode;

    switch (stmt->kind) {
        case Ast_Kind_Return:     emit_return(mod, &code, (AstReturn *) stmt); break;
        case Ast_Kind_If:         emit_if(mod, &code, (AstIfWhile *) stmt); break;
        case Ast_Kind_Static_If:  emit_if(mod, &code, (AstIfWhile *) stmt); break;
        case Ast_Kind_While:      emit_while(mod, &code, (AstIfWhile *) stmt); break;
        case Ast_Kind_For:        emit_for(mod, &code, (AstFor *) stmt); break;
        case Ast_Kind_Switch:     emit_switch(mod, &code, (AstSwitch *) stmt); break;
        case Ast_Kind_Jump:       emit_structured_jump(mod, &code, (AstJump *) stmt); break;
        case Ast_Kind_Block:      emit_block(mod, &code, (AstBlock *) stmt, 1); break;
        case Ast_Kind_Defer:      emit_defer(mod, &code, (AstDefer *) stmt); break;
        case Ast_Kind_Local:      emit_local_allocation(mod, &code, (AstTyped *) stmt); break;

        case Ast_Kind_Directive_Insert: break;

        default:                  emit_expression(mod, &code, (AstTyped *) stmt); break;
    }

    *pcode = code;
}

EMIT_FUNC(local_allocation, AstTyped* stmt) {
    bh_imap_put(&mod->local_map, (u64) stmt, local_allocate(mod->local_alloc, stmt));

    bh_arr_push(mod->local_allocations, ((AllocatedSpace) {
        .depth = bh_arr_length(mod->structured_jump_target),
        .expr  = stmt,
    }));
}

EMIT_FUNC_NO_ARGS(free_local_allocations) {
    if (bh_arr_length(mod->local_allocations) == 0) return;

    u64 depth = bh_arr_length(mod->structured_jump_target);
    while (bh_arr_length(mod->local_allocations) > 0 && bh_arr_last(mod->local_allocations).depth >= depth) {
        // CHECK: Not sure this next line is okay to be here...
        bh_imap_delete(&mod->local_map, (u64) bh_arr_last(mod->local_allocations).expr);

        local_free(mod->local_alloc, bh_arr_last(mod->local_allocations).expr);
        bh_arr_pop(mod->local_allocations);
    }
}

EMIT_FUNC(assignment, AstBinaryOp* assign) {
    bh_arr(WasmInstruction) code = *pcode;

    if (assign->right->type->kind == Type_Kind_Array) {
        emit_assignment_of_array(mod, &code, assign->left, assign->right);
        *pcode = code;
        return;
    }

    if (assign->right->type->kind == Type_Kind_Compound) {
        emit_compound_assignment(mod, &code, assign);
        *pcode = code;
        return;
    }

    AstTyped* lval = assign->left;

    if (lval->kind == Ast_Kind_Local || lval->kind == Ast_Kind_Param) {
        if (bh_imap_get(&mod->local_map, (u64) lval) & LOCAL_IS_WASM) {
            emit_expression(mod, &code, assign->right);

            u64 localidx = bh_imap_get(&mod->local_map, (u64) lval);

            if (lval->kind == Ast_Kind_Param && type_is_structlike_strict(lval->type)) {
                u32 mem_count = type_structlike_mem_count(lval->type);
                fori (i, 0, mem_count) WIL(WI_LOCAL_SET, localidx + i);

            } else {
                WIL(WI_LOCAL_SET, localidx);
            }

            *pcode = code;
            return;
        }
    }

    if (lval->kind == Ast_Kind_Field_Access) {
        AstFieldAccess* fa = (AstFieldAccess *) lval;
        if (fa->expr->kind == Ast_Kind_Param && type_is_structlike_strict(fa->expr->type)) {
            emit_expression(mod, &code, assign->right);

            u64 localidx = bh_imap_get(&mod->local_map, (u64) fa->expr);
            WIL(WI_LOCAL_SET, localidx + fa->idx);

            *pcode = code;
            return;
        }
    }

    if (lval->kind == Ast_Kind_Global) {
        emit_expression(mod, &code, assign->right);

        i32 globalidx = (i32) bh_imap_get(&mod->index_map, (u64) lval);
        WID(WI_GLOBAL_SET, globalidx);

        *pcode = code;
        return;
    }

    u64 offset = 0;
    emit_location_return_offset(mod, &code, lval, &offset);
    emit_expression(mod, &code, assign->right);
    emit_store_instruction(mod, &code, lval->type, offset);

    *pcode = code;
}

EMIT_FUNC(assignment_of_array, AstTyped* left, AstTyped* right) {
    bh_arr(WasmInstruction) code = *pcode;

    Type* rtype = right->type;
    assert(rtype->kind == Type_Kind_Array);

    if (right->kind == Ast_Kind_Array_Literal) {
        Type* elem_type = rtype;
        u32 elem_count = 1;
        while (elem_type->kind == Type_Kind_Array) {
            elem_count *= elem_type->Array.count;
            elem_type = elem_type->Array.elem;
        }
        u32 elem_size = type_size_of(elem_type);

        u64 lptr_local = local_raw_allocate(mod->local_alloc, WASM_TYPE_PTR);

        emit_location(mod, &code, left);
        WIL(WI_LOCAL_SET, lptr_local);

        AstArrayLiteral* al = (AstArrayLiteral *) right;
        fori (i, 0, elem_count) {
            WIL(WI_LOCAL_GET, lptr_local);
            emit_expression(mod, &code, al->values[i]);
            emit_store_instruction(mod, &code, elem_type, i * elem_size);
        }
        
        local_raw_free(mod->local_alloc, WASM_TYPE_PTR);

    } else {
        u64 offset = 0;
        emit_location_return_offset(mod, &code, left, &offset);
        emit_expression(mod, &code, right);
        emit_array_store(mod, &code, rtype, offset);
    }

    *pcode = code;
    return;
}

EMIT_FUNC(compound_assignment, AstBinaryOp* assign) {
    bh_arr(WasmInstruction) code = *pcode;

    emit_expression(mod, &code, assign->right);

    AstCompound* compound_lval = (AstCompound *) assign->left;
    bh_arr_rev_each(AstTyped *, plval, compound_lval->exprs) {
        AstTyped *lval = *plval;

        if (type_is_structlike_strict(lval->type)) {
            emit_struct_lval(mod, &code, lval);
            continue;
        }

        if (lval->kind == Ast_Kind_Local || lval->kind == Ast_Kind_Param) {
            if (bh_imap_get(&mod->local_map, (u64) lval) & LOCAL_IS_WASM) {
                u64 localidx = bh_imap_get(&mod->local_map, (u64) lval);
                WIL(WI_LOCAL_SET, localidx);
                continue;
            }
        }

        WasmType wt = onyx_type_to_wasm_type(lval->type);
        u64 expr_tmp = local_raw_allocate(mod->local_alloc, wt);
        WIL(WI_LOCAL_SET, expr_tmp);
        u64 offset = 0;
        emit_location_return_offset(mod, &code, lval, &offset);
        WIL(WI_LOCAL_GET, expr_tmp);
        
        local_raw_free(mod->local_alloc, wt);
        emit_store_instruction(mod, &code, lval->type, offset);
    }

    *pcode = code;
    return;
}

EMIT_FUNC(store_instruction, Type* type, u32 offset) {
    bh_arr(WasmInstruction) code = *pcode;

    if (type_is_compound(type)) {
        emit_compound_store(mod, pcode, type, offset, 0);
        return;
    }

    if (type->kind == Type_Kind_Array) {
        emit_array_store(mod, pcode, type, offset);
        return;
    }

    if (type->kind == Type_Kind_Enum)     type = type->Enum.backing;
    if (type->kind == Type_Kind_Function) type = &basic_types[Basic_Kind_U32];

    u32 alignment = type_get_alignment_log2(type);

    i32 store_size  = type_size_of(type);
    i32 is_basic    = type->kind == Type_Kind_Basic || type->kind == Type_Kind_Pointer;

    if (is_basic && (type->Basic.flags & Basic_Flag_Pointer)) {
        WID(WI_I32_STORE, ((WasmInstructionData) { 2, offset }));
    } else if (is_basic && ((type->Basic.flags & Basic_Flag_Integer)
                         || (type->Basic.flags & Basic_Flag_Boolean)
                         || (type->Basic.flags & Basic_Flag_Type_Index))) {
        if      (store_size == 1)   WID(WI_I32_STORE_8,  ((WasmInstructionData) { alignment, offset }));
        else if (store_size == 2)   WID(WI_I32_STORE_16, ((WasmInstructionData) { alignment, offset }));
        else if (store_size == 4)   WID(WI_I32_STORE,    ((WasmInstructionData) { alignment, offset }));
        else if (store_size == 8)   WID(WI_I64_STORE,    ((WasmInstructionData) { alignment, offset }));
    } else if (is_basic && (type->Basic.flags & Basic_Flag_Float)) {
        if      (store_size == 4)   WID(WI_F32_STORE, ((WasmInstructionData) { alignment, offset }));
        else if (store_size == 8)   WID(WI_F64_STORE, ((WasmInstructionData) { alignment, offset }));
    } else if (is_basic && (type->Basic.flags & Basic_Flag_SIMD)) {
        WID(WI_V128_STORE, ((WasmInstructionData) { alignment, offset }));
    } else {
        onyx_report_error((OnyxFilePos) { 0 },
            "Failed to generate store instruction for type '%s'.",
            type_get_name(type));
    }

    *pcode = code;
}

EMIT_FUNC(load_instruction, Type* type, u32 offset) {
    bh_arr(WasmInstruction) code = *pcode;

    if (type_is_compound(type)) {
        emit_compound_load(mod, pcode, type, offset);
        return;
    }

    if (type->kind == Type_Kind_Array) {
        if (offset != 0) {
            WID(WI_PTR_CONST, offset);
            WI(WI_PTR_ADD);
        }

        *pcode = code;
        return;
    }

    if (type->kind == Type_Kind_Enum)     type = type->Enum.backing;
    if (type->kind == Type_Kind_Function) type = &basic_types[Basic_Kind_U32];

    i32 load_size   = type_size_of(type);
    i32 is_basic    = type->kind == Type_Kind_Basic || type->kind == Type_Kind_Pointer;

    WasmInstructionType instr = WI_NOP;
    i32 alignment = type_get_alignment_log2(type);

    if (is_basic && (type->Basic.flags & Basic_Flag_Pointer)) {
        instr = WI_I32_LOAD;
        alignment = 2;
    }
    else if (is_basic && ((type->Basic.flags & Basic_Flag_Integer)
                       || (type->Basic.flags & Basic_Flag_Boolean)
                       || (type->Basic.flags & Basic_Flag_Type_Index))) {
        if      (load_size == 1) instr = WI_I32_LOAD_8_S;
        else if (load_size == 2) instr = WI_I32_LOAD_16_S;
        else if (load_size == 4) instr = WI_I32_LOAD;
        else if (load_size == 8) instr = WI_I64_LOAD;

        if (load_size < 4 && (type->Basic.flags & Basic_Flag_Unsigned)) instr += 1;
    }
    else if (is_basic && (type->Basic.flags & Basic_Flag_Float)) {
        if      (load_size == 4) instr = WI_F32_LOAD;
        else if (load_size == 8) instr = WI_F64_LOAD;
    }
    else if (is_basic && (type->Basic.flags & Basic_Flag_SIMD)) {
        instr = WI_V128_LOAD;
    }

    WID(instr, ((WasmInstructionData) { alignment, offset }));

    if (instr == WI_NOP) {
        onyx_report_error((OnyxFilePos) { 0 },
            "Failed to generate load instruction for type '%s'.",
            type_get_name(type));
    }

    *pcode = code;
}

EMIT_FUNC(if, AstIfWhile* if_node) {
    bh_arr(WasmInstruction) code = *pcode;

    if (if_node->initialization != NULL) {
        // bh_imap_put(&mod->local_map, (u64) if_node->local, local_allocate(mod->local_alloc, (AstTyped *) if_node->local));

        forll (AstNode, stmt, if_node->initialization, next) {
            emit_statement(mod, &code, stmt);
        }
    }

    if (if_node->kind == Ast_Kind_Static_If) {
        if (static_if_resolution(if_node)) {
            if (if_node->true_stmt) emit_block(mod, &code, if_node->true_stmt, 1);
        } else {
            if (if_node->false_stmt) emit_block(mod, &code, if_node->false_stmt, 1);
        }

        *pcode = code;
        return;
    }

    emit_expression(mod, &code, if_node->cond);

    emit_enter_structured_block(mod, &code, SBT_Basic_If);
    if (if_node->true_stmt) emit_block(mod, &code, if_node->true_stmt, 0);

    if (if_node->false_stmt) {
        WI(WI_ELSE);

        if (if_node->false_stmt->kind == Ast_Kind_If) {
            emit_if(mod, &code, (AstIfWhile *) if_node->false_stmt);
        } else {
            emit_block(mod, &code, if_node->false_stmt, 0);
        }
    }

    emit_leave_structured_block(mod, &code);

    *pcode = code;
}

EMIT_FUNC(while, AstIfWhile* while_node) {
    bh_arr(WasmInstruction) code = *pcode;

    if (while_node->initialization != NULL) {
        forll (AstNode, stmt, while_node->initialization, next) {
            emit_statement(mod, &code, stmt);
        }
    }

    if (while_node->false_stmt == NULL) {
        emit_enter_structured_block(mod, &code, SBT_Breakable_Block);
        emit_enter_structured_block(mod, &code, SBT_Continue_Loop);

        emit_expression(mod, &code, while_node->cond);
        WI(WI_I32_EQZ);
        WID(WI_COND_JUMP, 0x01);

        emit_block(mod, &code, while_node->true_stmt, 0);

        if (bh_arr_last(code).type != WI_JUMP)
            WID(WI_JUMP, 0x00);

        emit_leave_structured_block(mod, &code);
        emit_leave_structured_block(mod, &code);

    } else {
        emit_expression(mod, &code, while_node->cond);

        emit_enter_structured_block(mod, &code, SBT_Breakable_If);
        emit_enter_structured_block(mod, &code, SBT_Continue_Loop);

        emit_block(mod, &code, while_node->true_stmt, 0);

        emit_expression(mod, &code, while_node->cond);
        WID(WI_COND_JUMP, 0x00);

        emit_leave_structured_block(mod, &code);
        WI(WI_ELSE);

        emit_block(mod, &code, while_node->false_stmt, 0);

        emit_leave_structured_block(mod, &code);
    }

    *pcode = code;
}

EMIT_FUNC(for_range, AstFor* for_node, u64 iter_local) {
    bh_arr(WasmInstruction) code = *pcode;

    // NOTE: There are some aspects of the code below that rely on the
    // low, high, and step members to be i32's. This restriction can be lifted,
    // but it is important to change the code here.
    //                                              -brendanfh   2020/09/04

    AstLocal* var = for_node->var;
    u64 offset = 0;

    StructMember low_mem, high_mem, step_mem;
    type_lookup_member(builtin_range_type_type, "low", &low_mem);
    type_lookup_member(builtin_range_type_type, "high", &high_mem);
    type_lookup_member(builtin_range_type_type, "step", &step_mem);
    u64 low_local  = local_raw_allocate(mod->local_alloc, onyx_type_to_wasm_type(low_mem.type));
    u64 high_local = local_raw_allocate(mod->local_alloc, onyx_type_to_wasm_type(high_mem.type));
    u64 step_local = local_raw_allocate(mod->local_alloc, onyx_type_to_wasm_type(step_mem.type));

    WIL(WI_LOCAL_SET, step_local);
    WIL(WI_LOCAL_SET, high_local);
    WIL(WI_LOCAL_TEE, low_local);
    WIL(WI_LOCAL_SET, iter_local);

    emit_enter_structured_block(mod, &code, SBT_Breakable_Block);
    emit_enter_structured_block(mod, &code, SBT_Basic_Loop);
    emit_enter_structured_block(mod, &code, SBT_Continue_Block);

    WIL(WI_LOCAL_GET, iter_local);
    WIL(WI_LOCAL_GET, high_local);
    WI(WI_I32_GE_S);
    WID(WI_COND_JUMP, 0x02);

    emit_block(mod, &code, for_node->stmt, 0);

    emit_leave_structured_block(mod, &code);

    WIL(WI_LOCAL_GET, iter_local);
    WIL(WI_LOCAL_GET, step_local);
    WI(WI_I32_ADD);
    WIL(WI_LOCAL_SET, iter_local);

    if (bh_arr_last(code).type != WI_JUMP)
        WID(WI_JUMP, 0x00);

    emit_leave_structured_block(mod, &code);
    emit_leave_structured_block(mod, &code);

    local_raw_free(mod->local_alloc, onyx_type_to_wasm_type(low_mem.type));
    local_raw_free(mod->local_alloc, onyx_type_to_wasm_type(high_mem.type));
    local_raw_free(mod->local_alloc, onyx_type_to_wasm_type(step_mem.type));

    *pcode = code;
}

EMIT_FUNC(for_array, AstFor* for_node, u64 iter_local) {
    bh_arr(WasmInstruction) code = *pcode;

    u64 end_ptr_local, ptr_local;
    end_ptr_local = local_raw_allocate(mod->local_alloc, WASM_TYPE_PTR);

    if (for_node->by_pointer) {
        ptr_local = iter_local;
    } else {
        ptr_local = local_raw_allocate(mod->local_alloc, WASM_TYPE_PTR);
    }

    AstLocal* var = for_node->var;
    b32 it_is_local = (b32) ((iter_local & LOCAL_IS_WASM) != 0);
    u64 offset = 0;

    u64 elem_size;
    if (for_node->by_pointer) elem_size = type_size_of(var->type->Pointer.elem);
    else                      elem_size = type_size_of(var->type);

    WIL(WI_LOCAL_TEE, ptr_local);
    WIL(WI_PTR_CONST, for_node->iter->type->Array.count * elem_size);
    WI(WI_PTR_ADD);
    WIL(WI_LOCAL_SET, end_ptr_local);

    emit_enter_structured_block(mod, &code, SBT_Breakable_Block);
    emit_enter_structured_block(mod, &code, SBT_Basic_Loop);
    emit_enter_structured_block(mod, &code, SBT_Continue_Block);

    WIL(WI_LOCAL_GET, ptr_local);
    WIL(WI_LOCAL_GET, end_ptr_local);
    WI(WI_PTR_GE);
    WID(WI_COND_JUMP, 0x02);

    if (!for_node->by_pointer) {
        if (!it_is_local) emit_local_location(mod, &code, var, &offset);

        WIL(WI_LOCAL_GET, ptr_local);
        emit_load_instruction(mod, &code, var->type, 0);

        if (!it_is_local) emit_store_instruction(mod, &code, var->type, offset);
        else              WIL(WI_LOCAL_SET, iter_local);
    }

    emit_block(mod, &code, for_node->stmt, 0);

    emit_leave_structured_block(mod, &code);

    WIL(WI_LOCAL_GET, ptr_local);
    WIL(WI_PTR_CONST, elem_size);
    WI(WI_PTR_ADD);
    WIL(WI_LOCAL_SET, ptr_local);

    if (bh_arr_last(code).type != WI_JUMP)
        WID(WI_JUMP, 0x00);

    emit_leave_structured_block(mod, &code);
    emit_leave_structured_block(mod, &code);

    local_raw_free(mod->local_alloc, WASM_TYPE_PTR);
    if (!for_node->by_pointer) local_raw_free(mod->local_alloc, WASM_TYPE_PTR);

    *pcode = code;
}

EMIT_FUNC(for_slice, AstFor* for_node, u64 iter_local) {
    bh_arr(WasmInstruction) code = *pcode;

    u64 end_ptr_local, ptr_local;
    end_ptr_local = local_raw_allocate(mod->local_alloc, WASM_TYPE_PTR);

    if (for_node->by_pointer) {
        ptr_local = iter_local;
    } else {
        ptr_local = local_raw_allocate(mod->local_alloc, WASM_TYPE_PTR);
    }

    AstLocal* var = for_node->var;
    b32 it_is_local = (b32) ((iter_local & LOCAL_IS_WASM) != 0);
    u64 offset = 0;

    u64 elem_size;
    if (for_node->by_pointer) elem_size = type_size_of(var->type->Pointer.elem);
    else                      elem_size = type_size_of(var->type);

    WIL(WI_LOCAL_SET, end_ptr_local);
    WIL(WI_LOCAL_TEE, ptr_local);
    WIL(WI_LOCAL_GET, end_ptr_local);
    if (elem_size != 1) {
        WID(WI_PTR_CONST, elem_size);
        WI(WI_PTR_MUL);
    }
    WI(WI_PTR_ADD);
    WIL(WI_LOCAL_SET, end_ptr_local);

    emit_enter_structured_block(mod, &code, SBT_Breakable_Block);
    emit_enter_structured_block(mod, &code, SBT_Basic_Loop);
    emit_enter_structured_block(mod, &code, SBT_Continue_Block);

    WIL(WI_LOCAL_GET, ptr_local);
    WIL(WI_LOCAL_GET, end_ptr_local);
    WI(WI_PTR_GE);
    WID(WI_COND_JUMP, 0x02);

    if (!for_node->by_pointer) {
        if (!it_is_local) emit_local_location(mod, &code, var, &offset);

        WIL(WI_LOCAL_GET, ptr_local);
        emit_load_instruction(mod, &code, var->type, 0);

        if (!it_is_local) emit_store_instruction(mod, &code, var->type, offset);
        else              WIL(WI_LOCAL_SET, iter_local);
    }

    emit_block(mod, &code, for_node->stmt, 0);

    emit_leave_structured_block(mod, &code);

    WIL(WI_LOCAL_GET, ptr_local);
    WIL(WI_PTR_CONST, elem_size);
    WI(WI_PTR_ADD);
    WIL(WI_LOCAL_SET, ptr_local);

    if (bh_arr_last(code).type != WI_JUMP)
        WID(WI_JUMP, 0x00);

    emit_leave_structured_block(mod, &code);
    emit_leave_structured_block(mod, &code);

    local_raw_free(mod->local_alloc, WASM_TYPE_PTR);
    if (!for_node->by_pointer) local_raw_free(mod->local_alloc, WASM_TYPE_PTR);

    *pcode = code;
}

EMIT_FUNC(for_iterator, AstFor* for_node, u64 iter_local) {
    bh_arr(WasmInstruction) code = *pcode;

    // Allocate temporaries for iterator contents
    u64 iterator_data_ptr   = local_raw_allocate(mod->local_alloc, WASM_TYPE_PTR);
    u64 iterator_next_func  = local_raw_allocate(mod->local_alloc, WASM_TYPE_FUNC);
    u64 iterator_close_func = local_raw_allocate(mod->local_alloc, WASM_TYPE_FUNC);
    u64 iterator_done_bool  = local_raw_allocate(mod->local_alloc, WASM_TYPE_INT32);
    WIL(WI_LOCAL_SET, iterator_close_func);
    WIL(WI_LOCAL_SET, iterator_next_func);
    WIL(WI_LOCAL_SET, iterator_data_ptr);

    AstLocal* var = for_node->var;
    b32 it_is_local = (b32) ((iter_local & LOCAL_IS_WASM) != 0);
    u64 offset = 0;

    // Enter a deferred statement for the auto-close
    emit_enter_structured_block(mod, &code, SBT_Basic_Block);

    TypeWithOffset close_func_type;
    type_linear_member_lookup(for_node->iter->type, 2, &close_func_type);
    i32 close_type_idx = generate_type_idx(mod, close_func_type.type);

    WasmInstruction* close_instructions = bh_alloc_array(global_heap_allocator, WasmInstruction, 8);
    close_instructions[0] = (WasmInstruction) { WI_LOCAL_GET,     { .l = iterator_close_func } };
    close_instructions[1] = (WasmInstruction) { WI_I32_CONST,     { .l = mod->null_proc_func_idx } };
    close_instructions[2] = (WasmInstruction) { WI_I32_NE,        { .l = 0x00 } };
    close_instructions[3] = (WasmInstruction) { WI_IF_START,      { .l = 0x40 } };
    close_instructions[4] = (WasmInstruction) { WI_LOCAL_GET,     { .l = iterator_data_ptr } };
    close_instructions[5] = (WasmInstruction) { WI_LOCAL_GET,     { .l = iterator_close_func } };
    close_instructions[6] = (WasmInstruction) { WI_CALL_INDIRECT, { .l = close_type_idx } };
    close_instructions[7] = (WasmInstruction) { WI_IF_END,        { .l = 0x00 } };

    emit_defer_code(mod, &code, close_instructions, 8);
    
    emit_enter_structured_block(mod, &code, SBT_Breakable_Block);
    emit_enter_structured_block(mod, &code, SBT_Continue_Loop);

    if (!it_is_local) emit_local_location(mod, &code, var, &offset);

    {
        WIL(WI_LOCAL_GET, iterator_data_ptr);
        WIL(WI_LOCAL_GET, iterator_next_func);

        // CLEANUP: Calling a function is way too f-ing complicated. FACTOR IT!!
        u64 stack_top_idx = bh_imap_get(&mod->index_map, (u64) &builtin_stack_top);
        
        TypeWithOffset next_func_type;
        type_linear_member_lookup(for_node->iter->type, 1, &next_func_type);
        Type* return_type = next_func_type.type->Function.return_type;

        u32 return_size = type_size_of(return_type);
        u32 return_align = type_alignment_of(return_type);
        bh_align(return_size, return_align);

        u64 reserve_size = return_size;
        bh_align(reserve_size, 16);
        
        WID(WI_GLOBAL_GET, stack_top_idx);
        WID(WI_PTR_CONST, reserve_size);
        WI(WI_PTR_ADD);
        WID(WI_GLOBAL_SET, stack_top_idx);

        i32 type_idx = generate_type_idx(mod, next_func_type.type);
        WID(WI_CALL_INDIRECT, ((WasmInstructionData) { type_idx, 0x00 }));

        WID(WI_GLOBAL_GET, stack_top_idx);
        WID(WI_PTR_CONST, reserve_size);
        WI(WI_PTR_SUB);
        WID(WI_GLOBAL_SET, stack_top_idx);

        WID(WI_GLOBAL_GET, stack_top_idx);
        emit_load_instruction(mod, &code, return_type, reserve_size - return_size);
    }

    WIL(WI_LOCAL_SET, iterator_done_bool);

    if (!it_is_local) emit_store_instruction(mod, &code, var->type, offset);
    else              WIL(WI_LOCAL_SET, iter_local);

    WIL(WI_LOCAL_GET, iterator_done_bool);
    WI(WI_I32_EQZ);
    WID(WI_COND_JUMP, 0x01);

    emit_block(mod, &code, for_node->stmt, 0);
    WID(WI_JUMP, 0x00); 

    emit_leave_structured_block(mod, &code);
    emit_leave_structured_block(mod, &code);

    emit_deferred_stmts(mod, &code);
    emit_leave_structured_block(mod, &code);

    local_raw_free(mod->local_alloc, WASM_TYPE_PTR);
    local_raw_free(mod->local_alloc, WASM_TYPE_FUNC);
    local_raw_free(mod->local_alloc, WASM_TYPE_FUNC);
    local_raw_free(mod->local_alloc, WASM_TYPE_INT32);
    *pcode = code;
}

EMIT_FUNC(for, AstFor* for_node) {
    bh_arr(WasmInstruction) code = *pcode;

    AstLocal* var = for_node->var;
    u64 iter_local = local_allocate(mod->local_alloc, (AstTyped *) var);
    bh_imap_put(&mod->local_map, (u64) var, iter_local);

    emit_expression(mod, &code, for_node->iter);

    switch (for_node->loop_type) {
        case For_Loop_Range:    emit_for_range(mod, &code, for_node, iter_local); break;
        case For_Loop_Array:    emit_for_array(mod, &code, for_node, iter_local); break;
        // NOTE: A dynamic array is just a slice with a capacity and allocator on the end.
        // Just dropping the extra fields will mean we can just use the slice implementation.
        //                                                  - brendanfh   2020/09/04
        //                                                  - brendanfh   2021/04/13
        case For_Loop_DynArr:   WI(WI_DROP); WI(WI_DROP); WI(WI_DROP);
        case For_Loop_Slice:    emit_for_slice(mod, &code, for_node, iter_local); break;
        case For_Loop_Iterator: emit_for_iterator(mod, &code, for_node, iter_local); break;
        default: onyx_report_error(for_node->token->pos, "Invalid for loop type. You should probably not be seeing this...");
    }

    local_free(mod->local_alloc, (AstTyped *) var);

    *pcode = code;
}

EMIT_FUNC(switch, AstSwitch* switch_node) {
    bh_arr(WasmInstruction) code = *pcode;

    bh_imap block_map;
    bh_imap_init(&block_map, global_heap_allocator, bh_arr_length(switch_node->cases));

    if (switch_node->initialization != NULL) {
        forll (AstNode, stmt, switch_node->initialization, next) {
            emit_statement(mod, &code, stmt);
        }
    }

    emit_enter_structured_block(mod, &code, SBT_Breakable_Block);

    u64 block_num = 0;
    bh_arr_each(AstSwitchCase, sc, switch_node->cases) {
        if (bh_imap_has(&block_map, (u64) sc->block)) continue;

        emit_enter_structured_block(mod, &code, SBT_Fallthrough_Block);

        bh_imap_put(&block_map, (u64) sc->block, block_num);
        block_num++;
    }

    u64 count = switch_node->max_case + 1 - switch_node->min_case;
    BranchTable* bt = bh_alloc(mod->extended_instr_alloc, sizeof(BranchTable) + sizeof(u32) * count);
    bt->count = count;
    bt->default_case = block_num;
    fori (i, 0, bt->count) bt->cases[i] = bt->default_case;

    bh_arr_each(bh__imap_entry, sc, switch_node->case_map.entries) {
        bt->cases[sc->key - switch_node->min_case] = bh_imap_get(&block_map, (u64) sc->value);
    }

    // CLEANUP: We enter a new block here in order to setup the correct
    // indicies for the jump targets in the branch table. For example,
    //
    // <expr>
    // jump_table
    // label0:
    // ...
    // label1:
    // ...
    //
    // If we didn't enter a new block, then jumping to label 0, would jump
    // to the second block, and so on.
    WID(WI_BLOCK_START, 0x40);
    emit_expression(mod, &code, switch_node->expr);
    if (switch_node->min_case != 0) {
        WID(WI_I32_CONST, switch_node->min_case);
        WI(WI_I32_SUB);
    }
    WIP(WI_JUMP_TABLE, bt);
    WI(WI_BLOCK_END);

    bh_arr_each(AstSwitchCase, sc, switch_node->cases) {
        if (bh_imap_get(&block_map, (u64) sc->block) == 0xdeadbeef) continue;

        u64 bn = bh_imap_get(&block_map, (u64) sc->block);

        emit_block(mod, &code, sc->block, 0);

        if (bh_arr_last(code).type != WI_JUMP)
            WID(WI_JUMP, block_num - bn);

        emit_leave_structured_block(mod, &code);

        bh_imap_put(&block_map, (u64) sc->block, 0xdeadbeef);
    }

    if (switch_node->default_case != NULL) {
        emit_block(mod, &code, switch_node->default_case, 0);
    }

    emit_leave_structured_block(mod, &code);

    bh_imap_free(&block_map);
    *pcode = code;
}

EMIT_FUNC(defer, AstDefer* defer) {
    bh_arr_push(mod->deferred_stmts, ((DeferredStmt) {
        .type = Deferred_Stmt_Node,
        .depth = bh_arr_length(mod->structured_jump_target),
        .stmt = defer->stmt,
    }));
}

EMIT_FUNC(defer_code, WasmInstruction* deferred_code, u32 code_count) {
    bh_arr_push(mod->deferred_stmts, ((DeferredStmt) {
        .type = Deferred_Stmt_Code,
        .depth = bh_arr_length(mod->structured_jump_target),
        .instructions = deferred_code,
        .instruction_count = code_count,
    }));
}

EMIT_FUNC(deferred_stmt, DeferredStmt deferred_stmt) {
    bh_arr(WasmInstruction) code = *pcode;

    switch (deferred_stmt.type) {
        case Deferred_Stmt_Node: emit_statement(mod, &code, deferred_stmt.stmt); break;
        case Deferred_Stmt_Code: {
            fori (i, 0, deferred_stmt.instruction_count) {
                bh_arr_push(code, deferred_stmt.instructions[i]);
            }
            break;
        }
    }

    *pcode = code;
}

EMIT_FUNC_NO_ARGS(deferred_stmts) {
    if (bh_arr_length(mod->deferred_stmts) <= 0) return;

    u64 depth = bh_arr_length(mod->structured_jump_target);

    while (bh_arr_length(mod->deferred_stmts) > 0 && bh_arr_last(mod->deferred_stmts).depth >= depth) {
        DeferredStmt stmt = bh_arr_pop(mod->deferred_stmts);
        emit_deferred_stmt(mod, pcode, stmt);
    }
}

// NOTE: These need to be in the same order as
// the OnyxBinaryOp enum
static const WasmInstructionType binop_map[][4] = {
    //           I32           I64           F32         F64
    /* ADD */  { WI_I32_ADD,   WI_I64_ADD,   WI_F32_ADD, WI_F64_ADD },
    /* SUB */  { WI_I32_SUB,   WI_I64_SUB,   WI_F32_SUB, WI_F64_SUB },
    /* MUL */  { WI_I32_MUL,   WI_I64_MUL,   WI_F32_MUL, WI_F64_MUL },
    /* DIV */  { WI_I32_DIV_S, WI_I64_DIV_S, WI_F32_DIV, WI_F64_DIV },
    /* REM */  { WI_I32_REM_S, WI_I64_REM_S, WI_NOP,     WI_NOP     },

    /* EQ  */  { WI_I32_EQ,    WI_I64_EQ,    WI_F32_EQ,  WI_F64_EQ },
    /* NEQ */  { WI_I32_NE,    WI_I64_NE,    WI_F32_NE , WI_F64_NE },
    /* LT  */  { WI_I32_LT_S,  WI_I64_LT_S,  WI_F32_LT,  WI_F64_LT },
    /* LTE */  { WI_I32_LE_S,  WI_I64_LE_S,  WI_F32_LE,  WI_F64_LE },
    /* GT  */  { WI_I32_GT_S,  WI_I64_GT_S,  WI_F32_GT,  WI_F64_GT },
    /* GTE */  { WI_I32_GE_S,  WI_I64_GE_S,  WI_F32_GE,  WI_F64_GE },

    /* AND */  { WI_I32_AND,   WI_I64_AND,   WI_NOP,     WI_NOP },
    /* OR  */  { WI_I32_OR,    WI_I64_OR,    WI_NOP,     WI_NOP },
    /* XOR */  { WI_I32_XOR,   WI_I64_XOR,   WI_NOP,     WI_NOP },
    /* SHL */  { WI_I32_SHL,   WI_I64_SHL,   WI_NOP,     WI_NOP },
    /* SHR */  { WI_I32_SHR_U, WI_I64_SHR_U, WI_NOP,     WI_NOP },
    /* SAR */  { WI_I32_SHR_S, WI_I64_SHR_S, WI_NOP,     WI_NOP },

    /* BAND */ { WI_I32_AND,   WI_I64_AND,   WI_NOP,     WI_NOP },
    /* BOR  */ { WI_I32_OR,    WI_I64_OR,    WI_NOP,     WI_NOP },
};

EMIT_FUNC(binop, AstBinaryOp* binop) {
    bh_arr(WasmInstruction) code = *pcode;

    if (binop_is_assignment(binop->operation)) {
        emit_assignment(mod, &code, binop);
        *pcode = code;
        return;
    }

    b32 is_sign_significant = 0;
    switch (binop->operation) {
        case Binary_Op_Divide:  case Binary_Op_Modulus:
        case Binary_Op_Less:    case Binary_Op_Less_Equal:
        case Binary_Op_Greater: case Binary_Op_Greater_Equal:
            is_sign_significant = 1;
    }

    WasmType operator_type = onyx_type_to_wasm_type(binop->left->type);
    i32 optype = 0;
    if      (operator_type == WASM_TYPE_INT32)   optype = 0;
    else if (operator_type == WASM_TYPE_INT64)   optype = 1;
    else if (operator_type == WASM_TYPE_FLOAT32) optype = 2;
    else if (operator_type == WASM_TYPE_FLOAT64) optype = 3;

    WasmInstructionType binop_instr = binop_map[(i32) binop->operation][optype];

    assert(binop_instr != WI_NOP);

    // NOTE: Use unsigned variant if needed
    // Unsigned instructions are always right after
    // the signed equivalent
    if (is_sign_significant) {
        if (binop->left->type->Basic.flags & Basic_Flag_Unsigned) {
            binop_instr = (WasmInstructionType) ((i32) binop_instr + 1);
        }
    }

    emit_expression(mod, &code, binop->left);
    emit_expression(mod, &code, binop->right);

    WI(binop_instr);

    *pcode = code;
}

EMIT_FUNC(unaryop, AstUnaryOp* unop) {
    bh_arr(WasmInstruction) code = *pcode;

    switch (unop->operation) {
        case Unary_Op_Negate: {
            TypeBasic* type = &unop->type->Basic;

            if (type->kind == Basic_Kind_I32
                    || type->kind == Basic_Kind_I16
                    || type->kind == Basic_Kind_I8) {
                WID(WI_I32_CONST, 0x00);
                emit_expression(mod, &code, unop->expr);
                WI(WI_I32_SUB);

            }
            else if (type->kind == Basic_Kind_I64) {
                WID(WI_I64_CONST, 0x00);
                emit_expression(mod, &code, unop->expr);
                WI(WI_I64_SUB);

            }
            else {
                emit_expression(mod, &code, unop->expr);

                if (type->kind == Basic_Kind_F32) WI(WI_F32_NEG);
                if (type->kind == Basic_Kind_F64) WI(WI_F64_NEG);
            }

            break;
        }

        case Unary_Op_Not:
            emit_expression(mod, &code, unop->expr);

            WI(WI_I32_EQZ);
            break;

        case Unary_Op_Bitwise_Not: {
            emit_expression(mod, &code, unop->expr);

            TypeBasic* type = &unop->type->Basic;

            if (type->kind == Basic_Kind_I8 || type->kind == Basic_Kind_U8) {
                WID(WI_I32_CONST, 0xff);
                WI(WI_I32_XOR);
            }
            else if (type->kind == Basic_Kind_I16 || type->kind == Basic_Kind_U16) {
                WID(WI_I32_CONST, 0xffff);
                WI(WI_I32_XOR);
            }
            else if (type->kind == Basic_Kind_I32 || type->kind == Basic_Kind_U32) {
                WID(WI_I32_CONST, 0xffffffff);
                WI(WI_I32_XOR);
            }
            else if (type->kind == Basic_Kind_I64 || type->kind == Basic_Kind_U64) {
                WIL(WI_I64_CONST, 0xffffffffffffffff);
                WI(WI_I64_XOR);
            }

            break;
        }

        case Unary_Op_Auto_Cast:
        case Unary_Op_Cast: emit_cast(mod, &code, unop); break;
    }

    *pcode = code;
}

// Calling a procedure in Onyx.
//
// This documentation should be placed elsewhere, but for right now I'm going to write it in the relevant
// piece of code. Calling a procedure is relatively simple, at least compared to other calling conventions
// out there, mostly due the fact that this is WebAssembly, where registers are "infinite" and there's no
// really need to use stack canaries for security.
//
// The biggest piece to understand is how the stack gets laid out for the called procedure. To be confusing,
// there are two stacks at play: the WASM expression stack, and the linear memory stack. Here is the general
// lay out for calling a procedure with the following signature.
//
//    foo :: (x: i32, y: str, z: [..] i32, va: ..i32) -> (i32, i32)
//
// WASM stack: top is last pushed
//
//    vararg count
//    vararg pointer to variadic arguments in the linear memory
//    pointer to struct-like arguments (z)
//    simple structs/primitives (y, x)
//
// Linear memory stack:
//  
//     ... | struct-like arguments (z) | variadic arguments (va) | return space | ...
//
// The interesting part from above is the fact that 'y' gets passed on the WASM stack, not the linear memory
// stack, even though a 'str' in Onyx is 2-component structure. This is because so-called "simple" structures,
// i.e. structures that are completely flat, with no sub-structures, are passed as multiple primitives. I do
// this because many times for interoperability, it is nicer to get two primitive values for the pointer and
// count of a slice, instead of a pointer.
EMIT_FUNC(call, AstCall* call) {
    bh_arr(WasmInstruction) code = *pcode;

    u64 stack_top_idx = bh_imap_get(&mod->index_map, (u64) &builtin_stack_top);
    u64 stack_top_store_local = local_raw_allocate(mod->local_alloc, WASM_TYPE_PTR);

    // Because it would be inefficient to increment and decrement the global stack pointer for every argument,
    // a simple set of instructions increments it once to the size it will need to be. However, because it is
    // impossible to know what size the reserved memory will be, a location patch is taken in order to fill it
    // in later.
    u32 reserve_space_patch = bh_arr_length(code);
    WID(WI_GLOBAL_GET, stack_top_idx);
    WIL(WI_LOCAL_TEE, stack_top_store_local);
    WID(WI_PTR_CONST, 0);                           // This will be filled in later.
    WI(WI_PTR_ADD);
    WID(WI_GLOBAL_SET, stack_top_idx);

    u32 reserve_size  = 0;
    u32 vararg_count  = 0;
    i32 vararg_offset = -1;

    u32* vararg_any_offsets=NULL;
    u32* vararg_any_types=NULL;
    if (call->va_kind == VA_Kind_Any) {
        vararg_any_offsets = bh_alloc_array(global_scratch_allocator, u32, bh_arr_length(call->args.values));
        vararg_any_types   = bh_alloc_array(global_scratch_allocator, u32, bh_arr_length(call->args.values));
    }

    bh_arr_each(AstTyped *, parg, call->args.values) {
        AstArgument* arg = (AstArgument *) *parg;
        if (arg->is_baked) continue;

        b32 place_on_stack = 0;

        if (type_get_param_pass(arg->value->type) == Param_Pass_By_Implicit_Pointer) {
            // This arguments needs to be written to the stack because it is not a simple structure.
            place_on_stack = 1;
        }

        if (arg->va_kind != VA_Kind_Not_VA) {
            // This is a variadic argument and needs to be written to the stack. If the starting
            // location of the vararg array hasn't been noted, note it.
            if (vararg_offset < 0) vararg_offset = reserve_size;

            place_on_stack = 1;
            vararg_count += 1;
        }

        if (arg->pass_as_any) {
            place_on_stack = 1;
        }

        if (place_on_stack) WIL(WI_LOCAL_GET, stack_top_store_local);

        emit_expression(mod, &code, arg->value);

        if (place_on_stack) {
            emit_store_instruction(mod, &code, arg->value->type, reserve_size);

            if (arg->va_kind == VA_Kind_Not_VA) {
                // Non-variadic arguments on the stack need a pointer to them placed on the WASM stack.
                WIL(WI_LOCAL_GET, stack_top_store_local);
                WID(WI_PTR_CONST, reserve_size);
                WI(WI_PTR_ADD);
            }

            if (arg->va_kind == VA_Kind_Any) {
                vararg_any_offsets[vararg_count - 1] = reserve_size;
                vararg_any_types[vararg_count - 1] = arg->value->type->id;
            }

            if (arg->pass_as_any) {
                WIL(WI_I32_CONST, arg->value->type->id);
            }

            reserve_size += type_size_of(arg->value->type);
        }
    }

    switch (call->va_kind) {
        case VA_Kind_Any: {
            vararg_offset = reserve_size;

            i32 any_size = type_size_of(type_build_from_ast(context.ast_alloc, builtin_any_type));

            fori (i, 0, vararg_count) {
                WIL(WI_LOCAL_GET, stack_top_store_local);
                WIL(WI_LOCAL_GET, stack_top_store_local);
                WID(WI_PTR_CONST, vararg_any_offsets[i]);
                WI(WI_PTR_ADD);
                emit_store_instruction(mod, &code, &basic_types[Basic_Kind_Rawptr], vararg_offset + i * any_size);

                WIL(WI_LOCAL_GET, stack_top_store_local);
                WID(WI_I32_CONST, vararg_any_types[i]);
                emit_store_instruction(mod, &code, &basic_types[Basic_Kind_Type_Index], vararg_offset + i * any_size + 8);

                reserve_size += any_size;
            }

            // fallthrough
        }

        case VA_Kind_Typed: {
            WIL(WI_LOCAL_GET, stack_top_store_local);
            if (vararg_offset > 0) {
                WID(WI_PTR_CONST, vararg_offset);
                WI(WI_PTR_ADD);
            }
            WID(WI_I32_CONST, vararg_count);
            break;
        }

        case VA_Kind_Untyped: {
            WIL(WI_LOCAL_GET, stack_top_store_local);
            WIL(WI_LOCAL_GET, stack_top_store_local);
            if (vararg_offset > 0) {
                WID(WI_PTR_CONST, vararg_offset);
                WI(WI_PTR_ADD);
            }
            emit_store_instruction(mod, &code, &basic_types[Basic_Kind_Rawptr], reserve_size);

            // NOTE: There will be 4 uninitialized bytes here, because pointers are only 4 bytes in WASM.

            WIL(WI_LOCAL_GET, stack_top_store_local);
            WID(WI_I32_CONST, vararg_count);
            emit_store_instruction(mod, &code, &basic_types[Basic_Kind_I32], reserve_size + 8);

            WIL(WI_LOCAL_GET, stack_top_store_local);
            if (reserve_size > 0) {
                WID(WI_PTR_CONST, reserve_size);
                WI(WI_PTR_ADD);
            }

            reserve_size += 12;
            break;
        }
    }

    CallingConvention cc = type_function_get_cc(call->callee->type);
    assert(cc != CC_Undefined);

    Type* return_type = call->callee->type->Function.return_type;
    u32 return_size = type_size_of(return_type);
    assert(return_size % type_alignment_of(return_type) == 0);

    if (cc == CC_Return_Stack) reserve_size += return_size;

    if (call->callee->kind == Ast_Kind_Function) {
        i32 func_idx = (i32) bh_imap_get(&mod->index_map, (u64) call->callee);
        bh_arr_push(code, ((WasmInstruction){ WI_CALL, func_idx }));

    } else {
        emit_expression(mod, &code, call->callee);

        i32 type_idx = generate_type_idx(mod, call->callee->type);
        WID(WI_CALL_INDIRECT, ((WasmInstructionData) { type_idx, 0x00 }));
    }

    if (reserve_size > 0) {
        WIL(WI_LOCAL_GET,  stack_top_store_local);
        WID(WI_GLOBAL_SET, stack_top_idx);

        bh_align(reserve_size, 16);
        code[reserve_space_patch + 2].data.l = reserve_size;

    } else {
        fori (i, 0, 5) code[reserve_space_patch + i].type = WI_NOP;
    }

    if (cc == CC_Return_Stack) {
        WID(WI_GLOBAL_GET, stack_top_idx);
        emit_load_instruction(mod, &code, return_type, reserve_size - return_size);
    }

    local_raw_free(mod->local_alloc, WASM_TYPE_PTR);
    *pcode = code;
}

// BUG: This implementation assumes that the host system C's implementation is using
// little endian integers.
#define SIMD_INT_CONST_INTRINSIC(type, count) { \
        type* byte_buffer = bh_alloc(mod->extended_instr_alloc, 16); \
        bh_arr(AstArgument *) arg_arr = (bh_arr(AstArgument *)) call->args.values; \
        fori (i, 0, count) { \
            if (arg_arr[i]->value->kind != Ast_Kind_NumLit) { \
                onyx_report_error(arg_arr[i]->token->pos, \
                        "SIMD constants expect compile time constants as parameters. The %d%s parameter was not.", \
                        i, bh_num_suffix(i)); \
                *pcode = code; \
                return; \
            } \
            byte_buffer[i] = (type) ((AstNumLit *) arg_arr[i]->value)->value.l; \
        } \
        WIP(WI_V128_CONST, byte_buffer); \
    }

#define SIMD_EXTRACT_LANE_INSTR(instr, arg_arr) \
    emit_expression(mod, &code, arg_arr[0]->value);\
    if (arg_arr[1]->value->kind != Ast_Kind_NumLit) { \
        onyx_report_error(arg_arr[1]->token->pos, "SIMD lane instructions expect a compile time lane number."); \
        *pcode = code; \
        return; \
    } \
    WID(instr, (u8) ((AstNumLit *) arg_arr[1]->value)->value.i);

#define SIMD_REPLACE_LANE_INSTR(instr, arg_arr) { \
    emit_expression(mod, &code, arg_arr[0]->value);\
    if (arg_arr[1]->value->kind != Ast_Kind_NumLit) { \
        onyx_report_error(arg_arr[1]->token->pos, "SIMD lane instructions expect a compile time lane number."); \
        *pcode = code; \
        return; \
    } \
    u8 lane = (u8) ((AstNumLit *) arg_arr[1]->value)->value.i; \
    emit_expression(mod, &code, arg_arr[2]->value); \
    WID(instr, lane); \
}


EMIT_FUNC(intrinsic_call, AstCall* call) {
    bh_arr(WasmInstruction) code = *pcode;

    b32 place_arguments_normally = 1;

    switch (call->intrinsic) {
        case ONYX_INTRINSIC_V128_CONST:
        case ONYX_INTRINSIC_I8X16_CONST: case ONYX_INTRINSIC_I16X8_CONST:
        case ONYX_INTRINSIC_I32X4_CONST: case ONYX_INTRINSIC_I64X2_CONST:
        case ONYX_INTRINSIC_F32X4_CONST: case ONYX_INTRINSIC_F64X2_CONST:
        case ONYX_INTRINSIC_I8X16_EXTRACT_LANE_S: case ONYX_INTRINSIC_I8X16_EXTRACT_LANE_U:
        case ONYX_INTRINSIC_I16X8_EXTRACT_LANE_S: case ONYX_INTRINSIC_I16X8_EXTRACT_LANE_U:
        case ONYX_INTRINSIC_I32X4_EXTRACT_LANE:   case ONYX_INTRINSIC_I64X2_EXTRACT_LANE:
        case ONYX_INTRINSIC_F32X4_EXTRACT_LANE:   case ONYX_INTRINSIC_F64X2_EXTRACT_LANE:
        case ONYX_INTRINSIC_I8X16_REPLACE_LANE:   case ONYX_INTRINSIC_I16X8_REPLACE_LANE:
        case ONYX_INTRINSIC_I32X4_REPLACE_LANE:   case ONYX_INTRINSIC_I64X2_REPLACE_LANE:
        case ONYX_INTRINSIC_F32X4_REPLACE_LANE:   case ONYX_INTRINSIC_F64X2_REPLACE_LANE:
        case ONYX_INTRINSIC_I8X16_SHUFFLE:
            place_arguments_normally = 0;

        default: break;
    }

    if (place_arguments_normally) {
        bh_arr_each(AstTyped *, arg, call->args.values) {
            emit_expression(mod, &code, *arg);
        }
    }

    switch (call->intrinsic) {
        case ONYX_INTRINSIC_MEMORY_SIZE:  WID(WI_MEMORY_SIZE, 0x00); break;
        case ONYX_INTRINSIC_MEMORY_GROW:  WID(WI_MEMORY_GROW, 0x00); break;
        case ONYX_INTRINSIC_MEMORY_COPY:
            if (context.options->use_post_mvp_features) {
                WIL(WI_MEMORY_COPY, 0x00);
            } else {
                emit_intrinsic_memory_copy(mod, &code);
            }
            break;
        case ONYX_INTRINSIC_MEMORY_FILL:        
            if (context.options->use_post_mvp_features) {
                WIL(WI_MEMORY_FILL, 0x00);
            } else {
                emit_intrinsic_memory_fill(mod, &code);
            }
            break;

        case ONYX_INTRINSIC_INITIALIZE: {
            Type* type_to_initialize = ((AstArgument *) call->args.values[0])->value->type->Pointer.elem;
            emit_initialize_type(mod, &code, type_to_initialize, call->token);
            break;
        }

        case ONYX_INTRINSIC_ZERO_VALUE: {
            // NOTE: This probably will not have to make an allocation.
            Type* zero_type = type_build_from_ast(context.ast_alloc, (AstType *) ((AstArgument *) call->original_args.values[0])->value);
            emit_zero_value_for_type(mod, &code, zero_type, call->token);
            break;
        }

        case ONYX_INTRINSIC_I32_CLZ:      WI(WI_I32_CLZ); break;
        case ONYX_INTRINSIC_I32_CTZ:      WI(WI_I32_CTZ); break;
        case ONYX_INTRINSIC_I32_POPCNT:   WI(WI_I32_POPCNT); break;
        case ONYX_INTRINSIC_I32_AND:      WI(WI_I32_AND); break;
        case ONYX_INTRINSIC_I32_OR:       WI(WI_I32_OR); break;
        case ONYX_INTRINSIC_I32_XOR:      WI(WI_I32_XOR); break;
        case ONYX_INTRINSIC_I32_SHL:      WI(WI_I32_SHL); break;
        case ONYX_INTRINSIC_I32_SLR:      WI(WI_I32_SHR_U); break;
        case ONYX_INTRINSIC_I32_SAR:      WI(WI_I32_SHR_S); break;
        case ONYX_INTRINSIC_I32_ROTL:     WI(WI_I32_ROTL); break;
        case ONYX_INTRINSIC_I32_ROTR:     WI(WI_I32_ROTR); break;

        case ONYX_INTRINSIC_I64_CLZ:      WI(WI_I64_CLZ); break;
        case ONYX_INTRINSIC_I64_CTZ:      WI(WI_I64_CTZ); break;
        case ONYX_INTRINSIC_I64_POPCNT:   WI(WI_I64_POPCNT); break;
        case ONYX_INTRINSIC_I64_AND:      WI(WI_I64_AND); break;
        case ONYX_INTRINSIC_I64_OR:       WI(WI_I64_OR); break;
        case ONYX_INTRINSIC_I64_XOR:      WI(WI_I64_XOR); break;
        case ONYX_INTRINSIC_I64_SHL:      WI(WI_I64_SHL); break;
        case ONYX_INTRINSIC_I64_SLR:      WI(WI_I64_SHR_U); break;
        case ONYX_INTRINSIC_I64_SAR:      WI(WI_I64_SHR_S); break;
        case ONYX_INTRINSIC_I64_ROTL:     WI(WI_I64_ROTL); break;
        case ONYX_INTRINSIC_I64_ROTR:     WI(WI_I64_ROTR); break;

        case ONYX_INTRINSIC_F32_ABS:      WI(WI_F32_ABS); break;
        case ONYX_INTRINSIC_F32_CEIL:     WI(WI_F32_CEIL); break;
        case ONYX_INTRINSIC_F32_FLOOR:    WI(WI_F32_FLOOR); break;
        case ONYX_INTRINSIC_F32_TRUNC:    WI(WI_F32_TRUNC); break;
        case ONYX_INTRINSIC_F32_NEAREST:  WI(WI_F32_NEAREST); break;
        case ONYX_INTRINSIC_F32_SQRT:     WI(WI_F32_SQRT); break;
        case ONYX_INTRINSIC_F32_MIN:      WI(WI_F32_MIN); break;
        case ONYX_INTRINSIC_F32_MAX:      WI(WI_F32_MAX); break;
        case ONYX_INTRINSIC_F32_COPYSIGN: WI(WI_F32_COPYSIGN); break;

        case ONYX_INTRINSIC_F64_ABS:      WI(WI_F64_ABS); break;
        case ONYX_INTRINSIC_F64_CEIL:     WI(WI_F64_CEIL); break;
        case ONYX_INTRINSIC_F64_FLOOR:    WI(WI_F64_FLOOR); break;
        case ONYX_INTRINSIC_F64_TRUNC:    WI(WI_F64_TRUNC); break;
        case ONYX_INTRINSIC_F64_NEAREST:  WI(WI_F64_NEAREST); break;
        case ONYX_INTRINSIC_F64_SQRT:     WI(WI_F64_SQRT); break;
        case ONYX_INTRINSIC_F64_MIN:      WI(WI_F64_MIN); break;
        case ONYX_INTRINSIC_F64_MAX:      WI(WI_F64_MAX); break;
        case ONYX_INTRINSIC_F64_COPYSIGN: WI(WI_F64_COPYSIGN); break;

        case ONYX_INTRINSIC_I8X16_CONST:
        case ONYX_INTRINSIC_V128_CONST:   SIMD_INT_CONST_INTRINSIC(u8, 16);   break;
        case ONYX_INTRINSIC_I16X8_CONST:  SIMD_INT_CONST_INTRINSIC(u16, 8);   break;
        case ONYX_INTRINSIC_I32X4_CONST:  SIMD_INT_CONST_INTRINSIC(u32, 4);   break;
        case ONYX_INTRINSIC_I64X2_CONST:  SIMD_INT_CONST_INTRINSIC(u64, 2);   break;
        case ONYX_INTRINSIC_F32X4_CONST: {
            f32* byte_buffer = bh_alloc(mod->extended_instr_alloc, 16);
            bh_arr(AstArgument *) arg_arr = (bh_arr(AstArgument *)) call->args.values;
            fori (i, 0, 4) {
                if (arg_arr[i]->value->kind != Ast_Kind_NumLit) {
                    onyx_report_error(arg_arr[i]->token->pos,
                            "SIMD constants expect compile time constants as parameters. The %d%s parameter was not.",
                            i, bh_num_suffix(i));
                    *pcode = code;
                    return;
                }
                byte_buffer[i] = (f32) ((AstNumLit *) arg_arr[i]->value)->value.f;
            }
            WIP(WI_V128_CONST, byte_buffer);
            break;
        }

        case ONYX_INTRINSIC_F64X2_CONST: {
            f64* byte_buffer = bh_alloc(mod->extended_instr_alloc, 16);
            bh_arr(AstArgument *) arg_arr = (bh_arr(AstArgument *)) call->args.values;
            fori (i, 0, 2) {
                if (arg_arr[i]->value->kind != Ast_Kind_NumLit) {
                    onyx_report_error(arg_arr[i]->token->pos,
                            "SIMD constants expect compile time constants as parameters. The %d%s parameter was not.",
                            i, bh_num_suffix(i));
                    *pcode = code;
                    return;
                }
                byte_buffer[i] = (f64) ((AstNumLit *) arg_arr[i]->value)->value.d;
            }
            WIP(WI_V128_CONST, byte_buffer);
            break;
        }

        case ONYX_INTRINSIC_I8X16_SHUFFLE: {
            u8* byte_buffer = bh_alloc(mod->extended_instr_alloc, 16);
            bh_arr(AstArgument *) arg_arr = (bh_arr(AstArgument *)) call->args.values;

            // NOTE: There are two parameters that have to be outputted before
            // the immediate bytes
            emit_expression(mod, &code, arg_arr[0]->value);
            emit_expression(mod, &code, arg_arr[1]->value);

            fori (i, 0, 16) {
                if (arg_arr[i + 2]->value->kind != Ast_Kind_NumLit) {
                    onyx_report_error(arg_arr[i + 2]->token->pos,
                            "SIMD constants expect compile time constants as parameters. The %d%s parameter was not.",
                            i, bh_num_suffix(i));
                    *pcode = code;
                    return;
                }
                byte_buffer[i] = (u8) ((AstNumLit *) arg_arr[i + 2]->value)->value.i;
            }
            WIP(WI_I8X16_SHUFFLE, byte_buffer);
            break;
        }

        // CLEANUP ALL OF THIS
        case ONYX_INTRINSIC_I8X16_EXTRACT_LANE_S: SIMD_EXTRACT_LANE_INSTR(WI_I8X16_EXTRACT_LANE_S, ((bh_arr(AstArgument *)) call->args.values)); break;
        case ONYX_INTRINSIC_I8X16_EXTRACT_LANE_U: SIMD_EXTRACT_LANE_INSTR(WI_I8X16_EXTRACT_LANE_U, ((bh_arr(AstArgument *)) call->args.values)); break;
        case ONYX_INTRINSIC_I8X16_REPLACE_LANE:   SIMD_REPLACE_LANE_INSTR(WI_I8X16_REPLACE_LANE, ((bh_arr(AstArgument *)) call->args.values)); break;
        case ONYX_INTRINSIC_I16X8_EXTRACT_LANE_S: SIMD_EXTRACT_LANE_INSTR(WI_I16X8_EXTRACT_LANE_S, ((bh_arr(AstArgument *)) call->args.values)); break;
        case ONYX_INTRINSIC_I16X8_EXTRACT_LANE_U: SIMD_EXTRACT_LANE_INSTR(WI_I16X8_EXTRACT_LANE_U, ((bh_arr(AstArgument *)) call->args.values)); break;
        case ONYX_INTRINSIC_I16X8_REPLACE_LANE:   SIMD_REPLACE_LANE_INSTR(WI_I16X8_REPLACE_LANE, ((bh_arr(AstArgument *)) call->args.values)); break;
        case ONYX_INTRINSIC_I32X4_EXTRACT_LANE:   SIMD_EXTRACT_LANE_INSTR(WI_I32X4_EXTRACT_LANE, ((bh_arr(AstArgument *)) call->args.values)); break;
        case ONYX_INTRINSIC_I32X4_REPLACE_LANE:   SIMD_REPLACE_LANE_INSTR(WI_I32X4_REPLACE_LANE, ((bh_arr(AstArgument *)) call->args.values)); break;
        case ONYX_INTRINSIC_I64X2_EXTRACT_LANE:   SIMD_EXTRACT_LANE_INSTR(WI_I64X2_EXTRACT_LANE, ((bh_arr(AstArgument *)) call->args.values)); break;
        case ONYX_INTRINSIC_I64X2_REPLACE_LANE:   SIMD_REPLACE_LANE_INSTR(WI_I64X2_REPLACE_LANE, ((bh_arr(AstArgument *)) call->args.values)); break;
        case ONYX_INTRINSIC_F32X4_EXTRACT_LANE:   SIMD_EXTRACT_LANE_INSTR(WI_F32X4_EXTRACT_LANE, ((bh_arr(AstArgument *)) call->args.values)); break;
        case ONYX_INTRINSIC_F32X4_REPLACE_LANE:   SIMD_REPLACE_LANE_INSTR(WI_F32X4_REPLACE_LANE, ((bh_arr(AstArgument *)) call->args.values)); break;
        case ONYX_INTRINSIC_F64X2_EXTRACT_LANE:   SIMD_EXTRACT_LANE_INSTR(WI_F64X2_EXTRACT_LANE, ((bh_arr(AstArgument *)) call->args.values)); break;
        case ONYX_INTRINSIC_F64X2_REPLACE_LANE:   SIMD_REPLACE_LANE_INSTR(WI_F64X2_REPLACE_LANE, ((bh_arr(AstArgument *)) call->args.values)); break;

        case ONYX_INTRINSIC_I8X16_SWIZZLE: WI(WI_I8X16_SWIZZLE); break;
        case ONYX_INTRINSIC_I8X16_SPLAT:   WI(WI_I8X16_SPLAT); break;
        case ONYX_INTRINSIC_I16X8_SPLAT:   WI(WI_I16X8_SPLAT); break;
        case ONYX_INTRINSIC_I32X4_SPLAT:   WI(WI_I32X4_SPLAT); break;
        case ONYX_INTRINSIC_I64X2_SPLAT:   WI(WI_I64X2_SPLAT); break;
        case ONYX_INTRINSIC_F32X4_SPLAT:   WI(WI_F32X4_SPLAT); break;
        case ONYX_INTRINSIC_F64X2_SPLAT:   WI(WI_F64X2_SPLAT); break;

        case ONYX_INTRINSIC_I8X16_EQ:   WI(WI_I8X16_EQ); break;
        case ONYX_INTRINSIC_I8X16_NEQ:  WI(WI_I8X16_NEQ); break;
        case ONYX_INTRINSIC_I8X16_LT_S: WI(WI_I8X16_LT_S); break;
        case ONYX_INTRINSIC_I8X16_LT_U: WI(WI_I8X16_LT_U); break;
        case ONYX_INTRINSIC_I8X16_GT_S: WI(WI_I8X16_GT_S); break;
        case ONYX_INTRINSIC_I8X16_GT_U: WI(WI_I8X16_GT_U); break;
        case ONYX_INTRINSIC_I8X16_LE_S: WI(WI_I8X16_LE_S); break;
        case ONYX_INTRINSIC_I8X16_LE_U: WI(WI_I8X16_LE_U); break;
        case ONYX_INTRINSIC_I8X16_GE_S: WI(WI_I8X16_GE_S); break;
        case ONYX_INTRINSIC_I8X16_GE_U: WI(WI_I8X16_GE_U); break;

        case ONYX_INTRINSIC_I16X8_EQ:   WI(WI_I16X8_EQ); break;
        case ONYX_INTRINSIC_I16X8_NEQ:  WI(WI_I16X8_NEQ); break;
        case ONYX_INTRINSIC_I16X8_LT_S: WI(WI_I16X8_LT_S); break;
        case ONYX_INTRINSIC_I16X8_LT_U: WI(WI_I16X8_LT_U); break;
        case ONYX_INTRINSIC_I16X8_GT_S: WI(WI_I16X8_GT_S); break;
        case ONYX_INTRINSIC_I16X8_GT_U: WI(WI_I16X8_GT_U); break;
        case ONYX_INTRINSIC_I16X8_LE_S: WI(WI_I16X8_LE_S); break;
        case ONYX_INTRINSIC_I16X8_LE_U: WI(WI_I16X8_LE_U); break;
        case ONYX_INTRINSIC_I16X8_GE_S: WI(WI_I16X8_GE_S); break;
        case ONYX_INTRINSIC_I16X8_GE_U: WI(WI_I16X8_GE_U); break;

        case ONYX_INTRINSIC_I32X4_EQ:   WI(WI_I32X4_EQ); break;
        case ONYX_INTRINSIC_I32X4_NEQ:  WI(WI_I32X4_NEQ); break;
        case ONYX_INTRINSIC_I32X4_LT_S: WI(WI_I32X4_LT_S); break;
        case ONYX_INTRINSIC_I32X4_LT_U: WI(WI_I32X4_LT_U); break;
        case ONYX_INTRINSIC_I32X4_GT_S: WI(WI_I32X4_GT_S); break;
        case ONYX_INTRINSIC_I32X4_GT_U: WI(WI_I32X4_GT_U); break;
        case ONYX_INTRINSIC_I32X4_LE_S: WI(WI_I32X4_LE_S); break;
        case ONYX_INTRINSIC_I32X4_LE_U: WI(WI_I32X4_LE_U); break;
        case ONYX_INTRINSIC_I32X4_GE_S: WI(WI_I32X4_GE_S); break;
        case ONYX_INTRINSIC_I32X4_GE_U: WI(WI_I32X4_GE_U); break;

        case ONYX_INTRINSIC_F32X4_EQ:  WI(WI_F32X4_EQ); break;
        case ONYX_INTRINSIC_F32X4_NEQ: WI(WI_F32X4_NEQ); break;
        case ONYX_INTRINSIC_F32X4_LT:  WI(WI_F32X4_LT); break;
        case ONYX_INTRINSIC_F32X4_GT:  WI(WI_F32X4_GT); break;
        case ONYX_INTRINSIC_F32X4_LE:  WI(WI_F32X4_LE); break;
        case ONYX_INTRINSIC_F32X4_GE:  WI(WI_F32X4_GE); break;

        case ONYX_INTRINSIC_F64X2_EQ:  WI(WI_F64X2_EQ); break;
        case ONYX_INTRINSIC_F64X2_NEQ: WI(WI_F64X2_NEQ); break;
        case ONYX_INTRINSIC_F64X2_LT:  WI(WI_F64X2_LT); break;
        case ONYX_INTRINSIC_F64X2_GT:  WI(WI_F64X2_GT); break;
        case ONYX_INTRINSIC_F64X2_LE:  WI(WI_F64X2_LE); break;
        case ONYX_INTRINSIC_F64X2_GE:  WI(WI_F64X2_GE); break;

        case ONYX_INTRINSIC_V128_NOT:       WI(WI_V128_NOT); break;
        case ONYX_INTRINSIC_V128_AND:       WI(WI_V128_AND); break;
        case ONYX_INTRINSIC_V128_ANDNOT:    WI(WI_V128_ANDNOT); break;
        case ONYX_INTRINSIC_V128_OR:        WI(WI_V128_OR); break;
        case ONYX_INTRINSIC_V128_XOR:       WI(WI_V128_XOR); break;
        case ONYX_INTRINSIC_V128_BITSELECT: WI(WI_V128_BITSELECT); break;

        case ONYX_INTRINSIC_I8X16_ABS:            WI(WI_I8X16_ABS); break;
        case ONYX_INTRINSIC_I8X16_NEG:            WI(WI_I8X16_NEG); break;
        case ONYX_INTRINSIC_I8X16_ANY_TRUE:       WI(WI_I8X16_ANY_TRUE); break;
        case ONYX_INTRINSIC_I8X16_ALL_TRUE:       WI(WI_I8X16_ALL_TRUE); break;
        case ONYX_INTRINSIC_I8X16_BITMASK:        WI(WI_I8X16_BITMASK); break;
        case ONYX_INTRINSIC_I8X16_NARROW_I16X8_S: WI(WI_I8X16_NARROW_I16X8_S); break;
        case ONYX_INTRINSIC_I8X16_NARROW_I16X8_U: WI(WI_I8X16_NARROW_I16X8_U); break;
        case ONYX_INTRINSIC_I8X16_SHL:            WI(WI_I8X16_SHL); break;
        case ONYX_INTRINSIC_I8X16_SHR_S:          WI(WI_I8X16_SHR_S); break;
        case ONYX_INTRINSIC_I8X16_SHR_U:          WI(WI_I8X16_SHR_U); break;
        case ONYX_INTRINSIC_I8X16_ADD:            WI(WI_I8X16_ADD); break;
        case ONYX_INTRINSIC_I8X16_ADD_SAT_S:      WI(WI_I8X16_ADD_SAT_S); break;
        case ONYX_INTRINSIC_I8X16_ADD_SAT_U:      WI(WI_I8X16_ADD_SAT_U); break;
        case ONYX_INTRINSIC_I8X16_SUB:            WI(WI_I8X16_SUB); break;
        case ONYX_INTRINSIC_I8X16_SUB_SAT_S:      WI(WI_I8X16_SUB_SAT_S); break;
        case ONYX_INTRINSIC_I8X16_SUB_SAT_U:      WI(WI_I8X16_SUB_SAT_U); break;
        case ONYX_INTRINSIC_I8X16_MIN_S:          WI(WI_I8X16_MIN_S); break;
        case ONYX_INTRINSIC_I8X16_MIN_U:          WI(WI_I8X16_MIN_U); break;
        case ONYX_INTRINSIC_I8X16_MAX_S:          WI(WI_I8X16_MAX_S); break;
        case ONYX_INTRINSIC_I8X16_MAX_U:          WI(WI_I8X16_MAX_U); break;
        case ONYX_INTRINSIC_I8X16_AVGR_U:         WI(WI_I8X16_AVGR_U); break;

        case ONYX_INTRINSIC_I16X8_ABS:                WI(WI_I16X8_ABS); break;
        case ONYX_INTRINSIC_I16X8_NEG:                WI(WI_I16X8_NEG); break;
        case ONYX_INTRINSIC_I16X8_ANY_TRUE:           WI(WI_I16X8_ANY_TRUE); break;
        case ONYX_INTRINSIC_I16X8_ALL_TRUE:           WI(WI_I16X8_ALL_TRUE); break;
        case ONYX_INTRINSIC_I16X8_BITMASK:            WI(WI_I16X8_BITMASK); break;
        case ONYX_INTRINSIC_I16X8_NARROW_I32X4_S:     WI(WI_I16X8_NARROW_I32X4_S); break;
        case ONYX_INTRINSIC_I16X8_NARROW_I32X4_U:     WI(WI_I16X8_NARROW_I32X4_U); break;
        case ONYX_INTRINSIC_I16X8_WIDEN_LOW_I8X16_S:  WI(WI_I16X8_WIDEN_LOW_I8X16_S); break;
        case ONYX_INTRINSIC_I16X8_WIDEN_HIGH_I8X16_S: WI(WI_I16X8_WIDEN_HIGH_I8X16_S); break;
        case ONYX_INTRINSIC_I16X8_WIDEN_LOW_I8X16_U:  WI(WI_I16X8_WIDEN_LOW_I8X16_U); break;
        case ONYX_INTRINSIC_I16X8_WIDEN_HIGH_I8X16_U: WI(WI_I16X8_WIDEN_HIGH_I8X16_U); break;
        case ONYX_INTRINSIC_I16X8_SHL:                WI(WI_I16X8_SHL); break;
        case ONYX_INTRINSIC_I16X8_SHR_S:              WI(WI_I16X8_SHR_S); break;
        case ONYX_INTRINSIC_I16X8_SHR_U:              WI(WI_I16X8_SHR_U); break;
        case ONYX_INTRINSIC_I16X8_ADD:                WI(WI_I16X8_ADD); break;
        case ONYX_INTRINSIC_I16X8_ADD_SAT_S:          WI(WI_I16X8_ADD_SAT_S); break;
        case ONYX_INTRINSIC_I16X8_ADD_SAT_U:          WI(WI_I16X8_ADD_SAT_U); break;
        case ONYX_INTRINSIC_I16X8_SUB:                WI(WI_I16X8_SUB); break;
        case ONYX_INTRINSIC_I16X8_SUB_SAT_S:          WI(WI_I16X8_SUB_SAT_S); break;
        case ONYX_INTRINSIC_I16X8_SUB_SAT_U:          WI(WI_I16X8_SUB_SAT_U); break;
        case ONYX_INTRINSIC_I16X8_MUL:                WI(WI_I16X8_MUL); break;
        case ONYX_INTRINSIC_I16X8_MIN_S:              WI(WI_I16X8_MIN_S); break;
        case ONYX_INTRINSIC_I16X8_MIN_U:              WI(WI_I16X8_MIN_U); break;
        case ONYX_INTRINSIC_I16X8_MAX_S:              WI(WI_I16X8_MAX_S); break;
        case ONYX_INTRINSIC_I16X8_MAX_U:              WI(WI_I16X8_MAX_U); break;
        case ONYX_INTRINSIC_I16X8_AVGR_U:             WI(WI_I16X8_AVGR_U); break;

        case ONYX_INTRINSIC_I32X4_ABS:                WI(WI_I32X4_ABS); break;
        case ONYX_INTRINSIC_I32X4_NEG:                WI(WI_I32X4_NEG); break;
        case ONYX_INTRINSIC_I32X4_ANY_TRUE:           WI(WI_I32X4_ANY_TRUE); break;
        case ONYX_INTRINSIC_I32X4_ALL_TRUE:           WI(WI_I32X4_ALL_TRUE); break;
        case ONYX_INTRINSIC_I32X4_BITMASK:            WI(WI_I32X4_BITMASK); break;
        case ONYX_INTRINSIC_I32X4_WIDEN_LOW_I16X8_S:  WI(WI_I32X4_WIDEN_LOW_I16X8_S); break;
        case ONYX_INTRINSIC_I32X4_WIDEN_HIGH_I16X8_S: WI(WI_I32X4_WIDEN_HIGH_I16X8_S); break;
        case ONYX_INTRINSIC_I32X4_WIDEN_LOW_I16X8_U:  WI(WI_I32X4_WIDEN_LOW_I16X8_U); break;
        case ONYX_INTRINSIC_I32X4_WIDEN_HIGH_I16X8_U: WI(WI_I32X4_WIDEN_HIGH_I16X8_U); break;
        case ONYX_INTRINSIC_I32X4_SHL:                WI(WI_I32X4_SHL); break;
        case ONYX_INTRINSIC_I32X4_SHR_S:              WI(WI_I32X4_SHR_S); break;
        case ONYX_INTRINSIC_I32X4_SHR_U:              WI(WI_I32X4_SHR_U); break;
        case ONYX_INTRINSIC_I32X4_ADD:                WI(WI_I32X4_ADD); break;
        case ONYX_INTRINSIC_I32X4_SUB:                WI(WI_I32X4_SUB); break;
        case ONYX_INTRINSIC_I32X4_MUL:                WI(WI_I32X4_MUL); break;
        case ONYX_INTRINSIC_I32X4_MIN_S:              WI(WI_I32X4_MIN_S); break;
        case ONYX_INTRINSIC_I32X4_MIN_U:              WI(WI_I32X4_MIN_U); break;
        case ONYX_INTRINSIC_I32X4_MAX_S:              WI(WI_I32X4_MAX_S); break;
        case ONYX_INTRINSIC_I32X4_MAX_U:              WI(WI_I32X4_MAX_U); break;

        case ONYX_INTRINSIC_I64X2_NEG:   WI(WI_I64X2_NEG); break;
        case ONYX_INTRINSIC_I64X2_SHL:   WI(WI_I64X2_SHL); break;
        case ONYX_INTRINSIC_I64X2_SHR_S: WI(WI_I64X2_SHR_S); break;
        case ONYX_INTRINSIC_I64X2_SHR_U: WI(WI_I64X2_SHR_U); break;
        case ONYX_INTRINSIC_I64X2_ADD:   WI(WI_I64X2_ADD); break;
        case ONYX_INTRINSIC_I64X2_SUB:   WI(WI_I64X2_SUB); break;
        case ONYX_INTRINSIC_I64X2_MUL:   WI(WI_I64X2_MUL); break;

        case ONYX_INTRINSIC_F32X4_ABS:  WI(WI_F32X4_ABS); break;
        case ONYX_INTRINSIC_F32X4_NEG:  WI(WI_F32X4_NEG); break;
        case ONYX_INTRINSIC_F32X4_SQRT: WI(WI_F32X4_SQRT); break;
        case ONYX_INTRINSIC_F32X4_ADD:  WI(WI_F32X4_ADD); break;
        case ONYX_INTRINSIC_F32X4_SUB:  WI(WI_F32X4_SUB); break;
        case ONYX_INTRINSIC_F32X4_MUL:  WI(WI_F32X4_MUL); break;
        case ONYX_INTRINSIC_F32X4_DIV:  WI(WI_F32X4_DIV); break;
        case ONYX_INTRINSIC_F32X4_MIN:  WI(WI_F32X4_MIN); break;
        case ONYX_INTRINSIC_F32X4_MAX:  WI(WI_F32X4_MAX); break;

        case ONYX_INTRINSIC_F64X2_ABS:  WI(WI_F64X2_ABS); break;
        case ONYX_INTRINSIC_F64X2_NEG:  WI(WI_F64X2_NEG); break;
        case ONYX_INTRINSIC_F64X2_SQRT: WI(WI_F64X2_SQRT); break;
        case ONYX_INTRINSIC_F64X2_ADD:  WI(WI_F64X2_ADD); break;
        case ONYX_INTRINSIC_F64X2_SUB:  WI(WI_F64X2_SUB); break;
        case ONYX_INTRINSIC_F64X2_MUL:  WI(WI_F64X2_MUL); break;
        case ONYX_INTRINSIC_F64X2_DIV:  WI(WI_F64X2_DIV); break;
        case ONYX_INTRINSIC_F64X2_MIN:  WI(WI_F64X2_MIN); break;
        case ONYX_INTRINSIC_F64X2_MAX:  WI(WI_F64X2_MAX); break;

        case ONYX_INTRINSIC_I32X4_TRUNC_SAT_F32X4_S: WI(WI_I32X4_TRUNC_SAT_F32X4_S); break;
        case ONYX_INTRINSIC_I32X4_TRUNC_SAT_F32X4_U: WI(WI_I32X4_TRUNC_SAT_F32X4_U); break;
        case ONYX_INTRINSIC_F32X4_CONVERT_I32X4_S:   WI(WI_F32X4_CONVERT_I32X4_S); break;
        case ONYX_INTRINSIC_F32X4_CONVERT_I32X4_U:   WI(WI_F32X4_CONVERT_I32X4_U); break;

        case ONYX_INTRINSIC_ATOMIC_WAIT: {
            Type* atomic_type = ((AstArgument *) call->args.values[0])->value->type->Pointer.elem;
            emit_intrinsic_atomic_wait(mod, &code, atomic_type, call->token);
            break;
        }

        case ONYX_INTRINSIC_ATOMIC_NOTIFY: {
            emit_intrinsic_atomic_notify(mod, &code);
            break;
        }

        case ONYX_INTRINSIC_ATOMIC_FENCE: {
            emit_intrinsic_atomic_fence(mod, &code);
            break;
        }

        case ONYX_INTRINSIC_ATOMIC_LOAD: {
            Type* atomic_type = ((AstArgument *) call->args.values[0])->value->type->Pointer.elem;
            emit_intrinsic_atomic_load(mod, &code, atomic_type, call->token);
            break;
        }

        case ONYX_INTRINSIC_ATOMIC_STORE: {
            Type* atomic_type = ((AstArgument *) call->args.values[0])->value->type->Pointer.elem;
            emit_intrinsic_atomic_store(mod, &code, atomic_type, call->token);
            break;
        }

        case ONYX_INTRINSIC_ATOMIC_ADD: {
            Type* atomic_type = ((AstArgument *) call->args.values[0])->value->type->Pointer.elem;
            emit_intrinsic_atomic_add(mod, &code, atomic_type, call->token);
            break;
        }

        case ONYX_INTRINSIC_ATOMIC_SUB: {
            Type* atomic_type = ((AstArgument *) call->args.values[0])->value->type->Pointer.elem;
            emit_intrinsic_atomic_sub(mod, &code, atomic_type, call->token);
            break;
        }

        case ONYX_INTRINSIC_ATOMIC_AND: {
            Type* atomic_type = ((AstArgument *) call->args.values[0])->value->type->Pointer.elem;
            emit_intrinsic_atomic_and(mod, &code, atomic_type, call->token);
            break;
        }

        case ONYX_INTRINSIC_ATOMIC_OR: {
            Type* atomic_type = ((AstArgument *) call->args.values[0])->value->type->Pointer.elem;
            emit_intrinsic_atomic_or(mod, &code, atomic_type, call->token);
            break;
        }

        case ONYX_INTRINSIC_ATOMIC_XOR: {
            Type* atomic_type = ((AstArgument *) call->args.values[0])->value->type->Pointer.elem;
            emit_intrinsic_atomic_xor(mod, &code, atomic_type, call->token);
            break;
        }

        case ONYX_INTRINSIC_ATOMIC_XCHG: {
            Type* atomic_type = ((AstArgument *) call->args.values[0])->value->type->Pointer.elem;
            emit_intrinsic_atomic_xchg(mod, &code, atomic_type, call->token);
            break;
        }

        case ONYX_INTRINSIC_ATOMIC_CMPXCHG: {
            Type* atomic_type = ((AstArgument *) call->args.values[0])->value->type->Pointer.elem;
            emit_intrinsic_atomic_cmpxchg(mod, &code, atomic_type, call->token);
            break;
        }

        default: assert(("Unsupported intrinsic", 0));
    }

    *pcode = code;
}

EMIT_FUNC(subscript_location, AstSubscript* sub, u64* offset_return) {
    bh_arr(WasmInstruction) code = *pcode;

    emit_expression(mod, &code, sub->expr);
    if (sub->elem_size != 1) {
        WID(WI_PTR_CONST, sub->elem_size);
        WI(WI_PTR_MUL);
    }

    // CLEANUP: This is one dense clusterf**k of code...
    u64 offset = 0;
    if (sub->addr->kind == Ast_Kind_Subscript
        && sub->addr->type->kind == Type_Kind_Array) {
        emit_subscript_location(mod, &code, (AstSubscript *) sub->addr, &offset);
    } else if (sub->addr->kind == Ast_Kind_Field_Access
        && sub->addr->type->kind == Type_Kind_Array) {
        emit_field_access_location(mod, &code, (AstFieldAccess *) sub->addr, &offset);
    } else if ((sub->addr->kind == Ast_Kind_Local || sub->addr->kind == Ast_Kind_Param)
        && sub->addr->type->kind == Type_Kind_Array) {
        emit_local_location(mod, &code, (AstLocal *) sub->addr, &offset);
    } else if (sub->addr->kind == Ast_Kind_Memres
        && sub->addr->type->kind != Type_Kind_Array) {
        emit_memory_reservation_location(mod, &code, (AstMemRes *) sub->addr);
    } else {
        emit_expression(mod, &code, sub->addr);
    }

    WI(WI_PTR_ADD);

    *offset_return += offset;

    *pcode = code;
}

EMIT_FUNC(field_access_location, AstFieldAccess* field, u64* offset_return) {
    bh_arr(WasmInstruction) code = *pcode;

    u64 offset = field->offset;
    AstTyped* source_expr = field->expr;
    while (source_expr->kind == Ast_Kind_Field_Access
            && type_is_structlike_strict(source_expr->type)) {
        offset += ((AstFieldAccess *) source_expr)->offset;
        source_expr = (AstTyped *) ((AstFieldAccess *) source_expr)->expr;
    }

    if (source_expr->kind == Ast_Kind_Subscript
        && source_expr->type->kind != Type_Kind_Pointer) {
        u64 o2 = 0;
        emit_subscript_location(mod, &code, (AstSubscript *) source_expr, &o2);
        offset += o2;

    } else if ((source_expr->kind == Ast_Kind_Local || source_expr->kind == Ast_Kind_Param)
        && source_expr->type->kind != Type_Kind_Pointer) {
        u64 o2 = 0;
        emit_local_location(mod, &code, (AstLocal *) source_expr, &o2);
        offset += o2;

    } else if (source_expr->kind == Ast_Kind_Memres
        && source_expr->type->kind != Type_Kind_Pointer) {
        emit_memory_reservation_location(mod, &code, (AstMemRes *) source_expr);

    } else {
        emit_expression(mod, &code, source_expr);
    }

    *offset_return = offset;

    *pcode = code;
}

EMIT_FUNC(memory_reservation_location, AstMemRes* memres) {
    bh_arr(WasmInstruction) code = *pcode;
    WID(WI_PTR_CONST, memres->addr);

    if (memres->threadlocal) {
        u64 tls_base_idx = bh_imap_get(&mod->index_map, (u64) &builtin_tls_base);
        WIL(WI_GLOBAL_GET, tls_base_idx);
        WI(WI_PTR_ADD);
    }

    *pcode = code;
}

EMIT_FUNC(local_location, AstLocal* local, u64* offset_return) {
    bh_arr(WasmInstruction) code = *pcode;

    u64 local_offset = (u64) bh_imap_get(&mod->local_map, (u64) local);

    if (local_offset & LOCAL_IS_WASM) {
        // This is a weird condition but it is relied on in a couple places including
        // passing non-simple structs by value.             -brendanfh 2020/09/18
        WIL(WI_LOCAL_GET, local_offset);

    } else {
        WIL(WI_LOCAL_GET, mod->stack_base_idx);

        *offset_return += local_offset;
    }

    *pcode = code;
}

EMIT_FUNC(struct_lval, AstTyped* lval) {
    bh_arr(WasmInstruction) code = *pcode;

    assert(type_is_structlike_strict(lval->type));

    u64 offset = 0;
    emit_location_return_offset(mod, &code, lval, &offset);
    emit_compound_store(mod, &code, lval->type, offset, 1);

    *pcode = code;
}

EMIT_FUNC(compound_load, Type* type, u64 offset) {
    bh_arr(WasmInstruction) code = *pcode;
    i32 mem_count = type_linear_member_count(type);
    TypeWithOffset two;

    if (mem_count == 1) {
        type_linear_member_lookup(type, 0, &two);
        emit_load_instruction(mod, &code, two.type, offset + two.offset); // two.offset should be 0
    } else {
        u64 tmp_idx = local_raw_allocate(mod->local_alloc, WASM_TYPE_PTR);
        WIL(WI_LOCAL_TEE, tmp_idx);

        fori (i, 0, mem_count) {
            type_linear_member_lookup(type, i, &two);
            if (i != 0) WIL(WI_LOCAL_GET, tmp_idx);
            emit_load_instruction(mod, &code, two.type, offset + two.offset);
        }

        local_raw_free(mod->local_alloc, WASM_TYPE_PTR);
    }

    *pcode = code;
    return;
}

EMIT_FUNC(compound_store, Type* type, u64 offset, b32 location_first) {
    bh_arr(WasmInstruction) code = *pcode;

    TypeWithOffset two;

    u64 loc_idx = local_raw_allocate(mod->local_alloc, WASM_TYPE_PTR);
    if (location_first) WIL(WI_LOCAL_SET, loc_idx);

    i32 elem_count = type_linear_member_count(type);
    u64 *temp_locals = bh_alloc_array(global_scratch_allocator, u64, elem_count);

    forir (i, elem_count - 1, 0) {
        type_linear_member_lookup(type, i, &two);

        WasmType wt = onyx_type_to_wasm_type(two.type);
        temp_locals[i] = local_raw_allocate(mod->local_alloc, wt);
        WIL(WI_LOCAL_SET, temp_locals[i]);
    }

    if (!location_first) WIL(WI_LOCAL_SET, loc_idx);

    fori (i, 0, elem_count) {
        type_linear_member_lookup(type, i, &two);

        u64 tmp_idx = temp_locals[i]; 
        WIL(WI_LOCAL_GET, loc_idx);
        WIL(WI_LOCAL_GET, tmp_idx);
        emit_store_instruction(mod, &code, two.type, offset + two.offset);

        WasmType wt = onyx_type_to_wasm_type(two.type);
        local_raw_free(mod->local_alloc, wt);
    }

    local_raw_free(mod->local_alloc, WASM_TYPE_PTR);

    // This shouldn't be necessary because the scratch allocator doesn't free.
    bh_free(global_scratch_allocator, temp_locals);

    *pcode = code;
}

EMIT_FUNC(struct_literal, AstStructLiteral* sl) {
    bh_arr(WasmInstruction) code = *pcode;

    bh_arr_each(AstTyped *, val, sl->args.values) {
        emit_expression(mod, &code, *val);
    }

    *pcode = code;
}

EMIT_FUNC(array_store, Type* type, u32 offset) {
    assert(type->kind == Type_Kind_Array);
    bh_arr(WasmInstruction) code = *pcode;

    Type* elem_type = type;
    u32 elem_count = 1;
    while (elem_type->kind == Type_Kind_Array) {
        elem_count *= elem_type->Array.count;
        elem_type = elem_type->Array.elem;
    }
    u32 elem_size = type_size_of(elem_type);

    u64 lptr_local = local_raw_allocate(mod->local_alloc, WASM_TYPE_PTR);
    u64 rptr_local = local_raw_allocate(mod->local_alloc, WASM_TYPE_PTR);
    WIL(WI_LOCAL_SET, rptr_local);
    WIL(WI_LOCAL_SET, lptr_local);

    WIL(WI_LOCAL_GET, rptr_local);
    WID(WI_I32_CONST, 0);
    WI(WI_I32_NE);
    emit_enter_structured_block(mod, &code, SBT_Basic_If);

    if (elem_count <= 2) {
        // Inline copying for a small number of elements. It still may be faster to do this in a tight loop.

        fori (i, 0, elem_count) {
            if (bh_arr_last(code).type == WI_LOCAL_SET && (u64) bh_arr_last(code).data.l == lptr_local)
                bh_arr_last(code).type = WI_LOCAL_TEE;
            else
                WIL(WI_LOCAL_GET, lptr_local);

            WIL(WI_LOCAL_GET, rptr_local);
            emit_load_instruction(mod, &code, elem_type, i * elem_size);

            emit_store_instruction(mod, &code, elem_type, i * elem_size + offset);
        }

    } else if (context.options->use_post_mvp_features) {
        // Use a simple memory copy if it is available. This may be what happens in the case below too at a later time.

        if (bh_arr_last(code).type == WI_LOCAL_SET && (u64) bh_arr_last(code).data.l == lptr_local)
            bh_arr_last(code).type = WI_LOCAL_TEE;
        else
            WIL(WI_LOCAL_GET, lptr_local);

        if (offset != 0) {
            WIL(WI_PTR_CONST, offset);
            WI(WI_PTR_ADD);
        }

        WIL(WI_LOCAL_GET, rptr_local);
        WIL(WI_I32_CONST, elem_count * elem_size);
        WI(WI_MEMORY_COPY);

    } else {
        // Emit a loop that copies the memory. This could be switched to a tight loop that just copies word per word.

        u64 offset_local = local_raw_allocate(mod->local_alloc, WASM_TYPE_PTR);
        WIL(WI_PTR_CONST, 0);
        WIL(WI_LOCAL_SET, offset_local);

        WID(WI_BLOCK_START, 0x40);
        WID(WI_LOOP_START, 0x40);
            WIL(WI_LOCAL_GET, offset_local);
            WIL(WI_LOCAL_GET, lptr_local);
            WI(WI_PTR_ADD);

            WIL(WI_LOCAL_GET, offset_local);
            WIL(WI_LOCAL_GET, rptr_local);
            WI(WI_PTR_ADD);

            emit_load_instruction(mod, &code, elem_type, 0);
            emit_store_instruction(mod, &code, elem_type, offset);

            WIL(WI_LOCAL_GET, offset_local);
            WIL(WI_PTR_CONST, elem_size);
            WI(WI_PTR_ADD);
            WIL(WI_LOCAL_TEE, offset_local);

            WIL(WI_PTR_CONST, elem_count * elem_size);
            WI(WI_PTR_GE);
            WID(WI_COND_JUMP, 0x01);

            WID(WI_JUMP, 0x00);

        WI(WI_LOOP_END);
        WI(WI_BLOCK_END);

        local_raw_free(mod->local_alloc, WASM_TYPE_PTR);
    }

    WI(WI_ELSE);

    { // If the source ptr is null (0), then just copy in 0 bytes.
        WIL(WI_LOCAL_GET, lptr_local);
        if (offset != 0) {
            WIL(WI_PTR_CONST, offset);
            WI(WI_PTR_ADD);
        }

        WIL(WI_I32_CONST, 0);

        WIL(WI_I32_CONST, elem_count * elem_size);

        if (context.options->use_post_mvp_features) {
            WI(WI_MEMORY_FILL);
        } else {
            emit_intrinsic_memory_fill(mod, &code);
        }
    }

    local_raw_free(mod->local_alloc, WASM_TYPE_PTR);
    local_raw_free(mod->local_alloc, WASM_TYPE_PTR);

    emit_leave_structured_block(mod, &code);
    *pcode = code;
    return;
}

EMIT_FUNC(array_literal, AstArrayLiteral* al) {
    bh_arr(WasmInstruction) code = *pcode;

    u64 local_offset = (u64) bh_imap_get(&mod->local_map, (u64) al);
    assert((local_offset & LOCAL_IS_WASM) == 0);

    assert(al->type->kind == Type_Kind_Array);

    u32 elem_size = type_size_of(al->type->Array.elem);

    fori (i, 0, al->type->Array.count) {
        WIL(WI_LOCAL_GET, mod->stack_base_idx);
        emit_expression(mod, &code, al->values[i]);
        emit_store_instruction(mod, &code, al->type->Array.elem, local_offset + i * elem_size);
    }

    WIL(WI_LOCAL_GET, mod->stack_base_idx);
    WIL(WI_PTR_CONST, local_offset);
    WI(WI_PTR_ADD);

    *pcode = code;
}

EMIT_FUNC(range_literal, AstRangeLiteral* range) {
    bh_arr(WasmInstruction) code = *pcode;

    emit_expression(mod, &code, range->low);
    emit_expression(mod, &code, range->high);
    emit_expression(mod, &code, range->step);

    *pcode = code;
}

EMIT_FUNC(if_expression, AstIfExpression* if_expr) {
    bh_arr(WasmInstruction) code = *pcode;

    u64 offset = 0;
    u64 result_local    = local_allocate(mod->local_alloc, (AstTyped *) if_expr);
    b32 result_is_local = (b32) ((result_local & LOCAL_IS_WASM) != 0);
    bh_imap_put(&mod->local_map, (u64) if_expr, result_local);

    emit_expression(mod, &code, if_expr->cond); 

    emit_enter_structured_block(mod, &code, SBT_Basic_If);
        if (!result_is_local) emit_local_location(mod, &code, (AstLocal *) if_expr, &offset);

        emit_expression(mod, &code, if_expr->true_expr);

        if (!result_is_local) emit_store_instruction(mod, &code, if_expr->type, offset);
        else                  WIL(WI_LOCAL_SET, result_local);

    offset = 0;
    WI(WI_ELSE);
        if (!result_is_local) emit_local_location(mod, &code, (AstLocal *) if_expr, &offset);

        emit_expression(mod, &code, if_expr->false_expr);

        if (!result_is_local) emit_store_instruction(mod, &code, if_expr->type, offset);
        else                  WIL(WI_LOCAL_SET, result_local);

    emit_leave_structured_block(mod, &code);

    offset = 0;
    if (!result_is_local) {
        emit_local_location(mod, &code, (AstLocal *) if_expr, &offset);
        emit_load_instruction(mod, &code, if_expr->type, offset);

    } else {
        WIL(WI_LOCAL_GET, result_local);
    }

    local_free(mod->local_alloc, (AstTyped *) if_expr);

    *pcode = code;
}

EMIT_FUNC(do_block, AstDoBlock* doblock) {
    bh_arr(WasmInstruction) code = *pcode;

    u64 result_local    = local_allocate(mod->local_alloc, (AstTyped *) doblock);
    b32 result_is_local = (b32) ((result_local & LOCAL_IS_WASM) != 0);

    bh_imap_put(&mod->local_map, (u64) doblock, result_local);
    bh_arr_push(mod->return_location_stack, (AstLocal *) doblock);

    emit_block(mod, &code, doblock->block, 1);

    u64 offset = 0;
    if (!result_is_local) {
        emit_local_location(mod, &code, (AstLocal *) doblock, &offset);
        emit_load_instruction(mod, &code, doblock->type, offset);

    } else {
        WIL(WI_LOCAL_GET, result_local);
    }

    bh_arr_pop(mod->return_location_stack);
    local_free(mod->local_alloc, (AstTyped *) doblock);

    *pcode = code;
}

EMIT_FUNC(location_return_offset, AstTyped* expr, u64* offset_return) {
    bh_arr(WasmInstruction) code = *pcode;

    expr = (AstTyped *) strip_aliases((AstNode *) expr);

    switch (expr->kind) {
        case Ast_Kind_Param:
        case Ast_Kind_Local: {
            emit_local_location(mod, &code, (AstLocal *) expr, offset_return);
            break;
        }

        case Ast_Kind_Dereference: {
            emit_expression(mod, &code, ((AstDereference *) expr)->expr);
            *offset_return = 0;
            break;
        }

        case Ast_Kind_Subscript: {
            AstSubscript* sub = (AstSubscript *) expr;
            emit_subscript_location(mod, &code, sub, offset_return);
            break;
        }

        case Ast_Kind_Field_Access: {
            AstFieldAccess* field = (AstFieldAccess *) expr;
            emit_field_access_location(mod, &code, field, offset_return);
            break;
        }

        case Ast_Kind_Memres: {
            AstMemRes* memres = (AstMemRes *) expr;
            emit_memory_reservation_location(mod, &code, memres);
            *offset_return = 0;
            break;
        }

        default: {
            onyx_report_error(expr->token->pos, "Unable to generate locate for '%s'.", onyx_ast_node_kind_string(expr->kind));
            break;
        }
    }

    *pcode = code;
}

EMIT_FUNC(location, AstTyped* expr) {
    bh_arr(WasmInstruction) code = *pcode;

    u64 offset = 0;
    emit_location_return_offset(mod, &code, expr, &offset);
    if (offset != 0) {
        WID(WI_PTR_CONST, offset);
        WI(WI_PTR_ADD);
    } 

    *pcode = code;
}

EMIT_FUNC(expression, AstTyped* expr) {
    bh_arr(WasmInstruction) code = *pcode;

    if (node_is_type((AstNode *) expr)) {
        AstType* type = (AstType *) expr;
        if (type->flags & Ast_Flag_Expr_Ignored) return;

        if (type->type_id != 0) {
            WID(WI_I32_CONST, ((AstType *) expr)->type_id);
        } else {
            Type* t = type_build_from_ast(context.ast_alloc, type);
            WID(WI_I32_CONST, t->id);
        }


        *pcode = code;
        return;
    }

    switch (expr->kind) {
        case Ast_Kind_Param: {
            AstLocal* param = (AstLocal *) expr;
            u64 localidx = bh_imap_get(&mod->local_map, (u64) param);

            switch (type_get_param_pass(param->type)) {
                case Param_Pass_By_Value: {
                    if (type_is_structlike_strict(expr->type)) {
                        u32 mem_count = type_structlike_mem_count(expr->type);
                        fori (idx, 0, mem_count) WIL(WI_LOCAL_GET, localidx + idx);

                    } else {
                        assert(localidx & LOCAL_IS_WASM);
                        WIL(WI_LOCAL_GET, localidx);
                    }
                    break;
                }

                case Param_Pass_By_Implicit_Pointer: {
                    assert(localidx & LOCAL_IS_WASM);
                    WIL(WI_LOCAL_GET, localidx);
                    emit_load_instruction(mod, &code, expr->type, 0);
                    break;
                }

                default: assert(0);
            }

            break;
        }

        case Ast_Kind_Local: {
            u64 tmp = bh_imap_get(&mod->local_map, (u64) expr);

            if (tmp & LOCAL_IS_WASM) {
                if (bh_arr_last(code).type == WI_LOCAL_SET && (u64) bh_arr_last(code).data.l == tmp) {
                    bh_arr_last(code).type = WI_LOCAL_TEE;
                } else {
                    WIL(WI_LOCAL_GET, tmp);
                }

            } else {
                u64 offset = 0;
                emit_local_location(mod, &code, (AstLocal *) expr, &offset);

                if (expr->type->kind != Type_Kind_Array) {
                    emit_load_instruction(mod, &code, expr->type, offset);
                } else if (offset != 0) {
                    WID(WI_PTR_CONST, offset);
                    WI(WI_PTR_ADD);
                }
            }

            break;
        }

        case Ast_Kind_Global: {
            i32 globalidx = (i32) bh_imap_get(&mod->index_map, (u64) expr);

            WID(WI_GLOBAL_GET, globalidx);
            break;
        }

        case Ast_Kind_NumLit: {
            AstNumLit* lit = (AstNumLit *) expr;
            WasmType lit_type = onyx_type_to_wasm_type(lit->type);
            WasmInstruction instr = { WI_NOP, 0 };

            if (lit_type == WASM_TYPE_INT32) {
                instr.type = WI_I32_CONST;
                instr.data.i1 = lit->value.i;
            } else if (lit_type == WASM_TYPE_INT64) {
                instr.type = WI_I64_CONST;
                instr.data.l = lit->value.l;
            } else if (lit_type == WASM_TYPE_FLOAT32) {
                instr.type = WI_F32_CONST;
                instr.data.f = lit->value.f;
            } else if (lit_type == WASM_TYPE_FLOAT64) {
                instr.type = WI_F64_CONST;
                instr.data.d = lit->value.d;
            }

            bh_arr_push(code, instr);
            break;
        }

        case Ast_Kind_StrLit: {
            WID(WI_PTR_CONST, ((AstStrLit *) expr)->addr);
            WID(WI_I32_CONST, ((AstStrLit *) expr)->length);
            break;
        }

        case Ast_Kind_Struct_Literal: {
            emit_struct_literal(mod, &code, (AstStructLiteral *) expr);
            break;
        }

        case Ast_Kind_Array_Literal: {
            emit_local_allocation(mod, &code, expr);
            emit_array_literal(mod, &code, (AstArrayLiteral *) expr);
            break;
        }

        case Ast_Kind_Range_Literal: {
            emit_range_literal(mod, &code, (AstRangeLiteral *) expr);
            break;
        }

        case Ast_Kind_Function: {
            i32 elemidx = get_element_idx(mod, (AstFunction *) expr);

            WID(WI_I32_CONST, elemidx);
            break;
        }

        case Ast_Kind_Block:          emit_block(mod, &code, (AstBlock *) expr, 1); break;
        case Ast_Kind_Do_Block:       emit_do_block(mod, &code, (AstDoBlock *) expr); break;
        case Ast_Kind_Call:           emit_call(mod, &code, (AstCall *) expr); break;
        case Ast_Kind_Argument:       emit_expression(mod, &code, ((AstArgument *) expr)->value); break;
        case Ast_Kind_Intrinsic_Call: emit_intrinsic_call(mod, &code, (AstCall *) expr); break;
        case Ast_Kind_Binary_Op:      emit_binop(mod, &code, (AstBinaryOp *) expr); break;
        case Ast_Kind_Unary_Op:       emit_unaryop(mod, &code, (AstUnaryOp *) expr); break;
        case Ast_Kind_Alias:          emit_expression(mod, &code, ((AstAlias *) expr)->alias); break;

        case Ast_Kind_Address_Of: {
            AstAddressOf* aof = (AstAddressOf *) expr;
            emit_location(mod, &code, aof->expr);
            break;
        }

        case Ast_Kind_Dereference: {
            AstDereference* deref = (AstDereference *) expr;
            emit_expression(mod, &code, deref->expr);
            emit_load_instruction(mod, &code, deref->type, 0);
            break;
        }

        case Ast_Kind_Subscript: {
            AstSubscript* sub = (AstSubscript *) expr;
            u64 offset = 0;
            emit_subscript_location(mod, &code, sub, &offset);
            emit_load_instruction(mod, &code, sub->type, offset);
            break;
        }

        case Ast_Kind_Field_Access: {
            AstFieldAccess* field = (AstFieldAccess* ) expr;

            if (field->expr->kind == Ast_Kind_Param) {
                if (type_get_param_pass(field->expr->type) == Param_Pass_By_Value && !type_is_pointer(field->expr->type)) {
                    u64 localidx = bh_imap_get(&mod->local_map, (u64) field->expr) + field->idx;
                    assert(localidx & LOCAL_IS_WASM);
                    WIL(WI_LOCAL_GET, localidx);
                    break;
                }
            }

            if (is_lval((AstNode *) field->expr) || type_is_pointer(field->expr->type)) {
                u64 offset = 0;
                emit_field_access_location(mod, &code, field, &offset);
                emit_load_instruction(mod, &code, field->type, offset);

            } else {
                emit_expression(mod, &code, field->expr);

                i32 idx = type_get_idx_of_linear_member_with_offset(field->expr->type, field->offset);
                i32 field_linear_members = type_linear_member_count(field->type);
                i32 total_linear_members = type_linear_member_count(field->expr->type);

                if (idx == 0) {
                    // Easy case: the member is the first one and all other members just have to be dropped.
                    fori (i, 0, total_linear_members - field_linear_members) WI(WI_DROP);

                } else {
                    // Tough case: Stack shuffling to make the member the only thing on the stack.
                    // This is very similar to the compound_load/compound_store procedures but it is different enough
                    // that I cannot find a good way to factor them all without just introducing a ton of complexity.
                    fori (i, 0, total_linear_members - idx - field_linear_members) WI(WI_DROP);

                    u64 *temporaries = bh_alloc_array(global_scratch_allocator, u64, field_linear_members);
                    fori (i, 0, field_linear_members) temporaries[i] = 0;

                    TypeWithOffset two = { 0 };
                    forir (i, field_linear_members - 1, 0) {
                        type_linear_member_lookup(field->type, i, &two);

                        WasmType wt = onyx_type_to_wasm_type(two.type);
                        temporaries[i] = local_raw_allocate(mod->local_alloc, wt);
                        WIL(WI_LOCAL_SET, temporaries[i]);
                    }

                    fori (i, 0, idx) WI(WI_DROP);

                    fori (i, 0, field_linear_members) {
                        type_linear_member_lookup(field->type, i, &two);

                        WIL(WI_LOCAL_GET, temporaries[i]);

                        WasmType wt = onyx_type_to_wasm_type(two.type);
                        local_raw_free(mod->local_alloc, wt);
                    }

                    bh_free(global_scratch_allocator, temporaries);
                }
            }

            break;
        }

        case Ast_Kind_Slice: {
            AstSubscript* sl = (AstSubscript *) expr;

            emit_expression(mod, &code, sl->expr);

            u64 lo_local = local_raw_allocate(mod->local_alloc, WASM_TYPE_INT32);
            u64 hi_local = local_raw_allocate(mod->local_alloc, WASM_TYPE_INT32);

            WI(WI_DROP);
            WIL(WI_LOCAL_SET, hi_local);
            WIL(WI_LOCAL_TEE, lo_local);
            if (sl->elem_size != 1) {
                WID(WI_I32_CONST, sl->elem_size);
                WI(WI_I32_MUL);
            }
            emit_expression(mod, &code, sl->addr);
            WI(WI_I32_ADD);
            WIL(WI_LOCAL_GET, hi_local);
            WIL(WI_LOCAL_GET, lo_local);
            WI(WI_I32_SUB);

            local_raw_free(mod->local_alloc, lo_local);
            local_raw_free(mod->local_alloc, hi_local);
            break;
        }

        case Ast_Kind_Size_Of: {
            AstSizeOf* so = (AstSizeOf *) expr;
            WID(WI_I32_CONST, so->size);
            break;
        }

        case Ast_Kind_Align_Of: {
            AstAlignOf* ao = (AstAlignOf *) expr;
            WID(WI_I32_CONST, ao->alignment);
            break;
        }

        case Ast_Kind_Enum_Value: {
            AstEnumValue* ev = (AstEnumValue *) expr;
            AstNumLit   * num = (AstNumLit *) ev->value;
            assert(num->kind == Ast_Kind_NumLit);

            WasmType backing_type = onyx_type_to_wasm_type(ev->type);
            if      (backing_type == WASM_TYPE_INT32) WID(WI_I32_CONST, num->value.i);
            else if (backing_type == WASM_TYPE_INT64) WID(WI_I64_CONST, num->value.l);
            else onyx_report_error(ev->token->pos, "Invalid backing type for enum.");
            break;
        }

        case Ast_Kind_Memres: {
            AstMemRes* memres = (AstMemRes *) expr;
            emit_memory_reservation_location(mod, &code, memres);
            emit_load_instruction(mod, &code, memres->type, 0);
            break;
        }

        case Ast_Kind_File_Contents: {
            AstFileContents* fc = (AstFileContents *) expr;

            assert(fc->addr > 0);
            assert(fc->size > 0);

            WID(WI_PTR_CONST, fc->addr);
            WID(WI_I32_CONST, fc->size);
            break;
        }

        case Ast_Kind_Compound: {
            AstCompound* compound = (AstCompound *) expr;

            bh_arr_each(AstTyped *, expr, compound->exprs) {
                emit_expression(mod, &code, *expr);
            }
            break;
        }

        case Ast_Kind_Call_Site: {
            AstCallSite* callsite = (AstCallSite *) expr;

            emit_expression(mod, &code, (AstTyped *) callsite->filename);
            emit_expression(mod, &code, (AstTyped *) callsite->line);
            emit_expression(mod, &code, (AstTyped *) callsite->column);
            break;
        }

        case Ast_Kind_If_Expression: {
            AstIfExpression* if_expr = (AstIfExpression *) expr;
            emit_if_expression(mod, &code, if_expr);
            break;
        }

        default:
            bh_printf("Unhandled case: %d\n", expr->kind);
            DEBUG_HERE;
            assert(0);
    }

    if ((expr->flags & Ast_Flag_Expr_Ignored) != 0 && !type_results_in_void(expr->type)) {
        i32 mem_count = 1;
        if (type_is_compound(expr->type)) mem_count = type_linear_member_count(expr->type);
        fori (i, 0, mem_count) WI(WI_DROP);
    }

    *pcode = code;
}

static const WasmInstructionType cast_map[][12] = {
    //          I8              U8                  I16                 U16                I32                 U32                I64                U64                F32                F64                PTR
    /* I8  */ { WI_NOP,         WI_NOP,             WI_I32_EXTEND_8_S,  WI_NOP,            WI_I32_EXTEND_8_S,  WI_NOP,            WI_I64_FROM_I32_S, WI_I64_FROM_I32_S, WI_UNREACHABLE,    WI_UNREACHABLE,    WI_UNREACHABLE,  WI_UNREACHABLE },
    /* U8  */ { WI_NOP,         WI_NOP,             WI_NOP,             WI_NOP,            WI_NOP,             WI_NOP,            WI_I64_FROM_I32_U, WI_I64_FROM_I32_U, WI_UNREACHABLE,    WI_UNREACHABLE,    WI_UNREACHABLE,  WI_UNREACHABLE },
    /* I16 */ { WI_NOP,         WI_NOP,             WI_NOP,             WI_NOP,            WI_I32_EXTEND_16_S, WI_NOP,            WI_I64_FROM_I32_S, WI_I64_FROM_I32_S, WI_UNREACHABLE,    WI_UNREACHABLE,    WI_UNREACHABLE,  WI_UNREACHABLE },
    /* U16 */ { WI_NOP,         WI_NOP,             WI_NOP,             WI_NOP,            WI_NOP,             WI_NOP,            WI_I64_FROM_I32_U, WI_I64_FROM_I32_U, WI_UNREACHABLE,    WI_UNREACHABLE,    WI_UNREACHABLE,  WI_UNREACHABLE },
    /* I32 */ { WI_NOP,         WI_NOP,             WI_NOP,             WI_NOP,            WI_NOP,             WI_NOP,            WI_I64_FROM_I32_S, WI_I64_FROM_I32_S, WI_F32_FROM_I32_S, WI_F64_FROM_I32_S, WI_NOP,          WI_NOP         },
    /* U32 */ { WI_NOP,         WI_NOP,             WI_NOP,             WI_NOP,            WI_NOP,             WI_NOP,            WI_I64_FROM_I32_U, WI_I64_FROM_I32_U, WI_F32_FROM_I32_U, WI_F64_FROM_I32_U, WI_NOP,          WI_NOP         },
    /* I64 */ { WI_NOP,         WI_I32_FROM_I64,    WI_I32_FROM_I64,    WI_I32_FROM_I64,   WI_I32_FROM_I64,    WI_I32_FROM_I64,   WI_NOP,            WI_NOP,            WI_F32_FROM_I64_S, WI_F64_FROM_I64_S, WI_I32_FROM_I64, WI_UNREACHABLE },
    /* U64 */ { WI_NOP,         WI_I32_FROM_I64,    WI_I32_FROM_I64,    WI_I32_FROM_I64,   WI_I32_FROM_I64,    WI_I32_FROM_I64,   WI_NOP,            WI_NOP,            WI_F32_FROM_I64_U, WI_F64_FROM_I64_U, WI_I32_FROM_I64, WI_UNREACHABLE },
    /* F32 */ { WI_UNREACHABLE, WI_UNREACHABLE,     WI_UNREACHABLE,     WI_UNREACHABLE,    WI_I32_FROM_F32_S,  WI_I32_FROM_F32_U, WI_I64_FROM_F32_S, WI_I64_FROM_F32_U, WI_NOP,            WI_F64_FROM_F32,   WI_UNREACHABLE,  WI_UNREACHABLE },
    /* F64 */ { WI_UNREACHABLE, WI_UNREACHABLE,     WI_UNREACHABLE,     WI_UNREACHABLE,    WI_I32_FROM_F64_S,  WI_I32_FROM_F64_U, WI_I64_FROM_F64_S, WI_I64_FROM_F64_U, WI_F32_FROM_F64,   WI_NOP,            WI_UNREACHABLE,  WI_UNREACHABLE },
    /* PTR */ { WI_UNREACHABLE, WI_UNREACHABLE,     WI_UNREACHABLE,     WI_UNREACHABLE,    WI_NOP,             WI_NOP,            WI_I64_FROM_I32_U, WI_I64_FROM_I32_U, WI_UNREACHABLE,    WI_UNREACHABLE,    WI_NOP,          WI_UNREACHABLE },
    /* TYP */ { WI_UNREACHABLE, WI_UNREACHABLE,     WI_UNREACHABLE,     WI_UNREACHABLE,    WI_NOP,             WI_NOP,            WI_UNREACHABLE,    WI_UNREACHABLE,    WI_UNREACHABLE,    WI_UNREACHABLE,    WI_UNREACHABLE,  WI_NOP         },
};

EMIT_FUNC(cast, AstUnaryOp* cast) {
    bh_arr(WasmInstruction) code = *pcode;

    emit_expression(mod, &code, cast->expr);

    Type* from = cast->expr->type;
    Type* to = cast->type;
    if (from->kind == Type_Kind_Enum) from = from->Enum.backing;
    if (to->kind == Type_Kind_Enum) to = to->Enum.backing;

    if (type_is_simd(from) && type_is_simd(to)) {
        *pcode = code;
        return;
    }

    if (to->kind == Type_Kind_Basic && to->Basic.kind == Basic_Kind_Void) {
        WI(WI_DROP);
        *pcode = code;
        return;
    }

    if (to->kind == Type_Kind_Slice && from->kind == Type_Kind_Array) {
        WID(WI_I32_CONST, from->Array.count);
        *pcode = code;
        return;
    }

    if (to->kind == Type_Kind_Slice && from->kind == Type_Kind_DynArray) {
        WI(WI_DROP);
        WI(WI_DROP);
        WI(WI_DROP);
        *pcode = code;
        return;
    }

    if (to->kind == Type_Kind_Slice && from->kind == Type_Kind_VarArgs) {
        // Nothing needs to be done because they are identical
        *pcode = code;
        return;
    }

    i32 fromidx = -1, toidx = -1;
    if (from->Basic.flags & Basic_Flag_Pointer || from->kind == Type_Kind_Array) {
        fromidx = 10;
    }
    else if (from->Basic.flags & Basic_Flag_Integer) {
        b32 unsign = (from->Basic.flags & Basic_Flag_Unsigned) != 0;

        fromidx = log2_dumb(from->Basic.size) * 2 + unsign;
    }
    else if (from->Basic.flags & Basic_Flag_Float) {
        if      (from->Basic.size == 4) fromidx = 8;
        else if (from->Basic.size == 8) fromidx = 9;
    }
    else if (from->Basic.flags & Basic_Flag_Boolean) {
        fromidx = 0;
    }
    else if (from->Basic.flags & Basic_Flag_Type_Index) {
        fromidx = 11;
    }

    if (to->Basic.flags & Basic_Flag_Pointer || to->kind == Type_Kind_Array) {
        toidx = 10;
    }
    else if (to->Basic.flags & Basic_Flag_Integer) {
        b32 unsign = (to->Basic.flags & Basic_Flag_Unsigned) != 0;

        toidx = log2_dumb(to->Basic.size) * 2 + unsign;
    }
    else if (to->Basic.flags & Basic_Flag_Float) {
        if      (to->Basic.size == 4) toidx = 8;
        else if (to->Basic.size == 8) toidx = 9;
    }
    else if (to->Basic.flags & Basic_Flag_Boolean) {
        toidx = 0;
    }
    else if (to->Basic.flags & Basic_Flag_Type_Index) {
        toidx = 11;
    }

    if (fromidx != -1 && toidx != -1) {
        WasmInstructionType cast_op = cast_map[fromidx][toidx];
        assert(cast_op != WI_UNREACHABLE);

        if (cast_op != WI_NOP) {
            WI(cast_op);
        }
    }

    *pcode = code;
}

EMIT_FUNC(return, AstReturn* ret) {
    bh_arr(WasmInstruction) code = *pcode;

    // If we have an expression to return, we see if it should be placed on the linear memory stack, or the WASM stack.
    if (ret->expr) {
        if (bh_arr_length(mod->return_location_stack) > 0) {
            AstLocal* dest = bh_arr_last(mod->return_location_stack);
            u64 dest_loc = bh_imap_get(&mod->local_map, (u64) dest);
            b32 dest_is_local = (b32) ((dest_loc & LOCAL_IS_WASM) != 0);

            u64 offset = 0;
            if (!dest_is_local) emit_local_location(mod, &code, dest, &offset);

            emit_expression(mod, &code, ret->expr);

            if (!dest_is_local) emit_store_instruction(mod, &code, dest->type, offset);
            else                WIL(WI_LOCAL_SET, dest_loc);

        } else if (mod->curr_cc == CC_Return_Stack) {
            WIL(WI_LOCAL_GET, mod->stack_base_idx);
            WID(WI_I32_CONST, type_size_of(ret->expr->type));
            WI(WI_I32_SUB);

            emit_expression(mod, &code, ret->expr);
            emit_store_instruction(mod, &code, ret->expr->type, 0);

        } else {
            emit_expression(mod, &code, ret->expr);
        }
    }

    // Clear the normal deferred statements
    emit_deferred_stmts(mod, &code);

    i64 jump_label = get_structured_jump_label(mod, Jump_Type_Return, 1);
    if (jump_label >= 0) {
        WIL(WI_JUMP, jump_label);

    } else {
        // Clear the rest of the deferred statements
        if (bh_arr_length(mod->deferred_stmts) > 0) {
            i32 i = bh_arr_length(mod->deferred_stmts) - 1;
            while (i >= 0) {
                emit_deferred_stmt(mod, &code, mod->deferred_stmts[i]);
                i--;
            }
        }

        // Make a patch for the two instructions needed to restore the stack pointer
        SUBMIT_PATCH(mod->stack_leave_patches, 0);
        WI(WI_NOP);
        WI(WI_NOP);

        WI(WI_RETURN);
    }

    *pcode = code;
}

EMIT_FUNC(stack_enter, u64 stacksize) {
    bh_arr(WasmInstruction) code = *pcode;

    bh_align(stacksize, 16);

    u64 stack_top_idx = bh_imap_get(&mod->index_map, (u64) &builtin_stack_top);

    // HACK: slightly... There will be space for 5 instructions
    if (stacksize == 0) {
        code[0] = (WasmInstruction) { WI_GLOBAL_GET, { .l = stack_top_idx } };
        code[1] = (WasmInstruction) { WI_LOCAL_SET,  { .l = mod->stack_base_idx} };
        code[2] = (WasmInstruction) { WI_NOP,        0 };
        code[3] = (WasmInstruction) { WI_NOP,        0 };
        code[4] = (WasmInstruction) { WI_NOP,        0 };
    } else {
        code[0] = (WasmInstruction) { WI_GLOBAL_GET, { .l = stack_top_idx } };
        code[1] = (WasmInstruction) { WI_LOCAL_TEE,  { .l = mod->stack_base_idx} };
        code[2] = (WasmInstruction) { WI_I32_CONST,  { .l = stacksize } };
        code[3] = (WasmInstruction) { WI_I32_ADD,    0 };
        code[4] = (WasmInstruction) { WI_GLOBAL_SET, { .l = stack_top_idx } };
    }

    *pcode = code;
}

EMIT_FUNC(zero_value, WasmType wt) {
    bh_arr(WasmInstruction) code = *pcode;

    switch (wt) {
        case WASM_TYPE_INT32:   WIL(WI_I32_CONST, 0); break;
        case WASM_TYPE_INT64:   WIL(WI_I64_CONST, 0); break;
        case WASM_TYPE_FLOAT32: WIL(WI_F32_CONST, 0); break;
        case WASM_TYPE_FLOAT64: WIL(WI_F64_CONST, 0); break;
        case WASM_TYPE_VAR128:  {
            static u8 zero_v128[] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
            WIP(WI_V128_CONST, &zero_v128);
            break;
        }
    }

    *pcode = code;
}

EMIT_FUNC(zero_value_for_type, Type* type, OnyxToken* where) {
    bh_arr(WasmInstruction) code = *pcode;

    if (type_is_structlike_strict(type)) {
        i32 mem_count = type_linear_member_count(type);
        TypeWithOffset two;

        fori (i, 0, mem_count) {
            type_linear_member_lookup(type, i, &two);
            emit_zero_value_for_type(mod, &code, two.type, where);
        }
    }
    else if (type->kind == Type_Kind_Function) {
        WID(WI_I32_CONST, mod->null_proc_func_idx);
    }
    else {
        WasmType wt = onyx_type_to_wasm_type(type);
        if (wt == WASM_TYPE_VOID) {
            onyx_report_error(where->pos, "Cannot produce a zero-value for this type.");
        }
        emit_zero_value(mod, &code, wt);
    }

    *pcode = code;
}

static i32 generate_type_idx(OnyxWasmModule* mod, Type* ft) {
    if (ft->kind != Type_Kind_Function) return -1;

    static char type_repr_buf[128];
    char* t = type_repr_buf;

    Type** param_type = ft->Function.params;
    i32 param_count = ft->Function.param_count;
    i32 params_left = param_count;

    while (params_left-- > 0) {
        if (type_get_param_pass(*param_type) == Param_Pass_By_Implicit_Pointer) {
            *(t++) = (char) onyx_type_to_wasm_type(&basic_types[Basic_Kind_Rawptr]);

        }
        else if (type_is_structlike_strict(*param_type)) {
            u32 mem_count = type_structlike_mem_count(*param_type);
            StructMember smem;

            fori (i, 0, mem_count) {
                type_lookup_member_by_idx(*param_type, i, &smem);
                *(t++) = (char) onyx_type_to_wasm_type(smem.type);
            }

            param_count += mem_count - 1;

        } else {
            *(t++) = (char) onyx_type_to_wasm_type(*param_type);
        }

        param_type++;
    }
    *(t++) = ':';

    // HACK: Slightly: the wasm type for structs has to be 0x00
    WasmType return_type = onyx_type_to_wasm_type(ft->Function.return_type);
    *(t++) = (char) return_type;
    *t = '\0';

    i32 type_idx = 0;
    if (bh_table_has(i32, mod->type_map, type_repr_buf)) {
        type_idx = bh_table_get(i32, mod->type_map, type_repr_buf);
    } else {
        // NOTE: Make a new type
        WasmFuncType* type = (WasmFuncType*) bh_alloc(mod->allocator, sizeof(WasmFuncType) + sizeof(WasmType) * param_count);
        type->return_type = return_type;
        type->param_count = param_count;

        // HACK ish thing
        memcpy(type->param_types, type_repr_buf, type->param_count);

        bh_arr_push(mod->types, type);

        bh_table_put(i32, mod->type_map, type_repr_buf, mod->next_type_idx);
        type_idx = mod->next_type_idx;
        mod->next_type_idx++;
    }

    return type_idx;
}

static i32 get_element_idx(OnyxWasmModule* mod, AstFunction* func) {
    if (bh_imap_has(&mod->elem_map, (u64) func)) {
        return bh_imap_get(&mod->elem_map, (u64) func);
    } else {
        i32 idx = mod->next_elem_idx;
        bh_imap_put(&mod->elem_map, (u64) func, idx);

        i32 func_idx = bh_imap_get(&mod->index_map, (u64) func);
        bh_arr_push(mod->elems, func_idx);

        mod->next_elem_idx++;

        return idx;
    }
}

static inline b32 should_emit_function(AstFunction* fd) {
    // NOTE: Don't output intrinsic functions
    if (fd->is_intrinsic) return 0;

    // NOTE: Don't output functions that are not used, only if
    // they are also not exported.
    if ((fd->flags & Ast_Flag_Function_Used) == 0) {
        if (fd->is_exported) {
            return 1;
        } else {
            return 0;
        }
    }

    return 1;
}


static void emit_function(OnyxWasmModule* mod, AstFunction* fd) {
    if (!should_emit_function(fd)) return;

    i32 type_idx = generate_type_idx(mod, fd->type);

    WasmFunc wasm_func = { 0 };
    wasm_func.type_idx = type_idx;

    bh_arr_new(mod->allocator, wasm_func.code, 4);

    i32 func_idx = (i32) bh_imap_get(&mod->index_map, (u64) fd);

    if (fd == builtin_initialize_data_segments && context.options->use_post_mvp_features) {
        emit_initialize_data_segments_body(mod, &wasm_func.code);
        bh_arr_push(wasm_func.code, ((WasmInstruction){ WI_BLOCK_END, 0x00 }));
        bh_arr_set_at(mod->funcs, func_idx - mod->foreign_function_count, wasm_func);
        return;
    }

    if (fd == builtin_run_init_procedures) {
        emit_run_init_procedures(mod, &wasm_func.code);
        bh_arr_push(wasm_func.code, ((WasmInstruction){ WI_BLOCK_END, 0x00 }));
        bh_arr_set_at(mod->funcs, func_idx - mod->foreign_function_count, wasm_func);
        return;
    }

    if (fd->body != NULL) {
        // NOTE: Generate the local map
        u64 localidx = 0;
        bh_arr_each(AstParam, param, fd->params) {
            switch (type_get_param_pass(param->local->type)) {
                case Param_Pass_By_Value: {
                    if (type_is_structlike_strict(param->local->type)) {
                        bh_imap_put(&mod->local_map, (u64) param->local, localidx | LOCAL_IS_WASM);
                        localidx += type_structlike_mem_count(param->local->type);

                        break;
                    }
                    // fallthrough
                }

                case Param_Pass_By_Implicit_Pointer: {
                    bh_imap_put(&mod->local_map, (u64) param->local, localidx++ | LOCAL_IS_WASM);
                    break;
                }

                default: assert(0);
            }
        }

        mod->local_alloc = &wasm_func.locals;
        mod->local_alloc->param_count = localidx;

        mod->curr_cc = type_function_get_cc(fd->type);
        assert(mod->curr_cc != CC_Undefined);

        bh_arr_clear(mod->stack_leave_patches);

        bh_arr_insert_end(wasm_func.code, 5);
        fori (i, 0, 5) wasm_func.code[i] = (WasmInstruction) { WI_NOP, 0 };

        mod->stack_base_idx = local_raw_allocate(mod->local_alloc, WASM_TYPE_PTR);

        // Generate code
        emit_function_body(mod, &wasm_func.code, fd);

        if (mod->local_alloc->max_stack > 0 || mod->curr_cc == CC_Return_Stack) {
            emit_stack_enter(mod, &wasm_func.code, mod->local_alloc->max_stack);

            // Place all stack leaves in patch locations. These will (probably) all be
            // right before a "return" instruction.
            u64 stack_top_idx = bh_imap_get(&mod->index_map, (u64) &builtin_stack_top);
            bh_arr_push(wasm_func.code, ((WasmInstruction) { WI_LOCAL_GET,  { .l = mod->stack_base_idx } }));
            bh_arr_push(wasm_func.code, ((WasmInstruction) { WI_GLOBAL_SET, { .l = stack_top_idx } }));

            bh_arr_each(PatchInfo, patch, mod->stack_leave_patches) {
                wasm_func.code[patch->instruction_index + 0] = (WasmInstruction) { WI_LOCAL_GET,  { .l = mod->stack_base_idx } };
                wasm_func.code[patch->instruction_index + 1] = (WasmInstruction) { WI_GLOBAL_SET, { .l = stack_top_idx } };
            }
        }
    }

    WasmFuncType* ft = mod->types[type_idx];
    emit_zero_value(mod, &wasm_func.code, ft->return_type);
    bh_arr_push(wasm_func.code, ((WasmInstruction){ WI_BLOCK_END, 0x00 }));

    bh_imap_clear(&mod->local_map);

    bh_arr_set_at(mod->funcs, func_idx - mod->foreign_function_count, wasm_func);
}

static void emit_foreign_function(OnyxWasmModule* mod, AstFunction* fd) {
    if (!should_emit_function(fd)) return;

    i32 type_idx = generate_type_idx(mod, fd->type);

    WasmImport import = {
        .kind = WASM_FOREIGN_FUNCTION,
        .idx  = type_idx,
        .mod  = bh_aprintf(global_heap_allocator, "%b", fd->foreign_module->text, fd->foreign_module->length),
        .name = bh_aprintf(global_heap_allocator, "%b", fd->foreign_name->text, fd->foreign_name->length),
    };

    bh_arr_push(mod->imports, import);
    return;
}

static void emit_export_directive(OnyxWasmModule* mod, AstDirectiveExport* export) {
    assert(export->export_name);
    assert(export->export);

    token_toggle_end(export->export_name);

    i64 idx = bh_imap_get(&mod->index_map, (u64) export->export);

    WasmExport wasm_export;
    wasm_export.idx = (i32) idx;

    switch (export->export->kind) {
        case Ast_Kind_Function: wasm_export.kind = WASM_FOREIGN_FUNCTION;
                                break;

        case Ast_Kind_Global:   wasm_export.kind = WASM_FOREIGN_GLOBAL;
                                break;
    }

    bh_table_put(WasmExport, mod->exports, export->export_name->text, wasm_export);
    mod->export_count++;
    
    token_toggle_end(export->export_name);
    return;
}

static void emit_global(OnyxWasmModule* module, AstGlobal* global) {
    WasmType global_type = onyx_type_to_wasm_type(global->type);

    WasmGlobal glob = {
        .type = global_type,
        .mutable = (global->flags & Ast_Flag_Const) == 0,
        .initial_value = NULL,
    };

    i32 global_idx = (i32) bh_imap_get(&module->index_map, (u64) global);

    bh_arr_new(global_heap_allocator, glob.initial_value, 1);

    switch (global_type) {
        case WASM_TYPE_INT32:   bh_arr_push(glob.initial_value, ((WasmInstruction) { WI_I32_CONST, 0 })); break;
        case WASM_TYPE_INT64:   bh_arr_push(glob.initial_value, ((WasmInstruction) { WI_I64_CONST, 0 })); break;
        case WASM_TYPE_FLOAT32: bh_arr_push(glob.initial_value, ((WasmInstruction) { WI_F32_CONST, 0 })); break;
        case WASM_TYPE_FLOAT64: bh_arr_push(glob.initial_value, ((WasmInstruction) { WI_F64_CONST, 0 })); break;

        default: assert(("Invalid global type", 0)); break;
    }

    bh_arr_set_at(module->globals, global_idx, glob);

    if (global == &builtin_stack_top)
        module->stack_top_ptr = &module->globals[global_idx].initial_value[0].data.i1;
    if (global == &builtin_tls_size)
        module->tls_size_ptr  = &module->globals[global_idx].initial_value[0].data.i1;
}

static void emit_string_literal(OnyxWasmModule* mod, AstStrLit* strlit) {

    // NOTE: Allocating more than necessary, but there are no cases
    // in a string literal that create more bytes than already
    // existed. You can create less however ('\n' => 0x0a).
    i8* strdata = bh_alloc_array(global_heap_allocator, i8, strlit->token->length + 1);
    i32 length  = string_process_escape_seqs(strdata, strlit->token->text, strlit->token->length);

    // Warning for having '%' in a string literal (because that probably is being used for a old print format)
    /*
    if (charset_contains((const char *) strdata, '%')) {
        onyx_report_warning(strlit->token->pos, "Found string literal with '%%'");
    }
    */

    if (bh_table_has(StrLitInfo, mod->string_literals, (char *) strdata)) {
        StrLitInfo sti = bh_table_get(StrLitInfo, mod->string_literals, (char *) strdata);
        strlit->addr   = sti.addr;
        strlit->length = sti.len;

        bh_free(global_heap_allocator, strdata);
        return;
    }

    WasmDatum datum = {
        .offset = mod->next_datum_offset,
        .length = length,
        .data = strdata,
    };

    strlit->addr = (u32) mod->next_datum_offset,
    strlit->length = length;
    mod->next_datum_offset += length;

    bh_table_put(StrLitInfo, mod->string_literals, (char *) strdata, ((StrLitInfo) { strlit->addr, strlit->length }));

    bh_arr_push(mod->data, datum);
}

static void emit_raw_data(OnyxWasmModule* mod, ptr data, AstTyped* node) {
    if (!emit_raw_data_(mod, data, node)) {
        onyx_report_error(node->token->pos,
            "Cannot generate constant data for '%s'.",
            onyx_ast_node_kind_string(node->kind));
    }
}

static b32 emit_raw_data_(OnyxWasmModule* mod, ptr data, AstTyped* node) {
    b32 retval = 1;

    if (node_is_type((AstNode *) node)) {
        Type* constructed_type = type_build_from_ast(context.ast_alloc, (AstType *) node);
        ((i32 *) data)[0] = constructed_type->id;
        return 1;
    }

    switch (node->kind) {
    case Ast_Kind_Array_Literal: {
        AstArrayLiteral* al = (AstArrayLiteral *) node;

        i32 i = 0;
        i32 elem_size = type_size_of(al->type->Array.elem);

        bh_arr_each(AstTyped *, expr, al->values) {
            retval &= emit_raw_data_(mod, bh_pointer_add(data, i * elem_size), *expr);
            i++;
        }

        break;
    }

    case Ast_Kind_Struct_Literal: {
        AstStructLiteral* sl = (AstStructLiteral *) node;

        Type* sl_type = sl->type;
        // ROBUSTNESS: Handle cases for slices and dynamic arrays
        if (sl_type->kind != Type_Kind_Struct && sl_type->kind != Type_Kind_Slice && sl_type->kind != Type_Kind_DynArray) {
            retval = 0;
            break;
        }

        i32 mem_count = type_structlike_mem_count(sl_type);
        StructMember smem;

        fori (i, 0, mem_count) {
            type_lookup_member_by_idx(sl_type, i, &smem);
            retval &= emit_raw_data_(mod, bh_pointer_add(data, smem.offset), sl->args.values[i]);
        }

        break;
    }

    case Ast_Kind_StrLit: {
        AstStrLit* sl = (AstStrLit *) node;

        // NOTE: This assumes the address and the length fields have been filled out
        // by emit_string_literal.
        u32* sdata = (u32 *) data;
        sdata[0] = sl->addr;
        sdata[1] = 0x00;
        sdata[2] = sl->length;
        break;
    }

    case Ast_Kind_Enum_Value: {
        AstEnumValue* ev = (AstEnumValue *) node;
        retval &= emit_raw_data_(mod, data, (AstTyped *) ev->value);
        break;
    }

    case Ast_Kind_Function: {
        AstFunction* func = (AstFunction *) node;
        *((u32 *) data) = get_element_idx(mod, func);
        break;
    }

    case Ast_Kind_Size_Of: {
        AstSizeOf* so = (AstSizeOf *) node;
        *((u32 *) data) = so->size;
        break;
    }

    case Ast_Kind_Align_Of: {
        AstAlignOf* ao = (AstAlignOf *) node;
        *((u32 *) data) = ao->alignment;
        break;
    }

    case Ast_Kind_NumLit: {
        // NOTE: This makes a big assumption that we are running on a
        // little endian machine, since WebAssembly is little endian
        // by specification. This is probably a safe assumption, but
        // should probably be fixed at some point.
        //                                - brendanfh  2020/12/15

        Type* effective_type = node->type;

        if (effective_type->kind == Type_Kind_Enum)
            effective_type = effective_type->Enum.backing;

        switch (effective_type->Basic.kind) {
        case Basic_Kind_Bool:
        case Basic_Kind_I8:
        case Basic_Kind_U8:
            *((i8 *) data) = (i8) ((AstNumLit *) node)->value.i;
            return retval;

        case Basic_Kind_I16:
        case Basic_Kind_U16:
            *((i16 *) data) = (i16) ((AstNumLit *) node)->value.i;
            return retval;

        case Basic_Kind_I32:
        case Basic_Kind_U32:
            *((i32 *) data) = ((AstNumLit *) node)->value.i;
            return retval;

        case Basic_Kind_I64:
        case Basic_Kind_U64:
        case Basic_Kind_Rawptr:
            *((i64 *) data) = ((AstNumLit *) node)->value.l;
            return retval;

        case Basic_Kind_F32:
            *((f32 *) data) = ((AstNumLit *) node)->value.f;
            return retval;

        case Basic_Kind_F64:
            *((f64 *) data) = ((AstNumLit *) node)->value.d;
            return retval;

        default: break;
        }

        //fallthrough
    }

    case Ast_Kind_Code_Block: break;

    default: retval = 0;
    }

    return retval;
}

static void emit_memory_reservation(OnyxWasmModule* mod, AstMemRes* memres) {
    Type* effective_type = memres->type;

    u64 alignment = type_alignment_of(effective_type);
    u64 size = type_size_of(effective_type);

    if (type_table_node != NULL && (AstMemRes *) type_table_node == memres) {
        u64 table_location = build_type_table(mod);
        memres->addr = table_location;

        return;
    }

    if (memres->threadlocal) {
        memres->addr = mod->next_tls_offset;
        bh_align(memres->addr, alignment);
        mod->next_tls_offset = memres->addr + size;

    } else {
        memres->addr = mod->next_datum_offset;
        bh_align(memres->addr, alignment);
        mod->next_datum_offset = memres->addr + size;
    }

    if (memres->initial_value != NULL) {
        assert(!memres->threadlocal);

        u8* data = bh_alloc(global_heap_allocator, size);
        emit_raw_data(mod, data, memres->initial_value);

        WasmDatum datum = {
            .offset = memres->addr,
            .length = size,
            .data = data,
        };

        bh_arr_push(mod->data, datum);
    }
}

static void emit_file_contents(OnyxWasmModule* mod, AstFileContents* fc) {
    token_toggle_end(fc->filename_token);

    // INVESTIGATE: I think that filename should always be NULL when this function starts because
    // a file contents entity is only processed once and therefore should only go through this step
    // once. But somehow filename isn't NULL occasionally so I have to check for that...
    //                                                                      - brendanfh  2021/05/23
    if (fc->filename == NULL) {
        const char* parent_file = fc->token->pos.filename;
        if (parent_file == NULL) parent_file = ".";

        char* parent_folder = bh_path_get_parent(parent_file, global_scratch_allocator);

        char* temp_fn     = bh_alloc_array(global_scratch_allocator, char, fc->filename_token->length);
        i32   temp_fn_len = string_process_escape_seqs(temp_fn, fc->filename_token->text, fc->filename_token->length);
        char* filename    = lookup_included_file(temp_fn, parent_folder, 0, 0);
        fc->filename      = bh_strdup(global_heap_allocator, filename);
    }

    token_toggle_end(fc->filename_token);

    if (bh_table_has(StrLitInfo, mod->loaded_file_info, fc->filename)) {
        StrLitInfo info = bh_table_get(StrLitInfo, mod->loaded_file_info, fc->filename);
        fc->addr = info.addr;
        fc->size = info.len;
        return;
    }

    u32 offset = mod->next_datum_offset;
    bh_align(offset, 16);

    if (!bh_file_exists(fc->filename)) {
        onyx_report_error(fc->filename_token->pos,
                "Unable to open file for reading, '%s'.",
                fc->filename);
        return;
    }

    // :RelativeFiles This should be relative to the current directory by default; However,
    // if the filename is prefixed with a './' or '.\\' then it should be relative to the
    // file in which is was inclded. The loaded file info above should probably use the full
    // file path in order to avoid duplicates.
    bh_file_contents contents = bh_file_read_contents(global_heap_allocator, fc->filename);
    u8* actual_data = bh_alloc(global_heap_allocator, contents.length + 1);
    u32 length = contents.length + 1;
    memcpy(actual_data, contents.data, contents.length);
    actual_data[contents.length] = 0;
    bh_file_contents_free(&contents);

    bh_table_put(StrLitInfo, mod->loaded_file_info, fc->filename, ((StrLitInfo) {
        .addr = offset,
        .len  = length - 1,
    }));

    fc->addr = offset;
    fc->size = length - 1;

    WasmDatum datum = {
        .offset = offset,
        .length = length,
        .data = actual_data,
    };

    bh_arr_push(mod->data, datum);

    mod->next_datum_offset = offset + length;
}

OnyxWasmModule onyx_wasm_module_create(bh_allocator alloc) {
    OnyxWasmModule module = {
        .allocator = alloc,

        .type_map = NULL,
        .next_type_idx = 0,
        .types = NULL,

        .funcs = NULL,
        .next_func_idx = 0,

        .exports = NULL,
        .export_count = 0,

        .imports = NULL,

        .globals = NULL,
        .next_global_idx = 0,

        .data = NULL,
        .next_datum_offset = 32, // Starting offset so null pointers don't immediately
                                 // break constant data.       - brendanfh 2020/12/16

        .next_tls_offset = 0,

        .elems = NULL,
        .next_elem_idx = 0,

        .structured_jump_target = NULL,
        .return_location_stack = NULL,
        .local_allocations = NULL,
        .stack_leave_patches = NULL,
        .deferred_stmts = NULL,

        .stack_top_ptr = NULL,
        .stack_base_idx = 0,

        .foreign_function_count = 0,

        .null_proc_func_idx = -1,
    };

    bh_arena* eid = bh_alloc(global_heap_allocator, sizeof(bh_arena));
    bh_arena_init(eid, global_heap_allocator, 8196);
    module.extended_instr_data = eid;
    module.extended_instr_alloc = bh_arena_allocator(eid);

    bh_arr_new(alloc, module.types, 4);
    bh_arr_new(alloc, module.funcs, 4);
    bh_arr_new(alloc, module.imports, 4);
    bh_arr_new(alloc, module.globals, 4);
    bh_arr_new(alloc, module.data, 4);
    bh_arr_new(alloc, module.elems, 4);

    bh_arr_new(global_heap_allocator, module.return_location_stack, 4);
    bh_arr_new(global_heap_allocator, module.structured_jump_target, 16);
    bh_arr_set_length(module.structured_jump_target, 0);

    bh_table_init(global_heap_allocator, module.type_map, 61);
    bh_table_init(global_heap_allocator, module.exports, 61);
    bh_table_init(global_heap_allocator, module.loaded_file_info, 7);
    bh_table_init(global_heap_allocator, module.string_literals, 16);

    bh_imap_init(&module.index_map, global_heap_allocator, 128);
    bh_imap_init(&module.local_map, global_heap_allocator, 16);
    bh_imap_init(&module.elem_map,  global_heap_allocator, 16);

    bh_arr_new(global_heap_allocator, module.deferred_stmts, 4);
    bh_arr_new(global_heap_allocator, module.local_allocations, 4);
    bh_arr_new(global_heap_allocator, module.stack_leave_patches, 4);

    if (context.options->use_multi_threading) {
        WasmImport mem_import = {
            .kind   = WASM_FOREIGN_MEMORY,
            .min    = 1024,
            .max    = 65536, // NOTE: Why not use all 4 Gigs of memory?
            .shared = context.options->runtime != Runtime_Onyx,

            .mod    = "onyx",
            .name   = "memory",
        };

        bh_arr_push(module.imports, mem_import);
    }

    WasmExport mem_export = {
        .kind = WASM_FOREIGN_MEMORY,
        .idx = 0,
    };

    bh_table_put(WasmExport, module.exports, "memory", mem_export);
    module.export_count++;

    WasmExport func_table_export = {
        .kind = WASM_FOREIGN_TABLE,
        .idx  = 0,
    };
    bh_table_put(WasmExport, module.exports, "func_table", func_table_export);
    module.export_count++;

    return module;
}

void emit_entity(Entity* ent) {
    OnyxWasmModule* module = context.wasm_module;

    if (module->stack_top_ptr) {
        *module->stack_top_ptr = module->next_datum_offset;

        if (*module->stack_top_ptr % 16 != 0) {
            *module->stack_top_ptr += 16 - (*module->stack_top_ptr % 16);
        }

        builtin_heap_start.value.i = *module->stack_top_ptr + (1 << 20);
        if (builtin_heap_start.value.i % 16 != 0) {
            builtin_heap_start.value.i += 16 - (builtin_heap_start.value.i % 16);
        }
    }

    switch (ent->type) {
        case Entity_Type_Foreign_Function_Header:
            if (!should_emit_function(ent->function)) break;

            module->foreign_function_count++;
            emit_foreign_function(module, ent->function);
            // fallthrough

        case Entity_Type_Function_Header:
            if (!should_emit_function(ent->function)) break;

            if (context.options->print_function_mappings) {
                bh_printf("%d -> %s:%d:%d\n",
                    module->next_func_idx,
                    ent->expr->token->pos.filename,
                    ent->expr->token->pos.line,
                    ent->expr->token->pos.column);
            }

            bh_imap_put(&module->index_map, (u64) ent->function, module->next_func_idx++);

            if (ent->function->flags & Ast_Flag_Proc_Is_Null) {
                if (module->null_proc_func_idx == -1) module->null_proc_func_idx = get_element_idx(module, ent->function);
            }
            break;

        case Entity_Type_Global_Header:
            bh_imap_put(&module->index_map, (u64) ent->global, module->next_global_idx++);
            break;

        case Entity_Type_String_Literal: {
            emit_string_literal(module, (AstStrLit *) ent->strlit);
            break;
        }

        case Entity_Type_File_Contents: {
            emit_file_contents(module, (AstFileContents *) ent->file_contents);
            break;
        }

        case Entity_Type_Memory_Reservation: {
            emit_memory_reservation(module, (AstMemRes *) ent->mem_res);
            break;
        }

        case Entity_Type_Process_Directive: {
            if (ent->expr->kind == Ast_Kind_Directive_Export) {
                emit_export_directive(module, (AstDirectiveExport *) ent->expr);
            }
            break;
        }

        case Entity_Type_Function: emit_function(module, ent->function); break;
        case Entity_Type_Global:   emit_global(module,   ent->global); break;

        // Cleanup: Maybe these should be printed elsewhere?
        // Also, they should be sorted? Or have that ability?
        case Entity_Type_Note: {
            if (!context.options->print_notes) break;

            AstNote* note = (AstNote *) ent->expr;
            OnyxFilePos pos = note->token->pos;

            bh_printf("Note: %b %s:%d:%d\n", note->token->text, note->token->length, pos.filename, pos.line, pos.column);

            break;
        }

        default: break;
    }

    ent->state = Entity_State_Finalized;
}

void onyx_wasm_module_free(OnyxWasmModule* module) {
    if (module->extended_instr_data != NULL)
        bh_arena_free(module->extended_instr_data);

    bh_arr_free(module->types);
    bh_arr_free(module->funcs);
    bh_imap_free(&module->local_map);
    bh_imap_free(&module->index_map);
    bh_table_free(module->type_map);
    bh_table_free(module->exports);
}


#include "wasm_output.c"
