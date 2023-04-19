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

static b32 onyx_type_is_stored_in_memory(Type *type) {
    if (type_struct_is_just_one_basic_value(type)) return 0;

    return type->kind == Type_Kind_Struct
        || type->kind == Type_Kind_DynArray;
}

static WasmType onyx_type_to_wasm_type(Type* type) {
    if (onyx_type_is_stored_in_memory(type)) return WASM_TYPE_PTR;

    if (type_struct_is_just_one_basic_value(type)) {
        Type *asdf = type_struct_is_just_one_basic_value(type);
        return onyx_type_to_wasm_type(asdf);
    }

    if (type->kind == Type_Kind_Slice)    return WASM_TYPE_VOID;
    if (type->kind == Type_Kind_Compound) return WASM_TYPE_VOID;
    if (type->kind == Type_Kind_Enum)     return onyx_type_to_wasm_type(type->Enum.backing);
    if (type->kind == Type_Kind_Distinct) return onyx_type_to_wasm_type(type->Distinct.base_type);
    if (type->kind == Type_Kind_Pointer)  return WASM_TYPE_PTR;
    if (type->kind == Type_Kind_Array)    return WASM_TYPE_PTR;
    if (type->kind == Type_Kind_Function) return WASM_TYPE_VOID;
    if (type->kind == Type_Kind_MultiPointer) return WASM_TYPE_PTR;

    if (type->kind == Type_Kind_Basic) {
        TypeBasic* basic = &type->Basic;
        if (basic->flags & Basic_Flag_Boolean) return WASM_TYPE_INT32;
        if (basic->flags & Basic_Flag_Integer) {
            if (basic->size <= 4) return WASM_TYPE_INT32;
            if (basic->size == 8) return WASM_TYPE_INT64;
        }
        if (basic->flags & Basic_Flag_Pointer) return WASM_TYPE_PTR;
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

static b32 onyx_type_is_multiple_wasm_values(Type *type) {
    // Dynamic arrays and slices are represented as a single pointer to
    // data for the structure.
    return type->kind == Type_Kind_Slice
        || type->kind == Type_Kind_VarArgs;
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
    if (local->type->kind == Type_Kind_Distinct && local->type->Distinct.base_type->kind == Type_Kind_Basic) return 1;
    if (local->type->kind == Type_Kind_Pointer) return 1;
    if (local->type->kind == Type_Kind_MultiPointer) return 1;
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

static u64 local_allocate_type_in_memory(LocalAllocator* la, Type *type) {
    u32 size = type_size_of(type);
    u32 alignment = type_alignment_of(type);

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

static u64 local_allocate(LocalAllocator* la, AstTyped* local) {
    if (local_is_wasm_local(local)) {
        WasmType wt = onyx_type_to_wasm_type(local->type);
        return local_raw_allocate(la, wt);

    } else {
        return local_allocate_type_in_memory(la, local->type);
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


static inline b32 should_emit_function(AstFunction* fd) {
    // NOTE: Don't output intrinsic functions
    if (fd->is_intrinsic) return 0;

    // NOTE: Don't output functions that are not used, only if
    // they are also not exported.
    if ((fd->flags & Ast_Flag_Function_Used) == 0) {
        if (fd->is_exported || bh_arr_length(fd->tags) > 0) {
            return 1;
        } else {
            return 0;
        }
    }

    return 1;
}


//
// Debug Info Generation
//

#ifdef ENABLE_DEBUG_INFO

static u32 debug_introduce_symbol(OnyxWasmModule *mod, OnyxToken *token, DebugSymLoc loc, u64 num, Type* type) {

    u32 id = mod->debug_context->next_sym_id++;

    DebugSymInfo sym_info;
    sym_info.sym_id = id;
    sym_info.location_type = loc;
    sym_info.location_num  = num;
    sym_info.type          = type->id;

    if (token) {
        token_toggle_end(token);
        sym_info.name = bh_strdup(context.ast_alloc, token->text);
        token_toggle_end(token);
    } else {
        sym_info.name = NULL;
    }

    bh_arr_push(mod->debug_context->sym_info, sym_info);

    if (loc == DSL_REGISTER) {
        assert(mod->local_alloc);
        DebugSymPatch patch;
        patch.func_idx = mod->current_func_idx;
        patch.sym_id = id;
        patch.local_idx = num;
        bh_arr_push(mod->debug_context->sym_patches, patch);
    }

    bh_buffer_write_byte(&mod->debug_context->op_buffer, DOT_SYM);
    u32 leb_len=0;
    u8 *bytes = uint_to_uleb128(id, &leb_len);
    bh_buffer_append(&mod->debug_context->op_buffer, bytes, leb_len);

    mod->debug_context->last_op_was_rep = 0;
    return id;
}

static u32 debug_get_file_id(OnyxWasmModule *mod, const char *name) {
    assert(mod && mod->debug_context);

    // This is not correct at all, but it will not cause an error.
    if (!name) return 0;

    i32 index = shgeti(mod->debug_context->file_info, name);
    if (index == -1) {
        u32 id = mod->debug_context->next_file_id++;
        DebugFileInfo file_info;
        file_info.file_id = id;

        bh_arr_each(bh_file_contents, fc, context.loaded_files) {
            if (!strcmp(fc->filename, name)) {
                file_info.line_count = fc->line_count;
            }
        }

        shput(mod->debug_context->file_info, name, file_info);
        return id;
    }

    return mod->debug_context->file_info[index].value.file_id;
}

static void debug_set_position(OnyxWasmModule *mod, OnyxToken *token) {
    i32 file_id = debug_get_file_id(mod, token->pos.filename);

    bh_buffer_write_byte(&mod->debug_context->op_buffer, DOT_SET);
    mod->debug_context->last_op_was_rep = 0;

    u32 leb_len=0;
    u8 *bytes = uint_to_uleb128(file_id, &leb_len);
    bh_buffer_append(&mod->debug_context->op_buffer, bytes, leb_len);

    bytes = uint_to_uleb128(token->pos.line, &leb_len);
    bh_buffer_append(&mod->debug_context->op_buffer, bytes, leb_len);

    mod->debug_context->last_token = token;
}

// Called for every instruction being emitted
// This has to emit either:
//    - INC
//    - DEC
//    - REP
//    - SET, REP 0
static void debug_emit_instruction(OnyxWasmModule *mod, OnyxToken *token) {
    if (!context.options->debug_enabled) {
        return;
    }

    DebugContext *ctx = mod->debug_context; 
    assert(ctx && ctx->last_token);

    // Sanity check
    if (ctx->last_op_was_rep) {
        assert((ctx->op_buffer.data[ctx->op_buffer.length - 1] & DOT_REP) == DOT_REP);
    }
    i32 file_id, old_file_id;

    b32 repeat_previous = 0;
    if (!token || !token->pos.filename) {
        repeat_previous = 1;

    } else {
        file_id = debug_get_file_id(mod, token->pos.filename);
        old_file_id = debug_get_file_id(mod, ctx->last_token->pos.filename);
        if (old_file_id == file_id && token->pos.line == ctx->last_token->pos.line) {
            repeat_previous = 1;
        }
    }
    
    if (repeat_previous) {
        // Output / increment REP instruction
        if (ctx->last_op_was_rep) {
            u8 rep_count = ctx->op_buffer.data[ctx->op_buffer.length - 1] & 0b00111111;
            if (rep_count != 63) {
                rep_count += 1;
                ctx->op_buffer.data[ctx->op_buffer.length - 1] = DOT_REP | rep_count;
                return;
            }
        }

        bh_buffer_write_byte(&mod->debug_context->op_buffer, DOT_REP);
        ctx->last_op_was_rep = 1;
        return;
    }

    // At this point, token is definitely non-null.
    ctx->last_op_was_rep = 0;

    if (old_file_id == file_id) {
        // We see if we can INC/DEC to get to the line number
        if (ctx->last_token->pos.line < token->pos.line) {
            u32 diff = token->pos.line - ctx->last_token->pos.line;
            if (diff <= 64) {
                bh_buffer_write_byte(&mod->debug_context->op_buffer, DOT_INC | (diff - 1));
                goto done;
            }
        }

        if (ctx->last_token->pos.line > token->pos.line) {
            u32 diff = ctx->last_token->pos.line - token->pos.line;
            if (diff <= 64) {
                bh_buffer_write_byte(&mod->debug_context->op_buffer, DOT_DEC | (diff - 1));
                goto done;
            }
        }
    }

    // Otherwise, we need to output a SET, followed by a REP 0,
    // which is what set_position does.
    debug_set_position(mod, token);
    bh_buffer_write_byte(&mod->debug_context->op_buffer, DOT_REP);
    ctx->last_op_was_rep = 1;

  done:
    ctx->last_token = token;
}

static void debug_begin_function(OnyxWasmModule *mod, u32 func_idx, OnyxToken *token, char *name) {
    u32 file_id = debug_get_file_id(mod, token->pos.filename);
    u32 line    = token->pos.line;

    assert(mod->debug_context);

    DebugFuncContext func;
    func.func_index = func_idx;
    func.file_id = file_id;
    func.line    = line;
    func.op_offset = mod->debug_context->op_buffer.length;
    func.stack_ptr_idx = 0;
    func.name_length = strlen(name);
    func.name = name;
    bh_arr_push(mod->debug_context->funcs, func);

    bh_buffer_write_byte(&mod->debug_context->op_buffer, DOT_PUSHF);
    debug_set_position(mod, token);
}

static void debug_function_set_ptr_idx(OnyxWasmModule *mod, u32 func_idx, u64 ptr_idx) {
    bh_arr_each(DebugFuncContext, func, mod->debug_context->funcs) {
        if (func->func_index == func_idx) {
            func->stack_ptr_idx = ptr_idx;
        }
    }
}

static void debug_end_function(OnyxWasmModule *mod) {
    bh_buffer_write_byte(&mod->debug_context->op_buffer, DOT_POPF);
    bh_buffer_write_byte(&mod->debug_context->op_buffer, DOT_END);
    mod->debug_context->last_op_was_rep = 0;
    mod->debug_context->last_token = NULL;
}

static void debug_enter_symbol_frame(OnyxWasmModule *mod) {
    bh_buffer_write_byte(&mod->debug_context->op_buffer, DOT_PUSHF);
    mod->debug_context->last_op_was_rep = 0;
}

static void debug_leave_symbol_frame(OnyxWasmModule *mod) {
    bh_buffer_write_byte(&mod->debug_context->op_buffer, DOT_POPF);
    mod->debug_context->last_op_was_rep = 0;
}

#else

#define debug_introduce_symbol(mod, name, loc, num, type) (void)0
#define debug_get_file_id(mod, name) (void)0
#define debug_set_position(mod, token) (void)0
#define debug_emit_instruction(mod, token) (void)0
#define debug_begin_function(mod, idx, token, name) (void)0
#define debug_function_set_ptr_idx(mod, func_idx, ptr_idx) (void)0
#define debug_end_function(mod) (void)0
#define debug_enter_symbol_frame(mod) (void)0
#define debug_leave_symbol_frame(mod) (void)0

#endif


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

#ifdef ENABLE_DEBUG_INFO
    #define WI(token, instr) (debug_emit_instruction(mod, token),        bh_arr_push(code, ((WasmInstruction){ instr, 0x00 })))
    #define WID(token, instr, data) (debug_emit_instruction(mod, token), bh_arr_push(code, ((WasmInstruction){ instr, data })))
    #define WIL(token, instr, data) (debug_emit_instruction(mod, token), bh_arr_push(code, ((WasmInstruction){ instr, { .l = data } })))
    #define WIP(token, instr, data) (debug_emit_instruction(mod, token), bh_arr_push(code, ((WasmInstruction){ instr, { .p = data } })))
    #define WIR(token, full_instr)  (debug_emit_instruction(mod, token), bh_arr_push(code, full_instr))
#else
    #define WI(token, instr) (bh_arr_push(code, ((WasmInstruction){ instr, 0x00 })))
    #define WID(token, instr, data) (bh_arr_push(code, ((WasmInstruction){ instr, data })))
    #define WIL(token, instr, data) (bh_arr_push(code, ((WasmInstruction){ instr, { .l = data } })))
    #define WIP(token, instr, data) (bh_arr_push(code, ((WasmInstruction){ instr, { .p = data } })))
    #define WIR(token, full_instr)  (bh_arr_push(code, full_instr))
#endif

#define EMIT_FUNC(kind, ...) static void emit_ ## kind (OnyxWasmModule* mod, bh_arr(WasmInstruction)* pcode, __VA_ARGS__)
#define EMIT_FUNC_RETURNING(ret_type, kind, ...) static ret_type emit_ ## kind (OnyxWasmModule* mod, bh_arr(WasmInstruction)* pcode, __VA_ARGS__)
#define EMIT_FUNC_NO_ARGS(kind) static void emit_ ## kind (OnyxWasmModule* mod, bh_arr(WasmInstruction)* pcode)
#define STACK_SWAP(token, type1, type2) { \
    u64 t0 = local_raw_allocate(mod->local_alloc, type1); \
    u64 t1 = local_raw_allocate(mod->local_alloc, type2); \
                                                          \
    WIL(token, WI_LOCAL_SET, t0);                         \
    WIL(token, WI_LOCAL_SET, t1);                         \
    WIL(token, WI_LOCAL_GET, t0);                         \
    WIL(token, WI_LOCAL_GET, t1);                         \
                                                          \
    local_raw_free(mod->local_alloc, type1);              \
    local_raw_free(mod->local_alloc, type2);              \
    }
#define SUBMIT_PATCH(patch_arr, offset) bh_arr_push((patch_arr), ((PatchInfo) { bh_arr_length(code) - offset }))
#define NEXT_DATA_ID(mod)               ((u32) bh_arr_length((mod)->data) + 1)

EMIT_FUNC(function_body,                   AstFunction* fd);
EMIT_FUNC(block,                           AstBlock* block, b32 generate_block_headers);
EMIT_FUNC(statement,                       AstNode* stmt);
EMIT_FUNC_RETURNING(u64, local_allocation, AstTyped* stmt);
EMIT_FUNC_NO_ARGS(free_local_allocations);
EMIT_FUNC(data_relocation,                 u32 data_id);
EMIT_FUNC(assignment,                      AstBinaryOp* assign);
EMIT_FUNC(assignment_of_array,             AstTyped* left, AstTyped* right);
EMIT_FUNC(compound_assignment,             AstBinaryOp* assign);
EMIT_FUNC(store_instruction,               Type* type, u32 offset);
EMIT_FUNC(flip_and_store_instruction,      AstTyped *lval, OnyxToken *token);
EMIT_FUNC(generic_store_instruction,       AstTyped *lval, OnyxToken *token);
EMIT_FUNC(load_instruction,                Type* type, u32 offset);
EMIT_FUNC(load_with_ignored_instruction,   Type* type, u32 offset, i32 ignored_value_count);
EMIT_FUNC(if,                              AstIfWhile* if_node);
EMIT_FUNC(while,                           AstIfWhile* while_node);
EMIT_FUNC(for,                             AstFor* for_node);
EMIT_FUNC(switch,                          AstSwitch* switch_node);
EMIT_FUNC(defer,                           AstDefer* defer);
EMIT_FUNC(defer_code,                      WasmInstruction* deferred_code, u32 code_count);
EMIT_FUNC(deferred_stmt,                   DeferredStmt deferred_stmt);
EMIT_FUNC_NO_ARGS(deferred_stmts);
EMIT_FUNC(remove_directive,                AstDirectiveRemove* remove);
EMIT_FUNC(binop,                           AstBinaryOp* binop);
EMIT_FUNC(unaryop,                         AstUnaryOp* unop);
EMIT_FUNC(call,                            AstCall* call);
EMIT_FUNC(intrinsic_call,                  AstCall* call);
EMIT_FUNC(subscript_location,              AstSubscript* sub, u64* offset_return);
EMIT_FUNC(field_access_location,           AstFieldAccess* field, u64* offset_return);
EMIT_FUNC(local_location,                  AstLocal* local, u64* offset_return);
EMIT_FUNC(memory_reservation_location,     AstMemRes* memres);
EMIT_FUNC(location_return_offset,          AstTyped* expr, u64* offset_return);
EMIT_FUNC(location,                        AstTyped* expr);
EMIT_FUNC(compound_load,                   Type* type, u64 offset, i32 ignored_value_count);
EMIT_FUNC(compound_store,                  Type* type, u64 offset, b32 location_first);
EMIT_FUNC(struct_store,                    Type* type, u32 offset);
EMIT_FUNC(struct_literal,                  AstStructLiteral* sl);
EMIT_FUNC(struct_as_separate_values,       Type *type, u32 offset);
EMIT_FUNC(array_store,                     Type* type, u32 offset);
EMIT_FUNC(array_literal,                   AstArrayLiteral* al);
EMIT_FUNC(range_literal,                   AstRangeLiteral* range);
EMIT_FUNC_NO_ARGS(load_slice);
EMIT_FUNC(if_expression,                   AstIfExpression* if_expr);
EMIT_FUNC(do_block,                        AstDoBlock* doblock);
EMIT_FUNC(expression,                      AstTyped* expr);
EMIT_FUNC(cast,                            AstUnaryOp* cast);
EMIT_FUNC(return,                          AstReturn* ret);
EMIT_FUNC(stack_enter,                     u64 stacksize);
EMIT_FUNC(zero_value,                      WasmType wt);
EMIT_FUNC(zero_value_for_type,             Type* type, OnyxToken* where, AstTyped *alloc_node);
EMIT_FUNC(stack_address,                   u32 offset, OnyxToken *token);
EMIT_FUNC(values_into_contiguous_memory,   u64 base_ptr_local, Type *type, u32 offset, i32 value_count, AstTyped **values);
EMIT_FUNC(struct_literal_into_contiguous_memory, AstStructLiteral* sl, u64 base_ptr_local, u32 offset);
EMIT_FUNC(wasm_copy,                       OnyxToken *token);
EMIT_FUNC(wasm_fill,                       OnyxToken *token);

EMIT_FUNC(enter_structured_block,        StructuredBlockType sbt, OnyxToken* block_token);
EMIT_FUNC_NO_ARGS(leave_structured_block);

static u32 emit_data_entry(OnyxWasmModule *mod, WasmDatum *datum);

static void emit_constexpr(ConstExprContext *ctx, AstTyped *node, u32 offset);
static b32 emit_constexpr_(ConstExprContext *ctx, AstTyped *node, u32 offset);

#include "wasm_intrinsics.h"
#include "wasm_type_table.h"

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
                                                : SBT_Breakable_Block,
                                                block->token);
        debug_enter_symbol_frame(mod);
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

    if (generate_block_headers) {
        emit_leave_structured_block(mod, &code);
        debug_leave_symbol_frame(mod);
    }

    *pcode = code;
}

EMIT_FUNC(enter_structured_block, StructuredBlockType sbt, OnyxToken* token) {
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


    WID(token, block_instrs[sbt], 0x40);
    bh_arr_push(mod->structured_jump_target, jump_numbers[sbt]);

    *pcode = code;
}

EMIT_FUNC_NO_ARGS(leave_structured_block) {
    bh_arr(WasmInstruction) code = *pcode;

    WI(NULL, WI_BLOCK_END);
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
            WID(jump->token, WI_JUMP, labelidx);
    } else {
        onyx_report_error(jump->token->pos, Error_Critical, "Invalid structured jump.");
    }

    *pcode = code;
}

EMIT_FUNC(statement, AstNode* stmt) {
    bh_arr(WasmInstruction) code = *pcode;

#ifdef ENABLE_DEBUG_INFO
    debug_set_position(mod, stmt->token);
#endif

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

        case Ast_Kind_Directive_Remove: emit_remove_directive(mod, &code, (AstDirectiveRemove *) stmt); break;
        case Ast_Kind_Directive_Insert: break;

        default:                  emit_expression(mod, &code, (AstTyped *) stmt); break;
    }

    *pcode = code;
}

EMIT_FUNC_RETURNING(u64, local_allocation, AstTyped* stmt) {
    //
    // If the statement does not have a type, it should not
    // be emitted. The only case this should be used by is
    // when a local is declared with "#auto" type and is
    // never used, therefore never declaring its type.
    if (stmt->type == NULL) {
        assert(stmt->kind == Ast_Kind_Local);
        onyx_report_warning(stmt->token->pos, "Unused local variable with unassigned type.");
        return 0;
    }

    u64 local_idx = local_allocate(mod->local_alloc, stmt);
    bh_imap_put(&mod->local_map, (u64) stmt, local_idx);

    if (stmt->kind == Ast_Kind_Local) {
        if (stmt->token && local_is_wasm_local(stmt)) {
            debug_introduce_symbol(mod, stmt->token, DSL_REGISTER, local_idx, stmt->type);
        } else {
            debug_introduce_symbol(mod, stmt->token, DSL_STACK, local_idx, stmt->type);
        }

        if (!(stmt->flags & Ast_Flag_Decl_Followed_By_Init)) {
            bh_arr(WasmInstruction) code = *pcode;
            if (local_is_wasm_local(stmt)) {
                emit_zero_value(mod, &code, onyx_type_to_wasm_type(stmt->type));
                WIL(stmt->token, WI_LOCAL_SET, local_idx);

            } else {
                emit_location(mod, &code, stmt);
                WID(stmt->token, WI_I32_CONST, 0);
                WID(stmt->token, WI_I32_CONST, type_size_of(stmt->type));
                emit_wasm_fill(mod, &code, NULL);
            }

            *pcode = code;
        }
    }

    bh_arr_push(mod->local_allocations, ((AllocatedSpace) {
        .depth = bh_arr_length(mod->structured_jump_target),
        .expr  = stmt,
    }));

    return local_idx;
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

EMIT_FUNC(data_relocation, u32 data_id) {
    bh_arr(WasmInstruction) code = *pcode;

    u32 instr_idx = bh_arr_length(code);
    WID(NULL, WI_PTR_CONST, 0);
    assert(mod->current_func_idx >= 0);

    DatumPatchInfo patch;
    patch.kind = Datum_Patch_Instruction;
    patch.index = mod->current_func_idx;
    patch.location = instr_idx;
    patch.data_id = data_id;
    patch.offset = 0;
    bh_arr_push(mod->data_patches, patch);

    *pcode = code;
}

EMIT_FUNC(stack_address, u32 offset, OnyxToken *token) {
    bh_arr(WasmInstruction) code = *pcode;

    WIL(token, WI_LOCAL_GET, mod->stack_base_idx);

    if (offset > 0) {
        WIL(token, WI_PTR_CONST, offset);
        WI(token, WI_PTR_ADD);
    }

    *pcode = code;
}

EMIT_FUNC(assignment, AstBinaryOp* assign) {
    if (assign->right->type->kind == Type_Kind_Array) {
        emit_assignment_of_array(mod, pcode, assign->left, assign->right);
        return;
    }

    if (assign->right->type->kind == Type_Kind_Compound) {
        emit_compound_assignment(mod, pcode, assign);
        return;
    }

    bh_arr(WasmInstruction) code = *pcode;

    AstTyped* lval = assign->left;

    if (lval->kind == Ast_Kind_Local || lval->kind == Ast_Kind_Param) {
        if (bh_imap_get(&mod->local_map, (u64) lval) & LOCAL_IS_WASM) {
            emit_expression(mod, &code, assign->right);

            u64 localidx = bh_imap_get(&mod->local_map, (u64) lval);

            if (lval->kind == Ast_Kind_Param && onyx_type_is_multiple_wasm_values(lval->type)) {
                u32 mem_count = type_structlike_mem_count(lval->type);
                fori (i, 0, mem_count) WIL(assign->token, WI_LOCAL_SET, localidx + i);

            } else {
                WIL(assign->token, WI_LOCAL_SET, localidx);
            }

            *pcode = code;
            return;
        }
    }

    if (lval->kind == Ast_Kind_Global) {
        emit_expression(mod, &code, assign->right);

        i32 globalidx = (i32) bh_imap_get(&mod->index_map, (u64) lval);
        WID(assign->token, WI_GLOBAL_SET, globalidx);

        *pcode = code;
        return;
    }

    if (assign->right->kind == Ast_Kind_Struct_Literal && onyx_type_is_stored_in_memory(assign->right->type)) {
        u64 offset = 0;
        emit_location_return_offset(mod, &code, lval, &offset);

        u64 base_ptr_local = local_raw_allocate(mod->local_alloc, WASM_TYPE_PTR);
        WIL(NULL, WI_LOCAL_SET, base_ptr_local);

        emit_struct_literal_into_contiguous_memory(mod, &code, (AstStructLiteral *) assign->right, base_ptr_local, offset);

        local_raw_free(mod->local_alloc, WASM_TYPE_PTR);

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
        WIL(left->token, WI_LOCAL_SET, lptr_local);

        AstArrayLiteral* al = (AstArrayLiteral *) right;
        fori (i, 0, elem_count) {
            WIL(left->token, WI_LOCAL_GET, lptr_local);
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

    if (assign->left->kind != Ast_Kind_Compound) {
        emit_generic_store_instruction(mod, &code, (AstTyped *) assign->left, assign->token);
        *pcode = code;
        return;
    }

    // It is assumed at this point that the correct number
    // of expression/values will be on the stack to consume.
    //
    // In reverse, for each location to store on the left hand side,
    // store the values on the stack into their respective locations.
    //
    AstCompound* compound_lval = (AstCompound *) assign->left;
    bh_arr_rev_each(AstTyped *, plval, compound_lval->exprs) {
        emit_generic_store_instruction(mod, &code, *plval, assign->token);
    }

    *pcode = code;
    return;
}

EMIT_FUNC(store_instruction, Type* type, u32 offset) {
    bh_arr(WasmInstruction) code = *pcode;

    if (onyx_type_is_stored_in_memory(type)) {
        emit_struct_store(mod, pcode, type, offset);
        return;
    }

    if (type->kind == Type_Kind_Array) {
        emit_array_store(mod, pcode, type, offset);
        return;
    }

    if (type_is_compound(type)) {
        emit_compound_store(mod, pcode, type, offset, 0);
        return;
    }

    if (type->kind == Type_Kind_Struct)   type = type_struct_is_just_one_basic_value(type);
    if (type->kind == Type_Kind_Enum)     type = type->Enum.backing;
    if (type->kind == Type_Kind_Distinct) type = type->Distinct.base_type;
    if (type->kind == Type_Kind_Function) assert(5678 && 0);

    assert(type);

    u32 alignment = type_get_alignment_log2(type);

    i32 store_size  = type_size_of(type);
    i32 is_basic    = type->kind == Type_Kind_Basic || type->kind == Type_Kind_Pointer || type->kind == Type_Kind_MultiPointer;

    if (is_basic && (type->Basic.flags & Basic_Flag_Pointer)) {
        WID(NULL, WI_I32_STORE, ((WasmInstructionData) { 2, offset }));
    } else if (is_basic && ((type->Basic.flags & Basic_Flag_Integer)
                         || (type->Basic.flags & Basic_Flag_Boolean)
                         || (type->Basic.flags & Basic_Flag_Type_Index))) {
        if      (store_size == 1)   WID(NULL, WI_I32_STORE_8,  ((WasmInstructionData) { alignment, offset }));
        else if (store_size == 2)   WID(NULL, WI_I32_STORE_16, ((WasmInstructionData) { alignment, offset }));
        else if (store_size == 4)   WID(NULL, WI_I32_STORE,    ((WasmInstructionData) { alignment, offset }));
        else if (store_size == 8)   WID(NULL, WI_I64_STORE,    ((WasmInstructionData) { alignment, offset }));
    } else if (is_basic && (type->Basic.flags & Basic_Flag_Float)) {
        if      (store_size == 4)   WID(NULL, WI_F32_STORE, ((WasmInstructionData) { alignment, offset }));
        else if (store_size == 8)   WID(NULL, WI_F64_STORE, ((WasmInstructionData) { alignment, offset }));
    } else if (is_basic && (type->Basic.flags & Basic_Flag_SIMD)) {
        WID(NULL, WI_V128_STORE, ((WasmInstructionData) { alignment, offset }));
    } else {
        onyx_report_error((OnyxFilePos) { 0 }, Error_Critical,
            "Failed to generate store instruction for type '%s'.",
            type_get_name(type));
    }

    *pcode = code;
}

EMIT_FUNC(flip_and_store_instruction, AstTyped *lval, OnyxToken *token) {
    bh_arr(WasmInstruction) code = *pcode;

    WasmType wt = onyx_type_to_wasm_type(lval->type);
    u64 expr_tmp = local_raw_allocate(mod->local_alloc, wt);
    WIL(token, WI_LOCAL_SET, expr_tmp);

    emit_location(mod, &code, lval);
    WIL(token, WI_LOCAL_GET, expr_tmp);

    local_raw_free(mod->local_alloc, wt);
    emit_store_instruction(mod, &code, lval->type, 0);

    *pcode = code;
    return;
}

//
// What "store_instruction" should have been. This takes an l-value, assumes
// a value of that type is on the value stack, and then stores it into the l-value,
// doing whatever is necessary.
EMIT_FUNC(generic_store_instruction, AstTyped *lval, OnyxToken *token) {
    bh_arr(WasmInstruction) code = *pcode;

    // If this is a WASM local, simply set the local and continue.
    if (bh_imap_get(&mod->local_map, (u64) lval) & LOCAL_IS_WASM) {
        u64 localidx = bh_imap_get(&mod->local_map, (u64) lval);
        WIL(token, WI_LOCAL_SET, localidx);
    }

    else if (type_is_compound(lval->type)) {
        u64 offset = 0;
        emit_location_return_offset(mod, &code, lval, &offset);
        emit_compound_store(mod, &code, lval->type, offset, 1);
    }

    // Otherwise, you have to do this "fun" sequence of instructions
    // where you temporarily store the top value on the stack, emit
    // the location, and then replace the value. If WASM would have
    // just decided to place the location parameter for the store
    // instructions at the top of the stack, not the second to top,
    // this would not be an issue.
    else {
        emit_flip_and_store_instruction(mod, &code, lval, token);
    }

    *pcode = code;
    return;
}

EMIT_FUNC(load_with_ignored_instruction, Type* type, u32 offset, i32 ignored_value_count) {
    if (type_is_compound(type)) {
        emit_compound_load(mod, pcode, type, offset, ignored_value_count);
        return;
    }

    emit_load_instruction(mod, pcode, type, offset);
}

EMIT_FUNC(load_instruction, Type* type, u32 offset) {
    bh_arr(WasmInstruction) code = *pcode;

    if (type->kind == Type_Kind_Array || onyx_type_is_stored_in_memory(type)) {
        if (offset != 0) {
            WID(NULL, WI_PTR_CONST, offset);
            WI(NULL, WI_PTR_ADD);
        }

        *pcode = code;
        return;
    }

    if (type_is_compound(type)) {
        emit_compound_load(mod, pcode, type, offset, 0);
        return;
    }

    if (type->kind == Type_Kind_Struct)   type = type_struct_is_just_one_basic_value(type);
    if (type->kind == Type_Kind_Enum)     type = type->Enum.backing;
    if (type->kind == Type_Kind_Distinct) type = type->Distinct.base_type;
    if (type->kind == Type_Kind_Function) assert(1234 && 0);

    assert(type);

    i32 load_size   = type_size_of(type);
    i32 is_basic    = type->kind == Type_Kind_Basic || type->kind == Type_Kind_Pointer || type->kind == Type_Kind_MultiPointer;

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

    WID(NULL, instr, ((WasmInstructionData) { alignment, offset }));

    if (instr == WI_NOP) {
        onyx_report_error((OnyxFilePos) { 0 }, Error_Critical,
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

    emit_enter_structured_block(mod, &code, SBT_Basic_If, if_node->token);
    if (if_node->true_stmt) emit_block(mod, &code, if_node->true_stmt, 0);

    if (if_node->false_stmt) {
        WI(if_node->false_stmt->token, WI_ELSE);

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
        emit_enter_structured_block(mod, &code, SBT_Breakable_Block, while_node->token);
        emit_enter_structured_block(mod, &code, SBT_Continue_Loop, while_node->token);

        if (!while_node->bottom_test) {
            emit_expression(mod, &code, while_node->cond);
            WI(NULL, WI_I32_EQZ);
            WID(NULL, WI_COND_JUMP, 0x01);
        }

        emit_block(mod, &code, while_node->true_stmt, 0);

        if (while_node->bottom_test) {
            emit_expression(mod, &code, while_node->cond);
            WID(while_node->cond->token, WI_COND_JUMP, 0x00);

        } else {
            if (bh_arr_last(code).type != WI_JUMP)
                WID(while_node->cond->token, WI_JUMP, 0x00);
        }

        emit_leave_structured_block(mod, &code);
        emit_leave_structured_block(mod, &code);

    } else {
        emit_expression(mod, &code, while_node->cond);

        emit_enter_structured_block(mod, &code, SBT_Breakable_If, while_node->token);
        emit_enter_structured_block(mod, &code, SBT_Continue_Loop, while_node->token);

        emit_block(mod, &code, while_node->true_stmt, 0);

        emit_expression(mod, &code, while_node->cond);
        WID(while_node->cond->token, WI_COND_JUMP, 0x00);

        emit_leave_structured_block(mod, &code);
        WI(while_node->false_stmt->token, WI_ELSE);

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

    // NOTE: This might not be a range literal
    AstStructLiteral *range = (AstStructLiteral *) for_node->iter;
    u64 offset = 0;

    StructMember low_mem, high_mem, step_mem;
    type_lookup_member(builtin_range_type_type, "low", &low_mem);
    type_lookup_member(builtin_range_type_type, "high", &high_mem);
    type_lookup_member(builtin_range_type_type, "step", &step_mem);
    u64 low_local  = local_raw_allocate(mod->local_alloc, onyx_type_to_wasm_type(low_mem.type));
    u64 high_local = local_raw_allocate(mod->local_alloc, onyx_type_to_wasm_type(high_mem.type));
    u64 step_local = local_raw_allocate(mod->local_alloc, onyx_type_to_wasm_type(step_mem.type));

    emit_struct_as_separate_values(mod, &code, builtin_range_type_type, 0);

    WIL(for_node->token, WI_LOCAL_SET, step_local);
    WIL(for_node->token, WI_LOCAL_SET, high_local);
    WIL(for_node->token, WI_LOCAL_TEE, low_local);
    WIL(for_node->token, WI_LOCAL_SET, iter_local);

    if (for_node->has_first) {
        for_node->first_local = local_raw_allocate(mod->local_alloc, WASM_TYPE_INT32);
        WIL(for_node->token, WI_I32_CONST, 1);
        WIL(for_node->token, WI_LOCAL_SET, for_node->first_local);
    }

    emit_enter_structured_block(mod, &code, SBT_Breakable_Block, for_node->token);
    emit_enter_structured_block(mod, &code, SBT_Basic_Loop, for_node->token);
    emit_enter_structured_block(mod, &code, SBT_Continue_Block, for_node->token);

    if (range->kind == Ast_Kind_Struct_Literal && (range->args.values[2]->flags & Ast_Flag_Comptime) != 0) {
        AstNumLit *step_value = (AstNumLit *) range->args.values[2];
        assert(step_value->kind == Ast_Kind_NumLit);

        if (step_value->value.l >= 0) {
            WIL(for_node->token, WI_LOCAL_GET, iter_local);
            WIL(for_node->token, WI_LOCAL_GET, high_local);
            WI(for_node->token, WI_I32_GE_S);
            WID(for_node->token, WI_COND_JUMP, 0x02);
        } else {
            WIL(for_node->token, WI_LOCAL_GET, iter_local);
            WIL(for_node->token, WI_LOCAL_GET, high_local);
            WI(for_node->token, WI_I32_LT_S);
            WID(for_node->token, WI_COND_JUMP, 0x02);
        }

    } else {
        WIL(for_node->token, WI_LOCAL_GET, step_local);
        WID(for_node->token, WI_I32_CONST, 0);
        WI(for_node->token, WI_I32_GE_S);
        WID(for_node->token, WI_IF_START, 0x40);
            WIL(for_node->token, WI_LOCAL_GET, iter_local);
            WIL(for_node->token, WI_LOCAL_GET, high_local);
            WI(for_node->token, WI_I32_GE_S);
            WID(for_node->token, WI_COND_JUMP, 0x03);
        WI(for_node->token, WI_ELSE);
            WIL(for_node->token, WI_LOCAL_GET, iter_local);
            WIL(for_node->token, WI_LOCAL_GET, high_local);
            WI(for_node->token, WI_I32_LT_S);
            WID(for_node->token, WI_COND_JUMP, 0x03);
        WI(for_node->token, WI_IF_END);
    }


    emit_block(mod, &code, for_node->stmt, 0);

    emit_leave_structured_block(mod, &code);

    WIL(for_node->token, WI_LOCAL_GET, iter_local);
    WIL(for_node->token, WI_LOCAL_GET, step_local);
    WI(for_node->token, WI_I32_ADD);
    WIL(for_node->token, WI_LOCAL_SET, iter_local);
    
    if (for_node->has_first) {
        WIL(for_node->token, WI_I32_CONST, 0);
        WIL(for_node->token, WI_LOCAL_SET, for_node->first_local);
    }

    if (bh_arr_last(code).type != WI_JUMP)
        WID(for_node->token, WI_JUMP, 0x00);

    emit_leave_structured_block(mod, &code);
    emit_leave_structured_block(mod, &code);

    if (for_node->has_first) local_raw_free(mod->local_alloc, WASM_TYPE_INT32);
    local_raw_free(mod->local_alloc, onyx_type_to_wasm_type(low_mem.type));
    local_raw_free(mod->local_alloc, onyx_type_to_wasm_type(high_mem.type));
    local_raw_free(mod->local_alloc, onyx_type_to_wasm_type(step_mem.type));

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

    WIL(for_node->token, WI_LOCAL_SET, end_ptr_local);
    WIL(for_node->token, WI_LOCAL_TEE, ptr_local);
    WIL(for_node->token, WI_LOCAL_GET, end_ptr_local);
    if (elem_size != 1) {
        WID(for_node->token, WI_PTR_CONST, elem_size);
        WI(for_node->token, WI_PTR_MUL);
    }
    WI(for_node->token, WI_PTR_ADD);
    WIL(for_node->token, WI_LOCAL_SET, end_ptr_local);

    if (for_node->has_first) {
        for_node->first_local = local_raw_allocate(mod->local_alloc, WASM_TYPE_INT32);
        WIL(for_node->token, WI_I32_CONST, 1);
        WIL(for_node->token, WI_LOCAL_SET, for_node->first_local);
    }

    emit_enter_structured_block(mod, &code, SBT_Breakable_Block, for_node->token);
    emit_enter_structured_block(mod, &code, SBT_Basic_Loop, for_node->token);
    emit_enter_structured_block(mod, &code, SBT_Continue_Block, for_node->token);

    WIL(for_node->token, WI_LOCAL_GET, ptr_local);
    WIL(for_node->token, WI_LOCAL_GET, end_ptr_local);
    WI(for_node->token, WI_PTR_GE);
    WID(for_node->token, WI_COND_JUMP, 0x02);

    if (!for_node->by_pointer) {
        if (!it_is_local) emit_local_location(mod, &code, var, &offset);

        WIL(for_node->token, WI_LOCAL_GET, ptr_local);
        emit_load_instruction(mod, &code, var->type, 0);

        if (!it_is_local) emit_store_instruction(mod, &code, var->type, offset);
        else              WIL(for_node->token, WI_LOCAL_SET, iter_local);
    }

    emit_block(mod, &code, for_node->stmt, 0);

    emit_leave_structured_block(mod, &code);

    WIL(for_node->token, WI_LOCAL_GET, ptr_local);
    WIL(for_node->token, WI_PTR_CONST, elem_size);
    WI(for_node->token, WI_PTR_ADD);
    WIL(for_node->token, WI_LOCAL_SET, ptr_local);

    if (for_node->has_first) {
        WIL(NULL, WI_I32_CONST, 0);
        WIL(NULL, WI_LOCAL_SET, for_node->first_local);
    }

    if (bh_arr_last(code).type != WI_JUMP)
        WID(for_node->token, WI_JUMP, 0x00);

    emit_leave_structured_block(mod, &code);
    emit_leave_structured_block(mod, &code);

    if (for_node->has_first) local_raw_free(mod->local_alloc, WASM_TYPE_INT32);
    local_raw_free(mod->local_alloc, WASM_TYPE_PTR);
    if (!for_node->by_pointer) local_raw_free(mod->local_alloc, WASM_TYPE_PTR);

    *pcode = code;
}

EMIT_FUNC(for_iterator, AstFor* for_node, u64 iter_local) {
    bh_arr(WasmInstruction) code = *pcode;

    // Allocate temporaries for iterator contents
    emit_struct_as_separate_values(mod, &code, for_node->iter->type, 0);

    u64 iterator_data_ptr    = local_raw_allocate(mod->local_alloc, WASM_TYPE_PTR);
    u64 iterator_next_func   = local_raw_allocate(mod->local_alloc, WASM_TYPE_FUNC);
    u64 iterator_close_func  = local_raw_allocate(mod->local_alloc, WASM_TYPE_FUNC);
    u64 iterator_remove_func = local_raw_allocate(mod->local_alloc, WASM_TYPE_FUNC);
    u64 iterator_done_bool   = local_raw_allocate(mod->local_alloc, WASM_TYPE_INT32);
    WI(for_node->token, WI_DROP);
    WIL(for_node->token, WI_LOCAL_SET, iterator_remove_func);
    WI(for_node->token, WI_DROP);
    WIL(for_node->token, WI_LOCAL_SET, iterator_close_func);
    WI(for_node->token, WI_DROP);
    WIL(for_node->token, WI_LOCAL_SET, iterator_next_func);
    WIL(for_node->token, WI_LOCAL_SET, iterator_data_ptr);


    {
        //
        // This pushes an entry onto the stack of for loops that have
        // are iterator that can have a '#remove' directive in them.
        ForRemoveInfo remove_info;
        remove_info.iterator_data_ptr = iterator_data_ptr;
        remove_info.iterator_remove_func = iterator_remove_func;

        StructMember remove_func_type;
        type_lookup_member_by_idx(for_node->iter->type, 3, &remove_func_type);
        remove_info.remove_func_type_idx = generate_type_idx(mod, remove_func_type.type);

        bh_arr_push(mod->for_remove_info, remove_info);
    }

    if (for_node->has_first) {
        for_node->first_local = local_raw_allocate(mod->local_alloc, WASM_TYPE_INT32);
        WIL(for_node->token, WI_I32_CONST, 1);
        WIL(for_node->token, WI_LOCAL_SET, for_node->first_local);
    }

    AstLocal* var = for_node->var;
    b32 it_is_local = (b32) ((iter_local & LOCAL_IS_WASM) != 0);
    u64 offset = 0;

    // Enter a deferred statement for the auto-close
    emit_enter_structured_block(mod, &code, SBT_Basic_Block, for_node->token);

    if (!for_node->no_close) {
        StructMember close_func_type;
        type_lookup_member_by_idx(for_node->iter->type, 2, &close_func_type);
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
    }

    emit_enter_structured_block(mod, &code, SBT_Breakable_Block, for_node->token);
    emit_enter_structured_block(mod, &code, SBT_Continue_Loop, for_node->token);

    if (!it_is_local) emit_local_location(mod, &code, var, &offset);

    {
        WIL(for_node->token, WI_LOCAL_GET, iterator_data_ptr);
        WIL(for_node->token, WI_LOCAL_GET, iterator_next_func);

        // CLEANUP: Calling a function is way too f-ing complicated. FACTOR IT!!
        u64 stack_top_idx = bh_imap_get(&mod->index_map, (u64) &builtin_stack_top);

        StructMember next_func_type;
        type_lookup_member_by_idx(for_node->iter->type, 1, &next_func_type);
        Type* return_type = next_func_type.type->Function.return_type;

        u32 return_size = type_size_of(return_type);
        u32 return_align = type_alignment_of(return_type);
        bh_align(return_size, return_align);

        u64 reserve_size = return_size;
        bh_align(reserve_size, 16);

        WID(for_node->token, WI_GLOBAL_GET, stack_top_idx);
        WID(for_node->token, WI_PTR_CONST, reserve_size);
        WI(for_node->token, WI_PTR_ADD);
        WID(for_node->token, WI_GLOBAL_SET, stack_top_idx);

        i32 type_idx = generate_type_idx(mod, next_func_type.type);
        WID(for_node->token, WI_CALL_INDIRECT, ((WasmInstructionData) { type_idx, 0x00 }));

        WID(for_node->token, WI_GLOBAL_GET, stack_top_idx);
        WID(for_node->token, WI_PTR_CONST, reserve_size);
        WI(for_node->token, WI_PTR_SUB);
        WID(for_node->token, WI_GLOBAL_SET, stack_top_idx);

        WID(for_node->token, WI_GLOBAL_GET, stack_top_idx);
        emit_load_instruction(mod, &code, return_type, reserve_size - return_size);
    }

    WIL(for_node->token, WI_LOCAL_SET, iterator_done_bool);

    if (!it_is_local) emit_store_instruction(mod, &code, var->type, offset);
    else              WIL(for_node->token, WI_LOCAL_SET, iter_local);

    WIL(for_node->token, WI_LOCAL_GET, iterator_done_bool);
    WI(for_node->token, WI_I32_EQZ);
    WID(for_node->token, WI_COND_JUMP, 0x01);

    emit_block(mod, &code, for_node->stmt, 0);

    if (for_node->has_first) {
        WIL(NULL, WI_I32_CONST, 0);
        WIL(NULL, WI_LOCAL_SET, for_node->first_local);
    }

    WID(for_node->token, WI_JUMP, 0x00);

    emit_leave_structured_block(mod, &code);
    emit_leave_structured_block(mod, &code);

    emit_deferred_stmts(mod, &code);
    emit_leave_structured_block(mod, &code);

    bh_arr_pop(mod->for_remove_info);

    if (for_node->has_first) local_raw_free(mod->local_alloc, WASM_TYPE_INT32);
    local_raw_free(mod->local_alloc, WASM_TYPE_PTR);
    local_raw_free(mod->local_alloc, WASM_TYPE_FUNC);
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

    debug_enter_symbol_frame(mod);
    debug_introduce_symbol(mod, var->token,
        local_is_wasm_local((AstTyped *) var) ? DSL_REGISTER : DSL_STACK,
        iter_local, var->type);

    emit_expression(mod, &code, for_node->iter);

    switch (for_node->loop_type) {
        case For_Loop_Range:    emit_for_range(mod, &code, for_node, iter_local); break;

        // NOTE: For static arrays, simply outputing the size
        // of the array right after the pointer to the start
        // of the array essentially makes it a slice.
        case For_Loop_Array:
            WIL(NULL, WI_I32_CONST, for_node->iter->type->Array.count);
            emit_for_slice(mod, &code, for_node, iter_local);
            break;

        // NOTE: A dynamic array is just a slice with a capacity and allocator on the end.
        // Just dropping the extra fields will mean we can just use the slice implementation.
        //                                                  - brendanfh   2020/09/04
        //                                                  - brendanfh   2021/04/13
        case For_Loop_DynArr:
            emit_load_slice(mod, &code);
            // fallthrough

        case For_Loop_Slice:    emit_for_slice(mod, &code, for_node, iter_local); break;
        case For_Loop_Iterator: emit_for_iterator(mod, &code, for_node, iter_local); break;
        default: onyx_report_error(for_node->token->pos, Error_Critical, "Invalid for loop type. You should probably not be seeing this...");
    }

    local_free(mod->local_alloc, (AstTyped *) var);
    debug_leave_symbol_frame(mod);

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

    emit_enter_structured_block(mod, &code, SBT_Breakable_Block, switch_node->token);

    u64 block_num = 0;
    bh_arr_each(AstSwitchCase *, sc, switch_node->cases) {
        if (bh_imap_has(&block_map, (u64) (*sc)->block)) continue;

        emit_enter_structured_block(mod, &code, SBT_Fallthrough_Block, (*sc)->block->token);

        bh_imap_put(&block_map, (u64) (*sc)->block, block_num);
        block_num++;
    }

    switch (switch_node->switch_kind) {
        case Switch_Kind_Integer: {
            u64 count = switch_node->max_case + 1 - switch_node->min_case;
            BranchTable* bt = bh_alloc(mod->extended_instr_alloc, sizeof(BranchTable) + sizeof(u32) * count);
            bt->count = count;
            bt->default_case = block_num;
            fori (i, 0, bt->count) bt->cases[i] = bt->default_case;

            bh_arr_each(bh__imap_entry, sc, switch_node->case_map.entries) {
                bt->cases[sc->key - switch_node->min_case] = bh_imap_get(&block_map, (u64) sc->value);
            }

            // NOTE: We enter a new block here in order to setup the correct
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
            WID(switch_node->expr->token, WI_BLOCK_START, 0x40);
            emit_expression(mod, &code, switch_node->expr);
            if (switch_node->min_case != 0) {
                if (onyx_type_to_wasm_type(switch_node->expr->type) == WASM_TYPE_INT64) {
                    WI(switch_node->expr->token, WI_I32_FROM_I64);
                }

                WID(switch_node->expr->token, WI_I32_CONST, switch_node->min_case);
                WI(switch_node->expr->token, WI_I32_SUB);
            }
            WIP(switch_node->expr->token, WI_JUMP_TABLE, bt);
            WI(switch_node->expr->token, WI_BLOCK_END);
            break;
        }

        case Switch_Kind_Use_Equals: {
            WID(switch_node->expr->token, WI_BLOCK_START, 0x40);

            bh_arr_each(CaseToBlock, ctb, switch_node->case_exprs) {
                emit_expression(mod, &code, (AstTyped *) ctb->comparison);

                u64 bn = bh_imap_get(&block_map, (u64) ctb->block);
                WID(switch_node->expr->token, WI_IF_START, 0x40);
                WID(switch_node->expr->token, WI_JUMP, bn + 1);
                WI(switch_node->expr->token, WI_IF_END);
            }

            WID(switch_node->expr->token, WI_JUMP, block_num);
            WI(switch_node->expr->token, WI_BLOCK_END);
            break;
        }
    }

    bh_arr_each(AstSwitchCase *, psc, switch_node->cases) {
        AstSwitchCase *sc = *psc;
        if (bh_imap_get(&block_map, (u64) sc->block) == 0xdeadbeef) continue;

        u64 bn = bh_imap_get(&block_map, (u64) sc->block);

        // Maybe the Symbol Frame idea should be controlled as a block_flag?
        debug_enter_symbol_frame(mod);
        emit_block(mod, &code, sc->block, 0);
        debug_leave_symbol_frame(mod);

        if (bh_arr_last(code).type != WI_JUMP)
            WID(NULL, WI_JUMP, block_num - bn);

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
        .defer_node= defer,
        .stmt = defer->stmt,
    }));
}

EMIT_FUNC(defer_code, WasmInstruction* deferred_code, u32 code_count) {
    bh_arr_push(mod->deferred_stmts, ((DeferredStmt) {
        .type = Deferred_Stmt_Code,
        .depth = bh_arr_length(mod->structured_jump_target),
        .defer_node= NULL,
        .instructions = deferred_code,
        .instruction_count = code_count,
    }));
}

EMIT_FUNC(deferred_stmt, DeferredStmt deferred_stmt) {
    bh_arr(WasmInstruction) code = *pcode;
    if (deferred_stmt.defer_node) {
        WI(deferred_stmt.defer_node->token, WI_NOP);
    }

    switch (deferred_stmt.type) {
        case Deferred_Stmt_Node: emit_statement(mod, &code, deferred_stmt.stmt); break;
        case Deferred_Stmt_Code: {
            fori (i, 0, deferred_stmt.instruction_count) {
                WIR(NULL, deferred_stmt.instructions[i]);
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

EMIT_FUNC(remove_directive, AstDirectiveRemove* remove) {
    assert(bh_arr_length(mod->for_remove_info) > 0);

    bh_arr(WasmInstruction) code = *pcode;

    ForRemoveInfo remove_info = bh_arr_last(mod->for_remove_info);

    WIL(remove->token, WI_LOCAL_GET, remove_info.iterator_remove_func);
    WIL(remove->token, WI_I32_CONST, mod->null_proc_func_idx);
    WI(remove->token, WI_I32_NE);
    WID(remove->token, WI_IF_START, 0x40);
    WIL(remove->token, WI_LOCAL_GET, remove_info.iterator_data_ptr);
    WIL(remove->token, WI_LOCAL_GET, remove_info.iterator_remove_func);
    WIL(remove->token, WI_CALL_INDIRECT, remove_info.remove_func_type_idx);
    WI(remove->token, WI_IF_END);

    *pcode = code;
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
    if (binop->left->type->kind == Type_Kind_Function) { // nocheckin
        WI(NULL, WI_DROP);
    }

    emit_expression(mod, &code, binop->right);
    if (binop->right->type->kind == Type_Kind_Function) { // nocheckin
        WI(NULL, WI_DROP);
    }

    WI(binop->token, binop_instr);

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
                WID(unop->token, WI_I32_CONST, 0x00);
                emit_expression(mod, &code, unop->expr);
                WI(unop->token, WI_I32_SUB);

            }
            else if (type->kind == Basic_Kind_I64) {
                WID(unop->token, WI_I64_CONST, 0x00);
                emit_expression(mod, &code, unop->expr);
                WI(unop->token, WI_I64_SUB);

            }
            else {
                emit_expression(mod, &code, unop->expr);

                if (type->kind == Basic_Kind_F32) WI(unop->token, WI_F32_NEG);
                if (type->kind == Basic_Kind_F64) WI(unop->token, WI_F64_NEG);
            }

            break;
        }

        case Unary_Op_Not:
            emit_expression(mod, &code, unop->expr);

            WI(unop->token, WI_I32_EQZ);
            break;

        case Unary_Op_Bitwise_Not: {
            emit_expression(mod, &code, unop->expr);

            TypeBasic* type = &unop->type->Basic;

            if (type->kind == Basic_Kind_I8 || type->kind == Basic_Kind_U8) {
                WID(unop->token, WI_I32_CONST, 0xff);
                WI(unop->token, WI_I32_XOR);
            }
            else if (type->kind == Basic_Kind_I16 || type->kind == Basic_Kind_U16) {
                WID(unop->token, WI_I32_CONST, 0xffff);
                WI(unop->token, WI_I32_XOR);
            }
            else if (type->kind == Basic_Kind_I32 || type->kind == Basic_Kind_U32) {
                WID(unop->token, WI_I32_CONST, 0xffffffff);
                WI(unop->token, WI_I32_XOR);
            }
            else if (type->kind == Basic_Kind_I64 || type->kind == Basic_Kind_U64) {
                WIL(unop->token, WI_I64_CONST, 0xffffffffffffffff);
                WI(unop->token, WI_I64_XOR);
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

    OnyxToken* call_token = call->token;

    // Because it would be inefficient to increment and decrement the global stack pointer for every argument,
    // a simple set of instructions increments it once to the size it will need to be. However, because it is
    // impossible to know what size the reserved memory will be, a location patch is taken in order to fill it
    // in later.
    u32 reserve_space_patch = bh_arr_length(code);
    WID(call_token, WI_GLOBAL_GET, stack_top_idx);
    WIL(call_token, WI_LOCAL_TEE, stack_top_store_local);
    WID(call_token, WI_PTR_CONST, 0);                           // This will be filled in later.
    WI(call_token, WI_PTR_ADD);
    WID(call_token, WI_GLOBAL_SET, stack_top_idx);

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

        if (arg->value->kind == Ast_Kind_Struct_Literal && onyx_type_is_stored_in_memory(arg->value->type)) {
            emit_struct_literal_into_contiguous_memory(mod, &code, (AstStructLiteral *) arg->value, stack_top_store_local, reserve_size);

        } else {
            if (place_on_stack) WIL(call_token, WI_LOCAL_GET, stack_top_store_local);
            emit_expression(mod, &code, arg->value);
            if (place_on_stack) emit_store_instruction(mod, &code, arg->value->type, reserve_size);
        }

        if (place_on_stack) {
            if (arg->va_kind == VA_Kind_Not_VA) {
                // Non-variadic arguments on the stack need a pointer to them placed on the WASM stack.
                WIL(call_token, WI_LOCAL_GET, stack_top_store_local);
                if (reserve_size > 0) {
                    WID(call_token, WI_PTR_CONST, reserve_size);
                    WI(call_token, WI_PTR_ADD);
                }
            }

            if (arg->va_kind == VA_Kind_Any) {
                vararg_any_offsets[vararg_count - 1] = reserve_size;
                vararg_any_types[vararg_count - 1] = arg->value->type->id;
            }

            reserve_size += type_size_of(arg->value->type);

            if (arg->pass_as_any) {
                Type *any_type = type_build_from_ast(context.ast_alloc, builtin_any_type);
                assert(any_type);

                u32 arg_size = type_size_of(any_type);

                u64 ugly_temporary = local_raw_allocate(mod->local_alloc, WASM_TYPE_PTR);
                WIL(call_token, WI_LOCAL_SET, ugly_temporary);

                WIL(call_token, WI_LOCAL_GET, stack_top_store_local);
                WIL(call_token, WI_LOCAL_GET, ugly_temporary);
                emit_store_instruction(mod, &code, &basic_types[Basic_Kind_Rawptr], reserve_size + 0);

                WIL(call_token, WI_LOCAL_GET, stack_top_store_local);
                WID(call_token, WI_I32_CONST, arg->value->type->id);
                emit_store_instruction(mod, &code, &basic_types[Basic_Kind_Type_Index], reserve_size + 4);

                local_raw_free(mod->local_alloc, WASM_TYPE_PTR);
                
                WIL(call_token, WI_LOCAL_GET, stack_top_store_local);
                WIL(call_token, WI_I32_CONST, reserve_size);
                WI(call_token, WI_I32_ADD);

                reserve_size += arg_size;
            }
        }
    }

    switch (call->va_kind) {
        case VA_Kind_Any: {
            vararg_offset = reserve_size;

            i32 any_size = type_size_of(type_build_from_ast(context.ast_alloc, builtin_any_type));

            fori (i, 0, vararg_count) {
                WIL(call_token, WI_LOCAL_GET, stack_top_store_local);
                WIL(call_token, WI_LOCAL_GET, stack_top_store_local);
                WID(call_token, WI_PTR_CONST, vararg_any_offsets[i]);
                WI(call_token, WI_PTR_ADD);
                emit_store_instruction(mod, &code, &basic_types[Basic_Kind_Rawptr], vararg_offset + i * any_size);

                WIL(call_token, WI_LOCAL_GET, stack_top_store_local);
                WID(call_token, WI_I32_CONST, vararg_any_types[i]);
                emit_store_instruction(mod, &code, &basic_types[Basic_Kind_Type_Index], vararg_offset + i * any_size + POINTER_SIZE);

                reserve_size += any_size;
            }

            // fallthrough
        }

        case VA_Kind_Typed: {
            WIL(call_token, WI_LOCAL_GET, stack_top_store_local);
            if (vararg_offset > 0) {
                WID(call_token, WI_PTR_CONST, vararg_offset);
                WI(call_token, WI_PTR_ADD);
            }
            WID(call_token, WI_I32_CONST, vararg_count);
            break;
        }

        case VA_Kind_Untyped: {
            WIL(call_token, WI_LOCAL_GET, stack_top_store_local);
            WIL(call_token, WI_LOCAL_GET, stack_top_store_local);
            if (vararg_offset > 0) {
                WID(call_token, WI_PTR_CONST, vararg_offset);
                WI(call_token, WI_PTR_ADD);
            }
            emit_store_instruction(mod, &code, &basic_types[Basic_Kind_Rawptr], reserve_size);

            // NOTE: There may be 4 uninitialized bytes here, because pointers are only 4 bytes in WASM.

            WIL(call_token, WI_LOCAL_GET, stack_top_store_local);
            WID(call_token, WI_I32_CONST, vararg_count);
            emit_store_instruction(mod, &code, &basic_types[Basic_Kind_I32], reserve_size + POINTER_SIZE);

            WIL(call_token, WI_LOCAL_GET, stack_top_store_local);
            if (reserve_size > 0) {
                WID(call_token, WI_PTR_CONST, reserve_size);
                WI(call_token, WI_PTR_ADD);
            }

            reserve_size += 4 + POINTER_SIZE;
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
        WIL(NULL, WI_CALL, func_idx);

    } else {
        emit_expression(mod, &code, call->callee);
        WI(NULL, WI_DROP);

        i32 type_idx = generate_type_idx(mod, call->callee->type);
        WID(NULL, WI_CALL_INDIRECT, ((WasmInstructionData) { type_idx, 0x00 }));
    }

    if (reserve_size > 0) {
        WIL(call_token, WI_LOCAL_GET,  stack_top_store_local);
        WID(call_token, WI_GLOBAL_SET, stack_top_idx);

        bh_align(reserve_size, 16);
        code[reserve_space_patch + 2].data.l = reserve_size;

    } else {
        fori (i, 0, 5) code[reserve_space_patch + i].type = WI_NOP;
    }

    if (cc == CC_Return_Stack) {
        WID(call_token, WI_GLOBAL_GET, stack_top_idx);
        emit_load_with_ignored_instruction(mod, &code, return_type, reserve_size - return_size, call->ignored_return_value_count);
    }

    local_raw_free(mod->local_alloc, WASM_TYPE_PTR);
    *pcode = code;
}

EMIT_FUNC(method_call, AstBinaryOp *mcall) {
    AstCall *call_node = (AstCall *) mcall->right;

    //
    // This this is calling a function directly, just emit the call.
    if (call_node->callee->kind == Ast_Kind_Function) {
        emit_call(mod, pcode, call_node);
        return;
    }

    //
    // If this method call is anything more complicated, there should
    // be a field access for the call's callee node.
    assert(call_node->callee->kind == Ast_Kind_Field_Access);

    bh_arr(WasmInstruction) code = *pcode;

    AstFieldAccess *fa      = (AstFieldAccess *) call_node->callee;
    AstFieldAccess **object = (AstFieldAccess **) &fa->expr;

    //
    // If this field access has another field access from a use
    // by pointer member, descend into that structure.
    if (fa->expr->flags & Ast_Flag_Extra_Field_Access) {
        object = (AstFieldAccess**) &(*object)->expr;
    }

    //
    // Create a local variable to store the result of the lookup.
    AstLocal *tmp_local = make_local_with_type(context.ast_alloc, NULL, (*object)->type);
    tmp_local->flags |= Ast_Flag_Decl_Followed_By_Init;
    u64 tmp_local_idx = emit_local_allocation(mod, &code, (AstTyped *) tmp_local);
    b32 tmp_is_wasm_local = (b32) ((tmp_local_idx & LOCAL_IS_WASM) != 0);

    //
    // Do the common assignment pattern found everywhere else.
    u64 offset = 0;
    if (!tmp_is_wasm_local) emit_local_location(mod, &code, tmp_local, &offset);

    emit_expression(mod, &code, (AstTyped *) *object);

    if (!tmp_is_wasm_local) emit_store_instruction(mod, &code, tmp_local->type, offset);
    else                    WIL(mcall->token, WI_LOCAL_SET, tmp_local_idx);

    //
    // Replace the field access with the local variable
    *object = (AstFieldAccess *) tmp_local;

    //
    // Replace the first argument of the function with a field access.
    // If the first argument was an implicit address of, take the address
    // of the local variable.
    AstArgument *first_arg = (AstArgument *) call_node->args.values[0];
    if (first_arg->value->kind == Ast_Kind_Address_Of && ((AstAddressOf *) first_arg->value)->can_be_removed) {
        first_arg->value = (AstTyped *) make_address_of(context.ast_alloc, (AstTyped *) tmp_local);
    } else {
        first_arg->value = (AstTyped *) tmp_local;
    }
    
    //
    // Actually emit the function call.
    emit_call(mod, &code, call_node);

    *pcode = code;
}



// BUG: This implementation assumes that the host system C's implementation is using
// little endian integers.
#define SIMD_INT_CONST_INTRINSIC(type, count) { \
        type* byte_buffer = bh_alloc(mod->extended_instr_alloc, 16); \
        bh_arr(AstArgument *) arg_arr = (bh_arr(AstArgument *)) call->args.values; \
        fori (i, 0, count) { \
            if (arg_arr[i]->value->kind != Ast_Kind_NumLit) { \
                onyx_report_error(arg_arr[i]->token->pos, Error_Critical, \
                        "SIMD constants expect compile time constants as parameters. The %d%s parameter was not.", \
                        i, bh_num_suffix(i)); \
                *pcode = code; \
                return; \
            } \
            byte_buffer[i] = (type) ((AstNumLit *) arg_arr[i]->value)->value.l; \
        } \
        WIP(call->token, WI_V128_CONST, byte_buffer); \
    }

#define SIMD_EXTRACT_LANE_INSTR(instr, arg_arr) \
    emit_expression(mod, &code, arg_arr[0]->value);\
    if (arg_arr[1]->value->kind != Ast_Kind_NumLit) { \
        onyx_report_error(arg_arr[1]->token->pos, Error_Critical, "SIMD lane instructions expect a compile time lane number."); \
        *pcode = code; \
        return; \
    } \
    WID(call->token, instr, (u8) ((AstNumLit *) arg_arr[1]->value)->value.i);

#define SIMD_REPLACE_LANE_INSTR(instr, arg_arr) { \
    emit_expression(mod, &code, arg_arr[0]->value);\
    if (arg_arr[1]->value->kind != Ast_Kind_NumLit) { \
        onyx_report_error(arg_arr[1]->token->pos, Error_Critical, "SIMD lane instructions expect a compile time lane number."); \
        *pcode = code; \
        return; \
    } \
    u8 lane = (u8) ((AstNumLit *) arg_arr[1]->value)->value.i; \
    emit_expression(mod, &code, arg_arr[2]->value); \
    WID(call->token, instr, lane); \
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
        case ONYX_INTRINSIC_UNREACHABLE:  WI(call->token, WI_UNREACHABLE); break;
        
        case ONYX_INTRINSIC_MEMORY_SIZE:  WID(call->token, WI_MEMORY_SIZE, 0x00); break;
        case ONYX_INTRINSIC_MEMORY_GROW:  WID(call->token, WI_MEMORY_GROW, 0x00); break;
        case ONYX_INTRINSIC_MEMORY_COPY:
            emit_wasm_copy(mod, &code, call->token);
            break;
        case ONYX_INTRINSIC_MEMORY_FILL:
            emit_wasm_fill(mod, &code, call->token);
            break;

        case ONYX_INTRINSIC_INITIALIZE: {
            Type* type_to_initialize = ((AstArgument *) call->args.values[0])->value->type->Pointer.elem;
            emit_initialize_type(mod, &code, type_to_initialize, call->token);
            break;
        }

        case ONYX_INTRINSIC_I32_CLZ:      WI(call->token, WI_I32_CLZ); break;
        case ONYX_INTRINSIC_I32_CTZ:      WI(call->token, WI_I32_CTZ); break;
        case ONYX_INTRINSIC_I32_POPCNT:   WI(call->token, WI_I32_POPCNT); break;
        case ONYX_INTRINSIC_I32_AND:      WI(call->token, WI_I32_AND); break;
        case ONYX_INTRINSIC_I32_OR:       WI(call->token, WI_I32_OR); break;
        case ONYX_INTRINSIC_I32_XOR:      WI(call->token, WI_I32_XOR); break;
        case ONYX_INTRINSIC_I32_SHL:      WI(call->token, WI_I32_SHL); break;
        case ONYX_INTRINSIC_I32_SLR:      WI(call->token, WI_I32_SHR_U); break;
        case ONYX_INTRINSIC_I32_SAR:      WI(call->token, WI_I32_SHR_S); break;
        case ONYX_INTRINSIC_I32_ROTL:     WI(call->token, WI_I32_ROTL); break;
        case ONYX_INTRINSIC_I32_ROTR:     WI(call->token, WI_I32_ROTR); break;

        case ONYX_INTRINSIC_I64_CLZ:      WI(call->token, WI_I64_CLZ); break;
        case ONYX_INTRINSIC_I64_CTZ:      WI(call->token, WI_I64_CTZ); break;
        case ONYX_INTRINSIC_I64_POPCNT:   WI(call->token, WI_I64_POPCNT); break;
        case ONYX_INTRINSIC_I64_AND:      WI(call->token, WI_I64_AND); break;
        case ONYX_INTRINSIC_I64_OR:       WI(call->token, WI_I64_OR); break;
        case ONYX_INTRINSIC_I64_XOR:      WI(call->token, WI_I64_XOR); break;
        case ONYX_INTRINSIC_I64_SHL:      WI(call->token, WI_I64_SHL); break;
        case ONYX_INTRINSIC_I64_SLR:      WI(call->token, WI_I64_SHR_U); break;
        case ONYX_INTRINSIC_I64_SAR:      WI(call->token, WI_I64_SHR_S); break;
        case ONYX_INTRINSIC_I64_ROTL:     WI(call->token, WI_I64_ROTL); break;
        case ONYX_INTRINSIC_I64_ROTR:     WI(call->token, WI_I64_ROTR); break;

        case ONYX_INTRINSIC_F32_ABS:      WI(call->token, WI_F32_ABS); break;
        case ONYX_INTRINSIC_F32_CEIL:     WI(call->token, WI_F32_CEIL); break;
        case ONYX_INTRINSIC_F32_FLOOR:    WI(call->token, WI_F32_FLOOR); break;
        case ONYX_INTRINSIC_F32_TRUNC:    WI(call->token, WI_F32_TRUNC); break;
        case ONYX_INTRINSIC_F32_NEAREST:  WI(call->token, WI_F32_NEAREST); break;
        case ONYX_INTRINSIC_F32_SQRT:     WI(call->token, WI_F32_SQRT); break;
        case ONYX_INTRINSIC_F32_MIN:      WI(call->token, WI_F32_MIN); break;
        case ONYX_INTRINSIC_F32_MAX:      WI(call->token, WI_F32_MAX); break;
        case ONYX_INTRINSIC_F32_COPYSIGN: WI(call->token, WI_F32_COPYSIGN); break;

        case ONYX_INTRINSIC_F64_ABS:      WI(call->token, WI_F64_ABS); break;
        case ONYX_INTRINSIC_F64_CEIL:     WI(call->token, WI_F64_CEIL); break;
        case ONYX_INTRINSIC_F64_FLOOR:    WI(call->token, WI_F64_FLOOR); break;
        case ONYX_INTRINSIC_F64_TRUNC:    WI(call->token, WI_F64_TRUNC); break;
        case ONYX_INTRINSIC_F64_NEAREST:  WI(call->token, WI_F64_NEAREST); break;
        case ONYX_INTRINSIC_F64_SQRT:     WI(call->token, WI_F64_SQRT); break;
        case ONYX_INTRINSIC_F64_MIN:      WI(call->token, WI_F64_MIN); break;
        case ONYX_INTRINSIC_F64_MAX:      WI(call->token, WI_F64_MAX); break;
        case ONYX_INTRINSIC_F64_COPYSIGN: WI(call->token, WI_F64_COPYSIGN); break;

        case ONYX_INTRINSIC_I32_REINTERPRET_F32: WI(call->token, WI_I32_REINTERPRET_F32); break;
        case ONYX_INTRINSIC_I64_REINTERPRET_F64: WI(call->token, WI_I64_REINTERPRET_F64); break;
        case ONYX_INTRINSIC_F32_REINTERPRET_I32: WI(call->token, WI_F32_REINTERPRET_I32); break;
        case ONYX_INTRINSIC_F64_REINTERPRET_I64: WI(call->token, WI_F64_REINTERPRET_I64); break;

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
                    onyx_report_error(arg_arr[i]->token->pos, Error_Critical,
                            "SIMD constants expect compile time constants as parameters. The %d%s parameter was not.",
                            i, bh_num_suffix(i));
                    *pcode = code;
                    return;
                }
                byte_buffer[i] = (f32) ((AstNumLit *) arg_arr[i]->value)->value.f;
            }
            WIP(call->token, WI_V128_CONST, byte_buffer);
            break;
        }

        case ONYX_INTRINSIC_F64X2_CONST: {
            f64* byte_buffer = bh_alloc(mod->extended_instr_alloc, 16);
            bh_arr(AstArgument *) arg_arr = (bh_arr(AstArgument *)) call->args.values;
            fori (i, 0, 2) {
                if (arg_arr[i]->value->kind != Ast_Kind_NumLit) {
                    onyx_report_error(arg_arr[i]->token->pos, Error_Critical,
                            "SIMD constants expect compile time constants as parameters. The %d%s parameter was not.",
                            i, bh_num_suffix(i));
                    *pcode = code;
                    return;
                }
                byte_buffer[i] = (f64) ((AstNumLit *) arg_arr[i]->value)->value.d;
            }
            WIP(call->token, WI_V128_CONST, byte_buffer);
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
                    onyx_report_error(arg_arr[i + 2]->token->pos, Error_Critical,
                            "SIMD constants expect compile time constants as parameters. The %d%s parameter was not.",
                            i, bh_num_suffix(i));
                    *pcode = code;
                    return;
                }
                byte_buffer[i] = (u8) ((AstNumLit *) arg_arr[i + 2]->value)->value.i;
            }
            WIP(call->token, WI_I8X16_SHUFFLE, byte_buffer);
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

        case ONYX_INTRINSIC_I8X16_SWIZZLE: WI(call->token, WI_I8X16_SWIZZLE); break;
        case ONYX_INTRINSIC_I8X16_SPLAT:   WI(call->token, WI_I8X16_SPLAT); break;
        case ONYX_INTRINSIC_I16X8_SPLAT:   WI(call->token, WI_I16X8_SPLAT); break;
        case ONYX_INTRINSIC_I32X4_SPLAT:   WI(call->token, WI_I32X4_SPLAT); break;
        case ONYX_INTRINSIC_I64X2_SPLAT:   WI(call->token, WI_I64X2_SPLAT); break;
        case ONYX_INTRINSIC_F32X4_SPLAT:   WI(call->token, WI_F32X4_SPLAT); break;
        case ONYX_INTRINSIC_F64X2_SPLAT:   WI(call->token, WI_F64X2_SPLAT); break;

        case ONYX_INTRINSIC_I8X16_EQ:   WI(call->token, WI_I8X16_EQ); break;
        case ONYX_INTRINSIC_I8X16_NEQ:  WI(call->token, WI_I8X16_NEQ); break;
        case ONYX_INTRINSIC_I8X16_LT_S: WI(call->token, WI_I8X16_LT_S); break;
        case ONYX_INTRINSIC_I8X16_LT_U: WI(call->token, WI_I8X16_LT_U); break;
        case ONYX_INTRINSIC_I8X16_GT_S: WI(call->token, WI_I8X16_GT_S); break;
        case ONYX_INTRINSIC_I8X16_GT_U: WI(call->token, WI_I8X16_GT_U); break;
        case ONYX_INTRINSIC_I8X16_LE_S: WI(call->token, WI_I8X16_LE_S); break;
        case ONYX_INTRINSIC_I8X16_LE_U: WI(call->token, WI_I8X16_LE_U); break;
        case ONYX_INTRINSIC_I8X16_GE_S: WI(call->token, WI_I8X16_GE_S); break;
        case ONYX_INTRINSIC_I8X16_GE_U: WI(call->token, WI_I8X16_GE_U); break;

        case ONYX_INTRINSIC_I16X8_EQ:   WI(call->token, WI_I16X8_EQ); break;
        case ONYX_INTRINSIC_I16X8_NEQ:  WI(call->token, WI_I16X8_NEQ); break;
        case ONYX_INTRINSIC_I16X8_LT_S: WI(call->token, WI_I16X8_LT_S); break;
        case ONYX_INTRINSIC_I16X8_LT_U: WI(call->token, WI_I16X8_LT_U); break;
        case ONYX_INTRINSIC_I16X8_GT_S: WI(call->token, WI_I16X8_GT_S); break;
        case ONYX_INTRINSIC_I16X8_GT_U: WI(call->token, WI_I16X8_GT_U); break;
        case ONYX_INTRINSIC_I16X8_LE_S: WI(call->token, WI_I16X8_LE_S); break;
        case ONYX_INTRINSIC_I16X8_LE_U: WI(call->token, WI_I16X8_LE_U); break;
        case ONYX_INTRINSIC_I16X8_GE_S: WI(call->token, WI_I16X8_GE_S); break;
        case ONYX_INTRINSIC_I16X8_GE_U: WI(call->token, WI_I16X8_GE_U); break;

        case ONYX_INTRINSIC_I32X4_EQ:   WI(call->token, WI_I32X4_EQ); break;
        case ONYX_INTRINSIC_I32X4_NEQ:  WI(call->token, WI_I32X4_NEQ); break;
        case ONYX_INTRINSIC_I32X4_LT_S: WI(call->token, WI_I32X4_LT_S); break;
        case ONYX_INTRINSIC_I32X4_LT_U: WI(call->token, WI_I32X4_LT_U); break;
        case ONYX_INTRINSIC_I32X4_GT_S: WI(call->token, WI_I32X4_GT_S); break;
        case ONYX_INTRINSIC_I32X4_GT_U: WI(call->token, WI_I32X4_GT_U); break;
        case ONYX_INTRINSIC_I32X4_LE_S: WI(call->token, WI_I32X4_LE_S); break;
        case ONYX_INTRINSIC_I32X4_LE_U: WI(call->token, WI_I32X4_LE_U); break;
        case ONYX_INTRINSIC_I32X4_GE_S: WI(call->token, WI_I32X4_GE_S); break;
        case ONYX_INTRINSIC_I32X4_GE_U: WI(call->token, WI_I32X4_GE_U); break;

        case ONYX_INTRINSIC_F32X4_EQ:  WI(call->token, WI_F32X4_EQ); break;
        case ONYX_INTRINSIC_F32X4_NEQ: WI(call->token, WI_F32X4_NEQ); break;
        case ONYX_INTRINSIC_F32X4_LT:  WI(call->token, WI_F32X4_LT); break;
        case ONYX_INTRINSIC_F32X4_GT:  WI(call->token, WI_F32X4_GT); break;
        case ONYX_INTRINSIC_F32X4_LE:  WI(call->token, WI_F32X4_LE); break;
        case ONYX_INTRINSIC_F32X4_GE:  WI(call->token, WI_F32X4_GE); break;

        case ONYX_INTRINSIC_F64X2_EQ:  WI(call->token, WI_F64X2_EQ); break;
        case ONYX_INTRINSIC_F64X2_NEQ: WI(call->token, WI_F64X2_NEQ); break;
        case ONYX_INTRINSIC_F64X2_LT:  WI(call->token, WI_F64X2_LT); break;
        case ONYX_INTRINSIC_F64X2_GT:  WI(call->token, WI_F64X2_GT); break;
        case ONYX_INTRINSIC_F64X2_LE:  WI(call->token, WI_F64X2_LE); break;
        case ONYX_INTRINSIC_F64X2_GE:  WI(call->token, WI_F64X2_GE); break;

        case ONYX_INTRINSIC_V128_NOT:       WI(call->token, WI_V128_NOT); break;
        case ONYX_INTRINSIC_V128_AND:       WI(call->token, WI_V128_AND); break;
        case ONYX_INTRINSIC_V128_ANDNOT:    WI(call->token, WI_V128_ANDNOT); break;
        case ONYX_INTRINSIC_V128_OR:        WI(call->token, WI_V128_OR); break;
        case ONYX_INTRINSIC_V128_XOR:       WI(call->token, WI_V128_XOR); break;
        case ONYX_INTRINSIC_V128_BITSELECT: WI(call->token, WI_V128_BITSELECT); break;

        case ONYX_INTRINSIC_I8X16_ABS:            WI(call->token, WI_I8X16_ABS); break;
        case ONYX_INTRINSIC_I8X16_NEG:            WI(call->token, WI_I8X16_NEG); break;
        case ONYX_INTRINSIC_I8X16_ANY_TRUE:       WI(call->token, WI_I8X16_ANY_TRUE); break;
        case ONYX_INTRINSIC_I8X16_ALL_TRUE:       WI(call->token, WI_I8X16_ALL_TRUE); break;
        case ONYX_INTRINSIC_I8X16_BITMASK:        WI(call->token, WI_I8X16_BITMASK); break;
        case ONYX_INTRINSIC_I8X16_NARROW_I16X8_S: WI(call->token, WI_I8X16_NARROW_I16X8_S); break;
        case ONYX_INTRINSIC_I8X16_NARROW_I16X8_U: WI(call->token, WI_I8X16_NARROW_I16X8_U); break;
        case ONYX_INTRINSIC_I8X16_SHL:            WI(call->token, WI_I8X16_SHL); break;
        case ONYX_INTRINSIC_I8X16_SHR_S:          WI(call->token, WI_I8X16_SHR_S); break;
        case ONYX_INTRINSIC_I8X16_SHR_U:          WI(call->token, WI_I8X16_SHR_U); break;
        case ONYX_INTRINSIC_I8X16_ADD:            WI(call->token, WI_I8X16_ADD); break;
        case ONYX_INTRINSIC_I8X16_ADD_SAT_S:      WI(call->token, WI_I8X16_ADD_SAT_S); break;
        case ONYX_INTRINSIC_I8X16_ADD_SAT_U:      WI(call->token, WI_I8X16_ADD_SAT_U); break;
        case ONYX_INTRINSIC_I8X16_SUB:            WI(call->token, WI_I8X16_SUB); break;
        case ONYX_INTRINSIC_I8X16_SUB_SAT_S:      WI(call->token, WI_I8X16_SUB_SAT_S); break;
        case ONYX_INTRINSIC_I8X16_SUB_SAT_U:      WI(call->token, WI_I8X16_SUB_SAT_U); break;
        case ONYX_INTRINSIC_I8X16_MIN_S:          WI(call->token, WI_I8X16_MIN_S); break;
        case ONYX_INTRINSIC_I8X16_MIN_U:          WI(call->token, WI_I8X16_MIN_U); break;
        case ONYX_INTRINSIC_I8X16_MAX_S:          WI(call->token, WI_I8X16_MAX_S); break;
        case ONYX_INTRINSIC_I8X16_MAX_U:          WI(call->token, WI_I8X16_MAX_U); break;
        case ONYX_INTRINSIC_I8X16_AVGR_U:         WI(call->token, WI_I8X16_AVGR_U); break;

        case ONYX_INTRINSIC_I16X8_ABS:                WI(call->token, WI_I16X8_ABS); break;
        case ONYX_INTRINSIC_I16X8_NEG:                WI(call->token, WI_I16X8_NEG); break;
        case ONYX_INTRINSIC_I16X8_ANY_TRUE:           WI(call->token, WI_I16X8_ANY_TRUE); break;
        case ONYX_INTRINSIC_I16X8_ALL_TRUE:           WI(call->token, WI_I16X8_ALL_TRUE); break;
        case ONYX_INTRINSIC_I16X8_BITMASK:            WI(call->token, WI_I16X8_BITMASK); break;
        case ONYX_INTRINSIC_I16X8_NARROW_I32X4_S:     WI(call->token, WI_I16X8_NARROW_I32X4_S); break;
        case ONYX_INTRINSIC_I16X8_NARROW_I32X4_U:     WI(call->token, WI_I16X8_NARROW_I32X4_U); break;
        case ONYX_INTRINSIC_I16X8_WIDEN_LOW_I8X16_S:  WI(call->token, WI_I16X8_WIDEN_LOW_I8X16_S); break;
        case ONYX_INTRINSIC_I16X8_WIDEN_HIGH_I8X16_S: WI(call->token, WI_I16X8_WIDEN_HIGH_I8X16_S); break;
        case ONYX_INTRINSIC_I16X8_WIDEN_LOW_I8X16_U:  WI(call->token, WI_I16X8_WIDEN_LOW_I8X16_U); break;
        case ONYX_INTRINSIC_I16X8_WIDEN_HIGH_I8X16_U: WI(call->token, WI_I16X8_WIDEN_HIGH_I8X16_U); break;
        case ONYX_INTRINSIC_I16X8_SHL:                WI(call->token, WI_I16X8_SHL); break;
        case ONYX_INTRINSIC_I16X8_SHR_S:              WI(call->token, WI_I16X8_SHR_S); break;
        case ONYX_INTRINSIC_I16X8_SHR_U:              WI(call->token, WI_I16X8_SHR_U); break;
        case ONYX_INTRINSIC_I16X8_ADD:                WI(call->token, WI_I16X8_ADD); break;
        case ONYX_INTRINSIC_I16X8_ADD_SAT_S:          WI(call->token, WI_I16X8_ADD_SAT_S); break;
        case ONYX_INTRINSIC_I16X8_ADD_SAT_U:          WI(call->token, WI_I16X8_ADD_SAT_U); break;
        case ONYX_INTRINSIC_I16X8_SUB:                WI(call->token, WI_I16X8_SUB); break;
        case ONYX_INTRINSIC_I16X8_SUB_SAT_S:          WI(call->token, WI_I16X8_SUB_SAT_S); break;
        case ONYX_INTRINSIC_I16X8_SUB_SAT_U:          WI(call->token, WI_I16X8_SUB_SAT_U); break;
        case ONYX_INTRINSIC_I16X8_MUL:                WI(call->token, WI_I16X8_MUL); break;
        case ONYX_INTRINSIC_I16X8_MIN_S:              WI(call->token, WI_I16X8_MIN_S); break;
        case ONYX_INTRINSIC_I16X8_MIN_U:              WI(call->token, WI_I16X8_MIN_U); break;
        case ONYX_INTRINSIC_I16X8_MAX_S:              WI(call->token, WI_I16X8_MAX_S); break;
        case ONYX_INTRINSIC_I16X8_MAX_U:              WI(call->token, WI_I16X8_MAX_U); break;
        case ONYX_INTRINSIC_I16X8_AVGR_U:             WI(call->token, WI_I16X8_AVGR_U); break;

        case ONYX_INTRINSIC_I32X4_ABS:                WI(call->token, WI_I32X4_ABS); break;
        case ONYX_INTRINSIC_I32X4_NEG:                WI(call->token, WI_I32X4_NEG); break;
        case ONYX_INTRINSIC_I32X4_ANY_TRUE:           WI(call->token, WI_I32X4_ANY_TRUE); break;
        case ONYX_INTRINSIC_I32X4_ALL_TRUE:           WI(call->token, WI_I32X4_ALL_TRUE); break;
        case ONYX_INTRINSIC_I32X4_BITMASK:            WI(call->token, WI_I32X4_BITMASK); break;
        case ONYX_INTRINSIC_I32X4_WIDEN_LOW_I16X8_S:  WI(call->token, WI_I32X4_WIDEN_LOW_I16X8_S); break;
        case ONYX_INTRINSIC_I32X4_WIDEN_HIGH_I16X8_S: WI(call->token, WI_I32X4_WIDEN_HIGH_I16X8_S); break;
        case ONYX_INTRINSIC_I32X4_WIDEN_LOW_I16X8_U:  WI(call->token, WI_I32X4_WIDEN_LOW_I16X8_U); break;
        case ONYX_INTRINSIC_I32X4_WIDEN_HIGH_I16X8_U: WI(call->token, WI_I32X4_WIDEN_HIGH_I16X8_U); break;
        case ONYX_INTRINSIC_I32X4_SHL:                WI(call->token, WI_I32X4_SHL); break;
        case ONYX_INTRINSIC_I32X4_SHR_S:              WI(call->token, WI_I32X4_SHR_S); break;
        case ONYX_INTRINSIC_I32X4_SHR_U:              WI(call->token, WI_I32X4_SHR_U); break;
        case ONYX_INTRINSIC_I32X4_ADD:                WI(call->token, WI_I32X4_ADD); break;
        case ONYX_INTRINSIC_I32X4_SUB:                WI(call->token, WI_I32X4_SUB); break;
        case ONYX_INTRINSIC_I32X4_MUL:                WI(call->token, WI_I32X4_MUL); break;
        case ONYX_INTRINSIC_I32X4_MIN_S:              WI(call->token, WI_I32X4_MIN_S); break;
        case ONYX_INTRINSIC_I32X4_MIN_U:              WI(call->token, WI_I32X4_MIN_U); break;
        case ONYX_INTRINSIC_I32X4_MAX_S:              WI(call->token, WI_I32X4_MAX_S); break;
        case ONYX_INTRINSIC_I32X4_MAX_U:              WI(call->token, WI_I32X4_MAX_U); break;

        case ONYX_INTRINSIC_I64X2_NEG:   WI(call->token, WI_I64X2_NEG); break;
        case ONYX_INTRINSIC_I64X2_SHL:   WI(call->token, WI_I64X2_SHL); break;
        case ONYX_INTRINSIC_I64X2_SHR_S: WI(call->token, WI_I64X2_SHR_S); break;
        case ONYX_INTRINSIC_I64X2_SHR_U: WI(call->token, WI_I64X2_SHR_U); break;
        case ONYX_INTRINSIC_I64X2_ADD:   WI(call->token, WI_I64X2_ADD); break;
        case ONYX_INTRINSIC_I64X2_SUB:   WI(call->token, WI_I64X2_SUB); break;
        case ONYX_INTRINSIC_I64X2_MUL:   WI(call->token, WI_I64X2_MUL); break;

        case ONYX_INTRINSIC_F32X4_ABS:  WI(call->token, WI_F32X4_ABS); break;
        case ONYX_INTRINSIC_F32X4_NEG:  WI(call->token, WI_F32X4_NEG); break;
        case ONYX_INTRINSIC_F32X4_SQRT: WI(call->token, WI_F32X4_SQRT); break;
        case ONYX_INTRINSIC_F32X4_ADD:  WI(call->token, WI_F32X4_ADD); break;
        case ONYX_INTRINSIC_F32X4_SUB:  WI(call->token, WI_F32X4_SUB); break;
        case ONYX_INTRINSIC_F32X4_MUL:  WI(call->token, WI_F32X4_MUL); break;
        case ONYX_INTRINSIC_F32X4_DIV:  WI(call->token, WI_F32X4_DIV); break;
        case ONYX_INTRINSIC_F32X4_MIN:  WI(call->token, WI_F32X4_MIN); break;
        case ONYX_INTRINSIC_F32X4_MAX:  WI(call->token, WI_F32X4_MAX); break;

        case ONYX_INTRINSIC_F64X2_ABS:  WI(call->token, WI_F64X2_ABS); break;
        case ONYX_INTRINSIC_F64X2_NEG:  WI(call->token, WI_F64X2_NEG); break;
        case ONYX_INTRINSIC_F64X2_SQRT: WI(call->token, WI_F64X2_SQRT); break;
        case ONYX_INTRINSIC_F64X2_ADD:  WI(call->token, WI_F64X2_ADD); break;
        case ONYX_INTRINSIC_F64X2_SUB:  WI(call->token, WI_F64X2_SUB); break;
        case ONYX_INTRINSIC_F64X2_MUL:  WI(call->token, WI_F64X2_MUL); break;
        case ONYX_INTRINSIC_F64X2_DIV:  WI(call->token, WI_F64X2_DIV); break;
        case ONYX_INTRINSIC_F64X2_MIN:  WI(call->token, WI_F64X2_MIN); break;
        case ONYX_INTRINSIC_F64X2_MAX:  WI(call->token, WI_F64X2_MAX); break;

        case ONYX_INTRINSIC_I32X4_TRUNC_SAT_F32X4_S: WI(call->token, WI_I32X4_TRUNC_SAT_F32X4_S); break;
        case ONYX_INTRINSIC_I32X4_TRUNC_SAT_F32X4_U: WI(call->token, WI_I32X4_TRUNC_SAT_F32X4_U); break;
        case ONYX_INTRINSIC_F32X4_CONVERT_I32X4_S:   WI(call->token, WI_F32X4_CONVERT_I32X4_S); break;
        case ONYX_INTRINSIC_F32X4_CONVERT_I32X4_U:   WI(call->token, WI_F32X4_CONVERT_I32X4_U); break;

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
        WID(sub->token, WI_PTR_CONST, sub->elem_size);
        WI(sub->token, WI_PTR_MUL);
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

    WI(sub->token, WI_PTR_ADD);

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
        && source_expr->type->kind != Type_Kind_Pointer && source_expr->type->kind != Type_Kind_MultiPointer) {
        u64 o2 = 0;
        emit_subscript_location(mod, &code, (AstSubscript *) source_expr, &o2);
        offset += o2;

    } else if ((source_expr->kind == Ast_Kind_Local || source_expr->kind == Ast_Kind_Param)
        && source_expr->type->kind != Type_Kind_Pointer && source_expr->type->kind != Type_Kind_MultiPointer) {
        u64 o2 = 0;
        emit_local_location(mod, &code, (AstLocal *) source_expr, &o2);
        offset += o2;

    } else if (source_expr->kind == Ast_Kind_Memres
        && source_expr->type->kind != Type_Kind_Pointer && source_expr->type->kind != Type_Kind_MultiPointer) {
        emit_memory_reservation_location(mod, &code, (AstMemRes *) source_expr);

    } else {
        emit_expression(mod, &code, source_expr);
    }

    *offset_return = offset;

    *pcode = code;
}

EMIT_FUNC(memory_reservation_location, AstMemRes* memres) {
    bh_arr(WasmInstruction) code = *pcode;

    if (memres->threadlocal) {
        u64 tls_base_idx = bh_imap_get(&mod->index_map, (u64) &builtin_tls_base);

        if (memres->tls_offset > 0) {
            WID(NULL, WI_PTR_CONST, memres->tls_offset);
            WIL(NULL, WI_GLOBAL_GET, tls_base_idx);
            WI(NULL, WI_PTR_ADD);

        } else {
            WIL(NULL, WI_GLOBAL_GET, tls_base_idx);
        }

    } else {
        // :ProperLinking
        assert(memres->data_id != 0);
        emit_data_relocation(mod, &code, memres->data_id);
    }

    *pcode = code;
}

EMIT_FUNC(local_location, AstLocal* local, u64* offset_return) {
    bh_arr(WasmInstruction) code = *pcode;

    u64 local_offset = (u64) bh_imap_get(&mod->local_map, (u64) local);

    if (local_offset & LOCAL_IS_WASM) {
        // This is a weird condition but it is relied on in a couple places including
        // passing non-simple structs by value.             -brendanfh 2020/09/18
        WIL(NULL, WI_LOCAL_GET, local_offset);

    } else {
        WIL(NULL, WI_LOCAL_GET, mod->stack_base_idx);

        *offset_return += local_offset;
    }

    *pcode = code;
}

EMIT_FUNC(compound_load, Type* type, u64 offset, i32 ignored_value_count) {
    bh_arr(WasmInstruction) code = *pcode;
    i32 mem_count = type_linear_member_count(type);
    TypeWithOffset two;

    assert(mem_count > ignored_value_count);
    mem_count -= ignored_value_count;

    if (mem_count == 1) {
        type_linear_member_lookup(type, 0, &two);
        emit_load_instruction(mod, &code, two.type, offset + two.offset); // two.offset should be 0

    } else {
        u64 tmp_idx = local_raw_allocate(mod->local_alloc, WASM_TYPE_PTR);
        WIL(NULL, WI_LOCAL_TEE, tmp_idx);

        fori (i, 0, mem_count) {
            type_linear_member_lookup(type, i, &two);
            if (i != 0) WIL(NULL, WI_LOCAL_GET, tmp_idx);
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
    if (location_first) WIL(NULL, WI_LOCAL_SET, loc_idx);

    i32 elem_count = type_linear_member_count(type);
    u64 *temp_locals = bh_alloc_array(global_scratch_allocator, u64, elem_count);

    forir (i, elem_count - 1, 0) {
        type_linear_member_lookup(type, i, &two);

        WasmType wt = onyx_type_to_wasm_type(two.type);
        temp_locals[i] = local_raw_allocate(mod->local_alloc, wt);
        WIL(NULL, WI_LOCAL_SET, temp_locals[i]);
    }

    if (!location_first) WIL(NULL, WI_LOCAL_SET, loc_idx);

    fori (i, 0, elem_count) {
        type_linear_member_lookup(type, i, &two);

        u64 tmp_idx = temp_locals[i];
        WIL(NULL, WI_LOCAL_GET, loc_idx);
        WIL(NULL, WI_LOCAL_GET, tmp_idx);
        emit_store_instruction(mod, &code, two.type, offset + two.offset);

        WasmType wt = onyx_type_to_wasm_type(two.type);
        local_raw_free(mod->local_alloc, wt);
    }

    local_raw_free(mod->local_alloc, WASM_TYPE_PTR);

    // This shouldn't be necessary because the scratch allocator doesn't free.
    bh_free(global_scratch_allocator, temp_locals);

    *pcode = code;
}

EMIT_FUNC(wasm_copy, OnyxToken *token) {
    bh_arr(WasmInstruction) code = *pcode;

    if (context.options->use_post_mvp_features) {
        WIL(token, WI_MEMORY_COPY, 0x00);
    } else {
        emit_intrinsic_memory_copy(mod, &code);
    }

    *pcode = code;
}

EMIT_FUNC(wasm_fill, OnyxToken *token) {
    bh_arr(WasmInstruction) code = *pcode;

    if (context.options->use_post_mvp_features) {
        WID(token, WI_MEMORY_FILL, 0x00);
    } else {
        emit_intrinsic_memory_fill(mod, &code);
    }

    *pcode = code;
}

EMIT_FUNC(values_into_contiguous_memory, u64 base_ptr_local, Type *type, u32 offset, i32 value_count, AstTyped **values) {
    bh_arr(WasmInstruction) code = *pcode;

    assert(onyx_type_is_stored_in_memory(type));
    assert(value_count == (i32) type_structlike_mem_count(type));

    StructMember smem;
    fori (i, 0, value_count) {
        type_lookup_member_by_idx(type, i, &smem);

        // When emitting a structure literal into memory, simply place it directly into the memory.
        // Otherwise, the structure literal would be placed somewhere in memory, and then needlessly
        // copied to its final destination.
        if (values[i]->kind == Ast_Kind_Struct_Literal && onyx_type_is_stored_in_memory(values[i]->type)) {
            emit_struct_literal_into_contiguous_memory(mod, &code, (AstStructLiteral *) values[i], base_ptr_local, smem.offset + offset);

        // When emitting a zero-value, simple zero the bytes in memory. Otherwise you run into the
        // same problem described above.
        } else if (values[i]->kind == Ast_Kind_Zero_Value && onyx_type_is_stored_in_memory(values[i]->type)) {
            WIL(NULL, WI_LOCAL_GET, base_ptr_local);
            WIL(NULL, WI_PTR_CONST, smem.offset + offset);
            WI(NULL, WI_PTR_ADD);

            WIL(NULL, WI_I32_CONST, 0);
            WIL(NULL, WI_I32_CONST, type_size_of(values[i]->type));

            emit_wasm_fill(mod, &code, NULL);

        } else {
            WIL(NULL, WI_LOCAL_GET, base_ptr_local);
            emit_expression(mod, &code, values[i]);
            emit_store_instruction(mod, &code, values[i]->type, smem.offset + offset);
        }
    }

    *pcode = code;
}

EMIT_FUNC(struct_literal_into_contiguous_memory, AstStructLiteral* sl, u64 base_ptr_local, u32 offset) {
    emit_values_into_contiguous_memory(mod, pcode, base_ptr_local, sl->type, offset, bh_arr_length(sl->args.values), sl->args.values);
}

EMIT_FUNC(struct_literal, AstStructLiteral* sl) {
    bh_arr(WasmInstruction) code = *pcode;

    if (!onyx_type_is_stored_in_memory(sl->type)) {
        bh_arr_each(AstTyped *, val, sl->args.values) {
            emit_expression(mod, &code, *val);
        }

        *pcode = code;
        return;
    }

    u64 local_offset = emit_local_allocation(mod, &code, (AstTyped *) sl);
    assert((local_offset & LOCAL_IS_WASM) == 0);

    emit_struct_literal_into_contiguous_memory(mod, &code, sl, mod->stack_base_idx, local_offset);
    emit_stack_address(mod, &code, local_offset, sl->token);

    *pcode = code;
}

// <src_ptr>   <- top of stack
// <dest_ptr>
EMIT_FUNC(struct_store, Type *type, u32 offset) {
    assert(onyx_type_is_stored_in_memory(type));
    bh_arr(WasmInstruction) code = *pcode;

    if (offset != 0) {
        u64 rptr_local = local_raw_allocate(mod->local_alloc, WASM_TYPE_PTR);
        WIL(NULL, WI_LOCAL_SET, rptr_local);

        WIL(NULL, WI_PTR_CONST, offset);
        WI(NULL, WI_PTR_ADD);
        WIL(NULL, WI_LOCAL_GET, rptr_local);

        local_raw_free(mod->local_alloc, WASM_TYPE_PTR);
    }

    WIL(NULL, WI_I32_CONST, type_size_of(type));

    // Use a simple memory copy if it is available.
    emit_wasm_copy(mod, &code, NULL);

    *pcode = code;
    return;
}

EMIT_FUNC(struct_as_separate_values, Type *type, u32 offset) {
    bh_arr(WasmInstruction) code = *pcode;

    u64 value_location = local_raw_allocate(mod->local_alloc, WASM_TYPE_PTR);
    WIL(NULL, WI_LOCAL_SET, value_location);

    assert(onyx_type_is_stored_in_memory(type));

    i32 mem_count = type_structlike_mem_count(type);
    StructMember smem;

    fori (i, 0, mem_count) {
        type_lookup_member_by_idx(type, i, &smem);

        WIL(NULL, WI_LOCAL_GET, value_location);
        emit_load_instruction(mod, &code, smem.type, offset + smem.offset);

        if (onyx_type_is_stored_in_memory(smem.type)) {
            // This load will be relative to the base address given above.
            emit_struct_as_separate_values(mod, &code, smem.type, smem.offset);
        }
    }

    local_raw_free(mod->local_alloc, WASM_TYPE_PTR);
    *pcode = code;
    return;
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
    WIL(NULL, WI_LOCAL_SET, rptr_local);
    WIL(NULL, WI_LOCAL_SET, lptr_local);

    WIL(NULL, WI_LOCAL_GET, rptr_local);
    WID(NULL, WI_I32_CONST, 0);
    WI(NULL, WI_I32_NE);
    emit_enter_structured_block(mod, &code, SBT_Basic_If, NULL);

    {
        WIL(NULL, WI_LOCAL_GET, lptr_local);
        if (offset != 0) {
            WIL(NULL, WI_PTR_CONST, offset);
            WI(NULL, WI_PTR_ADD);
        }

        WIL(NULL, WI_LOCAL_GET, rptr_local);
        WIL(NULL, WI_I32_CONST, elem_count * elem_size);
        emit_wasm_copy(mod, &code, NULL);
    }

    WI(NULL, WI_ELSE);

    { // If the source ptr is null (0), then just copy in 0 bytes.
        WIL(NULL, WI_LOCAL_GET, lptr_local);
        if (offset != 0) {
            WIL(NULL, WI_PTR_CONST, offset);
            WI(NULL, WI_PTR_ADD);
        }

        WIL(NULL, WI_I32_CONST, 0);
        WIL(NULL, WI_I32_CONST, elem_count * elem_size);
        emit_wasm_fill(mod, &code, NULL);
    }

    local_raw_free(mod->local_alloc, WASM_TYPE_PTR);
    local_raw_free(mod->local_alloc, WASM_TYPE_PTR);

    emit_leave_structured_block(mod, &code);
    *pcode = code;
    return;
}

EMIT_FUNC(array_literal, AstArrayLiteral* al) {
    bh_arr(WasmInstruction) code = *pcode;

    u64 local_offset = emit_local_allocation(mod, &code, (AstTyped *) al);
    assert((local_offset & LOCAL_IS_WASM) == 0);

    assert(al->type->kind == Type_Kind_Array);

    u32 elem_size = type_size_of(al->type->Array.elem);

    fori (i, 0, al->type->Array.count) {
        WIL(al->token, WI_LOCAL_GET, mod->stack_base_idx);
        emit_expression(mod, &code, al->values[i]);
        emit_store_instruction(mod, &code, al->type->Array.elem, local_offset + i * elem_size);
    }

    WIL(al->token, WI_LOCAL_GET, mod->stack_base_idx);
    if (local_offset > 0) {
        WIL(al->token, WI_PTR_CONST, local_offset);
        WI(al->token, WI_PTR_ADD);
    }

    *pcode = code;
}

EMIT_FUNC(range_literal, AstRangeLiteral* range) {
    bh_arr(WasmInstruction) code = *pcode;

    u64 local_offset = emit_local_allocation(mod, &code, (AstTyped *) range);
    assert((local_offset & LOCAL_IS_WASM) == 0);

    AstTyped *values[] = { range->low, range->high, range->step };
    emit_values_into_contiguous_memory(mod, &code, mod->stack_base_idx,
            range->type, local_offset, 3, values);

    WIL(range->token, WI_LOCAL_GET, mod->stack_base_idx);

    if (local_offset > 0) {
        WIL(NULL, WI_PTR_CONST, local_offset);
        WI(NULL, WI_PTR_ADD);
    }

    *pcode = code;
}

EMIT_FUNC_NO_ARGS(load_slice) {
    bh_arr(WasmInstruction) code = *pcode;

    u64 ugly_temporary = local_raw_allocate(mod->local_alloc, WASM_TYPE_PTR);
    WIL(NULL, WI_LOCAL_TEE, ugly_temporary);
    emit_load_instruction(mod, &code, &basic_types[Basic_Kind_Rawptr], 0);

    WIL(NULL, WI_LOCAL_GET, ugly_temporary);
    emit_load_instruction(mod, &code, &basic_types[Basic_Kind_I32], POINTER_SIZE);

    local_raw_free(mod->local_alloc, WASM_TYPE_PTR);

    *pcode = code;
}

EMIT_FUNC(if_expression, AstIfExpression* if_expr) {
    bh_arr(WasmInstruction) code = *pcode;

    u64 result_local    = local_allocate(mod->local_alloc, (AstTyped *) if_expr);
    b32 result_is_local = (b32) ((result_local & LOCAL_IS_WASM) != 0);
    bh_imap_put(&mod->local_map, (u64) if_expr, result_local);

    emit_expression(mod, &code, if_expr->cond);

    emit_enter_structured_block(mod, &code, SBT_Basic_If, if_expr->token);
        emit_expression(mod, &code, if_expr->true_expr);
        emit_generic_store_instruction(mod, &code, (AstTyped *) if_expr, if_expr->token);

    WI(if_expr->token, WI_ELSE);
        emit_expression(mod, &code, if_expr->false_expr);
        emit_generic_store_instruction(mod, &code, (AstTyped *) if_expr, if_expr->token);

    emit_leave_structured_block(mod, &code);

    if (!result_is_local) {
        u64 offset = 0;
        emit_local_location(mod, &code, (AstLocal *) if_expr, &offset);
        emit_load_instruction(mod, &code, if_expr->type, offset);

    } else {
        WIL(if_expr->token, WI_LOCAL_GET, result_local);
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
        WIL(doblock->block->token, WI_LOCAL_GET, result_local);
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
        case Ast_Kind_Local:
        case Ast_Kind_Array_Literal:
        case Ast_Kind_Struct_Literal:
        case Ast_Kind_Do_Block:
        case Ast_Kind_If_Expression:
        case Ast_Kind_Call_Site:
        case Ast_Kind_Zero_Value: {
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
            if (expr->token) {
                onyx_report_error(expr->token->pos, Error_Critical, "Unable to generate location for '%s'.", onyx_ast_node_kind_string(expr->kind));
            } else {
                OnyxFilePos pos = {0};
                onyx_report_error(pos, Error_Critical, "Unable to generate location for '%s'.", onyx_ast_node_kind_string(expr->kind));
            }
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
        WID(NULL, WI_PTR_CONST, offset);
        WI(NULL, WI_PTR_ADD);
    }

    *pcode = code;
}

EMIT_FUNC(expression, AstTyped* expr) {
    bh_arr(WasmInstruction) code = *pcode;

    if (node_is_type((AstNode *) expr)) {
        AstType* type = (AstType *) expr;
        if (type->flags & Ast_Flag_Expr_Ignored) return;

        if (type->type_id != 0) {
            WID(NULL, WI_I32_CONST, ((AstType *) expr)->type_id);
        } else {
            Type* t = type_build_from_ast(context.ast_alloc, type);
            WID(NULL, WI_I32_CONST, t->id);
        }


        *pcode = code;
        return;
    }

    switch (expr->kind) {
        case Ast_Kind_Param: {
            AstLocal* param = (AstLocal *) expr;
            u64 localidx = bh_imap_get(&mod->local_map, (u64) param);

            switch (type_get_param_pass(param->type)) {
                case Param_Pass_By_Multiple_Values: {
                    u32 mem_count = type_structlike_mem_count(expr->type);
                    fori (idx, 0, mem_count) WIL(NULL, WI_LOCAL_GET, localidx + idx);
                    break;
                }

                case Param_Pass_By_Value: {
                    assert(localidx & LOCAL_IS_WASM);
                    WIL(NULL, WI_LOCAL_GET, localidx);
                    break;
                }

                case Param_Pass_By_Implicit_Pointer: {
                    assert(localidx & LOCAL_IS_WASM);
                    WIL(NULL, WI_LOCAL_GET, localidx);
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
                    WIL(NULL, WI_LOCAL_GET, tmp);
                }

            } else {
                u64 offset = 0;
                emit_local_location(mod, &code, (AstLocal *) expr, &offset);

                if (expr->type->kind != Type_Kind_Array) {
                    emit_load_instruction(mod, &code, expr->type, offset);
                } else if (offset != 0) {
                    WID(NULL, WI_PTR_CONST, offset);
                    WI(NULL, WI_PTR_ADD);
                }
            }

            break;
        }

        case Ast_Kind_Global: {
            i32 globalidx = (i32) bh_imap_get(&mod->index_map, (u64) expr);

            WID(NULL, WI_GLOBAL_GET, globalidx);
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

            WIR(NULL, instr);
            break;
        }

        case Ast_Kind_StrLit: {
            // :ProperLinking
            AstStrLit *strlit = (AstStrLit *) expr;
            assert(strlit->data_id > 0);
            emit_data_relocation(mod, &code, strlit->data_id);

            if (strlit->is_cstr == 0)
                WID(NULL, WI_I32_CONST, strlit->length);
            break;
        }

        case Ast_Kind_Struct_Literal: {
            emit_struct_literal(mod, &code, (AstStructLiteral *) expr);
            break;
        }

        case Ast_Kind_Array_Literal: {
            emit_array_literal(mod, &code, (AstArrayLiteral *) expr);
            break;
        }

        case Ast_Kind_Range_Literal: {
            emit_range_literal(mod, &code, (AstRangeLiteral *) expr);
            break;
        }

        case Ast_Kind_Function: {
            i32 elemidx = get_element_idx(mod, (AstFunction *) expr);

            WID(NULL, WI_I32_CONST, elemidx);
            WIL(NULL, WI_I32_CONST, 0);
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
        case Ast_Kind_Method_Call:    emit_method_call(mod, &code, (AstBinaryOp *) expr); break;

        case Ast_Kind_Address_Of: {
            AstAddressOf* aof = (AstAddressOf *) expr;

            if (node_is_addressable_literal((AstNode *) aof->expr)) {
                aof->expr->flags |= Ast_Flag_Decl_Followed_By_Init;
                aof->expr->flags |= Ast_Flag_Address_Taken;
                emit_local_allocation(mod, &code, aof->expr);
                emit_location(mod, &code, aof->expr);
                emit_expression(mod, &code, aof->expr);
                emit_store_instruction(mod, &code, aof->expr->type, 0);
            }

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

            if (field->expr->kind == Ast_Kind_Param && type_get_param_pass(field->expr->type) == Param_Pass_By_Multiple_Values) {
                u64 localidx = bh_imap_get(&mod->local_map, (u64) field->expr) + field->idx;
                assert(localidx & LOCAL_IS_WASM);
                WIL(NULL, WI_LOCAL_GET, localidx);
            }

            else if (field->expr->kind == Ast_Kind_Param && type_struct_is_just_one_basic_value(field->expr->type)) {
                u64 localidx = bh_imap_get(&mod->local_map, (u64) field->expr);
                assert(localidx & LOCAL_IS_WASM);
                WIL(NULL, WI_LOCAL_GET, localidx);
            }

            else if (is_lval((AstNode *) field->expr) || type_is_pointer(field->expr->type)) {
                u64 offset = 0;
                emit_field_access_location(mod, &code, field, &offset);
                emit_load_instruction(mod, &code, field->type, offset);
            }

            else if (onyx_type_is_stored_in_memory(field->expr->type)) {
                emit_expression(mod, &code, field->expr);
                emit_load_instruction(mod, &code, field->type, field->offset);
            }

            else {
                emit_expression(mod, &code, field->expr);

                i32 idx = type_get_idx_of_linear_member_with_offset(field->expr->type, field->offset);
                i32 field_linear_members = type_linear_member_count(field->type);
                i32 total_linear_members = type_linear_member_count(field->expr->type);

                if (idx == 0) {
                    // Easy case: the member is the first one and all other members just have to be dropped.
                    fori (i, 0, total_linear_members - field_linear_members) WI(NULL, WI_DROP);

                } else {
                    // Tough case: Stack shuffling to make the member the only thing on the stack.
                    // This is very similar to the compound_load/compound_store procedures but it is different enough
                    // that I cannot find a good way to factor them all without just introducing a ton of complexity.
                    fori (i, 0, total_linear_members - idx - field_linear_members) WI(NULL, WI_DROP);

                    u64 *temporaries = bh_alloc_array(global_scratch_allocator, u64, field_linear_members);
                    fori (i, 0, field_linear_members) temporaries[i] = 0;

                    TypeWithOffset two = { 0 };
                    forir (i, field_linear_members - 1, 0) {
                        type_linear_member_lookup(field->type, i, &two);

                        WasmType wt = onyx_type_to_wasm_type(two.type);
                        temporaries[i] = local_raw_allocate(mod->local_alloc, wt);
                        WIL(NULL, WI_LOCAL_SET, temporaries[i]);
                    }

                    fori (i, 0, idx) WI(NULL, WI_DROP);

                    fori (i, 0, field_linear_members) {
                        type_linear_member_lookup(field->type, i, &two);

                        WIL(NULL, WI_LOCAL_GET, temporaries[i]);

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
            emit_struct_as_separate_values(mod, &code, sl->expr->type, 0); // nocheckin This should be optimized for range literals

            u64 lo_local = local_raw_allocate(mod->local_alloc, WASM_TYPE_INT32);
            u64 hi_local = local_raw_allocate(mod->local_alloc, WASM_TYPE_INT32);

            WI(NULL, WI_DROP);
            WIL(NULL, WI_LOCAL_SET, hi_local);
            WIL(NULL, WI_LOCAL_TEE, lo_local);
            if (sl->elem_size != 1) {
                WID(NULL, WI_I32_CONST, sl->elem_size);
                WI(NULL, WI_I32_MUL);
            }
            emit_expression(mod, &code, sl->addr);
            WI(NULL, WI_I32_ADD);
            WIL(NULL, WI_LOCAL_GET, hi_local);
            WIL(NULL, WI_LOCAL_GET, lo_local);
            WI(NULL, WI_I32_SUB);

            local_raw_free(mod->local_alloc, lo_local);
            local_raw_free(mod->local_alloc, hi_local);
            break;
        }

        case Ast_Kind_Size_Of: {
            AstSizeOf* so = (AstSizeOf *) expr;
            WID(NULL, WI_I32_CONST, so->size);
            break;
        }

        case Ast_Kind_Align_Of: {
            AstAlignOf* ao = (AstAlignOf *) expr;
            WID(NULL, WI_I32_CONST, ao->alignment);
            break;
        }

        case Ast_Kind_Enum_Value: {
            AstEnumValue* ev = (AstEnumValue *) expr;
            AstNumLit   * num = (AstNumLit *) ev->value;
            assert(num->kind == Ast_Kind_NumLit);

            WasmType backing_type = onyx_type_to_wasm_type(ev->type);
            if      (backing_type == WASM_TYPE_INT32) WID(NULL, WI_I32_CONST, num->value.i);
            else if (backing_type == WASM_TYPE_INT64) WID(NULL, WI_I64_CONST, num->value.l);
            else onyx_report_error(ev->token->pos, Error_Critical, "Invalid backing type for enum.");
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

            assert(fc->data_id > 0);
            assert(fc->size > 0);

            // :ProperLinking
            emit_data_relocation(mod, &code, fc->data_id);
            WID(NULL, WI_I32_CONST, fc->size);
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

            u64 local_offset = emit_local_allocation(mod, &code, (AstTyped *) callsite);
            assert((local_offset & LOCAL_IS_WASM) == 0);

            AstTyped *values[] = { (AstTyped *) callsite->filename, (AstTyped *) callsite->line, (AstTyped *) callsite->column };
            emit_values_into_contiguous_memory(mod, &code, mod->stack_base_idx,
                    callsite->type, local_offset, 3, values);

            WIL(NULL, WI_LOCAL_GET, mod->stack_base_idx);

            if (local_offset > 0) {
                WIL(NULL, WI_PTR_CONST, local_offset);
                WI(NULL, WI_PTR_ADD);
            }

            break;
        }

        case Ast_Kind_If_Expression: {
            AstIfExpression* if_expr = (AstIfExpression *) expr;
            emit_if_expression(mod, &code, if_expr);
            break;
        }

        case Ast_Kind_Switch_Case: {
            // This error message should be moved to checking, but this is the
            // best place to do it right now.
            onyx_report_error(expr->token->pos, Error_Critical, "'case' statements are only allowed in a 'switch' statement.");
            break;
        }

        case Ast_Kind_Code_Block: {
            // Like above, this error message should be moved to checking, but
            // this is the best place to do it right now.
            onyx_report_error(expr->token->pos, Error_Critical, "'#quote' blocks are only to be used at compile-time. Using them as a runtime value is not allowed.");
            break;
        }

        case Ast_Kind_Foreign_Block: {
            AstForeignBlock *fb = (AstForeignBlock *) expr;
            WID(NULL, WI_I32_CONST, fb->foreign_block_number);
            break;
        }

        case Ast_Kind_Zero_Value: {
            AstZeroValue *zv = (AstZeroValue *) expr;
            assert(zv->type);
            emit_zero_value_for_type(mod, &code, zv->type, zv->token, (AstTyped *) zv);
            break;
        }

        case Ast_Kind_Directive_First: {
            AstDirectiveFirst *first = (AstDirectiveFirst *) expr;
            WIL(first->token, WI_LOCAL_GET, first->for_node->first_local);
            break;
        }

        case Ast_Kind_Package: {
            AstPackage *package = (AstPackage *) expr;
            WID(NULL, WI_I32_CONST, package->package->id);
            break;
        }

        case Ast_Kind_Directive_Export_Name: {
            AstDirectiveExportName *ename = (AstDirectiveExportName *) expr;
            emit_expression(mod, &code, (AstTyped *) ename->name);
            break;
        }

        case Ast_Kind_Capture_Local: {
            printf("HANDLE CAPTURE LOCAL!!!\n");
            assert(0);
            break;
        }

        default:
            bh_printf("Unhandled case: %d\n", expr->kind);
            DEBUG_HERE;
            assert(0);
    }

    if ((expr->flags & Ast_Flag_Expr_Ignored) != 0 && !type_results_in_void(expr->type)) {
        i32 mem_count = type_linear_member_count(expr->type);
        fori (i, 0, mem_count) WI(NULL, WI_DROP);
    }

    *pcode = code;
}

static const WasmInstructionType cast_map[][12] = {
    //          I8               U8                  I16                 U16                I32                 U32                I64                U64                F32                F64                PTR
    /* I8  */ { WI_NOP,          WI_NOP,             WI_I32_EXTEND_8_S,  WI_NOP,            WI_I32_EXTEND_8_S,  WI_NOP,            WI_I64_FROM_I32_S, WI_I64_FROM_I32_S, WI_UNREACHABLE,    WI_UNREACHABLE,    WI_UNREACHABLE,  WI_UNREACHABLE },
    /* U8  */ { WI_NOP,          WI_NOP,             WI_NOP,             WI_NOP,            WI_NOP,             WI_NOP,            WI_I64_FROM_I32_U, WI_I64_FROM_I32_U, WI_UNREACHABLE,    WI_UNREACHABLE,    WI_UNREACHABLE,  WI_UNREACHABLE },
    /* I16 */ { WI_NOP,          WI_NOP,             WI_NOP,             WI_NOP,            WI_I32_EXTEND_16_S, WI_NOP,            WI_I64_FROM_I32_S, WI_I64_FROM_I32_S, WI_UNREACHABLE,    WI_UNREACHABLE,    WI_UNREACHABLE,  WI_UNREACHABLE },
    /* U16 */ { WI_NOP,          WI_NOP,             WI_NOP,             WI_NOP,            WI_NOP,             WI_NOP,            WI_I64_FROM_I32_U, WI_I64_FROM_I32_U, WI_UNREACHABLE,    WI_UNREACHABLE,    WI_UNREACHABLE,  WI_UNREACHABLE },
    /* I32 */ { WI_NOP,          WI_NOP,             WI_NOP,             WI_NOP,            WI_NOP,             WI_NOP,            WI_I64_FROM_I32_S, WI_I64_FROM_I32_S, WI_F32_FROM_I32_S, WI_F64_FROM_I32_S, WI_NOP,          WI_NOP         },
    /* U32 */ { WI_NOP,          WI_NOP,             WI_NOP,             WI_NOP,            WI_NOP,             WI_NOP,            WI_I64_FROM_I32_U, WI_I64_FROM_I32_U, WI_F32_FROM_I32_U, WI_F64_FROM_I32_U, WI_NOP,          WI_NOP         },
    /* I64 */ { WI_I32_FROM_I64, WI_I32_FROM_I64,    WI_I32_FROM_I64,    WI_I32_FROM_I64,   WI_I32_FROM_I64,    WI_I32_FROM_I64,   WI_NOP,            WI_NOP,            WI_F32_FROM_I64_S, WI_F64_FROM_I64_S, WI_I32_FROM_I64, WI_UNREACHABLE },
    /* U64 */ { WI_I32_FROM_I64, WI_I32_FROM_I64,    WI_I32_FROM_I64,    WI_I32_FROM_I64,   WI_I32_FROM_I64,    WI_I32_FROM_I64,   WI_NOP,            WI_NOP,            WI_F32_FROM_I64_U, WI_F64_FROM_I64_U, WI_I32_FROM_I64, WI_UNREACHABLE },
    /* F32 */ { WI_UNREACHABLE,  WI_UNREACHABLE,     WI_UNREACHABLE,     WI_UNREACHABLE,    WI_I32_FROM_F32_S,  WI_I32_FROM_F32_U, WI_I64_FROM_F32_S, WI_I64_FROM_F32_U, WI_NOP,            WI_F64_FROM_F32,   WI_UNREACHABLE,  WI_UNREACHABLE },
    /* F64 */ { WI_UNREACHABLE,  WI_UNREACHABLE,     WI_UNREACHABLE,     WI_UNREACHABLE,    WI_I32_FROM_F64_S,  WI_I32_FROM_F64_U, WI_I64_FROM_F64_S, WI_I64_FROM_F64_U, WI_F32_FROM_F64,   WI_NOP,            WI_UNREACHABLE,  WI_UNREACHABLE },
    /* PTR */ { WI_UNREACHABLE,  WI_UNREACHABLE,     WI_UNREACHABLE,     WI_UNREACHABLE,    WI_NOP,             WI_NOP,            WI_I64_FROM_I32_U, WI_I64_FROM_I32_U, WI_UNREACHABLE,    WI_UNREACHABLE,    WI_NOP,          WI_UNREACHABLE },
    /* TYP */ { WI_UNREACHABLE,  WI_UNREACHABLE,     WI_UNREACHABLE,     WI_UNREACHABLE,    WI_NOP,             WI_NOP,            WI_UNREACHABLE,    WI_UNREACHABLE,    WI_UNREACHABLE,    WI_UNREACHABLE,    WI_UNREACHABLE,  WI_NOP         },
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
        WI(NULL, WI_DROP);
        *pcode = code;
        return;
    }

    if (to->kind == Type_Kind_Slice && from->kind == Type_Kind_Array) {
        WID(NULL, WI_I32_CONST, from->Array.count);
        *pcode = code;
        return;
    }

    if (to->kind == Type_Kind_Slice && from->kind == Type_Kind_DynArray) {
        emit_load_slice(mod, &code);
        *pcode = code;
        return;
    }

    if (to->kind == Type_Kind_Slice && from->kind == Type_Kind_Slice) {
        // Nothing needs to be done because they are identical
        *pcode = code;
        return;
    }

    if (to->kind == Type_Kind_Slice && from->kind == Type_Kind_VarArgs) {
        // Nothing needs to be done because they are identical
        *pcode = code;
        return;
    }

    if (to->kind == Type_Kind_Distinct || from->kind == Type_Kind_Distinct) {
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
            WI(NULL, cast_op);
        }
    }

    *pcode = code;
}

EMIT_FUNC(return, AstReturn* ret) {
    bh_arr(WasmInstruction) code = *pcode;

    AstLocal* result_destination = NULL;
    i64 jump_label = get_structured_jump_label(mod, Jump_Type_Return, ret->count + 1);

    //
    // If this is return statement if an inner return of a `do` block,
    // we have to get the result destination out of the return location stack.
    // This can be computed as the -ret->count element.
    //
    //      2 | return from second do
    //      1 | return from first do
    //      0 | return from function
    //
    if (bh_arr_length(mod->return_location_stack) > 0 && jump_label >= 0) {
        i32 len = bh_arr_length(mod->return_location_stack);
        result_destination = mod->return_location_stack[len - ret->count - 1];
    }

    // If we have an expression to return, we see if it should be placed on the linear memory stack, or the WASM stack.
    if (ret->expr) {
        if (result_destination) {
            emit_expression(mod, &code, ret->expr);
            emit_generic_store_instruction(mod, &code, (AstTyped *) result_destination, NULL);

        } else if (mod->curr_cc == CC_Return_Stack) {
            WIL(NULL, WI_LOCAL_GET, mod->stack_base_idx);
            WID(NULL, WI_I32_CONST, type_size_of(ret->expr->type));
            WI(NULL, WI_I32_SUB);

            emit_expression(mod, &code, ret->expr);
            emit_store_instruction(mod, &code, ret->expr->type, 0);

        } else {
            //
            // This code needs to handle the case where you are returning a r-value structure
            // that lives on the stack of another function, AND you need to invoke deferred
            // statements afterwards. Because you cannot know what a deferred statement may
            // do, you have to prepare for the worst and copy the value to stack memory.
            // Otherwise, the deferred statement may invoke a function that stomps all over
            // the value's memory. For example,
            //
            //     f :: () => Foo.{1, 2, 3};
            //     g :: () => {
            //         defer printf("Doing lots of things! {}\n", 123);
            //         return f();
            //     }
            //
            b32 need_to_copy_to_separate_buffer_to_avoid_corrupted_from_deferred_calls = 0;
            u64 return_value_buffer;
            if (onyx_type_is_stored_in_memory(ret->expr->type)
                && !is_lval((AstNode *) ret->expr)
                && bh_arr_length(mod->deferred_stmts) > 0) {
                need_to_copy_to_separate_buffer_to_avoid_corrupted_from_deferred_calls = 1;

                return_value_buffer = local_allocate_type_in_memory(mod->local_alloc, ret->expr->type);
                emit_stack_address(mod, &code, return_value_buffer, NULL);
            }

            emit_expression(mod, &code, ret->expr);

            if (need_to_copy_to_separate_buffer_to_avoid_corrupted_from_deferred_calls) {
                WIL(NULL, WI_I32_CONST, type_size_of(ret->expr->type));
                emit_wasm_copy(mod, &code, NULL);
                emit_stack_address(mod, &code, return_value_buffer, NULL);
            }
        }
    }

    // Clear the normal deferred statements
    emit_deferred_stmts(mod, &code);

    if (jump_label >= 0) {
        WIL(NULL, WI_JUMP, jump_label);

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
        WI(NULL, WI_NOP);
        WI(NULL, WI_NOP);

        WI(NULL, WI_RETURN);
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
        case WASM_TYPE_INT32:   WIL(NULL, WI_I32_CONST, 0); break;
        case WASM_TYPE_INT64:   WIL(NULL, WI_I64_CONST, 0); break;
        case WASM_TYPE_FLOAT32: WIL(NULL, WI_F32_CONST, 0); break;
        case WASM_TYPE_FLOAT64: WIL(NULL, WI_F64_CONST, 0); break;
        case WASM_TYPE_VAR128:  {
            static u8 zero_v128[] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
            WIP(NULL, WI_V128_CONST, &zero_v128);
            break;
        }
    }

    *pcode = code;
}

EMIT_FUNC(zero_value_for_type, Type* type, OnyxToken* where, AstTyped *alloc_node) {
    bh_arr(WasmInstruction) code = *pcode;

    if (onyx_type_is_multiple_wasm_values(type)) {
        i32 mem_count = type_linear_member_count(type);
        TypeWithOffset two;

        fori (i, 0, mem_count) {
            type_linear_member_lookup(type, i, &two);
            emit_zero_value_for_type(mod, &code, two.type, where, NULL);
        }

    } else if (onyx_type_is_stored_in_memory(type) && alloc_node) {
        emit_local_allocation(mod, &code, alloc_node);

        emit_location(mod, &code, alloc_node);
        WIL(NULL, WI_I32_CONST, 0);
        WIL(NULL, WI_I32_CONST, type_size_of(type));
        emit_wasm_fill(mod, &code, NULL);

        emit_location(mod, &code, alloc_node);

    } else if (type->kind == Type_Kind_Function) {
        WID(NULL, WI_I32_CONST, mod->null_proc_func_idx);
        WIL(NULL, WI_I32_CONST, 0);

    } else {
        if (type == &basic_types[Basic_Kind_Void]) {
            return;
        }

        WasmType wt = onyx_type_to_wasm_type(type);
        if (wt == WASM_TYPE_VOID) {
            onyx_report_error(where->pos, Error_Critical, "Cannot produce a zero-value for this type.");
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
        switch (type_get_param_pass(*param_type)) {
            case Param_Pass_By_Value:            *(t++) = (char) onyx_type_to_wasm_type(*param_type); break;
            case Param_Pass_By_Implicit_Pointer: *(t++) = (char) onyx_type_to_wasm_type(&basic_types[Basic_Kind_Rawptr]); break;

            case Param_Pass_By_Multiple_Values: {
                u32 mem_count = type_structlike_mem_count(*param_type);
                StructMember smem;

                fori (i, 0, mem_count) {
                    type_lookup_member_by_idx(*param_type, i, &smem);
                    *(t++) = (char) onyx_type_to_wasm_type(smem.type);
                }

                param_count += mem_count - 1;
                break;
            }
        }

        param_type++;
    }
    *(t++) = ':';

    WasmType return_type = onyx_type_to_wasm_type(ft->Function.return_type);

    *(t++) = (char) return_type;
    *t = '\0';

    i32 type_idx = 0;
    i32 index = shgeti(mod->type_map, type_repr_buf);
    if (index != -1) {
        type_idx = mod->type_map[index].value;
    } else {
        // NOTE: Make a new type
        WasmFuncType* type = (WasmFuncType*) bh_alloc(mod->allocator, sizeof(WasmFuncType) + sizeof(WasmType) * param_count);
        type->return_type = return_type;
        type->param_count = param_count;

        fori (i, 0, type->param_count) {
            type->param_types[i] = type_repr_buf[i];
        }

        bh_arr_push(mod->types, type);

        shput(mod->type_map, type_repr_buf, mod->next_type_idx);
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

static void emit_function(OnyxWasmModule* mod, AstFunction* fd) {
    if (!should_emit_function(fd)) return;

    i32 type_idx = generate_type_idx(mod, fd->type);

    WasmFunc wasm_func = { 0 };
    wasm_func.type_idx = type_idx;
    wasm_func.location = fd->token;

    bh_arr_new(mod->allocator, wasm_func.code, 16);

    i32 func_idx = (i32) bh_imap_get(&mod->index_map, (u64) fd);
    mod->current_func_idx = func_idx;

    debug_begin_function(mod, func_idx, fd->token, get_function_name(fd));

    if (fd == builtin_initialize_data_segments && context.options->use_post_mvp_features) {
        emit_initialize_data_segments_body(mod, &wasm_func.code);

        debug_emit_instruction(mod, NULL);
        bh_arr_push(wasm_func.code, ((WasmInstruction){ WI_BLOCK_END, 0x00 }));

        bh_arr_set_at(mod->funcs, func_idx - mod->foreign_function_count, wasm_func);
        mod->current_func_idx = -1;

        debug_end_function(mod);
        return;
    }

    if (fd == builtin_run_init_procedures) {
        emit_run_init_procedures(mod, &wasm_func.code);

        debug_emit_instruction(mod, NULL);
        bh_arr_push(wasm_func.code, ((WasmInstruction){ WI_BLOCK_END, 0x00 }));

        bh_arr_set_at(mod->funcs, func_idx - mod->foreign_function_count, wasm_func);
        mod->current_func_idx = -1;

        debug_end_function(mod);
        return;
    }

    if (fd->body != NULL) {
        mod->local_alloc = &wasm_func.locals;

        // NOTE: Generate the local map
        u64 localidx = 0;
        bh_arr_each(AstParam, param, fd->params) {
            switch (type_get_param_pass(param->local->type)) {
                case Param_Pass_By_Multiple_Values: 
                    debug_introduce_symbol(mod, param->local->token, DSL_REGISTER, localidx | LOCAL_IS_WASM, param->local->type);
                    bh_imap_put(&mod->local_map, (u64) param->local, localidx | LOCAL_IS_WASM);
                    localidx += type_structlike_mem_count(param->local->type);
                    break;

                case Param_Pass_By_Value:
                case Param_Pass_By_Implicit_Pointer:
                    debug_introduce_symbol(mod, param->local->token, DSL_REGISTER, localidx | LOCAL_IS_WASM, param->local->type);
                    bh_imap_put(&mod->local_map, (u64) param->local, localidx++ | LOCAL_IS_WASM);
                    break;

                default: assert(0);
            }
        }

        mod->local_alloc->param_count = localidx;

        mod->curr_cc = type_function_get_cc(fd->type);
        assert(mod->curr_cc != CC_Undefined);

        bh_arr_clear(mod->stack_leave_patches);

        debug_emit_instruction(mod, fd->token);
        debug_emit_instruction(mod, fd->token);
        debug_emit_instruction(mod, fd->token);
        debug_emit_instruction(mod, fd->token);
        debug_emit_instruction(mod, fd->token);
        bh_arr_insert_end(wasm_func.code, 5);
        fori (i, 0, 5) wasm_func.code[i] = (WasmInstruction) { WI_NOP, 0 };

        mod->stack_base_idx = local_raw_allocate(mod->local_alloc, WASM_TYPE_PTR);
        debug_function_set_ptr_idx(mod, func_idx, mod->stack_base_idx);

        // Generate code
        emit_function_body(mod, &wasm_func.code, fd);

        if (mod->local_alloc->max_stack > 0 || mod->curr_cc == CC_Return_Stack) {
            emit_stack_enter(mod, &wasm_func.code, mod->local_alloc->max_stack);

            // Place all stack leaves in patch locations. These will (probably) all be
            // right before a "return" instruction.
            debug_emit_instruction(mod, NULL);
            debug_emit_instruction(mod, NULL);

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

    if (fd->closing_brace) {
        debug_emit_instruction(mod, fd->closing_brace);
        bh_arr_push(wasm_func.code, ((WasmInstruction){ WI_NOP, 0x00 }));
    }

    debug_emit_instruction(mod, NULL);
    bh_arr_push(wasm_func.code, ((WasmInstruction){ WI_BLOCK_END, 0x00 }));

    bh_imap_clear(&mod->local_map);

    bh_arr_set_at(mod->funcs, func_idx - mod->foreign_function_count, wasm_func);
    mod->current_func_idx = -1;

    debug_end_function(mod);
}

static void encode_type_as_dyncall_symbol(char *out, Type *t) {
    if (type_struct_is_just_one_basic_value(t)) {
        Type *inner = type_struct_is_just_one_basic_value(t);
        encode_type_as_dyncall_symbol(out, inner);
    }

    else if (t->kind == Type_Kind_Slice) strncat(out, "s", 64);
    else if (t->kind == Type_Kind_Pointer) strncat(out, "p", 64);
    else if (t->kind == Type_Kind_MultiPointer) strncat(out, "p", 64);
    else if (t->kind == Type_Kind_Enum) encode_type_as_dyncall_symbol(out, t->Enum.backing);
    else if (t->kind == Type_Kind_Basic) {
        TypeBasic* basic = &t->Basic;
        if (basic->flags & Basic_Flag_Boolean) strncat(out, "i", 64);
        else if (basic->flags & Basic_Flag_Integer) {
            if (basic->size <= 4) strncat(out, "i", 64);
            if (basic->size == 8) strncat(out, "l", 64);
        }
        else if (basic->flags & Basic_Flag_Pointer) strncat(out, "p", 64);
        else if (basic->flags & Basic_Flag_Float) {
            if (basic->size <= 4) strncat(out, "f", 64);
            if (basic->size == 8) strncat(out, "d", 64);
        }
        else if (basic->flags & Basic_Flag_SIMD) strncat(out, "v", 64);
        else if (basic->flags & Basic_Flag_Type_Index) strncat(out, "i", 64);
        else strncat(out, "v", 64);
    }
    else if (t->kind == Type_Kind_Distinct) {
        encode_type_as_dyncall_symbol(out, t->Distinct.base_type);
    }

    else strncat(out, "v", 64);
}

static void emit_foreign_function(OnyxWasmModule* mod, AstFunction* fd) {
    if (!should_emit_function(fd)) return;

    i32 type_idx = generate_type_idx(mod, fd->type);

    char *module, *name;
    OnyxToken *foreign_module = fd->foreign.module_name->token;
    OnyxToken *foreign_import = fd->foreign.import_name->token;

    if (fd->is_foreign_dyncall) {
        module = bh_aprintf(global_heap_allocator, "dyncall:%b", foreign_module->text, foreign_module->length);

        char type_encoding[65] = {0};
        encode_type_as_dyncall_symbol(type_encoding, fd->type->Function.return_type);

        bh_arr_each(AstParam, param, fd->params) {
            encode_type_as_dyncall_symbol(type_encoding, param->local->type);
        }

        name = bh_aprintf(global_heap_allocator, "%b:%s", foreign_import->text, foreign_import->length, type_encoding);

    } else {
        module = bh_aprintf(global_heap_allocator, "%b", foreign_module->text, foreign_module->length);
        name = bh_aprintf(global_heap_allocator, "%b", foreign_import->text, foreign_import->length);
    }

    WasmImport import = {
        .kind = WASM_FOREIGN_FUNCTION,
        .idx  = type_idx,
        .mod  = module,
        .name = name,
    };

    bh_arr_push(mod->imports, import);
    return;
}

static void emit_export_directive(OnyxWasmModule* mod, AstDirectiveExport* export) {
    assert(export->export_name);
    assert(export->export);

    token_toggle_end(export->export_name);

    AstTyped *the_export = (AstTyped *) strip_aliases((AstNode *) export->export);
    assert(the_export);

    i64 idx = bh_imap_get(&mod->index_map, (u64) the_export);

    WasmExport wasm_export;
    wasm_export.idx = (i32) idx;

    switch (the_export->kind) {
        case Ast_Kind_Function: wasm_export.kind = WASM_FOREIGN_FUNCTION;
                                break;

        case Ast_Kind_Global:   wasm_export.kind = WASM_FOREIGN_GLOBAL;
                                break;
    }

    shput(mod->exports, export->export_name->text, wasm_export);
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

    if (global == &builtin_heap_start)
        module->heap_start_ptr = &module->globals[global_idx].initial_value[0].data.i1;

    if (global == &builtin_tls_size)
        module->globals[global_idx].initial_value[0].data.i1 =  module->next_tls_offset;
}

static void emit_string_literal(OnyxWasmModule* mod, AstStrLit* strlit) {

    // NOTE: Allocating more than necessary, but there are no cases
    // in a string literal that create more bytes than already
    // existed. You can create less however ('\n' => 0x0a).
    i8* strdata = bh_alloc_array(global_heap_allocator, i8, strlit->token->length + 1);
    i32 length  = string_process_escape_seqs(strdata, strlit->token->text, strlit->token->length);

    i32 index = shgeti(mod->string_literals, (char *) strdata);
    if (index != -1) {
        StrLitInfo sti = mod->string_literals[index].value;
        strlit->data_id = sti.data_id;
        strlit->length  = sti.len + (strlit->is_cstr ? 1 : 0);

        bh_free(global_heap_allocator, strdata);
        return;
    }
    
    // :ProperLinking
    // The length used here is one greater than the string length, because
    // we DO want to include the null-terminator in the outputted string.
    WasmDatum datum = {
        .alignment = 1,
        .length = length + 1,
        .data = strdata,
    };

    strlit->data_id = emit_data_entry(mod, &datum);
    strlit->length  = length + (strlit->is_cstr ? 1 : 0);

    // :ProperLinking
    shput(mod->string_literals, (char *) strdata, ((StrLitInfo) { strlit->data_id, length }));
}

static u32 emit_data_entry(OnyxWasmModule *mod, WasmDatum *datum) {
    datum->offset_ = 0;
    datum->id = NEXT_DATA_ID(mod); 
    bh_arr_push(mod->data, *datum);
    return datum->id;
}

static void emit_constexpr(ConstExprContext *ctx, AstTyped *node, u32 offset) {
    if (!emit_constexpr_(ctx, node, offset)) {
        onyx_report_error(node->token->pos, Error_Critical,
            "Cannot generate constant data for '%s'.",
            onyx_ast_node_kind_string(node->kind));
    }
}

static b32 emit_constexpr_(ConstExprContext *ctx, AstTyped *node, u32 offset) {
#define CE(type, off) (*((type *) bh_pointer_add(ctx->data, offset + (off))))
    assert(ctx->data_id);
    assert(ctx->data);

    b32 retval = 1;
    node = (AstTyped *) strip_aliases((AstNode *) node);

    if (node_is_type((AstNode *) node)) {
        Type* constructed_type = type_build_from_ast(context.ast_alloc, (AstType *) node);
        CE(i32, 0) = constructed_type->id;
        return 1;
    }

    switch (node->kind) {
    case Ast_Kind_Array_Literal: {
        AstArrayLiteral* al = (AstArrayLiteral *) node;

        i32 i = 0;
        i32 elem_size = type_size_of(al->type->Array.elem);

        bh_arr_each(AstTyped *, expr, al->values) {
            retval &= emit_constexpr_(ctx, *expr, i * elem_size + offset);
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
            retval &= emit_constexpr_(ctx, sl->args.values[i], smem.offset + offset);
        }

        break;
    }

    case Ast_Kind_Directive_Export_Name: {
        AstDirectiveExportName *ename = (AstDirectiveExportName *) node;
        node = (AstTyped *) ename->name;

        // This fallthrough is very intentional. This replaces the value of "node"
        // so the case below thinks it is just generating the constexpr of a string.
        // fallthrough
    }

    case Ast_Kind_StrLit: {
        AstStrLit* sl = (AstStrLit *) node;

        // NOTE: This assumes the data_id and the length fields have been filled out
        // by emit_string_literal.
        if (POINTER_SIZE == 4) {
            CE(u32, 0) = 0;
            CE(u32, 4) = sl->length;
        } else {
            CE(u64, 0) = 0;
            CE(u64, 8) = sl->length;
        }

        assert(sl->data_id > 0);

        DatumPatchInfo patch;
        patch.kind = Datum_Patch_Data;
        patch.index = ctx->data_id;
        patch.location = offset;
        patch.data_id = sl->data_id;
        patch.offset = 0;
        bh_arr_push(ctx->module->data_patches, patch);

        break;
    }

    case Ast_Kind_Enum_Value: {
        AstEnumValue* ev = (AstEnumValue *) node;
        retval &= emit_constexpr_(ctx, (AstTyped *) ev->value, offset);
        break;
    }

    case Ast_Kind_Function: {
        AstFunction* func = (AstFunction *) node;
        CE(u32, 0) = get_element_idx(ctx->module, func);
        break;
    }

    case Ast_Kind_Size_Of: {
        AstSizeOf* so = (AstSizeOf *) node;
        CE(u32, 0) = so->size;
        break;
    }

    case Ast_Kind_Align_Of: {
        AstAlignOf* ao = (AstAlignOf *) node;
        CE(u32, 0) = ao->alignment;
        break;
    }

    case Ast_Kind_Zero_Value: {
        memset(bh_pointer_add(ctx->data, offset), 0, type_size_of(node->type));
        break;
    }

    case Ast_Kind_Package: {
        AstPackage *package = (AstPackage *) node;
        CE(u32, 0) = package->package->id;
        break;
    }

    case Ast_Kind_Address_Of: {
        AstAddressOf *aof = (AstAddressOf *) node;
        AstNode *expr = strip_aliases((AstNode *) aof->expr);
        assert(expr->kind == Ast_Kind_Memres);

        DatumPatchInfo patch;
        patch.kind = Datum_Patch_Data;
        patch.index = ctx->data_id;
        patch.location = offset;
        patch.offset = 0;
        patch.data_id = 0;

        // Here, we cannot use the data_id property of the
        // memory reservation because there is no guarantee that
        // it will be assigned yet. And unlike the rest of the
        // compiler, we cannot yield here, so we simply set a
        // pointer that will be used later in the linking phase
        // to get the actual data id of the addressed node.
        patch.node_to_use_if_data_id_is_null = expr;

        bh_arr_push(ctx->module->data_patches, patch);
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
            CE(i8, 0) = (i8) ((AstNumLit *) node)->value.i;
            return retval;

        case Basic_Kind_I16:
        case Basic_Kind_U16:
            CE(i16, 0) = (i16) ((AstNumLit *) node)->value.i;
            return retval;

        case Basic_Kind_I32:
        case Basic_Kind_U32:
        case Basic_Kind_Rawptr:
            CE(i32, 0) = ((AstNumLit *) node)->value.i;
            return retval;

        case Basic_Kind_I64:
        case Basic_Kind_U64:
            CE(i64, 0) = ((AstNumLit *) node)->value.l;
            return retval;

        case Basic_Kind_F32:
            CE(f32, 0) = ((AstNumLit *) node)->value.f;
            return retval;

        case Basic_Kind_F64:
            CE(f64, 0) = ((AstNumLit *) node)->value.d;
            return retval;

        default: break;
        }

        //fallthrough
    }

    case Ast_Kind_Code_Block: break;

    default: retval = 0;
    }

    return retval;

#undef CE
}

static void emit_memory_reservation(OnyxWasmModule* mod, AstMemRes* memres) {
    // :ProperLinking
    Type* effective_type = memres->type;

    u64 alignment = type_alignment_of(effective_type);
    u64 size = type_size_of(effective_type);

    if (type_table_node != NULL && (AstMemRes *) type_table_node == memres) {
        u64 table_location = build_type_table(mod);
        memres->data_id = table_location;
        return;
    }

    if (foreign_blocks_node != NULL && (AstMemRes *) foreign_blocks_node == memres) {
        u64 foreign_blocks_location = build_foreign_blocks(mod);
        memres->data_id = foreign_blocks_location;
        return;
    }

    if (tagged_procedures_node != NULL && (AstMemRes *) tagged_procedures_node == memres) {
        u64 tagged_procedures_location = build_tagged_procedures(mod);
        memres->data_id = tagged_procedures_location;
        return;
    }

    if (memres->threadlocal) {
        memres->tls_offset = mod->next_tls_offset;
        bh_align(memres->tls_offset, alignment);
        mod->next_tls_offset = memres->tls_offset + size;

    } else {
        // :ProperLinking
        u8* data = NULL;
        if (memres->initial_value != NULL) {
            assert(!memres->threadlocal);
            data = bh_alloc(global_heap_allocator, size);
        }

        WasmDatum datum = {
            .alignment = alignment,
            .length = size,
            .data = data,
        };
        memres->data_id = emit_data_entry(mod, &datum);

        if (memres->initial_value != NULL) {
            ConstExprContext constexpr_ctx;
            constexpr_ctx.module = mod;
            constexpr_ctx.data = data;
            constexpr_ctx.data_id = memres->data_id;
            emit_constexpr(&constexpr_ctx, memres->initial_value, 0);
        }
    }
}

static void emit_file_contents(OnyxWasmModule* mod, AstFileContents* fc) {
    // INVESTIGATE: I think that filename should always be NULL when this function starts because
    // a file contents entity is only processed once and therefore should only go through this step
    // once. But somehow filename isn't NULL occasionally so I have to check for that...
    //                                                                      - brendanfh  2021/05/23
    if (fc->filename == NULL) {
        const char* parent_file = fc->token->pos.filename;
        if (parent_file == NULL) parent_file = ".";

        char* parent_folder = bh_path_get_parent(parent_file, global_scratch_allocator);
        
        OnyxToken *filename_token = fc->filename_expr->token;

        token_toggle_end(filename_token);
        char* temp_fn     = bh_alloc_array(global_scratch_allocator, char, filename_token->length);
        i32   temp_fn_len = string_process_escape_seqs(temp_fn, filename_token->text, filename_token->length);
        char* filename    = bh_lookup_file(temp_fn, parent_folder, "", 0, NULL, 0);
        fc->filename      = bh_strdup(global_heap_allocator, filename);
        token_toggle_end(filename_token);
    }

    i32 index = shgeti(mod->loaded_file_info, fc->filename);
    if (index != -1) {
        StrLitInfo info = mod->loaded_file_info[index].value;
        fc->data_id = info.data_id;
        fc->size    = info.len;
        return;
    }

    if (!bh_file_exists(fc->filename)) {
        onyx_report_error(fc->token->pos, Error_Critical,
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

    WasmDatum datum = {
        .alignment = 16,
        .length = length,
        .data = actual_data,
    };
    fc->data_id = emit_data_entry(mod, &datum);
    fc->size = length - 1;

    shput(mod->loaded_file_info, fc->filename, ((StrLitInfo) {
        .data_id = fc->data_id,
        .len = fc->size,
    }));
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
        .data_patches = NULL,

        .next_tls_offset = 0,
        .tls_size_ptr = NULL,

        .elems = NULL,
        .next_elem_idx = 0,

        .needs_memory_section = 0,
        .memory_min_size = 0,
        .memory_max_size = 0,

        .structured_jump_target = NULL,
        .return_location_stack = NULL,
        .local_allocations = NULL,
        .stack_leave_patches = NULL,
        .deferred_stmts = NULL,

        .heap_start_ptr = NULL,

        .stack_top_ptr = NULL,
        .stack_base_idx = 0,

        .foreign_function_count = 0,

        .null_proc_func_idx = -1,

        .libraries = NULL,
        .library_paths = NULL,

        .foreign_blocks = NULL,
        .next_foreign_block_idx = 0,

        .procedures_with_tags = NULL
    };

    bh_arena* eid = bh_alloc(global_heap_allocator, sizeof(bh_arena));
    bh_arena_init(eid, global_heap_allocator, 16 * 1024 * 1024);
    module.extended_instr_data = eid;
    module.extended_instr_alloc = bh_arena_allocator(eid);

    bh_arr_new(alloc, module.types, 4);
    bh_arr_new(alloc, module.funcs, 4);
    bh_arr_new(alloc, module.imports, 4);
    bh_arr_new(alloc, module.globals, 4);
    bh_arr_new(alloc, module.data, 4);
    bh_arr_new(alloc, module.elems, 4);
    bh_arr_new(alloc, module.libraries, 4);
    bh_arr_new(alloc, module.library_paths, 4);

    bh_arr_new(global_heap_allocator, module.return_location_stack, 4);
    bh_arr_new(global_heap_allocator, module.structured_jump_target, 16);
    bh_arr_set_length(module.structured_jump_target, 0);

    sh_new_arena(module.type_map);
    sh_new_arena(module.exports);
    sh_new_arena(module.loaded_file_info);
    sh_new_arena(module.string_literals);

    bh_imap_init(&module.index_map, global_heap_allocator, 128);
    bh_imap_init(&module.local_map, global_heap_allocator, 16);
    bh_imap_init(&module.elem_map,  global_heap_allocator, 16);

    bh_arr_new(global_heap_allocator, module.deferred_stmts, 4);
    bh_arr_new(global_heap_allocator, module.local_allocations, 4);
    bh_arr_new(global_heap_allocator, module.stack_leave_patches, 4);
    bh_arr_new(global_heap_allocator, module.foreign_blocks, 4);
    bh_arr_new(global_heap_allocator, module.procedures_with_tags, 4);
    bh_arr_new(global_heap_allocator, module.data_patches, 4);

#ifdef ENABLE_DEBUG_INFO
    module.debug_context = bh_alloc_item(context.ast_alloc, DebugContext);
    module.debug_context->allocator = global_heap_allocator;
    module.debug_context->next_file_id = 0;
    module.debug_context->next_sym_id = 0;
    module.debug_context->last_token = NULL;
    module.debug_context->sym_info = NULL;
    module.debug_context->sym_patches = NULL;
    module.debug_context->funcs = NULL;

    sh_new_arena(module.debug_context->file_info);
    bh_arr_new(global_heap_allocator, module.debug_context->sym_info, 32);
    bh_arr_new(global_heap_allocator, module.debug_context->sym_patches, 32);
    bh_arr_new(global_heap_allocator, module.debug_context->funcs, 16);

    bh_buffer_init(&module.debug_context->op_buffer, global_heap_allocator, 1024);
#endif

    return module;
}

void emit_entity(Entity* ent) {
    OnyxWasmModule* module = context.wasm_module;
    module->current_func_idx = -1;

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

            if (ent->function->tags != NULL) {
                bh_arr_push(module->procedures_with_tags, ent->function);
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

            if (ent->expr->kind == Ast_Kind_Directive_Library) {
                bh_arr_push(module->libraries, ent->library->library_name);
            }
            break;
        }

        case Entity_Type_Foreign_Block: {
            ent->foreign_block->foreign_block_number = module->next_foreign_block_idx++;
            bh_arr_push(module->foreign_blocks, (AstForeignBlock *) ent->foreign_block);
            break;
        }

        case Entity_Type_Function: emit_function(module, ent->function); break;
        case Entity_Type_Global:   emit_global(module,   ent->global); break;

        default: break;
    }

    ent->state = Entity_State_Finalized;
}

void onyx_wasm_module_link(OnyxWasmModule *module, OnyxWasmLinkOptions *options) {
    // If the pointer size is going to change,
    // the code will probably need to be altered.
    assert(POINTER_SIZE == 4);

    module->memory_min_size = options->memory_min_size;
    module->memory_max_size = options->memory_max_size;

    if (context.options->use_multi_threading || options->import_memory) {
        module->needs_memory_section = 0;

        WasmImport mem_import = {
            .kind   = WASM_FOREIGN_MEMORY,
            .min    = options->memory_min_size,
            .max    = options->memory_max_size, // NOTE: Why not use all 4 Gigs of memory?
            .shared = context.options->runtime == Runtime_Js,

            .mod    = options->import_memory_module_name,
            .name   = options->import_memory_import_name,
        };

        bh_arr_push(module->imports, mem_import);

    } else {
        module->needs_memory_section = 1;
    }

    if (options->export_memory) {
        WasmExport mem_export = {
            .kind = WASM_FOREIGN_MEMORY,
            .idx = 0,
        };

        shput(module->exports, options->export_memory_name, mem_export);
        module->export_count++;
    }

    if (options->export_func_table) {
        WasmExport func_table_export = {
            .kind = WASM_FOREIGN_TABLE,
            .idx  = 0,
        };

        shput(module->exports, options->export_func_table_name, func_table_export);
        module->export_count++;
    }

    u32 datum_offset = options->null_reserve_size;
    bh_arr_each(WasmDatum, datum, module->data) {
        assert(datum->id > 0);

        bh_align(datum_offset, datum->alignment);
        datum->offset_ = datum_offset;

        datum_offset += datum->length;
    }

    bh_arr_each(DatumPatchInfo, patch, module->data_patches) {
        if (patch->data_id == 0) {
            if (patch->node_to_use_if_data_id_is_null
                && patch->node_to_use_if_data_id_is_null->kind == Ast_Kind_Memres) {

                patch->data_id = ((AstMemRes *) patch->node_to_use_if_data_id_is_null)->data_id;

            } else {
                assert(("Unexpected empty data_id in linking!", 0));
            }
        }

        WasmDatum *datum = &module->data[patch->data_id - 1];
        assert(datum->id == patch->data_id);

        switch (patch->kind) {
            case Datum_Patch_Instruction: {
                WasmFunc *func = &module->funcs[patch->index - module->foreign_function_count];

                assert(func->code[patch->location].type == WI_PTR_CONST);
                func->code[patch->location].data.l = (u64) datum->offset_ + patch->offset;
                break;
            }

            case Datum_Patch_Data: {
                WasmDatum *datum_to_alter = &module->data[patch->index - 1];
                assert(datum_to_alter->id == patch->index);

                *((u32 *) bh_pointer_add(datum_to_alter->data, patch->location)) = (u32) datum->offset_ + patch->offset;
                break;
            }

            case Datum_Patch_Relative: {
                WasmDatum *datum_to_alter = &module->data[patch->index - 1];
                assert(datum_to_alter->id == patch->index);

                u32 *addr = (u32 *) bh_pointer_add(datum_to_alter->data, patch->location);
                if (*addr != 0) {
                    *addr += (u32) datum->offset_ + patch->offset;
                }

                break;
            }

            default: assert(0);
        }
    }

    assert(module->stack_top_ptr && module->heap_start_ptr);

    *module->stack_top_ptr = datum_offset;
    bh_align(*module->stack_top_ptr, options->stack_alignment);

    *module->heap_start_ptr = *module->stack_top_ptr + options->stack_size;
    bh_align(*module->heap_start_ptr, 16);
}

void onyx_wasm_module_free(OnyxWasmModule* module) {
    if (module->extended_instr_data != NULL)
        bh_arena_free(module->extended_instr_data);

    bh_arr_free(module->types);
    bh_arr_free(module->funcs);
    bh_imap_free(&module->local_map);
    bh_imap_free(&module->index_map);
    shfree(module->type_map);
    shfree(module->exports);
}


b32 onyx_wasm_build_link_options_from_node(OnyxWasmLinkOptions *opts, AstTyped *node) {
    node = (AstTyped *) strip_aliases((AstNode *) node);

    assert(node && node->kind == Ast_Kind_Struct_Literal);
    assert(builtin_link_options_type);

    Type *link_options_type = type_build_from_ast(context.ast_alloc, builtin_link_options_type);
    
    AstStructLiteral *input = (AstStructLiteral *) node;

    StructMember smem;
    b32 out_is_valid;

    // TODO: These should be properly error handled.
    assert(type_lookup_member(link_options_type, "stack_first", &smem));
    opts->stack_first = get_expression_integer_value(input->args.values[smem.idx], &out_is_valid) != 0;
    if (!out_is_valid) return 0;

    assert(type_lookup_member(link_options_type, "stack_size", &smem));
    opts->stack_size = get_expression_integer_value(input->args.values[smem.idx], &out_is_valid);
    if (!out_is_valid) return 0;

    assert(type_lookup_member(link_options_type, "stack_alignment", &smem));
    opts->stack_alignment = get_expression_integer_value(input->args.values[smem.idx], &out_is_valid);
    if (!out_is_valid) return 0;

    assert(type_lookup_member(link_options_type, "null_reserve_size", &smem));
    opts->null_reserve_size = get_expression_integer_value(input->args.values[smem.idx], &out_is_valid);
    if (!out_is_valid) return 0;

    assert(type_lookup_member(link_options_type, "import_memory", &smem));
    opts->import_memory = get_expression_integer_value(input->args.values[smem.idx], &out_is_valid) != 0;
    if (!out_is_valid) return 0;

    assert(type_lookup_member(link_options_type, "import_memory_module_name", &smem));
    opts->import_memory_module_name = get_expression_string_value(input->args.values[smem.idx], &out_is_valid);
    if (!out_is_valid) return 0;

    assert(type_lookup_member(link_options_type, "import_memory_import_name", &smem));
    opts->import_memory_import_name = get_expression_string_value(input->args.values[smem.idx], &out_is_valid);
    if (!out_is_valid) return 0;

    assert(type_lookup_member(link_options_type, "export_memory", &smem));
    opts->export_memory = get_expression_integer_value(input->args.values[smem.idx], &out_is_valid) != 0;
    if (!out_is_valid) return 0;

    assert(type_lookup_member(link_options_type, "export_memory_name", &smem));
    opts->export_memory_name = get_expression_string_value(input->args.values[smem.idx], &out_is_valid);
    if (!out_is_valid) return 0;

    assert(type_lookup_member(link_options_type, "export_func_table", &smem));
    opts->export_func_table = get_expression_integer_value(input->args.values[smem.idx], &out_is_valid) != 0;
    if (!out_is_valid) return 0;

    assert(type_lookup_member(link_options_type, "export_func_table_name", &smem));
    opts->export_func_table_name = get_expression_string_value(input->args.values[smem.idx], &out_is_valid);
    if (!out_is_valid) return 0;

    assert(type_lookup_member(link_options_type, "memory_min_size", &smem));
    opts->memory_min_size = get_expression_integer_value(input->args.values[smem.idx], &out_is_valid);
    if (!out_is_valid) return 0;

    assert(type_lookup_member(link_options_type, "memory_max_size", &smem));
    opts->memory_max_size = get_expression_integer_value(input->args.values[smem.idx], &out_is_valid);
    if (!out_is_valid) return 0;

    return 1;
}

#include "wasm_output.h"
