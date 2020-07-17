#include "onyxir.h"

#define IR_FUNC(kind, ...) static void ir_ ## kind (IrContext* c, __VA_ARGS__)

IR_FUNC(function,       AstFunction* ast_func);
IR_FUNC(function_body,  AstFunction* fd);
IR_FUNC(block,          AstBlock* block);
IR_FUNC(statement,      AstNode* stmt);
IR_FUNC(assign_lval,    AstTyped* lval);
IR_FUNC(assignment,     AstAssign* assign);
IR_FUNC(if,             AstIf* if_node);
IR_FUNC(while,          AstWhile* while_node);
IR_FUNC(binop,          AstBinaryOp* binop);
IR_FUNC(unaryop,        AstUnaryOp* unop);
IR_FUNC(call,           AstCall* call);
IR_FUNC(intrinsic_call, AstIntrinsicCall* call);
IR_FUNC(expression,     AstTyped* expr);
IR_FUNC(cast,           AstUnaryOp* cast);
IR_FUNC(return,         AstReturn* ret);

static OnyxIntrinsic intrinsic_lookup(char* name) {
    if (!strcmp("memory_size", name))       return ONYX_INTRINSIC_MEMORY_SIZE;
    else if (!strcmp("memory_grow", name))  return ONYX_INTRINSIC_MEMORY_GROW;

    else if (!strcmp("clz_i32", name))      return ONYX_INTRINSIC_I32_CLZ;
    else if (!strcmp("ctz_i32", name))      return ONYX_INTRINSIC_I32_CTZ;
    else if (!strcmp("popcnt_i32", name))   return ONYX_INTRINSIC_I32_POPCNT;
    else if (!strcmp("and_i32", name))      return ONYX_INTRINSIC_I32_AND;
    else if (!strcmp("or_i32", name))       return ONYX_INTRINSIC_I32_OR;
    else if (!strcmp("xor_i32", name))      return ONYX_INTRINSIC_I32_XOR;
    else if (!strcmp("shl_i32", name))      return ONYX_INTRINSIC_I32_SHL;
    else if (!strcmp("slr_i32", name))      return ONYX_INTRINSIC_I32_SLR;
    else if (!strcmp("sar_i32", name))      return ONYX_INTRINSIC_I32_SAR;
    else if (!strcmp("rotl_i32", name))     return ONYX_INTRINSIC_I32_ROTL;
    else if (!strcmp("rotr_i32", name))     return ONYX_INTRINSIC_I32_ROTR;

    else if (!strcmp("clz_i64", name))      return ONYX_INTRINSIC_I64_CLZ;
    else if (!strcmp("ctz_i64", name))      return ONYX_INTRINSIC_I64_CTZ;
    else if (!strcmp("popcnt_i64", name))   return ONYX_INTRINSIC_I64_POPCNT;
    else if (!strcmp("and_i64", name))      return ONYX_INTRINSIC_I64_AND;
    else if (!strcmp("or_i64", name))       return ONYX_INTRINSIC_I64_OR;
    else if (!strcmp("xor_i64", name))      return ONYX_INTRINSIC_I64_XOR;
    else if (!strcmp("shl_i64", name))      return ONYX_INTRINSIC_I64_SHL;
    else if (!strcmp("slr_i64", name))      return ONYX_INTRINSIC_I64_SLR;
    else if (!strcmp("sar_i64", name))      return ONYX_INTRINSIC_I64_SAR;
    else if (!strcmp("rotl_i64", name))     return ONYX_INTRINSIC_I64_ROTL;
    else if (!strcmp("rotr_i64", name))     return ONYX_INTRINSIC_I64_ROTR;

    else if (!strcmp("abs_f32", name))      return ONYX_INTRINSIC_F32_ABS;
    else if (!strcmp("ceil_f32", name))     return ONYX_INTRINSIC_F32_CEIL;
    else if (!strcmp("floor_f32", name))    return ONYX_INTRINSIC_F32_FLOOR;
    else if (!strcmp("trunc_f32", name))    return ONYX_INTRINSIC_F32_TRUNC;
    else if (!strcmp("nearest_f32", name))  return ONYX_INTRINSIC_F32_NEAREST;
    else if (!strcmp("sqrt_f32", name))     return ONYX_INTRINSIC_F32_SQRT;
    else if (!strcmp("min_f32", name))      return ONYX_INTRINSIC_F32_MIN;
    else if (!strcmp("max_f32", name))      return ONYX_INTRINSIC_F32_MAX;
    else if (!strcmp("copysign_f32", name)) return ONYX_INTRINSIC_F32_COPYSIGN;

    else if (!strcmp("abs_f64", name))      return ONYX_INTRINSIC_F64_ABS;
    else if (!strcmp("ceil_f64", name))     return ONYX_INTRINSIC_F64_CEIL;
    else if (!strcmp("floor_f64", name))    return ONYX_INTRINSIC_F64_FLOOR;
    else if (!strcmp("trunc_f64", name))    return ONYX_INTRINSIC_F64_TRUNC;
    else if (!strcmp("nearest_f64", name))  return ONYX_INTRINSIC_F64_NEAREST;
    else if (!strcmp("sqrt_f64", name))     return ONYX_INTRINSIC_F64_SQRT;
    else if (!strcmp("min_f64", name))      return ONYX_INTRINSIC_F64_MIN;
    else if (!strcmp("max_f64", name))      return ONYX_INTRINSIC_F64_MAX;
    else if (!strcmp("copysign_f64", name)) return ONYX_INTRINSIC_F64_COPYSIGN;
    else                                    return ONYX_INTRINSIC_UNDEFINED;
}

static void ir_add_local(IrContext* c, AstLocal* local) {
    bh_arr_push(c->curr_function->locals, local);
}

IR_FUNC(ir_function, AstFunction* ast_func) {
    IrFunction* func = bh_alloc_item(c->allocator, IrFunction);

    func->ast_func = ast_func;
    func->body = ast_func->body->body;
    func->first_param = ast_func->params;

    // NOTE: This is actually the return type (for now)
    func->type = ast_func->base.type;

    func->locals = NULL;
    bh_arr_new(c->allocator, func->locals, 4);

    func->is_exported  = (ast_func->base.flags & Ast_Flag_Exported) != 0;
    func->is_foreign   = (ast_func->base.flags & Ast_Flag_Foreign) != 0;
    func->is_intrinsic = (ast_func->base.flags & Ast_Flag_Intrinsic) != 0;

    if (func->is_intrinsic) {
        token_toggle_end(ast_func->base.token);
        func->intrinsic = intrinsic_lookup(ast_func->base.token->text);
        token_toggle_end(ast_func->base.token);
    }

    else if (func->is_exported) {
        token_toggle_end(ast_func->base.token);
        func->exported_name = bh_aprintf(c->allocator, "%s", ast_func->base.token->text);
        token_toggle_end(ast_func->base.token);
    }

    else if (func->is_foreign) {
        token_toggle_end(ast_func->foreign_module);
        func->foreign_module = bh_aprintf(c->allocator, "%s", ast_func->foreign_module);
        token_toggle_end(ast_func->foreign_module);

        token_toggle_end(ast_func->foreign_name);
        func->foreign_module = bh_aprintf(c->allocator, "%s", ast_func->foreign_name);
        token_toggle_end(ast_func->foreign_name);
    }

    if (func->body != NULL) {
        c->curr_function = func;
    }
}

IR_FUNC(function_body, AstFunction* fd) {
    if (fd->body == NULL) return;
    ir_block(c, fd->body);
}

IR_FUNC(block, AstBlock* block) {
    forll (AstLocal, local, block->locals->last_local, prev_local) {
        ir_add_local(c, local);
    }

    forll (AstNode, stmt, block->body, next) {
        ir_statement(c, stmt);
    }
}

IR_FUNC(statement, AstNode* stmt) {
    switch (stmt->kind) {
        case Ast_Kind_Return:       return ir_return(c, (AstReturn *) stmt);
        case Ast_Kind_Assignment:   return ir_assignment(c, (AstAssign *) stmt);
        case Ast_Kind_If:           return ir_if(c, (AstIf *) stmt);
        case Ast_Kind_While:        return ir_while(c, (AstWhile *) stmt);
        case Ast_Kind_Block:        return ir_block(c, (AstBlock *) stmt);

        default: break;
    }
}

IR_FUNC(if, AstIf* if_node) {
    if (if_node->true_block.as_if->base.kind == Ast_Kind_Block) {
        ir_block(c, if_node->true_block.as_block);
    }

    if (if_node->false_block.as_if->base.kind == Ast_Kind_Block) {
        ir_block(c, if_node->false_block.as_block);
    }
}

IR_FUNC(while, AstWhile* while_node) {
    ir_block(c, while_node->body);
}

// NOTE: Currently, these functions don't have anything to
// do so they are empty and not called. When I have a reason
// to use them, I will populate their bodies
IR_FUNC(assign_lval,    AstTyped* lval)          {}
IR_FUNC(assignment,     AstAssign* assign)       {}
IR_FUNC(binop,          AstBinaryOp* binop)      {}
IR_FUNC(unaryop,        AstUnaryOp* unop)        {}
IR_FUNC(expression,     AstTyped* expr)          {}
IR_FUNC(cast,           AstUnaryOp* cast)        {}
IR_FUNC(return,         AstReturn* ret)          {}

IrContext ir_context_create(bh_allocator allocator) {
    IrContext context = {
        .allocator = allocator,
        .functions = NULL,

        .curr_function = NULL,
    };

    bh_arr_new(allocator, context.functions, 4);

    return context;
}

void ir_context_free(IrContext* context) {
    bh_arr_free(context.functions);
}

void ir_generate(IrContext* context, ParserOutput parse_output) {

}
