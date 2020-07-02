#include "onyxutils.h"
#include "onyxlex.h"
#include "onyxparser.h"

bh_scratch global_scratch;
bh_allocator global_scratch_allocator;

bh_managed_heap global_heap;
bh_allocator global_heap_allocator;

#define print_indent { if (indent > 0) bh_printf("\n"); for (int i = 0; i < indent; i++) bh_printf("  "); }

void onyx_ast_print(OnyxAstNode* node, i32 indent) {
    if (node == NULL) return;

    print_indent;
    bh_printf("(%d) %s ", node->flags, onyx_ast_node_kind_string(node->kind));

    switch (node->kind) {
    case ONYX_AST_NODE_KIND_USE: {
         OnyxAstNodeUse* use_node = &node->as_use;
         bh_printf("%b", use_node->filename->token, use_node->filename->length);

         if (use_node->next)
             onyx_ast_print(use_node->next, indent);

         break;
     }

    case ONYX_AST_NODE_KIND_FUNCTION: {
        if (node->token)
            bh_printf("(%b) ", node->token->token, node->token->length);
        OnyxAstNodeFunction* fd = &node->as_function;

        print_indent;
        bh_printf("Params ");
        if (fd->params)
            onyx_ast_print((OnyxAstNode *) fd->params, 0);

        print_indent;
        bh_printf("Returns %s", fd->return_type->name);

        print_indent;
        bh_printf("Body");
        if (fd->body)
            onyx_ast_print((OnyxAstNode *) fd->body, indent + 1);

        if (fd->next)
            onyx_ast_print((OnyxAstNode *) fd->next, indent);

        break;
    }

    case ONYX_AST_NODE_KIND_PARAM: {
        OnyxAstNodeParam* param = &node->as_param;
        bh_printf("%b %s", param->token->token, param->token->length, param->type->name);
        if (param->next && indent == 0) {
            bh_printf(", ");
            onyx_ast_print((OnyxAstNode *) param->next, 0);
        }

        break;
    }

    case ONYX_AST_NODE_KIND_BLOCK: {
        OnyxAstNodeBlock* block = &node->as_block;
        if (block->scope) {
            onyx_ast_print((OnyxAstNode *) block->scope, indent + 1);
        }

        if (block->body) {
            onyx_ast_print((OnyxAstNode *) block->body, indent + 1);
        }

        if (block->next) {
            onyx_ast_print(block->next, indent);
        }

        break;
    }

    case ONYX_AST_NODE_KIND_SCOPE: {
        OnyxAstNodeScope* scope = &node->as_scope;
        if (scope->last_local) {
            onyx_ast_print((OnyxAstNode *) scope->last_local, 0);
        }

        break;
    }

    case ONYX_AST_NODE_KIND_LOCAL: {
        OnyxAstNodeLocal* local = &node->as_local;
        bh_printf("%b %s", local->token->token, local->token->length, local->type->name);
        if (local->prev_local && indent == 0) {
            bh_printf(", ");
            onyx_ast_print((OnyxAstNode *) local->prev_local, 0);
        } else if (local->next && indent != 0) {
            onyx_ast_print(local->next, indent);
        }
        break;
    }

    case ONYX_AST_NODE_KIND_GLOBAL: {
        OnyxAstNodeGlobal* global = &node->as_global;
        bh_printf("%b %s", global->token->token, global->token->length, global->type->name);
        if (global->initial_value) {
            onyx_ast_print(global->initial_value, indent + 1);
        }

        if (node->next) {
            onyx_ast_print(node->next, indent);
        }
        break;
    }

    case ONYX_AST_NODE_KIND_SYMBOL: {
        bh_printf("%b", node->token->token, node->token->length);
        if (node->next) {
            onyx_ast_print(node->next, indent);
        }
        break;
    }

    case ONYX_AST_NODE_KIND_RETURN: {
        if (node->left) {
            onyx_ast_print(node->left, indent + 1);
        }

        break;
    }

    case ONYX_AST_NODE_KIND_LITERAL: {
        bh_printf("(%s) %b", node->type->name, node->token->token, node->token->length);
        if (node->next) {
            onyx_ast_print(node->next, indent);
        }
        break;
    }

    case ONYX_AST_NODE_KIND_CALL: {
        OnyxAstNodeCall* call = &node->as_call;
        if (call->callee) {
            if (call->callee->kind == ONYX_AST_NODE_KIND_FUNCTION) {
                bh_printf("function: %b", call->callee->token->token, call->callee->token->length);
            } else {
                onyx_ast_print(call->callee, indent + 1);
            }
        }
        onyx_ast_print(call->arguments, indent + 1);
        if (call->next) {
            onyx_ast_print(call->next, indent);
        }
        break;
    }

    case ONYX_AST_NODE_KIND_FOREIGN: {
        OnyxAstNodeForeign* foreign = &node->as_foreign;
        bh_printf("%b:%b",
                foreign->mod_token->token, foreign->mod_token->length,
                foreign->name_token->token, foreign->name_token->length);

        if (foreign->import) {
            onyx_ast_print(foreign->import, indent + 1);
        }

        if (foreign->next) {
            onyx_ast_print(foreign->next, indent);
        }
        break;
    }

    case ONYX_AST_NODE_KIND_IF: {
        OnyxAstNodeIf* if_node = &node->as_if;
        if (if_node->cond) {
            print_indent;
            bh_printf("Condition:");
            onyx_ast_print(if_node->cond, indent + 1);
        }
        if (if_node->true_block) {
            print_indent;
            bh_printf("True block:");
            onyx_ast_print(if_node->true_block, indent + 1);
        }
        if (if_node->false_block) {
            print_indent;
            bh_printf("False block:");
            onyx_ast_print(if_node->false_block, indent + 1);
        }
        if (if_node->next) {
            onyx_ast_print(if_node->next, indent);
        }
        break;
    }

    case ONYX_AST_NODE_KIND_BIN_OP: {
        OnyxAstNodeBinOp* binop = &node->as_binop;
        bh_printf("%b", binop->token->token, binop->token->length);

        onyx_ast_print(node->left, indent + 1);
        onyx_ast_print(node->right, indent + 1);
        if (node->next) {
            onyx_ast_print(node->next, indent);
        }

        break;
    }

    default: {
        onyx_ast_print(node->left, indent + 1);
        onyx_ast_print(node->right, indent + 1);
        if (node->next) {
            onyx_ast_print(node->next, indent);
        }
        break;
    }
    }
}
