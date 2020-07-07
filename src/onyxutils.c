#include "onyxutils.h"
#include "onyxlex.h"
#include "onyxastnodes.h"

bh_scratch global_scratch;
bh_allocator global_scratch_allocator;

bh_managed_heap global_heap;
bh_allocator global_heap_allocator;

static const char* ast_node_names[] = {
    "ERROR",
    "PROGRAM",
    "USE",

    "FUNCTION",
    "FOREIGN",
    "BLOCK",
    "SCOPE",
    "LOCAL",
    "GLOBAL",
    "SYMBOL",

    "UN_OP",
    "BIN_OP",

    "TYPE",
    "LITERAL",
    "PARAM",
    "ARGUMENT",
    "CALL",
    "ASSIGN",
    "RETURN",

    "IF",
    "WHILE",
    "BREAK",
    "CONTINUE",

    "AST_NODE_KIND_COUNT",
};

const char* onyx_ast_node_kind_string(AstNodeKind kind) {
    return ast_node_names[kind];
}



#define print_indent { if (indent > 0) bh_printf("\n"); for (int i = 0; i < indent; i++) bh_printf("  "); }

void onyx_ast_print(AstNode* node, i32 indent) {
    while (node) {
        print_indent;
        bh_printf("(%d) %s ", node->flags, onyx_ast_node_kind_string(node->kind));

        switch (node->kind) {
        case AST_NODE_KIND_USE: {
             AstNodeUse* use_node = (AstNodeUse *) node;
             bh_printf("%b", use_node->filename->token, use_node->filename->length);

             break;
         }

        case AST_NODE_KIND_FUNCTION: {
            if (node->token)
                bh_printf("(%b) ", node->token->token, node->token->length);
            AstNodeFunction* fd = (AstNodeFunction *) node;

            print_indent;
            bh_printf("Params ");
            if (fd->params)
                onyx_ast_print((AstNode *) fd->params, 0);

            print_indent;
            bh_printf("Returns %s", fd->base.type->name);

            print_indent;
            bh_printf("Body");
            if (fd->body)
                onyx_ast_print((AstNode *) fd->body, indent + 1);

            break;
        }

        case AST_NODE_KIND_PARAM: {
            AstNodeLocal* param = (AstNodeLocal *) node;
            bh_printf("%b %s", param->base.token->token, param->base.token->length, param->base.type->name);
            if (param->base.next && indent == 0) {
                bh_printf(", ");
                onyx_ast_print((AstNode *) param->base.next, 0);
            }

            return;
        }

        case AST_NODE_KIND_BLOCK: {
            AstNodeBlock* block = (AstNodeBlock *) node;
            if (block->scope) {
                onyx_ast_print((AstNode *) block->scope, indent + 1);
            }

            if (block->body) {
                onyx_ast_print((AstNode *) block->body, indent + 1);
            }

            break;
        }

        case AST_NODE_KIND_SCOPE: {
            AstNodeScope* scope = (AstNodeScope *) scope;
            if (scope->last_local) {
                onyx_ast_print((AstNode *) scope->last_local, 0);
            }

            break;
        }

        case AST_NODE_KIND_LOCAL: {
            AstNodeLocal* local = (AstNodeLocal *) node;
            bh_printf("%b %s", local->base.token->token, local->base.token->length, local->base.type->name);
            if (local->prev_local && indent == 0) {
                bh_printf(", ");
                onyx_ast_print((AstNode *) local->prev_local, 0);
            }
            break;
        }

        case AST_NODE_KIND_GLOBAL: {
            AstNodeGlobal* global = (AstNodeGlobal *) node;
            bh_printf("%b %s", global->base.token->token, global->base.token->length, global->base.type->name);
            if (global->initial_value) {
                onyx_ast_print((AstNode *) global->initial_value, indent + 1);
            }

            if (node->next) {
                onyx_ast_print(node->next, indent);
            }
            break;
        }

        case AST_NODE_KIND_SYMBOL: {
            bh_printf("%b", node->token->token, node->token->length);
            if (node->next) {
                onyx_ast_print(node->next, indent);
            }
            break;
        }

        case AST_NODE_KIND_RETURN: {
            AstNodeReturn* ret = (AstNodeReturn *) node;
            if (ret->expr) {
                onyx_ast_print((AstNode *) ret->expr, indent + 1);
            }

            break;
        }

        case AST_NODE_KIND_LITERAL: {
            AstNodeNumLit* lit = (AstNodeNumLit *) node;
            bh_printf("(%s) %b", lit->base.type->name, lit->base.token->token, lit->base.token->length);

            break;
        }

        case AST_NODE_KIND_CALL: {
            AstNodeCall* call = (AstNodeCall *) node;
            if (call->callee) {
                if (call->callee->kind == AST_NODE_KIND_FUNCTION) {
                    bh_printf("function: %b", call->callee->token->token, call->callee->token->length);
                } else {
                    onyx_ast_print(call->callee, indent + 1);
                }
            }
            onyx_ast_print((AstNode *) call->arguments, indent + 1);

            break;
        }

        case AST_NODE_KIND_FOREIGN: {
            AstNodeForeign* foreign = (AstNodeForeign *) node;
            bh_printf("%b:%b",
                    foreign->mod_token->token, foreign->mod_token->length,
                    foreign->name_token->token, foreign->name_token->length);

            if (foreign->import) {
                onyx_ast_print(foreign->import, indent + 1);
            }

            break;
        }

        case AST_NODE_KIND_IF: {
            AstNodeIf* if_node = (AstNodeIf *) node;
            if (if_node->cond) {
                print_indent;
                bh_printf("Condition:");
                onyx_ast_print((AstNode *) if_node->cond, indent + 1);
            }
            if (if_node->true_block.as_if) {
                print_indent;
                bh_printf("True block:");
                onyx_ast_print((AstNode *) if_node->true_block.as_if, indent + 1);
            }
            if (if_node->false_block.as_if) {
                print_indent;
                bh_printf("False block:");
                onyx_ast_print((AstNode *) if_node->false_block.as_if, indent + 1);
            }

            break;
        }

        case AST_NODE_KIND_BIN_OP: {
            AstNodeBinOp* binop = (AstNodeBinOp *) node;
            bh_printf("%b", binop->base.token->token, binop->base.token->length);

            onyx_ast_print((AstNode *) binop->left, indent + 1);
            onyx_ast_print((AstNode *) binop->right, indent + 1);

            break;
        }

        default:
            break;
        }

        node = node->next;
    }
}
