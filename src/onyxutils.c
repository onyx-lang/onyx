#include "onyxutils.h"
#include "onyxlex.h"
#include "onyxastnodes.h"
#include "onyxmsgs.h"

bh_scratch global_scratch;
bh_allocator global_scratch_allocator;

bh_managed_heap global_heap;
bh_allocator global_heap_allocator;

static const char* ast_node_names[] = {
    "ERROR",
    "PACKAGE",
    "PROGRAM",
    "USE",

    "BINDING",
    "FUNCTION",
    "OVERLOADED_FUNCTION",
    "BLOCK",
    "SCOPE",
    "LOCAL",
    "GLOBAL",
    "SYMBOL",

    "UN_OP",
    "BIN_OP",

    "TYPE_START (BAD)",
    "TYPE",
    "POINTER_TYPE",
    "FUNCTION_TYPE",
    "ARRAY TYPE",
    "STRUCT TYPE",
    "TYPE_END (BAD)",

    "STRUCT MEMBER",

    "NUMERIC LITERAL",
    "STRING LITERAL",
    "PARAM",
    "ARGUMENT",
    "CALL",
    "RETURN",
    "ADDRESS OF",
    "DEREFERENCE",
    "ARRAY_ACCESS",
    "FIELD_ACCESS",
    "SIZE OF",

    "IF",
    "FOR",
    "WHILE",
    "BREAK",
    "CONTINUE",

    "AST_NODE_KIND_COUNT",
};

const char* onyx_ast_node_kind_string(AstKind kind) {
    return ast_node_names[kind];
}


void program_info_init(ProgramInfo* prog, bh_allocator alloc) {
    prog->global_scope = scope_create(alloc, NULL);

    bh_table_init(alloc, prog->packages, 16);

    prog->entities = NULL;
    bh_arr_new(alloc, prog->entities, 4);
}

Package* program_info_package_lookup(ProgramInfo* prog, char* package_name) {
    if (bh_table_has(Package *, prog->packages, package_name)) {
        return bh_table_get(Package *, prog->packages, package_name);
    } else {
        return NULL;
    }
}

Package* program_info_package_lookup_or_create(ProgramInfo* prog, char* package_name, Scope* parent_scope, bh_allocator alloc) {
    if (bh_table_has(Package *, prog->packages, package_name)) {
        return bh_table_get(Package *, prog->packages, package_name);

    } else {
        Package* package = bh_alloc_item(alloc, Package);

        char* pac_name = bh_alloc_array(alloc, char, strlen(package_name) + 1);
        memcpy(pac_name, package_name, strlen(package_name) + 1);

        package->name = pac_name;
        package->include_scope = scope_create(alloc, parent_scope);
        package->scope = scope_create(alloc, package->include_scope);

        bh_table_put(Package *, prog->packages, pac_name, package);

        return package;
    }
}

Scope* scope_create(bh_allocator a, Scope* parent) {
    Scope* scope = bh_alloc_item(a, Scope);
    scope->parent = parent;
    scope->symbols = NULL;

    bh_table_init(global_heap_allocator, scope->symbols, 64);

    return scope;
}

void scope_include(Scope* target, Scope* source) {
    bh_table_each_start(AstNode *, source->symbols);
        symbol_raw_introduce(target, (char *) key, value->token->pos, value);
    bh_table_each_end;
}

b32 symbol_introduce(Scope* scope, OnyxToken* tkn, AstNode* symbol) {
    token_toggle_end(tkn);

    b32 ret = symbol_raw_introduce(scope, tkn->text, tkn->pos, symbol);

    token_toggle_end(tkn);
    return ret;
}

b32 symbol_raw_introduce(Scope* scope, char* name, OnyxFilePos pos, AstNode* symbol) {

    if (bh_table_has(AstNode *, scope->symbols, name)) {
        onyx_message_add(Msg_Type_Redeclare_Symbol, pos, name);
        return 0;
    }

    bh_table_put(AstNode *, scope->symbols, name, symbol);
    return 1;
}

void symbol_builtin_introduce(Scope* scope, char* sym, AstNode *node) {
    bh_table_put(AstNode *, scope->symbols, sym, node);
}

AstNode* symbol_resolve(Scope* start_scope, OnyxToken* tkn) {
    token_toggle_end(tkn);

    AstNode* res = NULL;
    Scope* scope = start_scope;

    while (res == NULL && scope != NULL) {
        if (bh_table_has(AstNode *, scope->symbols, tkn->text)) {
            res = bh_table_get(AstNode *, scope->symbols, tkn->text);
        } else {
            scope = scope->parent;
        }
    }

    if (res == NULL) {
        onyx_message_add(Msg_Type_Unknown_Symbol,
                tkn->pos,
                tkn->text);

        token_toggle_end(tkn);
        return NULL;
    }

    if (res->kind == Ast_Kind_Symbol) {
        token_toggle_end(tkn);
        return symbol_resolve(start_scope, res->token);
    }

    token_toggle_end(tkn);
    return res;
}


void onyx_ast_print(AstNode* node, i32 indent) {
    assert(0);
}
#if 0

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

#endif
