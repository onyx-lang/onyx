#include "onyxutils.h"
#include "onyxlex.h"
#include "onyxparser.h"

#define print_indent { if (indent > 0) bh_printf("\n"); for (int i = 0; i < indent; i++) bh_printf("  "); }

void onyx_ast_print(OnyxAstNode* node, i32 indent) {
	if (node == NULL) return;

	print_indent;
	bh_printf("(%d) %s ", node->flags, onyx_ast_node_kind_string(node->kind));

	switch (node->kind) {
	case ONYX_AST_NODE_KIND_PROGRAM: {
		if (node->next)
			onyx_ast_print(node->next, indent + 1);

		break;
	}

	case ONYX_AST_NODE_KIND_FUNCDEF: {
		bh_printf("(%b) ", node->token->token, node->token->length);
		OnyxAstNodeFuncDef* fd = &node->as_funcdef;

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
		}
		break;
	}

	case ONYX_AST_NODE_KIND_RETURN: {
		if (node->next) {
			onyx_ast_print(node->next, indent + 1);
		}

		break;
	}

	case ONYX_AST_NODE_KIND_LITERAL: {
		bh_printf("%b", node->token->token, node->token->length);
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
