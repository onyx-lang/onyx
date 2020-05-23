#include "onyxutils.h"

void onyx_ast_print(OnyxAstNode* node) {
	if (node == NULL) return;

	bh_printf("%s <%d> ", onyx_ast_node_kind_string(node->kind), node->flags);
	if (node->token)
		bh_printf("[%b] ", node->token->token, (int) node->token->length);

	if ((i64) node->left > 10) { // HACK: but okay
		bh_printf("(");
		onyx_ast_print(node->left);
		bh_printf(") ");
	}
	if ((i64) node->right > 10) { // HACK: but okay
		bh_printf("(");
		onyx_ast_print(node->right);
		bh_printf(")");
	}

	if (node->next) {
		bh_printf("{");
		onyx_ast_print(node->next);
		bh_printf("}");
	}
}
