#define BH_DEBUG
#include "onyxsempass.h"
#include "onyxutils.h"

OnyxSemPassState onyx_sempass_create(bh_allocator alloc, bh_allocator node_alloc, OnyxMessages* msgs) {
    OnyxSemPassState state = {
        .allocator = alloc,
        .node_allocator = node_alloc,

        .msgs = msgs,

        .curr_scope = NULL,
        .symbols = NULL,
    };

    bh_table_init(bh_heap_allocator(), state.symbols, 61);

    return state;
}


// NOTE: If the compiler is expanded to support more targets than just
// WASM, this function may not be needed. It brings all of the locals
// defined in sub-scopes up to the function-block level. This is a
// requirement of WASM, but not of other targets.
static void collapse_scopes(OnyxAstNode* root_node) {
    bh_arr(OnyxAstNodeBlock*) traversal_queue = NULL;
    bh_arr_new(global_scratch_allocator, traversal_queue, 4);
    bh_arr_set_length(traversal_queue, 0);

    OnyxAstNode* walker = root_node;
    while (walker) {
        if (walker->kind == ONYX_AST_NODE_KIND_FUNCDEF) {
            OnyxAstNodeScope* top_scope = walker->as_funcdef.body->scope;

            bh_arr_push(traversal_queue, walker->as_funcdef.body);
            while (!bh_arr_is_empty(traversal_queue)) {
                OnyxAstNodeBlock* block = traversal_queue[0];

                if (block->kind == ONYX_AST_NODE_KIND_IF) {
                    OnyxAstNodeIf* if_node = (OnyxAstNodeIf *) block;
                    if (if_node->true_block)
                        bh_arr_push(traversal_queue, (OnyxAstNodeBlock *) if_node->true_block);

                    if (if_node->false_block)
                        bh_arr_push(traversal_queue, (OnyxAstNodeBlock *) if_node->false_block);

                } else {

                    if (block->scope != top_scope && block->scope->last_local != NULL) {
                        OnyxAstNodeLocal* last_local = block->scope->last_local;
                        while (last_local && last_local->prev_local != NULL) last_local = last_local->prev_local;

                        last_local->prev_local = top_scope->last_local;
                        top_scope->last_local = block->scope->last_local;
                        block->scope->last_local = NULL;
                    }

                    OnyxAstNode* walker = block->body;
                    while (walker) {
                        if (walker->kind == ONYX_AST_NODE_KIND_BLOCK) {
                            bh_arr_push(traversal_queue, (OnyxAstNodeBlock *) walker);

                        } else if (walker->kind == ONYX_AST_NODE_KIND_IF) {
                            if (walker->as_if.true_block)
                                bh_arr_push(traversal_queue, (OnyxAstNodeBlock *) walker->as_if.true_block);

                            if (walker->as_if.false_block)
                                bh_arr_push(traversal_queue, (OnyxAstNodeBlock *) walker->as_if.false_block);
                        }

                        walker = walker->next;
                    }
                }

                bh_arr_deleten(traversal_queue, 0, 1);
            }
        }

        walker = walker->next;
    }
}

void onyx_sempass(OnyxSemPassState* state, OnyxAstNode* root_node) {
    onyx_resolve_symbols(state, root_node);
    onyx_type_check(state, root_node);
    collapse_scopes(root_node);
}
