#include "bh.h"

#include "onyxastnodes.h"

extern bh_scratch global_scratch;
extern bh_allocator global_scratch_allocator;

extern bh_managed_heap global_heap;
extern bh_allocator global_heap_allocator;

const char* onyx_ast_node_kind_string(AstNodeKind kind);

void onyx_ast_print(AstNode* program, i32 indent);
