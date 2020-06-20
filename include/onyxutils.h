#include "bh.h"

#include "onyxparser.h"

extern bh_scratch global_scratch;
extern bh_allocator global_scratch_allocator;

void onyx_ast_print(OnyxAstNode* program, i32 indent);
