#include "bh.h"

#include "onyxastnodes.h"

extern bh_scratch global_scratch;
extern bh_allocator global_scratch_allocator;

extern bh_managed_heap global_heap;
extern bh_allocator global_heap_allocator;

const char* onyx_ast_node_kind_string(AstKind kind);

void program_info_init(ProgramInfo* prog, bh_allocator alloc);
Package* program_info_package_lookup(ProgramInfo* prog, char* package_name);
Package* program_info_package_lookup_or_create(ProgramInfo* prog, char* package_name, Scope* parent_scope, bh_allocator alloc);

void scope_include(Scope* target, Scope* source);
b32 symbol_introduce(Scope* scope, OnyxToken* tkn, AstNode* symbol);
b32 symbol_raw_introduce(Scope* scope, char* tkn, OnyxFilePos pos, AstNode* symbol);
void symbol_builtin_introduce(Scope* scope, char* sym, AstNode *node);
AstNode* symbol_raw_resolve(Scope* start_scope, char* sym);
AstNode* symbol_resolve(Scope* start_scope, OnyxToken* tkn);

void onyx_ast_print(AstNode* program, i32 indent);
