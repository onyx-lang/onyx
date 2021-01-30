#include "bh.h"

#include "onyxastnodes.h"

extern bh_scratch global_scratch;
extern bh_allocator global_scratch_allocator;

extern bh_managed_heap global_heap;
extern bh_allocator global_heap_allocator;

const char* onyx_ast_node_kind_string(AstKind kind);

Package* package_lookup(char* package_name);
Package* package_lookup_or_create(char* package_name, Scope* parent_scope, bh_allocator alloc);

void scope_include(Scope* target, Scope* source, OnyxFilePos pos);
b32 symbol_introduce(Scope* scope, OnyxToken* tkn, AstNode* symbol);
b32 symbol_raw_introduce(Scope* scope, char* tkn, OnyxFilePos pos, AstNode* symbol);
void symbol_builtin_introduce(Scope* scope, char* sym, AstNode *node);
void symbol_subpackage_introduce(Scope* scope, OnyxToken* sym, AstPackage *node);
AstNode* symbol_raw_resolve(Scope* start_scope, char* sym);
AstNode* symbol_resolve(Scope* start_scope, OnyxToken* tkn);

u32 char_to_base16_value(char x);

// Returns the length after processing the string.
i32 string_process_escape_seqs(char* dest, char* src, i32 len);
