#include "bh.h"

#include "onyxastnodes.h"

extern bh_scratch global_scratch;
extern bh_allocator global_scratch_allocator;

extern bh_managed_heap global_heap;
extern bh_allocator global_heap_allocator;

const char* onyx_ast_node_kind_string(AstKind kind);

Package* package_lookup(char* package_name);
Package* package_lookup_or_create(char* package_name, Scope* parent_scope, bh_allocator alloc);
void package_track_use_package(Package* package, Entity* entity);
void package_reinsert_use_packages(Package* package);

Scope* scope_create(bh_allocator a, Scope* parent, OnyxFilePos created_at);
void scope_include(Scope* target, Scope* source, OnyxFilePos pos);
b32 symbol_introduce(Scope* scope, OnyxToken* tkn, AstNode* symbol);
b32 symbol_raw_introduce(Scope* scope, char* tkn, OnyxFilePos pos, AstNode* symbol);
void symbol_builtin_introduce(Scope* scope, char* sym, AstNode *node);
void symbol_subpackage_introduce(Scope* scope, char* sym, AstPackage *node);
AstNode* symbol_raw_resolve(Scope* start_scope, char* sym);
AstNode* symbol_resolve(Scope* start_scope, OnyxToken* tkn);
AstNode* try_symbol_raw_resolve_from_node(AstNode* node, char* symbol);
AstNode* try_symbol_resolve_from_node(AstNode* node, OnyxToken* token);

void build_all_overload_options(bh_arr(AstTyped *) overloads, bh_imap* all_overloads);

u32 char_to_base16_value(char x);

// Returns the length after processing the string.
i32 string_process_escape_seqs(char* dest, char* src, i32 len);

// NOTE: This should not be called until immediately before using the return value.
// This function can return a static variable which will change if this is called
// another time.                                        -brendanfh 2020/10/09
// :RelativeFiles This should lookup for the file relative to "relative_to"
char* lookup_included_file(char* filename, char* relative_to, b32 add_onyx_suffix, b32 search_included_folders);

extern AstNode node_that_signals_a_yield;