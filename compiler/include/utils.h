#include "bh.h"

#include "astnodes.h"

const char* onyx_ast_node_kind_string(AstKind kind);

Package* package_lookup(Context *context, char* package_name);
Package* package_lookup_or_create(Context *context, char* package_name, Scope* parent_scope, OnyxFilePos pos);
void package_track_use_package(Context *context, Package* package, Entity* entity);
void package_reinsert_use_packages(Context *context, Package* package);
void package_mark_as_used(Context *context, Package* package);

Scope* scope_create(Context *context, Scope* parent, OnyxFilePos created_at);
void scope_include(Context *context, Scope* target, Scope* source, OnyxFilePos pos);
b32 symbol_introduce(Context *context, Scope* scope, OnyxToken* tkn, AstNode* symbol);
b32 symbol_raw_introduce(Context *context, Scope* scope, char* tkn, OnyxFilePos pos, AstNode* symbol);
void symbol_builtin_introduce(Context *context, Scope* scope, char* sym, AstNode *node);
void symbol_subpackage_introduce(Context *context, Package *parent, char* sym, AstPackage *node);
AstNode* symbol_raw_resolve_no_ascend(Context *context, Scope* scope, char* sym);
AstNode* symbol_raw_resolve(Context *context, Scope* start_scope, char* sym);
AstNode* symbol_resolve(Context *context, Scope* start_scope, OnyxToken* tkn);
AstNode* try_symbol_raw_resolve_from_node(Context *context, AstNode* node, char* symbol);
AstNode* try_symbol_resolve_from_node(Context *context, AstNode* node, OnyxToken* token);
AstNode* try_symbol_raw_resolve_from_type(Context *context, Type *type, char* symbol);
AstNode* try_symbol_resolve_from_type(Context *context, Type *type, OnyxToken *token);
Scope *get_scope_from_node(Context *context, AstNode *node);
Scope *get_scope_from_node_or_create(Context *context, AstNode *node);

void build_all_overload_options(bh_arr(OverloadOption) overloads, bh_imap* all_overloads);

u32 char_to_base16_value(char x);

// Returns the length after processing the string.
i32 string_process_escape_seqs(char* dest, char* src, i32 len);

u32 levenshtein_distance(Context *context, const char *str1, const char *str2);
char *find_closest_symbol_in_scope_and_parents(Context *context, Scope *scope, char *sym);
char *find_closest_symbol_in_node(Context *context, AstNode *node, char *sym);
