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
    "ALIAS",

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
    "ENUM TYPE",
    "TYPE_ALIAS",
    "TYPE_END (BAD)",

    "STRUCT MEMBER",
    "ENUM VALUE",

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
    "ALIGN OF"
    "FILE CONTENTS"

    "IF",
    "FOR",
    "WHILE",
    "BREAK",
    "CONTINUE",
    "DEFER",

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
        package->private_scope = scope_create(alloc, package->scope);

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





void promote_numlit_to_larger(AstNumLit* num) {
    assert(num->type != NULL);

    if (type_is_integer(num->type) && num->type->Basic.size <= 4) {
        // NOTE: Int32, Int16, Int8
        i64 val = (i64) num->value.i;
        num->value.l = val;
    } else if (num->type->Basic.size <= 4) {
        // NOTE: Float32
        f64 val = (f64) num->value.f;
        num->value.d = val;
    }
}
