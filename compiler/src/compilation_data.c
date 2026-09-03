#include "astnodes.h"
#include "pb.h"
#include "utils.h"

#include <stddef.h>
#include <string.h>

enum {
    _compilation_symbol_id_same_offset = sizeof(char[
        (offsetof(AstNode, compilation_symbol_id) == offsetof(AstType, compilation_symbol_id)) ? 1 : -1
    ]),
    // parse_top_level_binding writes name through AstStructType * for several kinds.
    _named_type_name_offset = sizeof(char[
        (offsetof(AstInterface, name) == offsetof(AstStructType, name)
         && offsetof(AstEnumType, name) == offsetof(AstStructType, name)
         && offsetof(AstDistinctType, name) == offsetof(AstStructType, name)
         && offsetof(AstUnionType, name) == offsetof(AstStructType, name)
         && offsetof(AstPolyStructType, name) == offsetof(AstStructType, name)
         && offsetof(AstPolyUnionType, name) == offsetof(AstStructType, name)) ? 1 : -1
    ])
};

#define SYMBOL_KIND_UNKNOWN             0
#define SYMBOL_KIND_PACKAGE             1
#define SYMBOL_KIND_FUNCTION            2
#define SYMBOL_KIND_OVERLOADED_FUNCTION 3
#define SYMBOL_KIND_POLYMORPHIC_PROC    4
#define SYMBOL_KIND_MACRO               5
#define SYMBOL_KIND_STRUCT              6
#define SYMBOL_KIND_UNION               7
#define SYMBOL_KIND_ENUM                8
#define SYMBOL_KIND_DISTINCT            9
#define SYMBOL_KIND_INTERFACE           10
#define SYMBOL_KIND_EXPRESSION          11
#define SYMBOL_KIND_MEMBER              12
#define SYMBOL_KIND_PARAMETER           13

#define PROTO_TYPE_KIND_UNKNOWN         0
#define PROTO_TYPE_KIND_BASIC           1
#define PROTO_TYPE_KIND_POINTER         2
#define PROTO_TYPE_KIND_MULTI_POINTER   3
#define PROTO_TYPE_KIND_FUNCTION        4
#define PROTO_TYPE_KIND_STRUCT          5
#define PROTO_TYPE_KIND_POLY_STRUCT     6
#define PROTO_TYPE_KIND_COMPOUND        7
#define PROTO_TYPE_KIND_ARRAY           8
#define PROTO_TYPE_KIND_SLICE           9
#define PROTO_TYPE_KIND_DYN_ARRAY       10
#define PROTO_TYPE_KIND_VAR_ARGS        11
#define PROTO_TYPE_KIND_ENUM            12
#define PROTO_TYPE_KIND_DISTINCT        13
#define PROTO_TYPE_KIND_UNION           14
#define PROTO_TYPE_KIND_POLY_UNION      15

#define PROTO_BASIC_KIND_UNKNOWN        0
#define PROTO_BASIC_KIND_VOID           1
#define PROTO_BASIC_KIND_BOOL           2
#define PROTO_BASIC_KIND_INT_UNSIZED    3
#define PROTO_BASIC_KIND_I8             4
#define PROTO_BASIC_KIND_U8             5
#define PROTO_BASIC_KIND_I16            6
#define PROTO_BASIC_KIND_U16            7
#define PROTO_BASIC_KIND_I32            8
#define PROTO_BASIC_KIND_U32            9
#define PROTO_BASIC_KIND_I64            10
#define PROTO_BASIC_KIND_U64            11
#define PROTO_BASIC_KIND_FLOAT_UNSIZED  12
#define PROTO_BASIC_KIND_F32            13
#define PROTO_BASIC_KIND_F64            14
#define PROTO_BASIC_KIND_RAWPTR         15
#define PROTO_BASIC_KIND_I8X16          16
#define PROTO_BASIC_KIND_I16X8          17
#define PROTO_BASIC_KIND_I32X4          18
#define PROTO_BASIC_KIND_I64X2          19
#define PROTO_BASIC_KIND_F32X4          20
#define PROTO_BASIC_KIND_F64X2          21
#define PROTO_BASIC_KIND_V128           22
#define PROTO_BASIC_KIND_TYPE_EXPR      23

// Field numbers from core/onyx/toolchain/compiler_events.proto
#define PB_COMPILER_EVENTS_EVENTS 1

#define PB_COMPILER_EVENT_TIMESTAMP 1
#define PB_COMPILER_EVENT_START     10
#define PB_COMPILER_EVENT_FILE      11
#define PB_COMPILER_EVENT_PACK      12
#define PB_COMPILER_EVENT_SCOPE     13
#define PB_COMPILER_EVENT_SYMBOL    14
#define PB_COMPILER_EVENT_RESOLVED  15
#define PB_COMPILER_EVENT_BINDING   16
#define PB_COMPILER_EVENT_TYPE      17
#define PB_COMPILER_EVENT_TYPE_BODY 18
#define PB_COMPILER_EVENT_OVERLOAD  19
#define PB_COMPILER_EVENT_POLY_PROC 20

#define PB_EVENT_START_COMPILER_VERSION 1

#define PB_EVENT_FILE_ID   1
#define PB_EVENT_FILE_PATH 2

#define PB_EVENT_PACKAGE_ID                    1
#define PB_EVENT_PACKAGE_NAME                  2
#define PB_EVENT_PACKAGE_FULLY_QUALIFIED_NAME  3
#define PB_EVENT_PACKAGE_PARENT_ID             4
#define PB_EVENT_PACKAGE_SCOPE_ID              5

#define PB_EVENT_SCOPE_ID               1
#define PB_EVENT_SCOPE_PARENT_SCOPE_ID  2
#define PB_EVENT_SCOPE_FILE_ID          3
#define PB_EVENT_SCOPE_LINE             4
#define PB_EVENT_SCOPE_COLUMN           5
#define PB_EVENT_SCOPE_NAME             6
#define PB_EVENT_SCOPE_END_LINE         7
#define PB_EVENT_SCOPE_END_COLUMN       8

#define PB_EVENT_SYMBOL_ID        1
#define PB_EVENT_SYMBOL_SCOPE_ID  2
#define PB_EVENT_SYMBOL_NAME      3
#define PB_EVENT_SYMBOL_FILE_ID   4
#define PB_EVENT_SYMBOL_LINE      5
#define PB_EVENT_SYMBOL_COLUMN    6
#define PB_EVENT_SYMBOL_KIND      7
#define PB_EVENT_SYMBOL_PACKAGE_ID 8

#define PB_EVENT_RESOLVED_SYMBOL_ID        1
#define PB_EVENT_RESOLVED_FILE_ID          2
#define PB_EVENT_RESOLVED_LINE             3
#define PB_EVENT_RESOLVED_COLUMN           4
#define PB_EVENT_RESOLVED_LENGTH           5
#define PB_EVENT_RESOLVED_CHOSEN_SYMBOL_ID 6

#define PB_EVENT_OVERLOAD_GROUP_SYMBOL_ID   1
#define PB_EVENT_OVERLOAD_OPTION_SYMBOL_IDS 2

#define PB_EVENT_POLY_PROC_TEMPLATE_SYMBOL_ID 1
#define PB_EVENT_POLY_PROC_INSTANCE_SYMBOL_ID 2
#define PB_EVENT_POLY_PROC_SOLUTION_NAMES     3
#define PB_EVENT_POLY_PROC_SOLUTION_TYPE_IDS  4

#define PB_EVENT_BINDING_SYMBOL_ID        1
#define PB_EVENT_BINDING_DOCUMENTATION    2
#define PB_EVENT_BINDING_TYPE_ID          3
#define PB_EVENT_BINDING_PARAM_SYMBOL_IDS 4

#define PB_EVENT_TYPE_ID                    1
#define PB_EVENT_TYPE_KIND                  2
#define PB_EVENT_TYPE_NAME                  3
#define PB_EVENT_TYPE_CONSTRUCTED_FROM_ID   4
#define PB_EVENT_TYPE_POLY_ARG_NAMES        5
#define PB_EVENT_TYPE_POLY_ARG_TYPE_IDS     6
#define PB_EVENT_TYPE_BASIC        10
#define PB_EVENT_TYPE_ELEM         11
#define PB_EVENT_TYPE_ARRAY        12
#define PB_EVENT_TYPE_FUNCTION     13
#define PB_EVENT_TYPE_COMPOUND     14
#define PB_EVENT_TYPE_DISTINCT     15
#define PB_EVENT_TYPE_ENUMERATION  16

#define PB_TYPE_BASIC_KIND       1
#define PB_TYPE_BASIC_SIZE       2
#define PB_TYPE_BASIC_ALIGNMENT  3

#define PB_TYPE_ELEM_ELEM_ID 1

#define PB_TYPE_ARRAY_ELEM_ID 1
#define PB_TYPE_ARRAY_COUNT   2

#define PB_TYPE_FUNCTION_RETURN_ID 1
#define PB_TYPE_FUNCTION_PARAM_IDS 2

#define PB_TYPE_COMPOUND_TYPE_IDS 1

#define PB_TYPE_DISTINCT_BASE_ID 1

#define PB_TYPE_ENUM_BACKING_ID 1
#define PB_TYPE_ENUM_IS_FLAGS   2
#define PB_TYPE_ENUM_VALUES     3
#define PB_TYPE_ENUM_VALUE_NAME      1
#define PB_TYPE_ENUM_VALUE_VALUE     2
#define PB_TYPE_ENUM_VALUE_SYMBOL_ID 3

#define PB_EVENT_TYPE_BODY_TYPE_ID    1
#define PB_EVENT_TYPE_BODY_SIZE       2
#define PB_EVENT_TYPE_BODY_ALIGNMENT  3
#define PB_EVENT_TYPE_BODY_SCOPE_ID   4
#define PB_EVENT_TYPE_BODY_STRUCT     10
#define PB_EVENT_TYPE_BODY_UNION      11

#define PB_TYPE_STRUCT_BODY_MEMBERS 1
#define PB_TYPE_STRUCT_MEMBER_NAME                  1
#define PB_TYPE_STRUCT_MEMBER_TYPE_ID               2
#define PB_TYPE_STRUCT_MEMBER_OFFSET                3
#define PB_TYPE_STRUCT_MEMBER_IDX                   4
#define PB_TYPE_STRUCT_MEMBER_SYMBOL_ID             5
#define PB_TYPE_STRUCT_MEMBER_INCLUDED_THROUGH_USE  6
#define PB_TYPE_STRUCT_MEMBER_IS_USED               7

#define PB_TYPE_UNION_BODY_TAG_TYPE_ID 1
#define PB_TYPE_UNION_BODY_VARIANTS    2
#define PB_TYPE_UNION_VARIANT_NAME      1
#define PB_TYPE_UNION_VARIANT_TYPE_ID   2
#define PB_TYPE_UNION_VARIANT_TAG       3
#define PB_TYPE_UNION_VARIANT_SYMBOL_ID 4

static char *compilation_log_intern(Context *context, const char *name) {
    if (!name || !*name) return (char *) "";

    CompilationLog *log = &context->compilation_log;
    if (!log->interned_names) sh_new_arena(log->interned_names);

    i32 idx = shputi(log->interned_names, (char *) name, 1);
    return log->interned_names[idx].key;
}

static char *compilation_log_intern_len(Context *context, const char *s, i32 len) {
    if (!s || len <= 0) return (char *) "";

    char stack[256];
    if (len < (i32) sizeof(stack)) {
        memcpy(stack, s, len);
        stack[len] = 0;
        return compilation_log_intern(context, stack);
    }

    char *tmp = bh_alloc_array(context->scratch_alloc, char, len + 1);
    memcpy(tmp, s, len);
    tmp[len] = 0;
    return compilation_log_intern(context, tmp);
}

static char *compilation_log_get_symbol_name(CompilationLog *log, u32 id) {
    if (!id || id >= (u32) bh_arr_length(log->symbol_names)) return NULL;
    return log->symbol_names[id];
}

static b32 compilation_log_binding_type_get(CompilationLog *log, u32 id, u32 *out) {
    if (!id || id >= (u32) bh_arr_length(log->symbol_binding_type_id)) return 0;
    u32 stored = log->symbol_binding_type_id[id];
    if (!stored) return 0;
    if (out) *out = stored - 1;
    return 1;
}

static void compilation_log_u32s_set(Context *context, bh_arr(u32) *arr, u32 index, u32 value) {
    i32 n = (i32) index;
    if (n + 1 > bh_arr_capacity(*arr)) {
        bh_arr_grow(*arr, n + 1);
    }
    i32 len = bh_arr_length(*arr);
    if (n + 1 > len) {
        memset(*arr + len, 0, (n + 1 - len) * sizeof(u32));
        bh_arr_set_length(*arr, n + 1);
    }
    (*arr)[n] = value;
}

static void compilation_log_names_set(Context *context, bh_arr(char *) *arr, u32 index, char *value) {
    i32 n = (i32) index;
    if (n + 1 > bh_arr_capacity(*arr)) {
        bh_arr_grow(*arr, n + 1);
    }
    i32 len = bh_arr_length(*arr);
    if (n + 1 > len) {
        memset(*arr + len, 0, (n + 1 - len) * sizeof(char *));
        bh_arr_set_length(*arr, n + 1);
    }
    (*arr)[n] = value;
}

static void compilation_log_binding_type_put(Context *context, CompilationLog *log, u32 id, u32 type_id) {
    compilation_log_u32s_set(context, &log->symbol_binding_type_id, id, type_id + 1);
}

static char *compilation_log_type_name(Context *context, Type *type) {
    if (!type) return (char *) "";

    const char *stable = NULL;
    switch (type->kind) {
        case Type_Kind_Basic:      stable = type->Basic.name; break;
        case Type_Kind_PolyStruct: stable = type->PolyStruct.name; break;
        case Type_Kind_PolyUnion:  stable = type->PolyUnion.name; break;
        case Type_Kind_Struct:     stable = type->Struct.name ? type->Struct.name : "<anonymous struct>"; break;
        case Type_Kind_Enum:       stable = type->Enum.name ? type->Enum.name : "<anonymous enum>"; break;
        case Type_Kind_Union:      stable = type->Union.name ? type->Union.name : "<anonymous union>"; break;
        case Type_Kind_Distinct:   stable = type->Distinct.name; break;
        default: break;
    }

    if (stable) return (char *) stable;

    const char *name = type_get_name(context, type);
    return compilation_log_intern(context, name ? name : "");
}

static CompilationEvent *compilation_log_new_event(Context *context, CompilationEventKind kind) {
    CompilationLog *log = &context->compilation_log;
    i32 i = bh_arr_length(log->events);
    if (i >= bh_arr_capacity(log->events)) {
        bh_arr_grow(log->events, i + 1);
    }
    bh_arr_set_length(log->events, i + 1);
    CompilationEvent *ev = &log->events[i];
    ev->kind = kind;
    log->encoded_valid = 0;
    return ev;
}

static u32 compilation_log_type_kind(TypeKind kind);
static u32 compilation_log_basic_kind(enum BasicKind kind);
static b32 compilation_log_lookup_symbol_id(CompilationLog *log, AstNode *node, u32 *out);
static char *compilation_log_binding_docs(Context *context, CompilationLog *log, AstNode *node);

static void encode_type_poly_construction(Context *context, Type *type, bh_buffer *inner) {
    AstType *from = NULL;
    bh_arr(AstPolySolution) slns = NULL;

    if (type->kind == Type_Kind_Struct) {
        from = type->Struct.constructed_from;
        slns = type->Struct.poly_sln;
    } else if (type->kind == Type_Kind_Union) {
        from = type->Union.constructed_from;
        slns = type->Union.poly_sln;
    }

    if (from && from->type_id) {
        pb_write_uint32(inner, PB_EVENT_TYPE_CONSTRUCTED_FROM_ID, from->type_id);
    }

    u32 n = (u32) bh_arr_length(slns);
    for (u32 i = 0; i < n; i++) {
        AstPolySolution *sln = &slns[i];
        char *sname = (char *) "";
        if (sln->poly_sym && sln->poly_sym->token && sln->poly_sym->token->length > 0) {
            sname = compilation_log_intern_len(
                context, sln->poly_sym->token->text, sln->poly_sym->token->length
            );
        }
        pb_write_string(inner, PB_EVENT_TYPE_POLY_ARG_NAMES, sname);

        u32 tid = 0;
        if (sln->kind == PSK_Type && sln->type) {
            tid = sln->type->id;
        } else if (sln->kind == PSK_Value && sln->value && sln->value->type) {
            tid = sln->value->type->id;
        }
        pb_write_uint32_always(inner, PB_EVENT_TYPE_POLY_ARG_TYPE_IDS, tid);
    }
}

static void encode_type_payload(Context *context, Type *type) {
    CompilationLog *log = &context->compilation_log;
    bh_buffer *inner = &log->pb_field;
    bh_buffer *sub = &log->pb_sub;
    bh_buffer *item = &log->pb_item;

    pb_write_uint32(inner, PB_EVENT_TYPE_ID, type->id);
    pb_write_uint32(inner, PB_EVENT_TYPE_KIND, compilation_log_type_kind(type->kind));
    pb_write_string(inner, PB_EVENT_TYPE_NAME, compilation_log_type_name(context, type));
    encode_type_poly_construction(context, type, inner);

    bh_buffer_clear(sub);

    switch (type->kind) {
        case Type_Kind_Basic:
            pb_write_uint32(sub, PB_TYPE_BASIC_KIND, compilation_log_basic_kind(type->Basic.kind));
            pb_write_uint32(sub, PB_TYPE_BASIC_SIZE, type->Basic.size);
            pb_write_uint32(sub, PB_TYPE_BASIC_ALIGNMENT, type->Basic.alignment);
            pb_write_bytes(inner, PB_EVENT_TYPE_BASIC, sub->data, sub->length);
            break;

        case Type_Kind_Pointer:
            pb_write_uint32(sub, PB_TYPE_ELEM_ELEM_ID, type->Pointer.elem ? type->Pointer.elem->id : 0);
            pb_write_bytes(inner, PB_EVENT_TYPE_ELEM, sub->data, sub->length);
            break;

        case Type_Kind_MultiPointer:
            pb_write_uint32(sub, PB_TYPE_ELEM_ELEM_ID, type->MultiPointer.elem ? type->MultiPointer.elem->id : 0);
            pb_write_bytes(inner, PB_EVENT_TYPE_ELEM, sub->data, sub->length);
            break;

        case Type_Kind_Slice:
            pb_write_uint32(sub, PB_TYPE_ELEM_ELEM_ID, type->Slice.elem ? type->Slice.elem->id : 0);
            pb_write_bytes(inner, PB_EVENT_TYPE_ELEM, sub->data, sub->length);
            break;

        case Type_Kind_DynArray:
            pb_write_uint32(sub, PB_TYPE_ELEM_ELEM_ID, type->DynArray.elem ? type->DynArray.elem->id : 0);
            pb_write_bytes(inner, PB_EVENT_TYPE_ELEM, sub->data, sub->length);
            break;

        case Type_Kind_VarArgs:
            pb_write_uint32(sub, PB_TYPE_ELEM_ELEM_ID, type->VarArgs.elem ? type->VarArgs.elem->id : 0);
            pb_write_bytes(inner, PB_EVENT_TYPE_ELEM, sub->data, sub->length);
            break;

        case Type_Kind_Array:
            pb_write_uint32(sub, PB_TYPE_ARRAY_ELEM_ID, type->Array.elem ? type->Array.elem->id : 0);
            pb_write_uint32(sub, PB_TYPE_ARRAY_COUNT, type->Array.count);
            pb_write_bytes(inner, PB_EVENT_TYPE_ARRAY, sub->data, sub->length);
            break;

        case Type_Kind_Function: {
            u32 n = type->Function.param_count;
            pb_write_uint32(sub, PB_TYPE_FUNCTION_RETURN_ID, type->Function.return_type ? type->Function.return_type->id : 0);
            for (u32 i = 0; i < n; i++) {
                pb_write_uint32_always(sub, PB_TYPE_FUNCTION_PARAM_IDS, type->Function.params[i] ? type->Function.params[i]->id : 0);
            }
            pb_write_bytes(inner, PB_EVENT_TYPE_FUNCTION, sub->data, sub->length);
            break;
        }

        case Type_Kind_Compound: {
            u32 n = type->Compound.count;
            for (u32 i = 0; i < n; i++) {
                pb_write_uint32_always(sub, PB_TYPE_COMPOUND_TYPE_IDS, type->Compound.types[i] ? type->Compound.types[i]->id : 0);
            }
            pb_write_bytes(inner, PB_EVENT_TYPE_COMPOUND, sub->data, sub->length);
            break;
        }

        case Type_Kind_Distinct:
            pb_write_uint32(sub, PB_TYPE_DISTINCT_BASE_ID, type->Distinct.base_type ? type->Distinct.base_type->id : 0);
            pb_write_bytes(inner, PB_EVENT_TYPE_DISTINCT, sub->data, sub->length);
            break;

        case Type_Kind_Enum:
            pb_write_uint32(sub, PB_TYPE_ENUM_BACKING_ID, type->Enum.backing ? type->Enum.backing->id : 0);
            if (type->Enum.is_flags) {
                pb_write_uint32_always(sub, PB_TYPE_ENUM_IS_FLAGS, 1);
            }
            if (type->ast_type && type->ast_type->kind == Ast_Kind_Enum_Type) {
                AstEnumType *en = (AstEnumType *) type->ast_type;
                bh_arr_each(AstEnumValue *, pvalue, en->values) {
                    AstEnumValue *value = *pvalue;
                    if (!(value->flags & Ast_Flag_Has_Been_Checked)) continue;

                    bh_buffer_clear(item);
                    char *name = value->token
                        ? compilation_log_intern_len(context, value->token->text, value->token->length)
                        : (char *) "";
                    u32 sid = 0;
                    compilation_log_lookup_symbol_id(log, (AstNode *) value, &sid);
                    pb_write_string(item, PB_TYPE_ENUM_VALUE_NAME, name);
                    pb_write_int64_always(item, PB_TYPE_ENUM_VALUE_VALUE, value->value ? get_expression_integer_value(context, value->value, NULL) : 0);
                    pb_write_uint32(item, PB_TYPE_ENUM_VALUE_SYMBOL_ID, sid);
                    pb_write_bytes(sub, PB_TYPE_ENUM_VALUES, item->data, item->length);
                }
            }
            pb_write_bytes(inner, PB_EVENT_TYPE_ENUMERATION, sub->data, sub->length);
            break;

        default:
            break;
    }
}

static Scope *compilation_log_type_method_scope(Type *type);

static void encode_type_body_payload(Context *context, Type *type) {
    CompilationLog *log = &context->compilation_log;
    bh_buffer *inner = &log->pb_field;
    bh_buffer *sub = &log->pb_sub;
    bh_buffer *item = &log->pb_item;

    pb_write_uint32(inner, PB_EVENT_TYPE_BODY_TYPE_ID, type->id);

    if (type->kind == Type_Kind_Union) {
        pb_write_uint32(inner, PB_EVENT_TYPE_BODY_SIZE, type->Union.size);
        pb_write_uint32(inner, PB_EVENT_TYPE_BODY_ALIGNMENT, type->Union.alignment);
        pb_write_uint32(inner, PB_EVENT_TYPE_BODY_SCOPE_ID, type->Union.scope ? (u32) type->Union.scope->id : 0);

        bh_buffer_clear(sub);
        pb_write_uint32(sub, PB_TYPE_UNION_BODY_TAG_TYPE_ID, type->Union.tag_type ? type->Union.tag_type->id : 0);
        u32 n = (u32) bh_arr_length(type->Union.variants_ordered);
        for (u32 i = 0; i < n; i++) {
            UnionVariant *uv = type->Union.variants_ordered[i];
            u32 sid = 0;
            if (uv && uv->variant_node) {
                compilation_log_lookup_symbol_id(log, (AstNode *) uv->variant_node, &sid);
            }
            bh_buffer_clear(item);
            pb_write_string(item, PB_TYPE_UNION_VARIANT_NAME, uv && uv->name ? uv->name : "");
            pb_write_uint32(item, PB_TYPE_UNION_VARIANT_TYPE_ID, uv && uv->type ? uv->type->id : 0);
            pb_write_uint32_always(item, PB_TYPE_UNION_VARIANT_TAG, uv ? uv->tag_value : 0);
            pb_write_uint32(item, PB_TYPE_UNION_VARIANT_SYMBOL_ID, sid);
            pb_write_bytes(sub, PB_TYPE_UNION_BODY_VARIANTS, item->data, item->length);
        }
        pb_write_bytes(inner, PB_EVENT_TYPE_BODY_UNION, sub->data, sub->length);
        return;
    }

    if (type->kind == Type_Kind_Struct) {
        pb_write_uint32(inner, PB_EVENT_TYPE_BODY_SIZE, type->Struct.size);
        pb_write_uint32(inner, PB_EVENT_TYPE_BODY_ALIGNMENT, type->Struct.alignment);
        pb_write_uint32(inner, PB_EVENT_TYPE_BODY_SCOPE_ID, type->Struct.scope ? (u32) type->Struct.scope->id : 0);

        bh_buffer_clear(sub);
        u32 n = (u32) bh_arr_length(type->Struct.memarr);
        for (u32 i = 0; i < n; i++) {
            StructMember *smem = type->Struct.memarr[i];
            u32 sid = 0;
            if (smem && smem->member_node) {
                compilation_log_lookup_symbol_id(log, (AstNode *) smem->member_node, &sid);
            }
            bh_buffer_clear(item);
            pb_write_string(item, PB_TYPE_STRUCT_MEMBER_NAME, smem && smem->name ? smem->name : "");
            pb_write_uint32(item, PB_TYPE_STRUCT_MEMBER_TYPE_ID, smem && smem->type ? smem->type->id : 0);
            pb_write_uint32_always(item, PB_TYPE_STRUCT_MEMBER_OFFSET, smem ? smem->offset : 0);
            pb_write_uint32_always(item, PB_TYPE_STRUCT_MEMBER_IDX, smem ? smem->idx : 0);
            pb_write_uint32(item, PB_TYPE_STRUCT_MEMBER_SYMBOL_ID, sid);
            if (smem && smem->included_through_use) {
                pb_write_uint32_always(item, PB_TYPE_STRUCT_MEMBER_INCLUDED_THROUGH_USE, 1);
            }
            if (smem && smem->used) {
                pb_write_uint32_always(item, PB_TYPE_STRUCT_MEMBER_IS_USED, 1);
            }
            pb_write_bytes(sub, PB_TYPE_STRUCT_BODY_MEMBERS, item->data, item->length);
        }

        // Members flattened in by `use` live only in the lookup table, not in
        // memarr, because they are not part of the struct's own layout. Report
        // them so an extension can see the same names field access resolves.
        fori (i, 0, shlen(type->Struct.members)) {
            StructMember *smem = type->Struct.members[i].value;
            if (!smem || !smem->included_through_use) continue;

            u32 sid = 0;
            if (smem->member_node) {
                compilation_log_lookup_symbol_id(log, (AstNode *) smem->member_node, &sid);
            }
            bh_buffer_clear(item);
            pb_write_string(item, PB_TYPE_STRUCT_MEMBER_NAME, smem->name ? smem->name : "");
            pb_write_uint32(item, PB_TYPE_STRUCT_MEMBER_TYPE_ID, smem->type ? smem->type->id : 0);
            pb_write_uint32_always(item, PB_TYPE_STRUCT_MEMBER_OFFSET, smem->offset);
            pb_write_uint32_always(item, PB_TYPE_STRUCT_MEMBER_IDX, smem->idx);
            pb_write_uint32(item, PB_TYPE_STRUCT_MEMBER_SYMBOL_ID, sid);
            pb_write_uint32_always(item, PB_TYPE_STRUCT_MEMBER_INCLUDED_THROUGH_USE, 1);
            if (smem->used) {
                pb_write_uint32_always(item, PB_TYPE_STRUCT_MEMBER_IS_USED, 1);
            }
            pb_write_bytes(sub, PB_TYPE_STRUCT_BODY_MEMBERS, item->data, item->length);
        }

        pb_write_bytes(inner, PB_EVENT_TYPE_BODY_STRUCT, sub->data, sub->length);
        return;
    }

    Scope *scope = compilation_log_type_method_scope(type);
    pb_write_uint32(inner, PB_EVENT_TYPE_BODY_SIZE, type_size_of(type));
    pb_write_uint32(inner, PB_EVENT_TYPE_BODY_ALIGNMENT, type_alignment_of(type));
    pb_write_uint32(inner, PB_EVENT_TYPE_BODY_SCOPE_ID, scope ? (u32) scope->id : 0);
    bh_buffer_clear(sub);
    pb_write_bytes(inner, PB_EVENT_TYPE_BODY_STRUCT, sub->data, sub->length);
}

static void encode_event(Context *context, CompilationEvent *ev) {
    CompilationLog *log = &context->compilation_log;
    bh_buffer *out = &log->pb_event;
    bh_buffer *inner = &log->pb_field;

    bh_buffer_clear(out);
    bh_buffer_clear(inner);

    switch (ev->kind) {
        case COMPILATION_EVENT_START:
            pb_write_string(inner, PB_EVENT_START_COMPILER_VERSION, ev->ptr ? (char *) ev->ptr : "");
            pb_write_bytes(out, PB_COMPILER_EVENT_START, inner->data, inner->length);
            break;

        case COMPILATION_EVENT_FILE:
            pb_write_uint32(inner, PB_EVENT_FILE_ID, ev->a);
            pb_write_string(inner, PB_EVENT_FILE_PATH, ev->ptr ? (char *) ev->ptr : "");
            pb_write_bytes(out, PB_COMPILER_EVENT_FILE, inner->data, inner->length);
            break;

        case COMPILATION_EVENT_PACKAGE: {
            Package *package = (Package *) ev->ptr;
            pb_write_uint32(inner, PB_EVENT_PACKAGE_ID, package ? package->id : 0);
            pb_write_string(inner, PB_EVENT_PACKAGE_NAME, package && package->unqualified_name ? package->unqualified_name : "");
            pb_write_string(inner, PB_EVENT_PACKAGE_FULLY_QUALIFIED_NAME, package && package->name ? package->name : "");
            pb_write_uint32(inner, PB_EVENT_PACKAGE_PARENT_ID, package && package->parent_id >= 0 ? (u32) package->parent_id : 0);
            pb_write_uint32(inner, PB_EVENT_PACKAGE_SCOPE_ID, package && package->scope ? (u32) package->scope->id : 0);
            pb_write_bytes(out, PB_COMPILER_EVENT_PACK, inner->data, inner->length);
            break;
        }

        case COMPILATION_EVENT_SCOPE: {
            Scope *scope = (Scope *) ev->ptr;
            pb_write_uint32(inner, PB_EVENT_SCOPE_ID, scope ? (u32) scope->id : 0);
            pb_write_uint32(inner, PB_EVENT_SCOPE_PARENT_SCOPE_ID, scope && scope->parent ? (u32) scope->parent->id : 0);
            pb_write_uint32(inner, PB_EVENT_SCOPE_FILE_ID, ev->a);
            pb_write_uint32(inner, PB_EVENT_SCOPE_LINE, scope ? scope->created_at.line : 0);
            pb_write_uint32(inner, PB_EVENT_SCOPE_COLUMN, scope ? scope->created_at.column : 0);
            pb_write_string(inner, PB_EVENT_SCOPE_NAME, scope && scope->name ? scope->name : "");
            pb_write_uint32(inner, PB_EVENT_SCOPE_END_LINE, ev->b);
            pb_write_uint32(inner, PB_EVENT_SCOPE_END_COLUMN, ev->c);
            pb_write_bytes(out, PB_COMPILER_EVENT_SCOPE, inner->data, inner->length);
            break;
        }

        case COMPILATION_EVENT_SYMBOL: {
            char *sym_name = compilation_log_get_symbol_name(log, ev->a);
            pb_write_uint32(inner, PB_EVENT_SYMBOL_ID, ev->a);
            pb_write_uint32(inner, PB_EVENT_SYMBOL_SCOPE_ID, ev->b);
            pb_write_string(inner, PB_EVENT_SYMBOL_NAME, sym_name ? sym_name : "");
            pb_write_uint32(inner, PB_EVENT_SYMBOL_FILE_ID, ev->c);
            pb_write_uint32(inner, PB_EVENT_SYMBOL_LINE, ev->d);
            pb_write_uint32(inner, PB_EVENT_SYMBOL_COLUMN, ev->extra16);
            pb_write_uint32(inner, PB_EVENT_SYMBOL_KIND, ev->extra);
            pb_write_uint32(inner, PB_EVENT_SYMBOL_PACKAGE_ID, ev->e);
            pb_write_bytes(out, PB_COMPILER_EVENT_SYMBOL, inner->data, inner->length);
            break;
        }

        case COMPILATION_EVENT_SYMBOL_RESOLVED:
            pb_write_uint32(inner, PB_EVENT_RESOLVED_SYMBOL_ID, ev->a);
            pb_write_uint32(inner, PB_EVENT_RESOLVED_FILE_ID, ev->b);
            pb_write_uint32(inner, PB_EVENT_RESOLVED_LINE, ev->c);
            pb_write_uint32(inner, PB_EVENT_RESOLVED_COLUMN, ev->extra16);
            pb_write_uint32(inner, PB_EVENT_RESOLVED_LENGTH, ev->d);
            pb_write_uint32(inner, PB_EVENT_RESOLVED_CHOSEN_SYMBOL_ID, ev->e);
            pb_write_bytes(out, PB_COMPILER_EVENT_RESOLVED, inner->data, inner->length);
            break;

        case COMPILATION_EVENT_OVERLOAD: {
            u32 *ids = (u32 *) ev->ptr;
            pb_write_uint32(inner, PB_EVENT_OVERLOAD_GROUP_SYMBOL_ID, ev->a);
            for (u32 i = 0; i < ev->b; i++) {
                pb_write_uint32_always(inner, PB_EVENT_OVERLOAD_OPTION_SYMBOL_IDS, ids ? ids[i] : 0);
            }
            pb_write_bytes(out, PB_COMPILER_EVENT_OVERLOAD, inner->data, inner->length);
            break;
        }

        case COMPILATION_EVENT_POLY_PROC: {
            CompilationLogPoly *poly = (CompilationLogPoly *) ev->ptr;
            pb_write_uint32(inner, PB_EVENT_POLY_PROC_TEMPLATE_SYMBOL_ID, ev->a);
            pb_write_uint32(inner, PB_EVENT_POLY_PROC_INSTANCE_SYMBOL_ID, ev->b);
            for (u32 i = 0; i < ev->c; i++) {
                pb_write_string(inner, PB_EVENT_POLY_PROC_SOLUTION_NAMES, poly && poly->names ? poly->names[i] : "");
            }
            for (u32 i = 0; i < ev->c; i++) {
                pb_write_uint32_always(inner, PB_EVENT_POLY_PROC_SOLUTION_TYPE_IDS, poly && poly->type_ids ? poly->type_ids[i] : 0);
            }
            pb_write_bytes(out, PB_COMPILER_EVENT_POLY_PROC, inner->data, inner->length);
            break;
        }

        case COMPILATION_EVENT_BINDING: {
            pb_write_uint32(inner, PB_EVENT_BINDING_SYMBOL_ID, ev->a);
            pb_write_string(inner, PB_EVENT_BINDING_DOCUMENTATION, compilation_log_binding_docs(context, log, (AstNode *) ev->ptr));
            pb_write_uint32(inner, PB_EVENT_BINDING_TYPE_ID, ev->b);

            AstNode *node = (AstNode *) ev->ptr;
            if (node) {
                AstNode *inner_node = strip_aliases(node);
                if (inner_node && (inner_node->kind == Ast_Kind_Function || inner_node->kind == Ast_Kind_Polymorphic_Proc)) {
                    AstFunction *func = (AstFunction *) inner_node;
                    bh_arr_each(AstParam, param, func->params) {
                        u32 sid = 0;
                        if (param->local) {
                            compilation_log_lookup_symbol_id(log, (AstNode *) param->local, &sid);
                        }
                        pb_write_uint32_always(inner, PB_EVENT_BINDING_PARAM_SYMBOL_IDS, sid);
                    }
                }
            }

            pb_write_bytes(out, PB_COMPILER_EVENT_BINDING, inner->data, inner->length);
            break;
        }

        case COMPILATION_EVENT_TYPE:
            if (ev->ptr) encode_type_payload(context, (Type *) ev->ptr);
            pb_write_bytes(out, PB_COMPILER_EVENT_TYPE, inner->data, inner->length);
            break;

        case COMPILATION_EVENT_TYPE_BODY:
            if (ev->ptr) encode_type_body_payload(context, (Type *) ev->ptr);
            pb_write_bytes(out, PB_COMPILER_EVENT_TYPE_BODY, inner->data, inner->length);
            break;
    }
}

static void encode_event_into(Context *context, CompilationEvent *ev, bh_buffer *out) {
    encode_event(context, ev);
    pb_write_bytes(out, PB_COMPILER_EVENTS_EVENTS, context->compilation_log.pb_event.data, context->compilation_log.pb_event.length);
}

void compilation_log_encode_range(Context *context, u32 from, u32 to, bh_buffer *out) {
    u32 count = (u32)bh_arr_length(context->compilation_log.events);
    if (from > count) from = count;
    if (to > count) to = count;

    for (u32 i = from; i < to; i++) {
        encode_event_into(context, &context->compilation_log.events[i], out);
    }
}

void compilation_log_ensure_c_abi_encoded(Context *context) {
    CompilationLog *log = &context->compilation_log;
    u32 from = log->c_abi_cursor;
    u32 to = (u32)bh_arr_length(log->events);

    if (log->encoded_valid && log->encoded_from == from && log->encoded_to == to) {
        return;
    }

    if (!log->encoded.data) {
        bh_buffer_init(&log->encoded, context->gp_alloc, 256);
    } else {
        bh_buffer_clear(&log->encoded);
    }

    compilation_log_encode_range(context, from, to, &log->encoded);
    log->encoded_from = from;
    log->encoded_to = to;
    log->encoded_valid = 1;
}

void compilation_log_emit_start(Context *context) {
    CompilationEvent *ev = compilation_log_new_event(context, COMPILATION_EVENT_START);
    ev->ptr = bh_aprintf(
        context->gp_alloc,
        "%d.%d.%d%s",
        VERSION_MAJOR, VERSION_MINOR, VERSION_PATCH, VERSION_SUFFIX
    );
}

static void compilation_log_file_id_cache_put(CompilationLog *log, const char *filename, u32 id) {
    u32 n = log->file_id_cache_used;
    if (n < 16) {
        log->file_id_cache_name[n] = filename;
        log->file_id_cache_id[n] = id;
        log->file_id_cache_used = n + 1;
        return;
    }

    n = 15;
    memmove(log->file_id_cache_name, log->file_id_cache_name + 1, n * sizeof(const char *));
    memmove(log->file_id_cache_id, log->file_id_cache_id + 1, n * sizeof(u32));
    log->file_id_cache_name[n] = filename;
    log->file_id_cache_id[n] = id;
}

static u32 compilation_log_file_id_for(Context *context, const char *filename) {
    if (!filename || !*filename) return 0;

    CompilationLog *log = &context->compilation_log;
    if (log->cached_file_name == filename) return log->cached_file_id;

    u32 used = log->file_id_cache_used;
    for (u32 i = 0; i < used; i++) {
        if (log->file_id_cache_name[i] == filename) {
            log->cached_file_name = filename;
            log->cached_file_id = log->file_id_cache_id[i];
            return log->cached_file_id;
        }
    }

    if (!log->file_ids) return 0;

    i32 index = shgeti(log->file_ids, (char *)filename);
    u32 id = index == -1 ? 0 : log->file_ids[index].value;
    log->cached_file_name = filename;
    log->cached_file_id = id;
    compilation_log_file_id_cache_put(log, filename, id);
    return id;
}

u32 compilation_log_emit_file(Context *context, const char *path) {
    CompilationLog *log = &context->compilation_log;

    CompilationEvent *ev = compilation_log_new_event(context, COMPILATION_EVENT_FILE);
    ev->a = ++log->next_file_id;
    ev->ptr = path ? (char *)path : "";

    if (path && *path) {
        if (!log->file_ids) sh_new_arena(log->file_ids);
        shput(log->file_ids, (char *)path, ev->a);
        log->cached_file_name = path;
        log->cached_file_id = ev->a;
        compilation_log_file_id_cache_put(log, path, ev->a);
    }
    return ev->a;
}

static u32 compilation_log_pos_file_id(Context *context, OnyxFilePos pos) {
    if (pos.file_id) return pos.file_id;
    return compilation_log_file_id_for(context, pos.filename);
}

void compilation_log_emit_package(Context *context, Package *package) {
    if (!package) return;

    CompilationEvent *ev = compilation_log_new_event(context, COMPILATION_EVENT_PACKAGE);
    ev->ptr = package;
}

void compilation_log_emit_scope(Context *context, Scope *scope) {
    if (!scope) return;

    CompilationLog *log = &context->compilation_log;
    CompilationEvent *ev = compilation_log_new_event(context, COMPILATION_EVENT_SCOPE);
    ev->a = compilation_log_pos_file_id(context, scope->created_at);
    ev->b = 0;
    ev->c = 0;
    ev->ptr = scope;
    scope->log_event_index = (u32)(bh_arr_length(log->events) - 1);
}

void compilation_log_emit_scope_end(Context *context, Scope *scope, OnyxFilePos pos) {
    if (!scope) return;
    if (pos.line == 0) return;

    CompilationLog *log = &context->compilation_log;
    u32 idx = scope->log_event_index;
    if (idx != 0 && idx < (u32)bh_arr_length(log->events)) {
        CompilationEvent *ev = &log->events[idx];
        if (ev->kind == COMPILATION_EVENT_SCOPE && ev->ptr == scope) {
            if (ev->b != 0) return;
            if (idx >= log->delivered_until) {
                ev->b = pos.line;
                ev->c = pos.column;
                log->encoded_valid = 0;
                return;
            }
        }
    }

    compilation_log_emit_scope(context, scope);
    CompilationEvent *ev = &log->events[bh_arr_length(log->events) - 1];
    ev->b = pos.line;
    ev->c = pos.column;
}

static u32 compilation_log_symbol_kind(AstNode *node) {
    if (!node) return SYMBOL_KIND_UNKNOWN;

    if (node->kind == Ast_Kind_Alias) {
        node = strip_aliases(node);
        if (!node) return SYMBOL_KIND_UNKNOWN;
    }

    switch (node->kind) {
        case Ast_Kind_Package:             return SYMBOL_KIND_PACKAGE;
        case Ast_Kind_Function:            return SYMBOL_KIND_FUNCTION;
        case Ast_Kind_Overloaded_Function: return SYMBOL_KIND_OVERLOADED_FUNCTION;
        case Ast_Kind_Polymorphic_Proc:    return SYMBOL_KIND_POLYMORPHIC_PROC;
        case Ast_Kind_Macro:
        case Ast_Kind_Procedural_Macro:    return SYMBOL_KIND_MACRO;
        case Ast_Kind_Struct_Type:
        case Ast_Kind_Poly_Struct_Type:    return SYMBOL_KIND_STRUCT;
        case Ast_Kind_Union_Type:
        case Ast_Kind_Poly_Union_Type:     return SYMBOL_KIND_UNION;
        case Ast_Kind_Enum_Type:           return SYMBOL_KIND_ENUM;
        case Ast_Kind_Distinct_Type:       return SYMBOL_KIND_DISTINCT;
        case Ast_Kind_Interface:           return SYMBOL_KIND_INTERFACE;
        case Ast_Kind_Param:               return SYMBOL_KIND_PARAMETER;
        default:                           return SYMBOL_KIND_EXPRESSION;
    }
}

static u32 compilation_log_type_kind(TypeKind kind) {
    switch (kind) {
        case Type_Kind_Basic:        return PROTO_TYPE_KIND_BASIC;
        case Type_Kind_Pointer:      return PROTO_TYPE_KIND_POINTER;
        case Type_Kind_MultiPointer: return PROTO_TYPE_KIND_MULTI_POINTER;
        case Type_Kind_Function:     return PROTO_TYPE_KIND_FUNCTION;
        case Type_Kind_Struct:       return PROTO_TYPE_KIND_STRUCT;
        case Type_Kind_PolyStruct:   return PROTO_TYPE_KIND_POLY_STRUCT;
        case Type_Kind_Compound:     return PROTO_TYPE_KIND_COMPOUND;
        case Type_Kind_Array:        return PROTO_TYPE_KIND_ARRAY;
        case Type_Kind_Slice:        return PROTO_TYPE_KIND_SLICE;
        case Type_Kind_DynArray:     return PROTO_TYPE_KIND_DYN_ARRAY;
        case Type_Kind_VarArgs:      return PROTO_TYPE_KIND_VAR_ARGS;
        case Type_Kind_Enum:         return PROTO_TYPE_KIND_ENUM;
        case Type_Kind_Distinct:     return PROTO_TYPE_KIND_DISTINCT;
        case Type_Kind_Union:        return PROTO_TYPE_KIND_UNION;
        case Type_Kind_PolyUnion:    return PROTO_TYPE_KIND_POLY_UNION;
        default:                     return PROTO_TYPE_KIND_UNKNOWN;
    }
}

static u32 compilation_log_basic_kind(enum BasicKind kind) {
    switch (kind) {
        case Basic_Kind_Void:          return PROTO_BASIC_KIND_VOID;
        case Basic_Kind_Bool:          return PROTO_BASIC_KIND_BOOL;
        case Basic_Kind_Int_Unsized:   return PROTO_BASIC_KIND_INT_UNSIZED;
        case Basic_Kind_I8:            return PROTO_BASIC_KIND_I8;
        case Basic_Kind_U8:            return PROTO_BASIC_KIND_U8;
        case Basic_Kind_I16:           return PROTO_BASIC_KIND_I16;
        case Basic_Kind_U16:           return PROTO_BASIC_KIND_U16;
        case Basic_Kind_I32:           return PROTO_BASIC_KIND_I32;
        case Basic_Kind_U32:           return PROTO_BASIC_KIND_U32;
        case Basic_Kind_I64:           return PROTO_BASIC_KIND_I64;
        case Basic_Kind_U64:           return PROTO_BASIC_KIND_U64;
        case Basic_Kind_Float_Unsized: return PROTO_BASIC_KIND_FLOAT_UNSIZED;
        case Basic_Kind_F32:           return PROTO_BASIC_KIND_F32;
        case Basic_Kind_F64:           return PROTO_BASIC_KIND_F64;
        case Basic_Kind_Rawptr:        return PROTO_BASIC_KIND_RAWPTR;
        case Basic_Kind_I8X16:         return PROTO_BASIC_KIND_I8X16;
        case Basic_Kind_I16X8:         return PROTO_BASIC_KIND_I16X8;
        case Basic_Kind_I32X4:         return PROTO_BASIC_KIND_I32X4;
        case Basic_Kind_I64X2:         return PROTO_BASIC_KIND_I64X2;
        case Basic_Kind_F32X4:         return PROTO_BASIC_KIND_F32X4;
        case Basic_Kind_F64X2:         return PROTO_BASIC_KIND_F64X2;
        case Basic_Kind_V128:          return PROTO_BASIC_KIND_V128;
        case Basic_Kind_Type_Index:    return PROTO_BASIC_KIND_TYPE_EXPR;
        default:                       return PROTO_BASIC_KIND_UNKNOWN;
    }
}

static b32 compilation_log_emit_binding_for_id(Context *context, u32 symbol_id, AstNode *node, AstBinding *binding);
static void compilation_log_remember_alias_symbol(Context *context, AstNode *node, u32 symbol_id);
static void compilation_log_copy_binding_to_symbol(Context *context, u32 dest_id, u32 src_id);

static u32 compilation_log_emit_symbol_with_kind(Context *context, Scope *scope, char *name, OnyxFilePos pos, AstNode *node, u32 kind, b32 dedup) {
    if (!name || !*name || (name[0] == '_' && name[1] == '\0')) return 0;

    CompilationLog *log = &context->compilation_log;
    if (dedup && node && node->compilation_symbol_id) {
        return node->compilation_symbol_id;
    }

    CompilationEvent *ev = compilation_log_new_event(context, COMPILATION_EVENT_SYMBOL);
    ev->a = ++log->next_symbol_id;
    ev->b = scope ? (u32)scope->id : 0;
    ev->c = compilation_log_pos_file_id(context, pos);
    ev->d = pos.line;
    ev->extra16 = (u16) pos.column;
    ev->extra = (u8) kind;
    ev->e = 0;

    u32 symbol_id = ev->a;
    b32 is_alias = 0;
    if (node) {
        // Locals can be introduced again in a child scope (named returns are
        // added to the function scope, then spliced into the body). Keep the
        // mapping on the newest symbol so type notifications follow lookup.
        if (node->kind == Ast_Kind_Local) {
            node->compilation_symbol_id = symbol_id;
        } else if (node->compilation_symbol_id) {
            is_alias = 1;
        } else {
            node->compilation_symbol_id = symbol_id;
        }
        AstNode *inner = (node->kind == Ast_Kind_Alias) ? strip_aliases(node) : node;
        if (inner && inner != node && !inner->compilation_symbol_id) {
            inner->compilation_symbol_id = symbol_id;
        }
        if (inner && inner->kind == Ast_Kind_Package) {
            AstPackage *package = (AstPackage *) inner;
            if (package->package) ev->e = package->package->id;
        }
    }

    compilation_log_names_set(context, &log->symbol_names, symbol_id, name);

    if (is_alias) {
        compilation_log_remember_alias_symbol(context, node, symbol_id);
        u32 primary = node->compilation_symbol_id;
        if (primary && compilation_log_binding_type_get(log, primary, NULL)) {
            compilation_log_copy_binding_to_symbol(context, symbol_id, primary);
        }
    } else if (node && node->kind == Ast_Kind_Local) {
        AstTyped *typed = (AstTyped *) node;
        if (typed->type && typed->type->id) {
            compilation_log_emit_binding(context, node, NULL);
        }
    }

    return symbol_id;
}

void compilation_log_emit_symbol(Context *context, Scope *scope, char *name, OnyxFilePos pos, AstNode *node) {
    compilation_log_emit_symbol_with_kind(context, scope, name, pos, node, compilation_log_symbol_kind(node), 0);
}

static u32 compilation_log_emit_member_symbol(Context *context, Scope *scope, char *name, OnyxFilePos pos, AstNode *node) {
    return compilation_log_emit_symbol_with_kind(context, scope, name, pos, node, SYMBOL_KIND_MEMBER, 1);
}

static void compilation_log_emit_resolved_at(Context *context, OnyxToken *token, u32 symbol_id, u32 chosen_symbol_id) {
    if (!token || !symbol_id) return;

    CompilationEvent *ev = compilation_log_new_event(context, COMPILATION_EVENT_SYMBOL_RESOLVED);
    ev->a = symbol_id;
    ev->b = compilation_log_pos_file_id(context, token->pos);
    ev->c = token->pos.line;
    ev->d = token->length;
    ev->extra16 = (u16) token->pos.column;
    ev->e = chosen_symbol_id;
}

void compilation_log_emit_symbol_resolved(Context *context, AstNode *original, AstNode *resolved) {
    if (!original || !resolved || !original->token) return;

    u32 symbol_id;
    if (!compilation_log_lookup_symbol_id(&context->compilation_log, resolved, &symbol_id)) return;
    compilation_log_emit_resolved_at(context, original->token, symbol_id, 0);
}

void compilation_log_emit_overloads(Context *context, AstOverloadedFunction *ofunc) {
    if (!ofunc) return;

    CompilationLog *log = &context->compilation_log;
    u32 group_id = 0;
    if (!compilation_log_lookup_symbol_id(log, (AstNode *) ofunc, &group_id)) return;

    char *group_name = "overload";
    char *named = compilation_log_get_symbol_name(log, group_id);
    if (named) group_name = named;

    bh_imap all_overloads;
    bh_imap_init(&all_overloads, context->gp_alloc, 8);
    build_all_overload_options(ofunc->overloads, &all_overloads);

    u32 max = (u32) bh_arr_length(all_overloads.entries);
    u32 *ids = NULL;
    if (max > 0) ids = bh_alloc(context->gp_alloc, sizeof(u32) * max);
    u32 count = 0;

    bh_arr_each(bh__imap_entry, entry, all_overloads.entries) {
        AstTyped *node = (AstTyped *) strip_aliases((AstNode *) entry->key);
        if (!node) continue;
        if (node->kind == Ast_Kind_Overloaded_Function) continue;
        if (node->kind != Ast_Kind_Function
            && node->kind != Ast_Kind_Polymorphic_Proc
            && node->kind != Ast_Kind_Macro) {
            continue;
        }

        u32 sid = 0;
        if (!compilation_log_lookup_symbol_id(log, (AstNode *) node, &sid)) {
            OnyxFilePos pos = ofunc->token ? ofunc->token->pos : (OnyxFilePos){0};
            if (node->token) pos = node->token->pos;
            sid = compilation_log_emit_symbol_with_kind(
                context, NULL, group_name, pos, (AstNode *) node,
                compilation_log_symbol_kind((AstNode *) node), 1
            );
        }

        if (!sid) continue;
        compilation_log_emit_binding(context, (AstNode *) node, NULL);

        b32 seen = 0;
        for (u32 i = 0; i < count; i++) {
            if (ids[i] == sid) { seen = 1; break; }
        }
        if (!seen) ids[count++] = sid;
    }

    bh_imap_free(&all_overloads);

    CompilationEvent *ev = compilation_log_new_event(context, COMPILATION_EVENT_OVERLOAD);
    ev->a = group_id;
    ev->b = count;
    ev->ptr = ids;
}

void compilation_log_emit_overload_choice(Context *context, OnyxToken *site, AstNode *group, AstNode *chosen) {
    if (!site || !group || !chosen) return;

    CompilationLog *log = &context->compilation_log;
    u32 group_id = 0;
    u32 chosen_id = 0;
    if (!compilation_log_lookup_symbol_id(log, group, &group_id)) return;
    if (!compilation_log_lookup_symbol_id(log, chosen, &chosen_id)) return;
    if (!chosen_id || chosen_id == group_id) return;

    compilation_log_emit_resolved_at(context, site, group_id, chosen_id);
}

static char *compilation_log_function_display_name(Context *context, AstFunction *func) {
    if (!func) return compilation_log_intern(context, "proc");
    if (func->name && func->name[0]) return compilation_log_intern(context, func->name);
    if (func->token && func->token->text && func->token->length > 0) {
        return compilation_log_intern_len(context, func->token->text, func->token->length);
    }
    return compilation_log_intern(context, "proc");
}

void compilation_log_ensure_poly_template_params(Context *context, AstFunction *pp) {
    if (!pp) return;

    bh_arr_each(AstParam, param, pp->params) {
        if (!param->local || !param->local->token) continue;
        if (param->local->compilation_symbol_id) continue;
        char *name = compilation_log_intern_len(
            context, param->local->token->text, param->local->token->length
        );
        compilation_log_emit_symbol_with_kind(
            context, NULL, name, param->local->token->pos,
            (AstNode *) param->local, SYMBOL_KIND_PARAMETER, 1
        );
    }

    compilation_log_emit_binding(context, (AstNode *) pp, NULL);
}

void compilation_log_emit_poly_proc(Context *context, AstFunction *pp, AstFunction *instance, bh_arr(AstPolySolution) slns) {
    if (!pp || !instance) return;

    CompilationLog *log = &context->compilation_log;
    u32 template_id = 0;
    if (!compilation_log_lookup_symbol_id(log, (AstNode *) pp, &template_id)) {
        char *name = compilation_log_function_display_name(context, pp);
        OnyxFilePos pos = pp->token ? pp->token->pos : (OnyxFilePos){0};
        template_id = compilation_log_emit_symbol_with_kind(
            context, NULL, name, pos, (AstNode *) pp, SYMBOL_KIND_POLYMORPHIC_PROC, 1
        );
    }
    if (!template_id) return;

    compilation_log_ensure_poly_template_params(context, pp);

    u32 instance_id = 0;
    if (!compilation_log_lookup_symbol_id(log, (AstNode *) instance, &instance_id)) {
        char *name = compilation_log_function_display_name(context, pp);
        OnyxFilePos pos = pp->token ? pp->token->pos : (OnyxFilePos){0};
        if (instance->token) pos = instance->token->pos;
        instance_id = compilation_log_emit_symbol_with_kind(
            context, NULL, name, pos, (AstNode *) instance, SYMBOL_KIND_FUNCTION, 1
        );
    }
    if (!instance_id) return;

    compilation_log_emit_binding(context, (AstNode *) instance, NULL);

    u32 n = (u32) bh_arr_length(slns);
    char **names = NULL;
    u32 *type_ids = NULL;
    if (n > 0) {
        names = bh_alloc(context->gp_alloc, sizeof(char *) * n);
        type_ids = bh_alloc(context->gp_alloc, sizeof(u32) * n);
        u32 i = 0;
        bh_arr_each(AstPolySolution, sln, slns) {
            char *sname = compilation_log_intern(context, "?");
            if (sln->poly_sym && sln->poly_sym->token && sln->poly_sym->token->length > 0) {
                sname = compilation_log_intern_len(
                    context, sln->poly_sym->token->text, sln->poly_sym->token->length
                );
            }
            names[i] = sname;

            u32 tid = 0;
            if (sln->kind == PSK_Type && sln->type) {
                tid = sln->type->id;
            } else if (sln->kind == PSK_Value && sln->value && sln->value->type) {
                tid = sln->value->type->id;
            }
            type_ids[i] = tid;
            i++;
        }
    }

    CompilationLogPoly *poly = NULL;
    if (n > 0) {
        poly = bh_alloc_item(context->gp_alloc, CompilationLogPoly);
        poly->names = names;
        poly->type_ids = type_ids;
    }

    CompilationEvent *ev = compilation_log_new_event(context, COMPILATION_EVENT_POLY_PROC);
    ev->a = template_id;
    ev->b = instance_id;
    ev->c = n;
    ev->ptr = poly;
}

void compilation_log_emit_poly_choice(Context *context, OnyxToken *site, AstNode *template, AstNode *chosen) {
    if (!site || !template || !chosen) return;

    CompilationLog *log = &context->compilation_log;
    u32 template_id = 0;
    u32 chosen_id = 0;
    if (!compilation_log_lookup_symbol_id(log, template, &template_id)) return;
    if (!compilation_log_lookup_symbol_id(log, chosen, &chosen_id)) return;
    if (!chosen_id || chosen_id == template_id) return;

    compilation_log_emit_resolved_at(context, site, template_id, chosen_id);
}

// These kinds are AstNode_base only. Casting them to AstTyped and reading
// `type` walks off the allocation (the next arena object's `kind` then looks
// like a Type*).
static b32 compilation_log_kind_has_type_pointer(AstKind kind) {
    switch (kind) {
        case Ast_Kind_Error:
        case Ast_Kind_Load:
        case Ast_Kind_Load_All:
        case Ast_Kind_Library_Path:
        case Ast_Kind_Import:
        case Ast_Kind_Symbol:
        case Ast_Kind_Return:
        case Ast_Kind_Jump:
        case Ast_Kind_Block:
        case Ast_Kind_Defer:
        case Ast_Kind_If:
        case Ast_Kind_For:
        case Ast_Kind_While:
        case Ast_Kind_Static_If:
        case Ast_Kind_Switch:
        case Ast_Kind_Switch_Case:
        case Ast_Kind_Constraint:
        case Ast_Kind_Capture_Block:
        case Ast_Kind_Polymorph_Query:
        case Ast_Kind_Directive_Error:
        case Ast_Kind_Directive_Add_Overload:
        case Ast_Kind_Directive_Operator:
        case Ast_Kind_Directive_Export:
        case Ast_Kind_Directive_Remove:
        case Ast_Kind_Directive_Init:
        case Ast_Kind_Directive_Library:
        case Ast_Kind_Directive_Wasm_Section:
        case Ast_Kind_Js_Code:
        case Ast_Kind_Compiler_Extension:
        case Ast_Kind_Procedural_Macro:
            return 0;
        default:
            return 1;
    }
}

static u32 compilation_log_type_id_of_node(AstNode *node) {
    if (!node) return 0;

    AstNode *inner = strip_aliases(node);
    if (!inner) inner = node;

    if (node_is_type(inner)) {
        AstType *t = (AstType *) inner;

        if (inner->kind == Ast_Kind_Struct_Type) {
            AstStructType *s = (AstStructType *) inner;
            if (s->stcache) return s->stcache->id;
            if (s->pending_type) return s->pending_type->id;
        } else if (inner->kind == Ast_Kind_Union_Type) {
            AstUnionType *u = (AstUnionType *) inner;
            if (u->utcache) return u->utcache->id;
            if (u->pending_type) return u->pending_type->id;
        } else if (inner->kind == Ast_Kind_Enum_Type) {
            AstEnumType *e = (AstEnumType *) inner;
            if (e->etcache) return e->etcache->id;
        } else if (inner->kind == Ast_Kind_Distinct_Type) {
            AstDistinctType *d = (AstDistinctType *) inner;
            if (d->dtcache) return d->dtcache->id;
        }

        if (t->type_id) return (u32) t->type_id;

        if (t->type && t->type->id) {
            b32 is_type_expr = t->type->kind == Type_Kind_Basic
                && t->type->Basic.kind == Basic_Kind_Type_Index;
            if (!is_type_expr) return t->type->id;
        }

        return 0;
    }

    if (!compilation_log_kind_has_type_pointer(inner->kind)) return 0;

    AstTyped *typed = (AstTyped *) inner;
    if (typed->type) return typed->type->id;

    if (node != inner) {
        typed = (AstTyped *) node;
        if (typed->type) return typed->type->id;
    }

    return 0;
}

static b32 compilation_log_lookup_symbol_id(CompilationLog *log, AstNode *node, u32 *out) {
    (void) log;
    if (!node) return 0;

    if (node->compilation_symbol_id) {
        *out = node->compilation_symbol_id;
        return 1;
    }

    if (node->kind != Ast_Kind_Alias) return 0;

    AstNode *inner = strip_aliases(node);
    if (inner && inner != node && inner->compilation_symbol_id) {
        *out = inner->compilation_symbol_id;
        return 1;
    }

    return 0;
}

static void compilation_log_map_node_binding(CompilationLog *log, AstNode *node, AstBinding *binding) {
    if (!node || !binding) return;

    bh_imap_put(&log->node_to_binding, (u64) node, (u64) binding);
    if (node->kind != Ast_Kind_Alias) return;

    AstNode *inner = strip_aliases(node);
    if (inner && inner != node) {
        bh_imap_put(&log->node_to_binding, (u64) inner, (u64) binding);
    }
}

static AstBinding *compilation_log_lookup_binding(CompilationLog *log, AstNode *node) {
    if (!node) return NULL;

    u64 binding = 0;
    if (bh_imap_try_get(&log->node_to_binding, (u64) node, &binding)) {
        return (AstBinding *) binding;
    }

    if (node->kind != Ast_Kind_Alias) return NULL;

    AstNode *inner = strip_aliases(node);
    if (inner && inner != node && bh_imap_try_get(&log->node_to_binding, (u64) inner, &binding)) {
        return (AstBinding *) binding;
    }

    return NULL;
}

static char *compilation_log_binding_docs(Context *context, CompilationLog *log, AstNode *node) {
    AstBinding *binding = compilation_log_lookup_binding(log, node);
    if (!binding) return (char *) "";

    if (binding->documentation_token_old && binding->documentation_token_old->text) {
        return compilation_log_intern_len(
            context,
            binding->documentation_token_old->text,
            binding->documentation_token_old->length
        );
    }
    if (binding->documentation_string) return (char *) binding->documentation_string;
    return (char *) "";
}

static void compilation_log_copy_binding_to_symbol(Context *context, u32 dest_id, u32 src_id) {
    if (!dest_id || !src_id || dest_id == src_id) return;

    CompilationLog *log = &context->compilation_log;
    u32 type_id = 0;
    if (!compilation_log_binding_type_get(log, src_id, &type_id)) return;
    if (!type_id) return;

    u32 prev = 0;
    if (compilation_log_binding_type_get(log, dest_id, &prev)) {
        if (prev == type_id) return;
    }
    compilation_log_binding_type_put(context, log, dest_id, type_id);

    CompilationEvent *ev = compilation_log_new_event(context, COMPILATION_EVENT_BINDING);
    ev->a = dest_id;
    ev->b = type_id;
    ev->ptr = NULL;
}

static void compilation_log_remember_alias_symbol(Context *context, AstNode *node, u32 symbol_id) {
    if (!node || !symbol_id) return;

    CompilationLog *log = &context->compilation_log;
    u64 prev = 0;
    bh_imap_try_get(&log->node_to_first_alias, (u64) node, &prev);

    CompilationLogAliasSymbol alias;
    alias.symbol_id = symbol_id;
    alias.next = (u32) prev;
    bh_arr_push(log->alias_symbols, alias);
    bh_imap_put(&log->node_to_first_alias, (u64) node, (u64) bh_arr_length(log->alias_symbols));
}

static b32 compilation_log_function_has_param_symbols(CompilationLog *log, AstNode *node) {
    AstKind kind = node->kind;
    if (kind != Ast_Kind_Function && kind != Ast_Kind_Polymorphic_Proc && kind != Ast_Kind_Alias) {
        return 0;
    }

    AstNode *inner = (kind == Ast_Kind_Alias) ? strip_aliases(node) : node;
    if (!inner || (inner->kind != Ast_Kind_Function && inner->kind != Ast_Kind_Polymorphic_Proc)) {
        return 0;
    }

    AstFunction *func = (AstFunction *) inner;
    bh_arr_each(AstParam, param, func->params) {
        u32 sid = 0;
        if (param->local && compilation_log_lookup_symbol_id(log, (AstNode *) param->local, &sid) && sid) {
            return 1;
        }
    }
    return 0;
}

static u32 compilation_log_fast_type_id(AstNode *node) {
    if (!node) return 0;

    AstKind kind = node->kind;
    if (kind == Ast_Kind_Alias || (kind > Ast_Kind_Type_Start && kind < Ast_Kind_Type_End)) {
        return compilation_log_type_id_of_node(node);
    }
    if (!compilation_log_kind_has_type_pointer(kind)) return 0;

    Type *type = ((AstTyped *) node)->type;
    return (type && type->id) ? type->id : 0;
}

static b32 compilation_log_emit_binding_for_id(Context *context, u32 symbol_id, AstNode *node, AstBinding *binding) {
    if (!symbol_id || !node) return 0;

    CompilationLog *log = &context->compilation_log;

    if (binding) {
        compilation_log_map_node_binding(log, node, binding);
    } else {
        u32 prev = 0;
        if (compilation_log_binding_type_get(log, symbol_id, &prev) && prev != 0) {
            if (compilation_log_fast_type_id(node) == prev) return 0;
        }
    }

    u32 type_id = compilation_log_type_id_of_node(node);
    b32 any_param = 0;
    if (type_id == 0) {
        any_param = compilation_log_function_has_param_symbols(log, node);
        if (!binding && !any_param) {
            if (!compilation_log_lookup_binding(log, node)) return 0;
        }
    }

    u32 prev = 0;
    if (compilation_log_binding_type_get(log, symbol_id, &prev)) {
        if (prev == type_id) {
            if (type_id != 0) return 0;
            if (!any_param) return 0;
        }
        if (type_id == 0 && prev != 0) return 0;
    }
    compilation_log_binding_type_put(context, log, symbol_id, type_id);

    CompilationEvent *ev = compilation_log_new_event(context, COMPILATION_EVENT_BINDING);
    ev->a = symbol_id;
    ev->b = type_id;
    ev->ptr = node;
    return 1;
}

void compilation_log_emit_binding(Context *context, AstNode *node, AstBinding *binding) {
    if (!node) return;

    CompilationLog *log = &context->compilation_log;
    u32 symbol_id;
    if (!compilation_log_lookup_symbol_id(log, node, &symbol_id)) return;

    if (!compilation_log_emit_binding_for_id(context, symbol_id, node, binding)) return;

    u64 idx = 0;
    if (bh_imap_try_get(&log->node_to_first_alias, (u64) node, &idx)) {
        while (idx) {
            CompilationLogAliasSymbol *alias = &log->alias_symbols[idx - 1];
            compilation_log_copy_binding_to_symbol(context, alias->symbol_id, symbol_id);
            idx = alias->next;
        }
    }
}

void compilation_log_notify_node_typed(Context *context, AstNode *node) {
    if (!node) return;
    if (!node->compilation_symbol_id && node->kind != Ast_Kind_Alias) return;
    compilation_log_emit_binding(context, node, NULL);
}

void compilation_log_emit_type(Context *context, Type *type) {
    if (!type) return;
    if (!context->compilation_log.events) return;

    CompilationEvent *ev = compilation_log_new_event(context, COMPILATION_EVENT_TYPE);
    ev->ptr = type;

    if (type->kind != Type_Kind_Struct && type->kind != Type_Kind_Union) {
        if (compilation_log_type_method_scope(type)) {
            compilation_log_emit_type_body(context, type);
        }
    }
}

static Scope *compilation_log_type_method_scope(Type *type) {
    if (!type) return NULL;

    switch (type->kind) {
        case Type_Kind_Struct:     return type->Struct.scope;
        case Type_Kind_Union:      return type->Union.scope;
        case Type_Kind_PolyStruct: return type->PolyStruct.scope;
        case Type_Kind_PolyUnion:  return type->PolyUnion.scope;
        case Type_Kind_Slice:      return type->Slice.scope;
        case Type_Kind_DynArray:   return type->DynArray.scope;
        case Type_Kind_Distinct:   return type->Distinct.scope;
        case Type_Kind_Enum:
            if (type->ast_type && type->ast_type->kind == Ast_Kind_Enum_Type) {
                return ((AstEnumType *) type->ast_type)->scope;
            }
            return NULL;
        case Type_Kind_Basic:
            if (type->ast_type && type->ast_type->kind == Ast_Kind_Basic_Type) {
                return ((AstBasicType *) type->ast_type)->scope;
            }
            return NULL;
        default:
            return NULL;
    }
}

void compilation_log_emit_type_body(Context *context, Type *type) {
    if (!type) return;
    if (!context->compilation_log.events) return;

    if (type->kind == Type_Kind_Union) {
        u32 n = (u32) bh_arr_length(type->Union.variants_ordered);
        for (u32 i = 0; i < n; i++) {
            UnionVariant *uv = type->Union.variants_ordered[i];
            if (uv && uv->variant_node && type->Union.scope && uv->name) {
                OnyxFilePos pos = uv->token ? uv->token->pos : (OnyxFilePos){0};
                compilation_log_emit_member_symbol(
                    context,
                    type->Union.scope,
                    uv->name,
                    pos,
                    (AstNode *) uv->variant_node
                );
            }
        }
    } else if (type->kind == Type_Kind_Struct) {
        u32 n = (u32) bh_arr_length(type->Struct.memarr);
        for (u32 i = 0; i < n; i++) {
            StructMember *smem = type->Struct.memarr[i];
            if (smem && smem->member_node && type->Struct.scope && smem->name) {
                OnyxFilePos pos = smem->token ? smem->token->pos : (OnyxFilePos){0};
                compilation_log_emit_member_symbol(
                    context,
                    type->Struct.scope,
                    smem->name,
                    pos,
                    (AstNode *) smem->member_node
                );
            }
        }
    } else if (!compilation_log_type_method_scope(type)) {
        return;
    }

    CompilationEvent *ev = compilation_log_new_event(context, COMPILATION_EVENT_TYPE_BODY);
    ev->ptr = type;
}

void compilation_log_init(Context *context) {
    CompilationLog *log = &context->compilation_log;
    bh_arr_new(context->gp_alloc, log->events, 65536);
    sh_new_arena(log->file_ids);
    sh_new_arena(log->interned_names);
    bh_buffer_init(&log->encoded, context->gp_alloc, 64 * 1024);
    bh_buffer_init(&log->pb_event, context->gp_alloc, 256);
    bh_buffer_init(&log->pb_field, context->gp_alloc, 256);
    bh_buffer_init(&log->pb_sub, context->gp_alloc, 256);
    bh_buffer_init(&log->pb_item, context->gp_alloc, 128);
    bh_buffer_init(&log->pb_flush, context->gp_alloc, 64 * 1024);
    bh_imap_init(&log->node_to_binding, context->gp_alloc, 4096);
    bh_arr_new(context->gp_alloc, log->symbol_binding_type_id, 4096);
    bh_arr_new(context->gp_alloc, log->symbol_names, 4096);
    bh_imap_init(&log->node_to_first_alias, context->gp_alloc, 1024);
    bh_arr_new(context->gp_alloc, log->alias_symbols, 64);
    compilation_log_emit_start(context);
}
