#include "astnodes.h"
#include "pb.h"

#include <string.h>

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

#define PB_EVENT_RESOLVED_SYMBOL_ID 1
#define PB_EVENT_RESOLVED_FILE_ID   2
#define PB_EVENT_RESOLVED_LINE      3
#define PB_EVENT_RESOLVED_COLUMN    4
#define PB_EVENT_RESOLVED_LENGTH    5

#define PB_EVENT_BINDING_SYMBOL_ID        1
#define PB_EVENT_BINDING_DOCUMENTATION    2
#define PB_EVENT_BINDING_TYPE_ID          3
#define PB_EVENT_BINDING_PARAM_SYMBOL_IDS 4

#define PB_EVENT_TYPE_ID           1
#define PB_EVENT_TYPE_KIND         2
#define PB_EVENT_TYPE_NAME         3
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
#define PB_TYPE_STRUCT_MEMBER_NAME      1
#define PB_TYPE_STRUCT_MEMBER_TYPE_ID   2
#define PB_TYPE_STRUCT_MEMBER_OFFSET    3
#define PB_TYPE_STRUCT_MEMBER_IDX       4
#define PB_TYPE_STRUCT_MEMBER_SYMBOL_ID 5

#define PB_TYPE_UNION_BODY_TAG_TYPE_ID 1
#define PB_TYPE_UNION_BODY_VARIANTS    2
#define PB_TYPE_UNION_VARIANT_NAME      1
#define PB_TYPE_UNION_VARIANT_TYPE_ID   2
#define PB_TYPE_UNION_VARIANT_TAG       3
#define PB_TYPE_UNION_VARIANT_SYMBOL_ID 4

static b32 compilation_event_is_detailed(CompilationEventKind kind) {
    return kind == COMPILATION_EVENT_SYMBOL_RESOLVED
        || kind == COMPILATION_EVENT_BINDING
        || kind == COMPILATION_EVENT_TYPE
        || kind == COMPILATION_EVENT_TYPE_BODY;
}

static char *compilation_log_intern(Context *context, const char *name) {
    if (!name || !*name) return (char *) "";

    CompilationLog *log = &context->compilation_log;
    if (!log->interned_names) sh_new_arena(log->interned_names);

    i32 idx = shgeti(log->interned_names, (char *) name);
    if (idx >= 0) return log->interned_names[idx].key;

    idx = shputi(log->interned_names, (char *) name, 1);
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

static void compilation_log_push(Context *context, CompilationEvent event) {
    bh_arr_push(context->compilation_log.events, event);
    context->compilation_log.encoded_valid = 0;
}

static void encode_event(CompilationLog *log, CompilationEvent *ev) {
    bh_buffer *out = &log->pb_event;
    bh_buffer *inner = &log->pb_field;
    bh_buffer *sub = &log->pb_sub;
    bh_buffer *item = &log->pb_item;

    bh_buffer_clear(out);
    bh_buffer_clear(inner);

    switch (ev->kind) {
        case COMPILATION_EVENT_START:
            pb_write_string(inner, PB_EVENT_START_COMPILER_VERSION, ev->start.compiler_version);
            pb_write_bytes(out, PB_COMPILER_EVENT_START, inner->data, inner->length);
            break;

        case COMPILATION_EVENT_FILE:
            pb_write_uint32(inner, PB_EVENT_FILE_ID, ev->file.id);
            pb_write_string(inner, PB_EVENT_FILE_PATH, ev->file.path);
            pb_write_bytes(out, PB_COMPILER_EVENT_FILE, inner->data, inner->length);
            break;

        case COMPILATION_EVENT_PACKAGE:
            pb_write_uint32(inner, PB_EVENT_PACKAGE_ID, ev->pack.id);
            pb_write_string(inner, PB_EVENT_PACKAGE_NAME, ev->pack.package_name);
            pb_write_string(inner, PB_EVENT_PACKAGE_FULLY_QUALIFIED_NAME, ev->pack.fully_qualified_name);
            pb_write_uint32(inner, PB_EVENT_PACKAGE_PARENT_ID, ev->pack.parent_package_id);
            pb_write_uint32(inner, PB_EVENT_PACKAGE_SCOPE_ID, ev->pack.scope_id);
            pb_write_bytes(out, PB_COMPILER_EVENT_PACK, inner->data, inner->length);
            break;

        case COMPILATION_EVENT_SCOPE:
            pb_write_uint32(inner, PB_EVENT_SCOPE_ID, ev->scope.id);
            pb_write_uint32(inner, PB_EVENT_SCOPE_PARENT_SCOPE_ID, ev->scope.parent_scope_id);
            pb_write_uint32(inner, PB_EVENT_SCOPE_FILE_ID, ev->scope.file_id);
            pb_write_uint32(inner, PB_EVENT_SCOPE_LINE, ev->scope.line);
            pb_write_uint32(inner, PB_EVENT_SCOPE_COLUMN, ev->scope.column);
            pb_write_string(inner, PB_EVENT_SCOPE_NAME, ev->scope.name);
            pb_write_uint32(inner, PB_EVENT_SCOPE_END_LINE, ev->scope.end_line);
            pb_write_uint32(inner, PB_EVENT_SCOPE_END_COLUMN, ev->scope.end_column);
            pb_write_bytes(out, PB_COMPILER_EVENT_SCOPE, inner->data, inner->length);
            break;

        case COMPILATION_EVENT_SYMBOL:
            pb_write_uint32(inner, PB_EVENT_SYMBOL_ID, ev->symbol.id);
            pb_write_uint32(inner, PB_EVENT_SYMBOL_SCOPE_ID, ev->symbol.scope_id);
            pb_write_string(inner, PB_EVENT_SYMBOL_NAME, ev->symbol.name);
            pb_write_uint32(inner, PB_EVENT_SYMBOL_FILE_ID, ev->symbol.file_id);
            pb_write_uint32(inner, PB_EVENT_SYMBOL_LINE, ev->symbol.line);
            pb_write_uint32(inner, PB_EVENT_SYMBOL_COLUMN, ev->symbol.column);
            pb_write_uint32(inner, PB_EVENT_SYMBOL_KIND, ev->symbol.kind);
            pb_write_bytes(out, PB_COMPILER_EVENT_SYMBOL, inner->data, inner->length);
            break;

        case COMPILATION_EVENT_SYMBOL_RESOLVED:
            pb_write_uint32(inner, PB_EVENT_RESOLVED_SYMBOL_ID, ev->resolved.symbol_id);
            pb_write_uint32(inner, PB_EVENT_RESOLVED_FILE_ID, ev->resolved.file_id);
            pb_write_uint32(inner, PB_EVENT_RESOLVED_LINE, ev->resolved.line);
            pb_write_uint32(inner, PB_EVENT_RESOLVED_COLUMN, ev->resolved.column);
            pb_write_uint32(inner, PB_EVENT_RESOLVED_LENGTH, ev->resolved.length);
            pb_write_bytes(out, PB_COMPILER_EVENT_RESOLVED, inner->data, inner->length);
            break;

        case COMPILATION_EVENT_BINDING:
            pb_write_uint32(inner, PB_EVENT_BINDING_SYMBOL_ID, ev->binding.symbol_id);
            pb_write_string(inner, PB_EVENT_BINDING_DOCUMENTATION, ev->binding.documentation);
            pb_write_uint32(inner, PB_EVENT_BINDING_TYPE_ID, ev->binding.type_id);
            for (u32 i = 0; i < ev->binding.param_count; i++) {
                pb_write_uint32_always(inner, PB_EVENT_BINDING_PARAM_SYMBOL_IDS, ev->binding.param_symbol_ids[i]);
            }
            pb_write_bytes(out, PB_COMPILER_EVENT_BINDING, inner->data, inner->length);
            break;

        case COMPILATION_EVENT_TYPE: {
            pb_write_uint32(inner, PB_EVENT_TYPE_ID, ev->type.id);
            pb_write_uint32(inner, PB_EVENT_TYPE_KIND, ev->type.kind);
            pb_write_string(inner, PB_EVENT_TYPE_NAME, ev->type.name);

            bh_buffer_clear(sub);

            switch (ev->type.kind) {
                case PROTO_TYPE_KIND_BASIC:
                    pb_write_uint32(sub, PB_TYPE_BASIC_KIND, ev->type.body.basic.basic_kind);
                    pb_write_uint32(sub, PB_TYPE_BASIC_SIZE, ev->type.body.basic.size);
                    pb_write_uint32(sub, PB_TYPE_BASIC_ALIGNMENT, ev->type.body.basic.alignment);
                    pb_write_bytes(inner, PB_EVENT_TYPE_BASIC, sub->data, sub->length);
                    break;

                case PROTO_TYPE_KIND_POINTER:
                case PROTO_TYPE_KIND_MULTI_POINTER:
                case PROTO_TYPE_KIND_SLICE:
                case PROTO_TYPE_KIND_DYN_ARRAY:
                case PROTO_TYPE_KIND_VAR_ARGS:
                    pb_write_uint32(sub, PB_TYPE_ELEM_ELEM_ID, ev->type.body.elem.elem_id);
                    pb_write_bytes(inner, PB_EVENT_TYPE_ELEM, sub->data, sub->length);
                    break;

                case PROTO_TYPE_KIND_ARRAY:
                    pb_write_uint32(sub, PB_TYPE_ARRAY_ELEM_ID, ev->type.body.array.elem_id);
                    pb_write_uint32(sub, PB_TYPE_ARRAY_COUNT, ev->type.body.array.count);
                    pb_write_bytes(inner, PB_EVENT_TYPE_ARRAY, sub->data, sub->length);
                    break;

                case PROTO_TYPE_KIND_FUNCTION:
                    pb_write_uint32(sub, PB_TYPE_FUNCTION_RETURN_ID, ev->type.body.function.return_id);
                    for (u32 i = 0; i < ev->type.body.function.param_count; i++) {
                        pb_write_uint32_always(sub, PB_TYPE_FUNCTION_PARAM_IDS, ev->type.body.function.param_ids[i]);
                    }
                    pb_write_bytes(inner, PB_EVENT_TYPE_FUNCTION, sub->data, sub->length);
                    break;

                case PROTO_TYPE_KIND_COMPOUND:
                    for (u32 i = 0; i < ev->type.body.compound.count; i++) {
                        pb_write_uint32_always(sub, PB_TYPE_COMPOUND_TYPE_IDS, ev->type.body.compound.type_ids[i]);
                    }
                    pb_write_bytes(inner, PB_EVENT_TYPE_COMPOUND, sub->data, sub->length);
                    break;

                case PROTO_TYPE_KIND_DISTINCT:
                    pb_write_uint32(sub, PB_TYPE_DISTINCT_BASE_ID, ev->type.body.distinct.base_id);
                    pb_write_bytes(inner, PB_EVENT_TYPE_DISTINCT, sub->data, sub->length);
                    break;

                case PROTO_TYPE_KIND_ENUM:
                    pb_write_uint32(sub, PB_TYPE_ENUM_BACKING_ID, ev->type.body.enumeration.backing_id);
                    if (ev->type.body.enumeration.is_flags) {
                        pb_write_uint32_always(sub, PB_TYPE_ENUM_IS_FLAGS, 1);
                    }
                    for (u32 i = 0; i < ev->type.body.enumeration.value_count; i++) {
                        CompilationTypeEnumValue *v = &ev->type.body.enumeration.values[i];
                        bh_buffer_clear(item);
                        pb_write_string(item, PB_TYPE_ENUM_VALUE_NAME, v->name);
                        pb_write_int64_always(item, PB_TYPE_ENUM_VALUE_VALUE, v->value);
                        pb_write_uint32(item, PB_TYPE_ENUM_VALUE_SYMBOL_ID, v->symbol_id);
                        pb_write_bytes(sub, PB_TYPE_ENUM_VALUES, item->data, item->length);
                    }
                    pb_write_bytes(inner, PB_EVENT_TYPE_ENUMERATION, sub->data, sub->length);
                    break;

                default:
                    break;
            }

            pb_write_bytes(out, PB_COMPILER_EVENT_TYPE, inner->data, inner->length);
            break;
        }

        case COMPILATION_EVENT_TYPE_BODY: {
            pb_write_uint32(inner, PB_EVENT_TYPE_BODY_TYPE_ID, ev->type_body.type_id);
            pb_write_uint32(inner, PB_EVENT_TYPE_BODY_SIZE, ev->type_body.size);
            pb_write_uint32(inner, PB_EVENT_TYPE_BODY_ALIGNMENT, ev->type_body.alignment);
            pb_write_uint32(inner, PB_EVENT_TYPE_BODY_SCOPE_ID, ev->type_body.scope_id);

            bh_buffer_clear(sub);

            if (ev->type_body.is_union) {
                pb_write_uint32(sub, PB_TYPE_UNION_BODY_TAG_TYPE_ID, ev->type_body.tag_type_id);
                for (u32 i = 0; i < ev->type_body.variant_count; i++) {
                    CompilationTypeVariant *v = &ev->type_body.variants[i];
                    bh_buffer_clear(item);
                    pb_write_string(item, PB_TYPE_UNION_VARIANT_NAME, v->name);
                    pb_write_uint32(item, PB_TYPE_UNION_VARIANT_TYPE_ID, v->type_id);
                    pb_write_uint32_always(item, PB_TYPE_UNION_VARIANT_TAG, v->tag);
                    pb_write_uint32(item, PB_TYPE_UNION_VARIANT_SYMBOL_ID, v->symbol_id);
                    pb_write_bytes(sub, PB_TYPE_UNION_BODY_VARIANTS, item->data, item->length);
                }
                pb_write_bytes(inner, PB_EVENT_TYPE_BODY_UNION, sub->data, sub->length);
            } else {
                for (u32 i = 0; i < ev->type_body.member_count; i++) {
                    CompilationTypeMember *m = &ev->type_body.members[i];
                    bh_buffer_clear(item);
                    pb_write_string(item, PB_TYPE_STRUCT_MEMBER_NAME, m->name);
                    pb_write_uint32(item, PB_TYPE_STRUCT_MEMBER_TYPE_ID, m->type_id);
                    pb_write_uint32_always(item, PB_TYPE_STRUCT_MEMBER_OFFSET, m->offset);
                    pb_write_uint32_always(item, PB_TYPE_STRUCT_MEMBER_IDX, m->idx);
                    pb_write_uint32(item, PB_TYPE_STRUCT_MEMBER_SYMBOL_ID, m->symbol_id);
                    pb_write_bytes(sub, PB_TYPE_STRUCT_BODY_MEMBERS, item->data, item->length);
                }
                pb_write_bytes(inner, PB_EVENT_TYPE_BODY_STRUCT, sub->data, sub->length);
            }

            pb_write_bytes(out, PB_COMPILER_EVENT_TYPE_BODY, inner->data, inner->length);
            break;
        }
    }
}

static void encode_event_into(CompilationLog *log, CompilationEvent *ev, bh_buffer *out) {
    encode_event(log, ev);
    pb_write_bytes(out, PB_COMPILER_EVENTS_EVENTS, log->pb_event.data, log->pb_event.length);
}

void compilation_log_encode_range(Context *context, u32 from, u32 to, bh_buffer *out, b32 include_detailed) {
    u32 count = (u32)bh_arr_length(context->compilation_log.events);
    if (from > count) from = count;
    if (to > count) to = count;

    for (u32 i = from; i < to; i++) {
        CompilationEvent *ev = &context->compilation_log.events[i];
        if (!include_detailed && compilation_event_is_detailed(ev->kind)) continue;
        encode_event_into(&context->compilation_log, ev, out);
    }
}

static void compilation_log_encode_detailed_backfill(Context *context, u32 until, bh_buffer *out) {
    u32 count = (u32)bh_arr_length(context->compilation_log.events);
    if (until > count) until = count;

    for (u32 i = 0; i < until; i++) {
        CompilationEvent *ev = &context->compilation_log.events[i];
        if (!compilation_event_is_detailed(ev->kind)) continue;
        encode_event_into(&context->compilation_log, ev, out);
    }
}

void compilation_log_ensure_c_abi_encoded(Context *context) {
    CompilationLog *log = &context->compilation_log;
    u32 from = log->c_abi_cursor;
    u32 to = (u32)bh_arr_length(log->events);
    b32 want_detailed = context->options.compilation_data_detailed;
    b32 backfill = want_detailed && !log->c_abi_detailed_sent && from > 0;

    if (log->encoded_valid
        && log->encoded_from == from
        && log->encoded_to == to
        && log->encoded_include_detailed == want_detailed
        && log->encoded_backfill == backfill) {
        return;
    }

    if (!log->encoded.data) {
        bh_buffer_init(&log->encoded, context->gp_alloc, 256);
    } else {
        bh_buffer_clear(&log->encoded);
    }

    if (backfill) {
        compilation_log_encode_detailed_backfill(context, from, &log->encoded);
    }
    compilation_log_encode_range(context, from, to, &log->encoded, want_detailed);
    log->encoded_from = from;
    log->encoded_to = to;
    log->encoded_include_detailed = want_detailed;
    log->encoded_backfill = backfill;
    log->encoded_valid = 1;
}

void compilation_log_emit_start(Context *context) {
    CompilationEvent ev;
    memset(&ev, 0, sizeof(ev));
    ev.kind = COMPILATION_EVENT_START;
    ev.start.compiler_version = bh_aprintf(
        context->gp_alloc,
        "%d.%d.%d%s",
        VERSION_MAJOR, VERSION_MINOR, VERSION_PATCH, VERSION_SUFFIX
    );
    compilation_log_push(context, ev);
}

static u32 compilation_log_file_id_for(Context *context, const char *filename) {
    if (!filename || !*filename) return 0;

    CompilationLog *log = &context->compilation_log;
    if (log->cached_file_name == filename) return log->cached_file_id;
    if (!log->file_ids) return 0;

    i32 index = shgeti(log->file_ids, (char *)filename);
    u32 id = index == -1 ? 0 : log->file_ids[index].value;
    log->cached_file_name = filename;
    log->cached_file_id = id;
    return id;
}

void compilation_log_emit_file(Context *context, const char *path) {
    CompilationLog *log = &context->compilation_log;

    CompilationEvent ev;
    memset(&ev, 0, sizeof(ev));
    ev.kind = COMPILATION_EVENT_FILE;
    ev.file.id = ++log->next_file_id;
    ev.file.path = path ? (char *)path : "";

    if (path && *path) {
        if (!log->file_ids) sh_new_arena(log->file_ids);
        shput(log->file_ids, (char *)path, ev.file.id);
    }

    compilation_log_push(context, ev);
}

void compilation_log_emit_package(Context *context, Package *package) {
    if (!package) return;

    CompilationEvent ev;
    memset(&ev, 0, sizeof(ev));
    ev.kind = COMPILATION_EVENT_PACKAGE;
    ev.pack.id = package->id;
    ev.pack.package_name = package->unqualified_name ? package->unqualified_name : "";
    ev.pack.fully_qualified_name = package->name ? package->name : "";
    ev.pack.parent_package_id = package->parent_id >= 0 ? (u32)package->parent_id : 0;
    ev.pack.scope_id = package->scope ? (u32)package->scope->id : 0;
    compilation_log_push(context, ev);
}

void compilation_log_emit_scope(Context *context, Scope *scope) {
    if (!scope) return;

    CompilationEvent ev;
    memset(&ev, 0, sizeof(ev));
    ev.kind = COMPILATION_EVENT_SCOPE;
    ev.scope.id = (u32)scope->id;
    ev.scope.parent_scope_id = scope->parent ? (u32)scope->parent->id : 0;
    ev.scope.file_id = compilation_log_file_id_for(context, scope->created_at.filename);
    ev.scope.line = scope->created_at.line;
    ev.scope.column = scope->created_at.column;
    ev.scope.end_line = scope->closed_at.line;
    ev.scope.end_column = scope->closed_at.column;
    ev.scope.name = scope->name ? scope->name : "";
    compilation_log_push(context, ev);
}

void compilation_log_emit_scope_end(Context *context, Scope *scope, OnyxFilePos pos) {
    if (!scope) return;
    if (scope->closed_at.line != 0) return;
    if (pos.line == 0) return;

    scope->closed_at = pos;
    compilation_log_emit_scope(context, scope);
}

static u32 compilation_log_symbol_kind(AstNode *node) {
    if (!node) return SYMBOL_KIND_UNKNOWN;

    node = strip_aliases(node);
    if (!node) return SYMBOL_KIND_UNKNOWN;

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

static u32 compilation_log_emit_symbol_with_kind(Context *context, Scope *scope, char *name, OnyxFilePos pos, AstNode *node, u32 kind, b32 dedup) {
    if (!scope || !name || !*name) return 0;
    if (!strcmp(name, "_")) return 0;

    CompilationLog *log = &context->compilation_log;
    if (dedup && node && log->node_to_symbol_id.hashes && bh_imap_has(&log->node_to_symbol_id, (u64) node)) {
        return (u32) bh_imap_get(&log->node_to_symbol_id, (u64) node);
    }

    CompilationEvent ev;
    memset(&ev, 0, sizeof(ev));
    ev.kind = COMPILATION_EVENT_SYMBOL;
    ev.symbol.id = ++log->next_symbol_id;
    ev.symbol.scope_id = (u32)scope->id;
    ev.symbol.name = compilation_log_intern(context, name);
    ev.symbol.file_id = compilation_log_file_id_for(context, pos.filename);
    ev.symbol.line = pos.line;
    ev.symbol.column = pos.column;
    ev.symbol.kind = kind;

    if (node) {
        if (!bh_imap_has(&log->node_to_symbol_id, (u64) node)) {
            bh_imap_put(&log->node_to_symbol_id, (u64) node, ev.symbol.id);
        }
        AstNode *inner = strip_aliases(node);
        if (inner && inner != node && !bh_imap_has(&log->node_to_symbol_id, (u64) inner)) {
            bh_imap_put(&log->node_to_symbol_id, (u64) inner, ev.symbol.id);
        }
    }

    compilation_log_push(context, ev);
    return ev.symbol.id;
}

void compilation_log_emit_symbol(Context *context, Scope *scope, char *name, OnyxFilePos pos, AstNode *node) {
    compilation_log_emit_symbol_with_kind(context, scope, name, pos, node, compilation_log_symbol_kind(node), 0);
}

static u32 compilation_log_emit_member_symbol(Context *context, Scope *scope, char *name, OnyxFilePos pos, AstNode *node) {
    return compilation_log_emit_symbol_with_kind(context, scope, name, pos, node, SYMBOL_KIND_MEMBER, 1);
}

static b32 compilation_log_lookup_symbol_id(CompilationLog *log, AstNode *node, u32 *out);

void compilation_log_emit_symbol_resolved(Context *context, AstNode *original, AstNode *resolved) {
    if (!original || !resolved || !original->token) return;

    CompilationLog *log = &context->compilation_log;
    u32 symbol_id;
    if (!compilation_log_lookup_symbol_id(log, resolved, &symbol_id)) return;

    CompilationEvent ev;
    memset(&ev, 0, sizeof(ev));
    ev.kind = COMPILATION_EVENT_SYMBOL_RESOLVED;
    ev.resolved.symbol_id = symbol_id;
    ev.resolved.file_id = compilation_log_file_id_for(context, original->token->pos.filename);
    ev.resolved.line = original->token->pos.line;
    ev.resolved.column = original->token->pos.column;
    ev.resolved.length = original->token->length;
    compilation_log_push(context, ev);
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

    AstTyped *typed = (AstTyped *) inner;
    if (typed->type) return typed->type->id;

    if (node != inner) {
        typed = (AstTyped *) node;
        if (typed->type) return typed->type->id;
    }

    return 0;
}

static b32 compilation_log_lookup_symbol_id(CompilationLog *log, AstNode *node, u32 *out) {
    if (!node || !log->node_to_symbol_id.hashes) return 0;

    if (bh_imap_has(&log->node_to_symbol_id, (u64) node)) {
        *out = (u32) bh_imap_get(&log->node_to_symbol_id, (u64) node);
        return 1;
    }

    AstNode *inner = strip_aliases(node);
    if (inner && inner != node && bh_imap_has(&log->node_to_symbol_id, (u64) inner)) {
        *out = (u32) bh_imap_get(&log->node_to_symbol_id, (u64) inner);
        return 1;
    }

    return 0;
}

static void compilation_log_map_node_binding(CompilationLog *log, AstNode *node, AstBinding *binding) {
    if (!node || !binding) return;

    bh_imap_put(&log->node_to_binding, (u64) node, (u64) binding);
    AstNode *inner = strip_aliases(node);
    if (inner && inner != node) {
        bh_imap_put(&log->node_to_binding, (u64) inner, (u64) binding);
    }
}

static AstBinding *compilation_log_lookup_binding(CompilationLog *log, AstNode *node) {
    if (!node || !log->node_to_binding.hashes) return NULL;

    if (bh_imap_has(&log->node_to_binding, (u64) node)) {
        return (AstBinding *) bh_imap_get(&log->node_to_binding, (u64) node);
    }

    AstNode *inner = strip_aliases(node);
    if (inner && inner != node && bh_imap_has(&log->node_to_binding, (u64) inner)) {
        return (AstBinding *) bh_imap_get(&log->node_to_binding, (u64) inner);
    }

    return NULL;
}

void compilation_log_emit_binding(Context *context, AstNode *node, AstBinding *binding) {
    if (!node) return;

    CompilationLog *log = &context->compilation_log;
    u32 symbol_id;
    if (!compilation_log_lookup_symbol_id(log, node, &symbol_id)) return;

    if (binding) {
        compilation_log_map_node_binding(log, node, binding);
    } else {
        binding = compilation_log_lookup_binding(log, node);
    }

    u32 type_id = compilation_log_type_id_of_node(node);

    if (bh_imap_has(&log->symbol_binding_type_id, (u64) symbol_id)) {
        u32 prev = (u32) bh_imap_get(&log->symbol_binding_type_id, (u64) symbol_id);
        if (prev == type_id) return;
        if (type_id == 0) return;
    }
    bh_imap_put(&log->symbol_binding_type_id, (u64) symbol_id, (u64) type_id);

    u32 param_count = 0;
    u32 *param_symbol_ids = NULL;
    AstNode *inner = strip_aliases(node);
    if (inner && (inner->kind == Ast_Kind_Function || inner->kind == Ast_Kind_Polymorphic_Proc)) {
        AstFunction *func = (AstFunction *) inner;
        u32 n = (u32) bh_arr_length(func->params);
        if (n > 0) {
            param_symbol_ids = bh_alloc(context->gp_alloc, sizeof(u32) * n);
            bh_arr_each(AstParam, param, func->params) {
                u32 sid = 0;
                if (param->local) {
                    compilation_log_lookup_symbol_id(log, (AstNode *) param->local, &sid);
                }
                param_symbol_ids[param_count++] = sid;
            }
        }
    }

    char *docs = "";
    if (binding) {
        if (binding->documentation_token_old && binding->documentation_token_old->text) {
            docs = compilation_log_intern_len(
                context,
                binding->documentation_token_old->text,
                binding->documentation_token_old->length
            );
        } else if (binding->documentation_string) {
            docs = compilation_log_intern(context, (char *) binding->documentation_string);
        }
    }

    CompilationEvent ev;
    memset(&ev, 0, sizeof(ev));
    ev.kind = COMPILATION_EVENT_BINDING;
    ev.binding.symbol_id = symbol_id;
    ev.binding.documentation = docs;
    ev.binding.type_id = type_id;
    ev.binding.param_count = param_count;
    ev.binding.param_symbol_ids = param_symbol_ids;
    compilation_log_push(context, ev);
}

void compilation_log_notify_node_typed(Context *context, AstNode *node) {
    compilation_log_emit_binding(context, node, NULL);
}

void compilation_log_emit_type(Context *context, Type *type) {
    if (!type) return;
    if (!context->compilation_log.events) return;

    CompilationEvent ev;
    memset(&ev, 0, sizeof(ev));
    ev.kind = COMPILATION_EVENT_TYPE;
    ev.type.id = type->id;
    ev.type.kind = compilation_log_type_kind(type->kind);
    ev.type.name = compilation_log_type_name(context, type);

    switch (type->kind) {
        case Type_Kind_Basic:
            ev.type.body.basic.basic_kind = compilation_log_basic_kind(type->Basic.kind);
            ev.type.body.basic.size = type->Basic.size;
            ev.type.body.basic.alignment = type->Basic.alignment;
            break;

        case Type_Kind_Pointer:
            ev.type.body.elem.elem_id = type->Pointer.elem ? type->Pointer.elem->id : 0;
            break;

        case Type_Kind_MultiPointer:
            ev.type.body.elem.elem_id = type->MultiPointer.elem ? type->MultiPointer.elem->id : 0;
            break;

        case Type_Kind_Slice:
            ev.type.body.elem.elem_id = type->Slice.elem ? type->Slice.elem->id : 0;
            break;

        case Type_Kind_DynArray:
            ev.type.body.elem.elem_id = type->DynArray.elem ? type->DynArray.elem->id : 0;
            break;

        case Type_Kind_VarArgs:
            ev.type.body.elem.elem_id = type->VarArgs.elem ? type->VarArgs.elem->id : 0;
            break;

        case Type_Kind_Array:
            ev.type.body.array.elem_id = type->Array.elem ? type->Array.elem->id : 0;
            ev.type.body.array.count = type->Array.count;
            break;

        case Type_Kind_Function: {
            u32 n = type->Function.param_count;
            ev.type.body.function.return_id = type->Function.return_type ? type->Function.return_type->id : 0;
            ev.type.body.function.param_count = n;
            if (n > 0) {
                ev.type.body.function.param_ids = bh_alloc(context->gp_alloc, sizeof(u32) * n);
                for (u32 i = 0; i < n; i++) {
                    ev.type.body.function.param_ids[i] = type->Function.params[i] ? type->Function.params[i]->id : 0;
                }
            }
            break;
        }

        case Type_Kind_Compound: {
            u32 n = type->Compound.count;
            ev.type.body.compound.count = n;
            if (n > 0) {
                ev.type.body.compound.type_ids = bh_alloc(context->gp_alloc, sizeof(u32) * n);
                for (u32 i = 0; i < n; i++) {
                    ev.type.body.compound.type_ids[i] = type->Compound.types[i] ? type->Compound.types[i]->id : 0;
                }
            }
            break;
        }

        case Type_Kind_Distinct:
            ev.type.body.distinct.base_id = type->Distinct.base_type ? type->Distinct.base_type->id : 0;
            break;

        case Type_Kind_Enum:
            ev.type.body.enumeration.backing_id = type->Enum.backing ? type->Enum.backing->id : 0;
            ev.type.body.enumeration.is_flags = type->Enum.is_flags;
            if (type->ast_type && type->ast_type->kind == Ast_Kind_Enum_Type) {
                AstEnumType *en = (AstEnumType *) type->ast_type;
                u32 n = 0;
                bh_arr_each(AstEnumValue *, pvalue, en->values) {
                    if ((*pvalue)->flags & Ast_Flag_Has_Been_Checked) n++;
                }

                ev.type.body.enumeration.value_count = n;
                if (n > 0) {
                    ev.type.body.enumeration.values = bh_alloc(context->gp_alloc, sizeof(CompilationTypeEnumValue) * n);
                    u32 i = 0;
                    bh_arr_each(AstEnumValue *, pvalue, en->values) {
                        AstEnumValue *value = *pvalue;
                        if (!(value->flags & Ast_Flag_Has_Been_Checked)) continue;

                        CompilationTypeEnumValue *out = &ev.type.body.enumeration.values[i++];
                        out->name = value->token
                            ? compilation_log_intern_len(context, value->token->text, value->token->length)
                            : "";
                        out->value = value->value ? get_expression_integer_value(context, value->value, NULL) : 0;
                        out->symbol_id = 0;
                        compilation_log_lookup_symbol_id(&context->compilation_log, (AstNode *) value, &out->symbol_id);
                    }
                }
            }
            break;

        default:
            break;
    }

    compilation_log_push(context, ev);

    if (type->kind != Type_Kind_Struct && type->kind != Type_Kind_Union) {
        compilation_log_emit_type_body(context, type);
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

    CompilationEvent ev;
    memset(&ev, 0, sizeof(ev));
    ev.kind = COMPILATION_EVENT_TYPE_BODY;
    ev.type_body.type_id = type->id;

    if (type->kind == Type_Kind_Union) {
        ev.type_body.is_union = 1;
        ev.type_body.size = type->Union.size;
        ev.type_body.alignment = type->Union.alignment;
        ev.type_body.scope_id = type->Union.scope ? (u32) type->Union.scope->id : 0;
        ev.type_body.tag_type_id = type->Union.tag_type ? type->Union.tag_type->id : 0;

        u32 n = (u32) bh_arr_length(type->Union.variants_ordered);
        ev.type_body.variant_count = n;
        if (n > 0) {
            ev.type_body.variants = bh_alloc(context->gp_alloc, sizeof(CompilationTypeVariant) * n);
            for (u32 i = 0; i < n; i++) {
                UnionVariant *uv = type->Union.variants_ordered[i];
                ev.type_body.variants[i].name = compilation_log_intern(context, uv && uv->name ? uv->name : "");
                ev.type_body.variants[i].type_id = uv && uv->type ? uv->type->id : 0;
                ev.type_body.variants[i].tag = uv ? uv->tag_value : 0;
                ev.type_body.variants[i].symbol_id = 0;
                if (uv && uv->variant_node && type->Union.scope) {
                    OnyxFilePos pos = uv->token ? uv->token->pos : (OnyxFilePos){0};
                    ev.type_body.variants[i].symbol_id = compilation_log_emit_member_symbol(
                        context,
                        type->Union.scope,
                        uv->name,
                        pos,
                        (AstNode *) uv->variant_node
                    );
                }
            }
        }
    } else if (type->kind == Type_Kind_Struct) {
        ev.type_body.size = type->Struct.size;
        ev.type_body.alignment = type->Struct.alignment;
        ev.type_body.scope_id = type->Struct.scope ? (u32) type->Struct.scope->id : 0;

        u32 n = (u32) bh_arr_length(type->Struct.memarr);
        ev.type_body.member_count = n;
        if (n > 0) {
            ev.type_body.members = bh_alloc(context->gp_alloc, sizeof(CompilationTypeMember) * n);
            for (u32 i = 0; i < n; i++) {
                StructMember *smem = type->Struct.memarr[i];
                ev.type_body.members[i].name = compilation_log_intern(context, smem && smem->name ? smem->name : "");
                ev.type_body.members[i].type_id = smem && smem->type ? smem->type->id : 0;
                ev.type_body.members[i].offset = smem ? smem->offset : 0;
                ev.type_body.members[i].idx = smem ? smem->idx : 0;
                ev.type_body.members[i].symbol_id = 0;
                if (smem && smem->member_node && type->Struct.scope) {
                    OnyxFilePos pos = smem->token ? smem->token->pos : (OnyxFilePos){0};
                    ev.type_body.members[i].symbol_id = compilation_log_emit_member_symbol(
                        context,
                        type->Struct.scope,
                        smem->name,
                        pos,
                        (AstNode *) smem->member_node
                    );
                }
            }
        }
    } else {
        Scope *scope = compilation_log_type_method_scope(type);
        if (!scope) return;

        ev.type_body.size = type_size_of(type);
        ev.type_body.alignment = type_alignment_of(type);
        ev.type_body.scope_id = (u32) scope->id;
    }

    compilation_log_push(context, ev);
}

void compilation_log_init(Context *context) {
    CompilationLog *log = &context->compilation_log;
    bh_arr_new(context->gp_alloc, log->events, 8192);
    sh_new_arena(log->file_ids);
    sh_new_arena(log->interned_names);
    bh_buffer_init(&log->encoded, context->gp_alloc, 64 * 1024);
    bh_buffer_init(&log->pb_event, context->gp_alloc, 256);
    bh_buffer_init(&log->pb_field, context->gp_alloc, 256);
    bh_buffer_init(&log->pb_sub, context->gp_alloc, 256);
    bh_buffer_init(&log->pb_item, context->gp_alloc, 128);
    bh_buffer_init(&log->pb_flush, context->gp_alloc, 64 * 1024);
    bh_imap_init(&log->node_to_symbol_id, context->gp_alloc, 255);
    bh_imap_init(&log->node_to_binding, context->gp_alloc, 255);
    bh_imap_init(&log->symbol_binding_type_id, context->gp_alloc, 255);
    compilation_log_emit_start(context);
}
