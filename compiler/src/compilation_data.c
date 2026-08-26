#include "astnodes.h"
#include "pb.h"

#include <string.h>

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

#define PB_EVENT_SCOPE_ID               1
#define PB_EVENT_SCOPE_PARENT_SCOPE_ID  2
#define PB_EVENT_SCOPE_FILE_ID          3
#define PB_EVENT_SCOPE_LINE             4
#define PB_EVENT_SCOPE_COLUMN           5
#define PB_EVENT_SCOPE_NAME             6

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

#define PB_EVENT_BINDING_SYMBOL_ID     1
#define PB_EVENT_BINDING_DOCUMENTATION 2
#define PB_EVENT_BINDING_TYPE_ID       3

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

#define PB_EVENT_TYPE_BODY_TYPE_ID    1
#define PB_EVENT_TYPE_BODY_SIZE       2
#define PB_EVENT_TYPE_BODY_ALIGNMENT  3
#define PB_EVENT_TYPE_BODY_SCOPE_ID   4
#define PB_EVENT_TYPE_BODY_STRUCT     10
#define PB_EVENT_TYPE_BODY_UNION      11

#define PB_TYPE_STRUCT_BODY_MEMBERS 1
#define PB_TYPE_STRUCT_MEMBER_NAME    1
#define PB_TYPE_STRUCT_MEMBER_TYPE_ID 2
#define PB_TYPE_STRUCT_MEMBER_OFFSET  3
#define PB_TYPE_STRUCT_MEMBER_IDX     4

#define PB_TYPE_UNION_BODY_TAG_TYPE_ID 1
#define PB_TYPE_UNION_BODY_VARIANTS    2
#define PB_TYPE_UNION_VARIANT_NAME     1
#define PB_TYPE_UNION_VARIANT_TYPE_ID  2
#define PB_TYPE_UNION_VARIANT_TAG      3

static b32 compilation_event_is_detailed(CompilationEventKind kind) {
    return kind == COMPILATION_EVENT_SYMBOL_RESOLVED
        || kind == COMPILATION_EVENT_BINDING
        || kind == COMPILATION_EVENT_TYPE
        || kind == COMPILATION_EVENT_TYPE_BODY;
}

static void compilation_log_push(Context *context, CompilationEvent event) {
    event.timestamp = bh_time_curr();
    bh_arr_push(context->compilation_log.events, event);
    context->compilation_log.encoded_valid = 0;
}

static void encode_event(CompilationEvent *ev, bh_buffer *out) {
    pb_write_optional_uint64(out, PB_COMPILER_EVENT_TIMESTAMP, ev->timestamp);

    bh_buffer inner;
    bh_buffer_init(&inner, out->allocator, 64);

    switch (ev->kind) {
        case COMPILATION_EVENT_START:
            pb_write_string(&inner, PB_EVENT_START_COMPILER_VERSION, ev->start.compiler_version);
            pb_write_bytes(out, PB_COMPILER_EVENT_START, inner.data, inner.length);
            break;

        case COMPILATION_EVENT_FILE:
            pb_write_uint32(&inner, PB_EVENT_FILE_ID, ev->file.id);
            pb_write_string(&inner, PB_EVENT_FILE_PATH, ev->file.path);
            pb_write_bytes(out, PB_COMPILER_EVENT_FILE, inner.data, inner.length);
            break;

        case COMPILATION_EVENT_PACKAGE:
            pb_write_uint32(&inner, PB_EVENT_PACKAGE_ID, ev->pack.id);
            pb_write_string(&inner, PB_EVENT_PACKAGE_NAME, ev->pack.package_name);
            pb_write_string(&inner, PB_EVENT_PACKAGE_FULLY_QUALIFIED_NAME, ev->pack.fully_qualified_name);
            pb_write_uint32(&inner, PB_EVENT_PACKAGE_PARENT_ID, ev->pack.parent_package_id);
            pb_write_bytes(out, PB_COMPILER_EVENT_PACK, inner.data, inner.length);
            break;

        case COMPILATION_EVENT_SCOPE:
            pb_write_uint32(&inner, PB_EVENT_SCOPE_ID, ev->scope.id);
            pb_write_uint32(&inner, PB_EVENT_SCOPE_PARENT_SCOPE_ID, ev->scope.parent_scope_id);
            pb_write_uint32(&inner, PB_EVENT_SCOPE_FILE_ID, ev->scope.file_id);
            pb_write_uint32(&inner, PB_EVENT_SCOPE_LINE, ev->scope.line);
            pb_write_uint32(&inner, PB_EVENT_SCOPE_COLUMN, ev->scope.column);
            pb_write_string(&inner, PB_EVENT_SCOPE_NAME, ev->scope.name);
            pb_write_bytes(out, PB_COMPILER_EVENT_SCOPE, inner.data, inner.length);
            break;

        case COMPILATION_EVENT_SYMBOL:
            pb_write_uint32(&inner, PB_EVENT_SYMBOL_ID, ev->symbol.id);
            pb_write_uint32(&inner, PB_EVENT_SYMBOL_SCOPE_ID, ev->symbol.scope_id);
            pb_write_string(&inner, PB_EVENT_SYMBOL_NAME, ev->symbol.name);
            pb_write_uint32(&inner, PB_EVENT_SYMBOL_FILE_ID, ev->symbol.file_id);
            pb_write_uint32(&inner, PB_EVENT_SYMBOL_LINE, ev->symbol.line);
            pb_write_uint32(&inner, PB_EVENT_SYMBOL_COLUMN, ev->symbol.column);
            pb_write_uint32(&inner, PB_EVENT_SYMBOL_KIND, ev->symbol.kind);
            pb_write_bytes(out, PB_COMPILER_EVENT_SYMBOL, inner.data, inner.length);
            break;

        case COMPILATION_EVENT_SYMBOL_RESOLVED:
            pb_write_uint32(&inner, PB_EVENT_RESOLVED_SYMBOL_ID, ev->resolved.symbol_id);
            pb_write_uint32(&inner, PB_EVENT_RESOLVED_FILE_ID, ev->resolved.file_id);
            pb_write_uint32(&inner, PB_EVENT_RESOLVED_LINE, ev->resolved.line);
            pb_write_uint32(&inner, PB_EVENT_RESOLVED_COLUMN, ev->resolved.column);
            pb_write_uint32(&inner, PB_EVENT_RESOLVED_LENGTH, ev->resolved.length);
            pb_write_bytes(out, PB_COMPILER_EVENT_RESOLVED, inner.data, inner.length);
            break;

        case COMPILATION_EVENT_BINDING:
            pb_write_uint32(&inner, PB_EVENT_BINDING_SYMBOL_ID, ev->binding.symbol_id);
            pb_write_string(&inner, PB_EVENT_BINDING_DOCUMENTATION, ev->binding.documentation);
            pb_write_uint32(&inner, PB_EVENT_BINDING_TYPE_ID, ev->binding.type_id);
            pb_write_bytes(out, PB_COMPILER_EVENT_BINDING, inner.data, inner.length);
            break;

        case COMPILATION_EVENT_TYPE: {
            pb_write_uint32(&inner, PB_EVENT_TYPE_ID, ev->type.id);
            pb_write_uint32(&inner, PB_EVENT_TYPE_KIND, ev->type.kind);
            pb_write_string(&inner, PB_EVENT_TYPE_NAME, ev->type.name);

            bh_buffer body;
            bh_buffer_init(&body, out->allocator, 32);

            switch ((TypeKind) ev->type.kind) {
                case Type_Kind_Basic:
                    pb_write_uint32(&body, PB_TYPE_BASIC_KIND, ev->type.body.basic.basic_kind);
                    pb_write_uint32(&body, PB_TYPE_BASIC_SIZE, ev->type.body.basic.size);
                    pb_write_uint32(&body, PB_TYPE_BASIC_ALIGNMENT, ev->type.body.basic.alignment);
                    pb_write_bytes(&inner, PB_EVENT_TYPE_BASIC, body.data, body.length);
                    break;

                case Type_Kind_Pointer:
                case Type_Kind_MultiPointer:
                case Type_Kind_Slice:
                case Type_Kind_DynArray:
                case Type_Kind_VarArgs:
                    pb_write_uint32(&body, PB_TYPE_ELEM_ELEM_ID, ev->type.body.elem.elem_id);
                    pb_write_bytes(&inner, PB_EVENT_TYPE_ELEM, body.data, body.length);
                    break;

                case Type_Kind_Array:
                    pb_write_uint32(&body, PB_TYPE_ARRAY_ELEM_ID, ev->type.body.array.elem_id);
                    pb_write_uint32(&body, PB_TYPE_ARRAY_COUNT, ev->type.body.array.count);
                    pb_write_bytes(&inner, PB_EVENT_TYPE_ARRAY, body.data, body.length);
                    break;

                case Type_Kind_Function:
                    pb_write_uint32(&body, PB_TYPE_FUNCTION_RETURN_ID, ev->type.body.function.return_id);
                    for (u32 i = 0; i < ev->type.body.function.param_count; i++) {
                        pb_write_uint32_always(&body, PB_TYPE_FUNCTION_PARAM_IDS, ev->type.body.function.param_ids[i]);
                    }
                    pb_write_bytes(&inner, PB_EVENT_TYPE_FUNCTION, body.data, body.length);
                    break;

                case Type_Kind_Compound:
                    for (u32 i = 0; i < ev->type.body.compound.count; i++) {
                        pb_write_uint32_always(&body, PB_TYPE_COMPOUND_TYPE_IDS, ev->type.body.compound.type_ids[i]);
                    }
                    pb_write_bytes(&inner, PB_EVENT_TYPE_COMPOUND, body.data, body.length);
                    break;

                case Type_Kind_Distinct:
                    pb_write_uint32(&body, PB_TYPE_DISTINCT_BASE_ID, ev->type.body.distinct.base_id);
                    pb_write_bytes(&inner, PB_EVENT_TYPE_DISTINCT, body.data, body.length);
                    break;

                case Type_Kind_Enum:
                    pb_write_uint32(&body, PB_TYPE_ENUM_BACKING_ID, ev->type.body.enumeration.backing_id);
                    if (ev->type.body.enumeration.is_flags) {
                        pb_write_uint32_always(&body, PB_TYPE_ENUM_IS_FLAGS, 1);
                    }
                    pb_write_bytes(&inner, PB_EVENT_TYPE_ENUMERATION, body.data, body.length);
                    break;

                default:
                    break;
            }

            bh_buffer_free(&body);
            pb_write_bytes(out, PB_COMPILER_EVENT_TYPE, inner.data, inner.length);
            break;
        }

        case COMPILATION_EVENT_TYPE_BODY: {
            pb_write_uint32(&inner, PB_EVENT_TYPE_BODY_TYPE_ID, ev->type_body.type_id);
            pb_write_uint32(&inner, PB_EVENT_TYPE_BODY_SIZE, ev->type_body.size);
            pb_write_uint32(&inner, PB_EVENT_TYPE_BODY_ALIGNMENT, ev->type_body.alignment);
            pb_write_uint32(&inner, PB_EVENT_TYPE_BODY_SCOPE_ID, ev->type_body.scope_id);

            bh_buffer details;
            bh_buffer_init(&details, out->allocator, 64);

            if (ev->type_body.is_union) {
                pb_write_uint32(&details, PB_TYPE_UNION_BODY_TAG_TYPE_ID, ev->type_body.tag_type_id);
                for (u32 i = 0; i < ev->type_body.variant_count; i++) {
                    CompilationTypeVariant *v = &ev->type_body.variants[i];
                    bh_buffer var;
                    bh_buffer_init(&var, out->allocator, 32);
                    pb_write_string(&var, PB_TYPE_UNION_VARIANT_NAME, v->name);
                    pb_write_uint32(&var, PB_TYPE_UNION_VARIANT_TYPE_ID, v->type_id);
                    pb_write_uint32_always(&var, PB_TYPE_UNION_VARIANT_TAG, v->tag);
                    pb_write_bytes(&details, PB_TYPE_UNION_BODY_VARIANTS, var.data, var.length);
                    bh_buffer_free(&var);
                }
                pb_write_bytes(&inner, PB_EVENT_TYPE_BODY_UNION, details.data, details.length);
            } else {
                for (u32 i = 0; i < ev->type_body.member_count; i++) {
                    CompilationTypeMember *m = &ev->type_body.members[i];
                    bh_buffer mem;
                    bh_buffer_init(&mem, out->allocator, 32);
                    pb_write_string(&mem, PB_TYPE_STRUCT_MEMBER_NAME, m->name);
                    pb_write_uint32(&mem, PB_TYPE_STRUCT_MEMBER_TYPE_ID, m->type_id);
                    pb_write_uint32_always(&mem, PB_TYPE_STRUCT_MEMBER_OFFSET, m->offset);
                    pb_write_uint32_always(&mem, PB_TYPE_STRUCT_MEMBER_IDX, m->idx);
                    pb_write_bytes(&details, PB_TYPE_STRUCT_BODY_MEMBERS, mem.data, mem.length);
                    bh_buffer_free(&mem);
                }
                pb_write_bytes(&inner, PB_EVENT_TYPE_BODY_STRUCT, details.data, details.length);
            }

            bh_buffer_free(&details);
            pb_write_bytes(out, PB_COMPILER_EVENT_TYPE_BODY, inner.data, inner.length);
            break;
        }
    }

    bh_buffer_free(&inner);
}

static void encode_event_into(CompilationEvent *ev, bh_buffer *out) {
    bh_buffer inner;
    bh_buffer_init(&inner, out->allocator, 64);
    encode_event(ev, &inner);
    pb_write_bytes(out, PB_COMPILER_EVENTS_EVENTS, inner.data, inner.length);
    bh_buffer_free(&inner);
}

void compilation_log_encode_range(Context *context, u32 from, u32 to, bh_buffer *out, b32 include_detailed) {
    u32 count = (u32)bh_arr_length(context->compilation_log.events);
    if (from > count) from = count;
    if (to > count) to = count;

    for (u32 i = from; i < to; i++) {
        CompilationEvent *ev = &context->compilation_log.events[i];
        if (!include_detailed && compilation_event_is_detailed(ev->kind)) continue;
        encode_event_into(ev, out);
    }
}

static void compilation_log_encode_detailed_backfill(Context *context, u32 until, bh_buffer *out) {
    u32 count = (u32)bh_arr_length(context->compilation_log.events);
    if (until > count) until = count;

    for (u32 i = 0; i < until; i++) {
        CompilationEvent *ev = &context->compilation_log.events[i];
        if (!compilation_event_is_detailed(ev->kind)) continue;
        encode_event_into(ev, out);
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
    if (!log->file_ids) return 0;

    i32 index = shgeti(log->file_ids, (char *)filename);
    if (index == -1) return 0;
    return log->file_ids[index].value;
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
    ev.scope.name = scope->name ? scope->name : "";
    compilation_log_push(context, ev);
}

void compilation_log_emit_symbol(Context *context, Scope *scope, char *name, OnyxFilePos pos, AstNode *node) {
    if (!scope || !name || !*name) return;
    if (!strcmp(name, "_")) return;

    CompilationEvent ev;
    memset(&ev, 0, sizeof(ev));
    ev.kind = COMPILATION_EVENT_SYMBOL;
    ev.symbol.id = ++context->compilation_log.next_symbol_id;
    ev.symbol.scope_id = (u32)scope->id;
    ev.symbol.name = bh_strdup(context->gp_alloc, name);
    ev.symbol.file_id = compilation_log_file_id_for(context, pos.filename);
    ev.symbol.line = pos.line;
    ev.symbol.column = pos.column;
    ev.symbol.kind = node ? (u32)node->kind : 0;

    if (node) {
        CompilationLog *log = &context->compilation_log;
        bh_imap_put(&log->node_to_symbol_id, (u64) node, ev.symbol.id);
        AstNode *inner = strip_aliases(node);
        if (inner && inner != node) {
            bh_imap_put(&log->node_to_symbol_id, (u64) inner, ev.symbol.id);
        }
    }

    compilation_log_push(context, ev);
}

void compilation_log_emit_symbol_resolved(Context *context, AstNode *original, AstNode *resolved) {
    if (!original || !resolved || !original->token) return;

    CompilationLog *log = &context->compilation_log;
    if (!log->node_to_symbol_id.hashes) return;
    if (!bh_imap_has(&log->node_to_symbol_id, (u64) resolved)) return;

    CompilationEvent ev;
    memset(&ev, 0, sizeof(ev));
    ev.kind = COMPILATION_EVENT_SYMBOL_RESOLVED;
    ev.resolved.symbol_id = (u32) bh_imap_get(&log->node_to_symbol_id, (u64) resolved);
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
        if (t->type && t->type->id) return t->type->id;
        if (t->type_id) return (u32) t->type_id;

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

    char *docs = "";
    if (binding) {
        if (binding->documentation_token_old && binding->documentation_token_old->text) {
            docs = bh_strdup_len(
                context->gp_alloc,
                binding->documentation_token_old->text,
                binding->documentation_token_old->length
            );
        } else if (binding->documentation_string) {
            docs = bh_strdup(context->gp_alloc, (char *) binding->documentation_string);
        }
    }

    CompilationEvent ev;
    memset(&ev, 0, sizeof(ev));
    ev.kind = COMPILATION_EVENT_BINDING;
    ev.binding.symbol_id = symbol_id;
    ev.binding.documentation = docs;
    ev.binding.type_id = type_id;
    compilation_log_push(context, ev);
}

void compilation_log_notify_node_typed(Context *context, AstNode *node) {
    compilation_log_emit_binding(context, node, NULL);
}

void compilation_log_emit_type(Context *context, Type *type) {
    if (!type) return;
    if (!context->compilation_log.events) return;

    const char *name = type_get_name(context, type);

    CompilationEvent ev;
    memset(&ev, 0, sizeof(ev));
    ev.kind = COMPILATION_EVENT_TYPE;
    ev.type.id = type->id;
    ev.type.kind = (u32) type->kind;
    ev.type.name = bh_strdup(context->gp_alloc, (char *)(name ? name : ""));

    switch (type->kind) {
        case Type_Kind_Basic:
            ev.type.body.basic.basic_kind = (u32) type->Basic.kind;
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
            break;

        default:
            break;
    }

    compilation_log_push(context, ev);
}

void compilation_log_emit_type_body(Context *context, Type *type) {
    if (!type) return;
    if (!context->compilation_log.events) return;
    if (type->kind != Type_Kind_Struct && type->kind != Type_Kind_Union) return;

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
                ev.type_body.variants[i].name = bh_strdup(context->gp_alloc, uv && uv->name ? uv->name : "");
                ev.type_body.variants[i].type_id = uv && uv->type ? uv->type->id : 0;
                ev.type_body.variants[i].tag = uv ? uv->tag_value : 0;
            }
        }
    } else {
        ev.type_body.size = type->Struct.size;
        ev.type_body.alignment = type->Struct.alignment;
        ev.type_body.scope_id = type->Struct.scope ? (u32) type->Struct.scope->id : 0;

        u32 n = (u32) bh_arr_length(type->Struct.memarr);
        ev.type_body.member_count = n;
        if (n > 0) {
            ev.type_body.members = bh_alloc(context->gp_alloc, sizeof(CompilationTypeMember) * n);
            for (u32 i = 0; i < n; i++) {
                StructMember *smem = type->Struct.memarr[i];
                ev.type_body.members[i].name = bh_strdup(context->gp_alloc, smem && smem->name ? smem->name : "");
                ev.type_body.members[i].type_id = smem && smem->type ? smem->type->id : 0;
                ev.type_body.members[i].offset = smem ? smem->offset : 0;
                ev.type_body.members[i].idx = smem ? smem->idx : 0;
            }
        }
    }

    compilation_log_push(context, ev);
}

void compilation_log_init(Context *context) {
    CompilationLog *log = &context->compilation_log;
    bh_arr_new(context->gp_alloc, log->events, 32);
    sh_new_arena(log->file_ids);
    bh_imap_init(&log->node_to_symbol_id, context->gp_alloc, 255);
    bh_imap_init(&log->node_to_binding, context->gp_alloc, 255);
    bh_imap_init(&log->symbol_binding_type_id, context->gp_alloc, 255);
    compilation_log_emit_start(context);
}
