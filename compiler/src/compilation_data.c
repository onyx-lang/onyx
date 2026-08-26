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
    }

    bh_buffer_free(&inner);
}

void compilation_log_encode_range(Context *context, u32 from, u32 to, bh_buffer *out) {
    u32 count = (u32)bh_arr_length(context->compilation_log.events);
    if (from > count) from = count;
    if (to > count) to = count;

    for (u32 i = from; i < to; i++) {
        bh_buffer inner;
        bh_buffer_init(&inner, out->allocator, 64);
        encode_event(&context->compilation_log.events[i], &inner);
        pb_write_bytes(out, PB_COMPILER_EVENTS_EVENTS, inner.data, inner.length);
        bh_buffer_free(&inner);
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
    compilation_log_push(context, ev);
}

void compilation_log_init(Context *context) {
    CompilationLog *log = &context->compilation_log;
    bh_arr_new(context->gp_alloc, log->events, 32);
    sh_new_arena(log->file_ids);
    compilation_log_emit_start(context);
}
