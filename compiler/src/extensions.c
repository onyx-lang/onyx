
#include "astnodes.h"
#include "parser.h"
#include "utils.h"

#include <stdlib.h>
#include <string.h>

#define MSG_HOST_INIT              0
#define MSG_HOST_TERMINATE         1
#define MSG_HOST_EXPAND_MACRO      2
#define MSG_HOST_HOOK              3
#define MSG_HOST_COMPILATION_DATA  4

#define MSG_EXT_INIT             0
#define MSG_EXT_ERROR_REPORT     1
#define MSG_EXT_EXPANSION        2
#define MSG_EXT_INJECT_CODE      3
#define MSG_EXT_ACKNOWLEDGE_HOOK 4

#define HOOK_STALLED                      1
#define HOOK_COMPILATION_DATA             2
#define HOOK_COMPILATION_DATA_DETAILED    3
#define HOOK_ALL_CODE_CHECKED            4


#if defined(_BH_LINUX) || defined(_BH_DARWIN)
#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <sys/socket.h>
#include <sys/wait.h>
#include <sys/poll.h>
#include <unistd.h>

#define EXT_IO_BUF (1 << 20)

static void extension_set_sock_buf(int fd) {
    int sz = EXT_IO_BUF;
    setsockopt(fd, SOL_SOCKET, SO_SNDBUF, &sz, sizeof(sz));
    setsockopt(fd, SOL_SOCKET, SO_RCVBUF, &sz, sizeof(sz));
#ifdef SO_NOSIGPIPE
    int one = 1;
    setsockopt(fd, SOL_SOCKET, SO_NOSIGPIPE, &one, sizeof(one));
#endif
}

static b32 extension_make_channel(i32 fds[2]) {
    if (socketpair(AF_UNIX, SOCK_STREAM, 0, fds) == 0) {
        extension_set_sock_buf(fds[0]);
        extension_set_sock_buf(fds[1]);
        return 1;
    }

    if (pipe(fds) != 0) return 0;
#ifdef F_SETPIPE_SZ
    fcntl(fds[0], F_SETPIPE_SZ, EXT_IO_BUF);
    fcntl(fds[1], F_SETPIPE_SZ, EXT_IO_BUF);
#endif
    return 1;
}

static void extension_set_nonblock(int fd, b32 nb) {
    int flags = fcntl(fd, F_GETFL, 0);
    if (flags < 0) return;
    if (nb) fcntl(fd, F_SETFL, flags | O_NONBLOCK);
    else    fcntl(fd, F_SETFL, flags & ~O_NONBLOCK);
}

static void extension_try_flush(CompilerExtension *ext) {
    if (!ext->alive || !ext->outgoing.data) return;

    while (ext->outgoing_off < ext->outgoing.length) {
        i32 remain = ext->outgoing.length - ext->outgoing_off;
        i32 w = write((int) ext->send_file, ext->outgoing.data + ext->outgoing_off, remain);
        if (w > 0) {
            ext->outgoing_off += w;
            continue;
        }
        if (w < 0 && (errno == EAGAIN || errno == EWOULDBLOCK || errno == EINTR)) {
            break;
        }
        ext->alive = 0;
        break;
    }

    if (ext->outgoing_off >= ext->outgoing.length) {
        bh_buffer_clear(&ext->outgoing);
        ext->outgoing_off = 0;
    } else if (ext->outgoing_off > 64 * 1024) {
        i32 remain = ext->outgoing.length - ext->outgoing_off;
        if (remain > 0) {
            memmove(ext->outgoing.data, ext->outgoing.data + ext->outgoing_off, remain);
        }
        ext->outgoing.length = remain;
        ext->outgoing_off = 0;
    }
}

static void extension_drain(CompilerExtension *ext) {
    if (!ext->alive || !ext->outgoing.data) return;

    extension_set_nonblock((int) ext->send_file, 0);
    while (ext->outgoing_off < ext->outgoing.length) {
        i32 remain = ext->outgoing.length - ext->outgoing_off;
        i32 w = write((int) ext->send_file, ext->outgoing.data + ext->outgoing_off, remain);
        if (w > 0) {
            ext->outgoing_off += w;
            continue;
        }
        ext->alive = 0;
        break;
    }
    bh_buffer_clear(&ext->outgoing);
    ext->outgoing_off = 0;
    if (ext->alive) extension_set_nonblock((int) ext->send_file, 1);
}

static void extension_send(CompilerExtension *ext, void *data, i32 len) {
    if (!ext->alive) return;
    if (!ext->outgoing.data) return;

    bh_buffer_append(&ext->outgoing, data, len);
    extension_try_flush(ext);
}

static b32 extension_poll_recv(CompilerExtension *ext) {
    struct pollfd fd;
    fd.events = POLL_IN;
    fd.fd = ext->recv_file;
    fd.revents = 0;

    if (poll(&fd, 1, 0) < 0) return 0;
    if (fd.revents & (POLLERR | POLLHUP | POLLNVAL)) return 1;
    return (fd.revents & POLL_IN) != 0;
}

static i32 extension_recv(CompilerExtension *ext, void *buf, i32 maxlen) {
    if (!ext->alive) return 0;

    i32 bytes_read = read(ext->recv_file, buf, maxlen);
    if (bytes_read < 0) {
        ext->alive = 0;
        return 0;
    }
    return bytes_read;
}

static void extension_wait(CompilerExtension *ext) {
    int status;
    waitpid(ext->pid, &status, 0);
}

static void extension_kill(CompilerExtension *ext) {
    ext->alive = 0;
    kill(ext->pid, SIGKILL);
    extension_wait(ext);
}

static b32 extension_spawn(CompilerExtension *ext, const char *path) {
    i32 ext_to_comp[2];
    i32 comp_to_ext[2];

    if (!extension_make_channel(ext_to_comp) || !extension_make_channel(comp_to_ext)) {
        return 0;
    }

    u32 pid;
    switch (pid = fork()) {
        case -1:
            close(ext_to_comp[0]);
            close(ext_to_comp[1]);
            close(comp_to_ext[0]);
            close(comp_to_ext[1]);
            return 0;

        case 0:
            close(ext_to_comp[0]);
            close(comp_to_ext[1]);

            dup2(comp_to_ext[0], 0);
            dup2(ext_to_comp[1], 1);
            if (comp_to_ext[0] != 0) close(comp_to_ext[0]);
            if (ext_to_comp[1] != 1) close(ext_to_comp[1]);

            execlp("onyx", "onyx", "run", "--no-compiler-extensions", "--no-file-contents", path, NULL);
            exit(1);
            break;

        default:
            close(ext_to_comp[1]);
            close(comp_to_ext[0]);
            break;
    }

    ext->pid = pid;
    ext->send_file = comp_to_ext[1];
    ext->recv_file = ext_to_comp[0];
    extension_set_nonblock((int) ext->send_file, 1);
    return 1;
}

#endif

#ifdef _BH_WINDOWS

static void extension_try_flush(CompilerExtension *ext) {
}

static void extension_drain(CompilerExtension *ext) {
}

static void extension_send(CompilerExtension *ext, void *data, i32 len) {
}

static i32 extension_recv(CompilerExtension *ext, void *buf, i32 maxlen) {
    return 0;
}

static b32 extension_poll_recv(CompilerExtension *ext) {
    return 0;
}

static void extension_wait(CompilerExtension *ext) {
}

static void extension_kill(CompilerExtension *ext) {
}

static b32 extension_spawn(CompilerExtension *ext, const char *path) {
    printf("Compiler extensions are currently not support on Windows. Sorry! :(\n");
    return 0;
}

#endif



static void extension_send_int(CompilerExtension *ext, int v) {
    int value = v;
    extension_send(ext, &value, sizeof(value));
}

static void extension_send_bytes(CompilerExtension *ext, void *data, i32 len) {
    extension_send_int(ext, len);
    extension_send(ext, data, len);
}

static void extension_send_str(CompilerExtension *ext, const char *msg) {
    extension_send_bytes(ext, (void *) msg, strlen(msg));
}

static i32 extension_recv_int(CompilerExtension *ext) {
    int i;
    if (extension_recv(ext, &i, sizeof(i)) < (i32) sizeof(int)) return 0;
    return i;
}

// static char *extension_recv_bytes(CompilerExtension *ext, i32 len) {
//     bh_allocator a = bh_arena_allocator(&ext->arena);
//     char *buf = bh_alloc(a, len);
// 
//     i32 bytes_read = 0;
//     while (bytes_read < len) {
//         if (!ext->alive) break;
// 
//         bytes_read += extension_recv(ext, buf + bytes_read, len);
//     }
// 
//     return buf;
// }

static char *extension_recv_str(CompilerExtension *ext, i32 *out_len) {
    i32 len = extension_recv_int(ext);
    if (out_len) *out_len = len;

    bh_allocator a = bh_arena_allocator(&ext->arena);
    char *buf = bh_alloc(a, len + 1);
    if (!buf) return NULL;

    i32 bytes_read = 0; 
    while (bytes_read < len) {
        if (!ext->alive) break;

        bytes_read += extension_recv(ext, buf + bytes_read, len);
    }

    buf[bytes_read] = '\0';
    return buf;
}

static void compiler_extension_send_compilation_data(Context *context, CompilerExtension *ext) {
    if (!ext->alive || !ext->supports_compilation_data) return;
    if (ext->state != COMP_EXT_STATE_READY) return;

    u32 from = ext->compilation_data_cursor;
    u32 to = (u32)bh_arr_length(context->compilation_log.events);
    if (from >= to) return;

    CompilationLog *log = &context->compilation_log;
    bh_buffer_clear(&log->pb_flush);
    compilation_log_encode_range(context, from, to, &log->pb_flush, ext->supports_compilation_data_detailed);

    extension_send_int(ext, MSG_HOST_COMPILATION_DATA);
    extension_send_bytes(ext, log->pb_flush.data, log->pb_flush.length);

    ext->compilation_data_cursor = to;
    if (to > log->delivered_until) log->delivered_until = to;
}

#define COMPILATION_DATA_FLUSH_BATCH 8192

void compiler_extensions_flush_compilation_data(Context *context) {
    bh_arr_each(CompilerExtension, ext, context->extensions) {
        u32 pending = (u32)bh_arr_length(context->compilation_log.events);
        if (pending > ext->compilation_data_cursor &&
            pending - ext->compilation_data_cursor >= COMPILATION_DATA_FLUSH_BATCH) {
            compiler_extension_send_compilation_data(context, ext);
        }
        extension_try_flush(ext);
    }
}

void compiler_extensions_flush_compilation_data_now(Context *context) {
    bh_arr_each(CompilerExtension, ext, context->extensions) {
        compiler_extension_send_compilation_data(context, ext);
        extension_try_flush(ext);
    }
}

static i32 extension_queued_entity_count(Context *context) {
    EntityHeap *h = &context->entities;
    i32 n = h->ready.count + h->retry_count;
    i32 heap_n = bh_arr_length(h->entities);
    if (h->detached_top) heap_n -= 1;
    if (heap_n > 0) n += heap_n;
    return n;
}

// Yield only when something else can run. If the leftover queue is just
// other extensions also waiting on their children, block instead of spinning.
static b32 extension_startup_has_other_work(Context *context) {
    i32 queued = extension_queued_entity_count(context);
    if (queued <= 0) return 0;

    i32 waiting_in_queue = 0;
    bh_arr_each(CompilerExtension, ext, context->extensions) {
        if (!ext->alive) continue;
        if (ext->state != COMP_EXT_STATE_SPAWNING && ext->state != COMP_EXT_STATE_INITIATING) continue;
        if (context->checker.current_entity && ext->entity == context->checker.current_entity) continue;
        waiting_in_queue++;
    }

    return queued > waiting_in_queue;
}

b32 compiler_extension_negotiate_capabilities(Context *context, CompilerExtension *ext);

TypeMatch compiler_extension_start(Context *context, const char *name, const char *containing_filename, Entity *ent, i32 *out_extension_id) {
    if (*out_extension_id == 0) {
        char* parent_folder = bh_path_get_parent(containing_filename, context->scratch_alloc);

        char *path = bh_strdup(
                context->scratch_alloc,
                bh_lookup_file((char *) name, parent_folder, NULL, NULL, NULL, context->scratch_alloc)
        );

        if (!bh_file_exists(path)) {
            return TYPE_MATCH_FAILED;
        }

        CompilerExtension ext;
        memset(&ext, 0, sizeof(ext));
        ext.state = COMP_EXT_STATE_SPAWNING;
        bh_arena_init(&ext.arena, context->gp_alloc, 1 * 1024 * 1024); // 1MB
        ext.entity = ent;

        if (!extension_spawn(&ext, path)) {
            return TYPE_MATCH_FAILED;
        }

        ext.alive = 1;
        ext.id = bh_arr_length(context->extensions) + 1;
        *out_extension_id = ext.id;
        bh_buffer_init(&ext.outgoing, context->gp_alloc, 64 * 1024);
        bh_arr_push(context->extensions, ext);

        // Init message is 5 ints as defined in `core/onyx/compiler_extension`
        i32 init_msg[5];
        init_msg[0] = MSG_HOST_INIT;
        init_msg[1] = VERSION_MAJOR;
        init_msg[2] = VERSION_MINOR;
        init_msg[3] = VERSION_PATCH;
        init_msg[4] = 4;

        CompilerExtension *stored = &context->extensions[ext.id - 1];
        extension_send(stored, init_msg, sizeof(init_msg));

        return TYPE_MATCH_YIELD;

    } else {
        CompilerExtension *ext = &context->extensions[*out_extension_id - 1];
        if (!ext->alive) return TYPE_MATCH_FAILED;

        extension_try_flush(ext);

        if (!extension_poll_recv(ext)) {
            if (extension_startup_has_other_work(context)) {
                return TYPE_MATCH_YIELD;
            }
        }

        ext->state = COMP_EXT_STATE_INITIATING;
        extension_drain(ext);

        if (!compiler_extension_negotiate_capabilities(context, ext)) {
            return TYPE_MATCH_FAILED;
        }

        ext->state = COMP_EXT_STATE_READY;
        compiler_extensions_flush_compilation_data_now(context);
        return TYPE_MATCH_SUCCESS;
    }
}

b32 compiler_extension_negotiate_capabilities(Context *context, CompilerExtension *ext) {
    if (extension_recv_int(ext) != MSG_EXT_INIT) {
        extension_kill(ext);
        return 0;
    }

    int extension_protocol_version = extension_recv_int(ext);
    char *extension_name           = extension_recv_str(ext, NULL);

    ext->name = bh_strdup(context->ast_alloc, extension_name);

    // handle "hooks"
    if (extension_protocol_version >= 2) {
        int hook_count = extension_recv_int(ext);
        fori (i, 0, hook_count) {
            int hook = extension_recv_int(ext);

            if (hook == HOOK_STALLED) ext->supports_stalled_hook = 1;
            if (hook == HOOK_COMPILATION_DATA) ext->supports_compilation_data = 1;
            if (hook == HOOK_COMPILATION_DATA_DETAILED) {
                ext->supports_compilation_data = 1;
                ext->supports_compilation_data_detailed = 1;
            }
            if (hook == HOOK_ALL_CODE_CHECKED) ext->supports_all_code_checked_hook = 1;
        }
    }

    compiler_event_log(
        context,
        "Extension '%s' spawned with protocol version %d.",
        ext->name, extension_protocol_version
    );
    
    bh_arena_clear(&ext->arena);
    return 1;
}


static AstNode * parse_code(Context *context, ProceduralMacroExpansionKind kind, char *code, i32 code_length, Entity *entity, OnyxFilePos pos) {
    bh_arr_push(context->injected_code_descriptors, ((InjectedCodeDesc) {
        .pos = pos,
        .code = code,
        .code_length = code_length
    }));

    bh_file_contents file_contents;
    file_contents.allocator = context->ast_alloc;
    file_contents.data = code;
    file_contents.length = code_length;
    file_contents.filename = bh_aprintf(context->ast_alloc, "(expansion from %s:%d,%d)", pos.filename, pos.line, pos.column);

    OnyxTokenizer tokenizer = onyx_tokenizer_create(context, &file_contents);
    onyx_lex_tokens(&tokenizer);

    OnyxParser parser = onyx_parser_create(context, &tokenizer);
    parser.package = entity->package;
    parser.file_scope = entity->scope;

    AstNode *result = NULL;
    switch (kind) {
        case PMEK_Expression: result = (AstNode *) onyx_parse_expression(&parser, entity->scope); break;
        case PMEK_Statement:  result = (AstNode *) onyx_parse_statement(&parser, entity->scope); break;
        case PMEK_Top_Level:  result = NULL; onyx_parse_top_level_statements(&parser, entity->scope); break;
    }

    onyx_parser_free(&parser);
    return result;
}

static b32 handle_common_messages(Context *context, CompilerExtension *ext, int msg_type, OnyxToken *tkn, Entity *entity) {
    switch (msg_type) {
        case MSG_EXT_INIT:
            extension_kill(ext);
            ONYX_ERROR(tkn->pos, Error_Critical, "Protocol error when talking to '%s'.", ext->name);
            return 0;

        case MSG_EXT_ERROR_REPORT: {
            char *filename = extension_recv_str(ext, NULL);
            u32 line = extension_recv_int(ext);
            u32 column = extension_recv_int(ext);
            u32 length = extension_recv_int(ext);
            char *msg = extension_recv_str(ext, NULL);
            if (!msg) msg = "";

            OnyxFilePos pos;
            pos.column = column + (line == tkn->pos.line ? 2 : 0);
            pos.line = line;
            pos.filename = tkn->pos.filename;
            pos.length = length;

            i32 line_diff = line - tkn->pos.line;
            if (line_diff == 0) {
                pos.line_start = tkn->pos.line_start; 
            } else {
                char *c = tkn->text;
                while (*c && line_diff > 0) {
                    while (*c && *c != '\n') c++;
                    line_diff -= 1;
                    c++;
                }

                pos.line_start = c;
            }

            ONYX_ERROR(pos, Error_Critical, msg);
            break;
        }

        case MSG_EXT_INJECT_CODE: {
            i32 code_length;
            char *code = bh_strdup(context->ast_alloc, extension_recv_str(ext, &code_length));
            if (!code) {
                ONYX_ERROR(tkn->pos, Error_Critical, "Code expansion of %d bytes is too large.", code_length);
                return 0;
            }

            parse_code(context, PMEK_Top_Level, code, code_length, entity, tkn->pos);
            break;
        }

        default:
            extension_kill(ext);
            ONYX_ERROR(tkn->pos, Error_Critical, "Protocol error when talking to '%s'.", ext->name);
            return 0;
    }

    return 1;
}


TypeMatch compiler_extension_expand_macro(
    Context *context,
    int extension_id,
    ProceduralMacroExpansionKind kind,
    const char *macro_name,
    OnyxToken *body,
    Entity *entity,
    AstNode **out_node,
    u32 *expansion_id,
    b32 wait_for_response
) {
    if (extension_id <= 0 || extension_id > bh_arr_length(context->extensions)) return TYPE_MATCH_FAILED;

    CompilerExtension *ext = &context->extensions[extension_id - 1];

    if (!ext->alive) return TYPE_MATCH_FAILED;

    // Not ready yet (still spawning / negotiating), or busy with a
    // different expansion / hook. Do not read from the pipe or we can
    // steal the child's INIT message.
    if (ext->state != COMP_EXT_STATE_READY) {
        if (ext->state != COMP_EXT_STATE_EXPANDING || ext->current_expansion_id != (i32) *expansion_id) {
            return TYPE_MATCH_YIELD;
        }
    }

    bh_arena_clear(&ext->arena);

    // If the extension is in the ready state, then it is waiting for an expansion request.
    // We can issue this expansion request and flag that the extension is now in the expanding state.
    if (ext->state == COMP_EXT_STATE_READY) {
        *expansion_id = ++context->next_expansion_id;
        ext->current_expansion_id = *expansion_id;
        ext->state = COMP_EXT_STATE_EXPANDING;

        extension_send_int(ext, MSG_HOST_EXPAND_MACRO);
        extension_send_int(ext, *expansion_id);
        extension_send_int(ext, kind);
        extension_send_str(ext, body->pos.filename);
        extension_send_int(ext, body->pos.line);
        extension_send_int(ext, body->pos.column);
        extension_send_int(ext, 0);
        extension_send_str(ext, macro_name);
        extension_send_bytes(ext, body->text, body->length);
    }

    while (1) {
        if (!wait_for_response && !extension_poll_recv(ext)) {
            return TYPE_MATCH_YIELD;
        }

        extension_drain(ext);
        int msg_type = extension_recv_int(ext);

        if (msg_type == MSG_EXT_EXPANSION) {
            u32 id = extension_recv_int(ext);
            if (id != *expansion_id) {
                // PROTOCOL ERROR
                extension_kill(ext);
                ONYX_ERROR(body->pos, Error_Critical, "Protocol error when talking to '%s'.", ext->name);
                return TYPE_MATCH_FAILED;
            }

            int status = extension_recv_int(ext);
            if (status == 0) {
                int reason = extension_recv_int(ext);
                switch (reason) {
                    // TODO: Make this an enum
                    case 0: ONYX_ERROR(body->pos, Error_Critical, "Macro expansion '%s' is not supported by '%s'.", macro_name, ext->name); break;
                    // case 1: ONYX_ERROR(body->pos, Error_Critical, "Macro expansion '%s' failed because of a syntax error.", macro_name); break;
                }
                return TYPE_MATCH_FAILED;
            }

            i32 code_length;
            char *code = extension_recv_str(ext, &code_length);
            if (!code) {
                return TYPE_MATCH_FAILED;
            }

            code = bh_strdup(context->ast_alloc, code);
            *out_node = parse_code(context, kind, code, code_length, entity, body->pos);

            ext->state = COMP_EXT_STATE_READY;
            ext->current_expansion_id = 0;
            compiler_extensions_flush_compilation_data_now(context);
            return TYPE_MATCH_SUCCESS;
        }

        if (!handle_common_messages(context, ext, msg_type, body, entity)) {
            return TYPE_MATCH_FAILED;
        }
    }
}


static TypeMatch compiler_extension_send_hook(Context *context, CompilerExtension *ext, int hook) {
    if (!ext->alive) return TYPE_MATCH_FAILED;
    if (ext->state != COMP_EXT_STATE_READY) return TYPE_MATCH_FAILED;

    // Deliver remaining compilation events before the barrier so the
    // extension's CompilationInfo is complete when the hook runs.
    compiler_extension_send_compilation_data(context, ext);

    bh_arena_clear(&ext->arena);
    ext->state = COMP_EXT_STATE_HANDLING_HOOK;

    extension_send_int(ext, MSG_HOST_HOOK);
    extension_send_int(ext, hook);
    extension_send_int(ext, hook);
    extension_drain(ext);

    OnyxToken *tkn = ext->entity->compiler_extension->token;

    while (1) {
        int msg_type = extension_recv_int(ext);

        if (msg_type == MSG_EXT_ACKNOWLEDGE_HOOK) {
            u32 id = extension_recv_int(ext);
            if (id != (u32) hook) {
                extension_kill(ext);
                ONYX_ERROR(tkn->pos, Error_Critical, "Protocol error when talking to '%s'.", ext->name);
                return TYPE_MATCH_FAILED;
            }

            ext->state = COMP_EXT_STATE_READY;
            compiler_extensions_flush_compilation_data_now(context);
            return TYPE_MATCH_SUCCESS;
        }

        if (!handle_common_messages(context, ext, msg_type, tkn, ext->entity)) {
            return TYPE_MATCH_FAILED;
        }
    }
}

TypeMatch compiler_extension_hook_stalled(Context *context, int extension_id) {
    if (extension_id <= 0 || extension_id > bh_arr_length(context->extensions)) return TYPE_MATCH_FAILED;

    CompilerExtension *ext = &context->extensions[extension_id - 1];
    if (!ext->supports_stalled_hook) return TYPE_MATCH_FAILED;

    return compiler_extension_send_hook(context, ext, HOOK_STALLED);
}

TypeMatch compiler_extension_hook_all_code_checked(Context *context, int extension_id) {
    if (extension_id <= 0 || extension_id > bh_arr_length(context->extensions)) return TYPE_MATCH_FAILED;

    CompilerExtension *ext = &context->extensions[extension_id - 1];
    if (!ext->supports_all_code_checked_hook) return TYPE_MATCH_FAILED;

    return compiler_extension_send_hook(context, ext, HOOK_ALL_CODE_CHECKED);
}

void compiler_extension_terminate(Context *context, int extension_id) {
    if (extension_id <= 0 || extension_id > bh_arr_length(context->extensions)) return;

    CompilerExtension *ext = &context->extensions[extension_id - 1];

    if (!ext->alive) return;

    compiler_extension_send_compilation_data(context, ext);

    if (ext->state == COMP_EXT_STATE_READY) {
        extension_send_int(ext, MSG_HOST_TERMINATE);
        extension_drain(ext);

        compiler_event_log(
            context,
            "Waiting for extension '%s' to exit...",
            ext->name
        );

        extension_wait(ext);
    } else {
        extension_kill(ext);
    }

    if (ext->send_file) close((int) ext->send_file);
    if (ext->recv_file) close((int) ext->recv_file);
    ext->send_file = 0;
    ext->recv_file = 0;
    ext->alive = 0;
}

