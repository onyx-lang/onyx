
#include "astnodes.h"
#include "parser.h"
#include "utils.h"

#define MSG_HOST_INIT          0
#define MSG_HOST_TERMINATE     1
#define MSG_HOST_EXPAND_MACRO  2
#define MSG_HOST_HOOK          3

#define MSG_EXT_INIT             0
#define MSG_EXT_ERROR_REPORT     1
#define MSG_EXT_EXPANSION        2
#define MSG_EXT_INJECT_CODE      3
#define MSG_EXT_ACKNOWLEDGE_HOOK 4

#define HOOK_STALLED 1


#if defined(_BH_LINUX) || defined(_BH_DARWIN)
#include <signal.h>
#include <sys/wait.h>
#include <sys/poll.h>

static void extension_send(CompilerExtension *ext, void *data, i32 len) {
    if (!ext->alive) return;

    i32 wrote = 0;
    while (wrote < len) {
        i32 w = write(ext->send_file, bh_pointer_add(data, wrote), len - wrote);
        if (w > 0) wrote += w;
        else {
            ext->alive = 0;
            return;
        }
    }
}

static b32 extension_poll_recv(CompilerExtension *ext) {
    struct pollfd fd;
    fd.events = POLL_IN;
    fd.fd = ext->recv_file;

    poll(&fd, 1, 0);

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

static void extension_kill(CompilerExtension *ext) {
    ext->alive = 0;
    kill(ext->pid, SIGKILL);
    int status;
    waitpid(ext->pid, &status, 0);
}

static b32 extension_spawn(CompilerExtension *ext, const char *path) {
    i32 ext_to_comp[2];
    i32 comp_to_ext[2];

    if (pipe(ext_to_comp) || pipe(comp_to_ext)) {
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
    return 1;
}

#endif

#ifdef _BH_WINDOWS

static void extension_send(CompilerExtension *ext, void *data, i32 len) {
}

static i32 extension_recv(CompilerExtension *ext, void *buf, i32 maxlen) {
    return 0;
}

static b32 extension_poll_recv(CompilerExtension *ext) {
    return 0;
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
        ext.state = COMP_EXT_STATE_SPAWNING;
        bh_arena_init(&ext.arena, context->gp_alloc, 1 * 1024 * 1024); // 1MB
        ext.entity = ent;

        if (!extension_spawn(&ext, path)) {
            return TYPE_MATCH_FAILED;
        }

        ext.alive = 1;
        ext.id = bh_arr_length(context->extensions) + 1;
        *out_extension_id = ext.id;
        bh_arr_push(context->extensions, ext);

        // Init message is 5 ints as defined in `core/onyx/compiler_extension`
        i32 init_msg[5];
        init_msg[0] = MSG_HOST_INIT;
        init_msg[1] = VERSION_MAJOR;
        init_msg[2] = VERSION_MINOR;
        init_msg[3] = VERSION_PATCH;
        init_msg[4] = 1;

        extension_send(&ext, init_msg, sizeof(init_msg));

        return TYPE_MATCH_YIELD;

    } else {
        CompilerExtension *ext = &context->extensions[*out_extension_id - 1];

        ext->state = COMP_EXT_STATE_INITIATING;

        b32 compiler_extension_negotiate_capabilities(Context *context, CompilerExtension *ext);
        b32 negotiated = compiler_extension_negotiate_capabilities(context, ext);
        if (!negotiated) {
            return TYPE_MATCH_FAILED;
        }

        ext->state = COMP_EXT_STATE_READY;
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

            if (hook == 1) ext->supports_stalled_hook = 1;
        }
    }

    {
        CompilerEvent *e = compiler_event_add(context, 1);
        compiler_event_add_field_str(context, e, "message",
            bh_aprintf(context->scratch_alloc, "Extension '%s' spawned with protocol version %d.",
                ext->name, extension_protocol_version
            )
        );
    }
    
    bh_arena_clear(&ext->arena);
    return 1;
}


static AstNode * parse_code(Context *context, ProceduralMacroExpansionKind kind, char *code, i32 code_length, Entity *entity, OnyxFilePos pos) {
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
            char *msg =  extension_recv_str(ext, NULL);

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

    if (ext->state != COMP_EXT_STATE_READY && ext->current_expansion_id != (i32) *expansion_id) {
        return TYPE_MATCH_YIELD;
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
            return TYPE_MATCH_SUCCESS;
        }

        if (!handle_common_messages(context, ext, msg_type, body, entity)) {
            return TYPE_MATCH_FAILED;
        }
    }
}


TypeMatch compiler_extension_hook_stalled(Context *context, int extension_id) {
    if (extension_id <= 0 || extension_id > bh_arr_length(context->extensions)) return TYPE_MATCH_FAILED;

    CompilerExtension *ext = &context->extensions[extension_id - 1];
    if (!ext->supports_stalled_hook) return TYPE_MATCH_FAILED;

    if (!ext->alive) return TYPE_MATCH_FAILED;

    if (ext->state != COMP_EXT_STATE_READY) {
        return TYPE_MATCH_FAILED;
    }

    bh_arena_clear(&ext->arena);

    ext->state = COMP_EXT_STATE_HANDLING_HOOK;
    
    extension_send_int(ext, MSG_HOST_HOOK);
    extension_send_int(ext, HOOK_STALLED);
    extension_send_int(ext, HOOK_STALLED);

    OnyxToken *tkn = ext->entity->compiler_extension->token;

    while (1) {
        int msg_type = extension_recv_int(ext);

        if (msg_type == MSG_EXT_ACKNOWLEDGE_HOOK) {
            u32 id = extension_recv_int(ext);
            if (id != HOOK_STALLED) {
                // PROTOCOL ERROR
                extension_kill(ext);
                ONYX_ERROR(tkn->pos, Error_Critical, "Protocol error when talking to '%s'.", ext->name);
                return TYPE_MATCH_FAILED;
            }

            ext->state = COMP_EXT_STATE_READY;
            return TYPE_MATCH_SUCCESS;
        }

        if (!handle_common_messages(context, ext, msg_type, tkn, ext->entity)) {
            return TYPE_MATCH_FAILED;
        }
    }
}

