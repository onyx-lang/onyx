
#include "astnodes.h"
#include "parser.h"
#include "utils.h"

#define MSG_HOST_INIT          0
#define MSG_HOST_TERMINATE     1
#define MSG_HOST_EXPAND_MACRO  2

#define MSG_EXT_INIT         0
#define MSG_EXT_ERROR_REPORT 1
#define MSG_EXT_EXPANSION    2
#define MSG_EXT_INJECT_CODE  3


#if defined(_BH_LINUX) || defined(_BH_DARWIN)
#include <signal.h>
#include <sys/wait.h>

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

            execlp("onyx", "onyx", "run", path, NULL);
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

static char *extension_recv_bytes(CompilerExtension *ext, i32 len) {
    bh_allocator a = bh_arena_allocator(&ext->arena);
    char *buf = bh_alloc(a, len);

    i32 bytes_read = 0;
    while (bytes_read < len) {
        bytes_read += extension_recv(ext, buf, len);
    }

    return buf;
}

static char *extension_recv_str(CompilerExtension *ext, i32 *out_len) {
    i32 len = extension_recv_int(ext);
    if (out_len) *out_len = len;

    bh_allocator a = bh_arena_allocator(&ext->arena);
    char *buf = bh_alloc(a, len + 1);

    i32 bytes_read = 0; 
    while (bytes_read < len) {
        bytes_read += extension_recv(ext, buf, len);
    }

    buf[bytes_read] = '\0';
    return buf;
}



i32 compiler_extension_start(const char *name) {
    CompilerExtension ext;
    bh_arena_init(&ext.arena, global_heap_allocator, 32 * 1024);

    if (!extension_spawn(&ext, name)) {
        return -1;
    }

    ext.alive = 1;

    b32 compiler_extension_negotiate_capabilities(CompilerExtension *ext);
    b32 negotiated = compiler_extension_negotiate_capabilities(&ext);
    if (!negotiated) {
        return -1;
    }

    bh_arr_push(context.extensions, ext);
    return bh_arr_length(context.extensions) -1;
}

b32 compiler_extension_negotiate_capabilities(CompilerExtension *ext) {
    // Init message is 5 ints as defined in `core/onyx/compiler_extension`
    i32 init_msg[5];
    init_msg[0] = MSG_HOST_INIT;
    init_msg[1] = VERSION_MAJOR;
    init_msg[2] = VERSION_MINOR;
    init_msg[3] = VERSION_PATCH;
    init_msg[4] = 1;

    extension_send(ext, init_msg, sizeof(init_msg));

    if (extension_recv_int(ext) != MSG_EXT_INIT) {
        extension_kill(ext);
        return 0;
    }

    int extension_protocol_version = extension_recv_int(ext);
    char *extension_name           = extension_recv_str(ext, NULL);

    ext->name = bh_strdup(context.ast_alloc, extension_name);
    
    if (context.options->verbose_output >= 1) {
        bh_printf("Extension '%s' loaded with protocol version %d\n", extension_name, extension_protocol_version);
    }

    bh_arena_clear(&ext->arena);
    return 1;
}


static AstNode * parse_code(ProceduralMacroExpansionKind kind, char *code, i32 code_length, Entity *entity, OnyxFilePos pos) {
    bh_file_contents file_contents;
    file_contents.allocator = context.ast_alloc;
    file_contents.data = code;
    file_contents.length = code_length;
    file_contents.filename = bh_aprintf(context.ast_alloc, "(expansion from %s:%d,%d)", pos.filename, pos.line, pos.column);

    OnyxTokenizer tokenizer = onyx_tokenizer_create(context.token_alloc, &file_contents);
    onyx_lex_tokens(&tokenizer);

    OnyxParser parser = onyx_parser_create(context.ast_alloc, &tokenizer);
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

// TODO This function should return something like `TypeMatch`, and then have several
// out parameters based on what happened: the generated node, errors, expansion id used, etc
TypeMatch compiler_extension_expand_macro(
    int extension_id,
    ProceduralMacroExpansionKind kind,
    const char *macro_name,
    OnyxToken *body,
    Entity *entity,
    AstNode **out_node,
    u32 *expansion_id
) {
    if (extension_id < 0 || extension_id >= bh_arr_length(context.extensions)) return TYPE_MATCH_FAILED;

    CompilerExtension *ext = &context.extensions[extension_id];
    bh_arena_clear(&ext->arena);

    if (!ext->alive) return TYPE_MATCH_FAILED;

    *expansion_id = ++context.next_expansion_id;
    
    extension_send_int(ext, MSG_HOST_EXPAND_MACRO);
    extension_send_int(ext, *expansion_id);
    extension_send_int(ext, kind);
    extension_send_str(ext, body->pos.filename);
    extension_send_int(ext, body->pos.line);
    extension_send_int(ext, body->pos.column);
    extension_send_int(ext, 0);
    extension_send_str(ext, macro_name);
    extension_send_bytes(ext, body->text, body->length);

    while (1) {
        int msg_type = extension_recv_int(ext);
        switch (msg_type) {
            case MSG_EXT_INIT:
                extension_kill(ext);
                onyx_report_error(body->pos, Error_Critical, "Procotol error when talking to '%s'.", ext->name);
                return TYPE_MATCH_FAILED;

            case MSG_EXT_ERROR_REPORT: {
                char *filename = extension_recv_str(ext, NULL);
                u32 line = extension_recv_int(ext);
                u32 column = extension_recv_int(ext);
                u32 length = extension_recv_int(ext);
                char *msg =  extension_recv_str(ext, NULL);

                OnyxFilePos pos;
                pos.column = column + (line == body->pos.line ? 2 : 0);
                pos.line = line;
                pos.filename = body->pos.filename;
                pos.length = length;
                pos.line_start = body->pos.line_start; // FIXME This is wrong...

                onyx_report_error(pos, Error_Critical, msg);
                break;
            }

            case MSG_EXT_INJECT_CODE: {
                i32 code_length;
                char *code = bh_strdup(context.ast_alloc, extension_recv_str(ext, &code_length));

                parse_code(PMEK_Top_Level, code, code_length, entity, body->pos);
                break;
            }

            case MSG_EXT_EXPANSION: {
                u32 id = extension_recv_int(ext);
                if (id != *expansion_id) {
                    // PROTOCOL ERROR
                    extension_kill(ext);
                    onyx_report_error(body->pos, Error_Critical, "Procotol error when talking to '%s'.", ext->name);
                    return TYPE_MATCH_FAILED;
                }

                int status = extension_recv_int(ext);
                if (status == 0) {
                    int reason = extension_recv_int(ext);
                    switch (reason) {
                        // TODO: Make this an enum
                        case 0: onyx_report_error(body->pos, Error_Critical, "Macro expansion '%s' is not supported by '%s'.", macro_name, ext->name); break;
                        case 1: onyx_report_error(body->pos, Error_Critical, "Macro expansion '%s' failed because of a syntax error.", macro_name); break;
                    }
                    return TYPE_MATCH_FAILED;
                }

                i32 code_length;
                char *code = bh_strdup(context.ast_alloc, extension_recv_str(ext, &code_length));
                if (context.options->verbose_output == 2) {
                    bh_printf("Expansion '%d':\n%s\n", *expansion_id, code);
                }

                *out_node = parse_code(kind, code, code_length, entity, body->pos);
                return TYPE_MATCH_SUCCESS;
            }

            default:
                extension_kill(ext);
                onyx_report_error(body->pos, Error_Critical, "Procotol error when talking to '%s'.", ext->name);
                return TYPE_MATCH_FAILED;
        }
    }
}

