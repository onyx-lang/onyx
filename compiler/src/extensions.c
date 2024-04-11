
#include "astnodes.h"
#include "utils.h"
#include <signal.h>

#define MSG_HOST_INIT          0
#define MSG_HOST_TERMINATE     1
#define MSG_HOST_EXPAND_MACRO  2

#define MSG_EXT_INIT         0
#define MSG_EXT_ERROR_REPORT 1
#define MSG_EXT_EXPANSION    2
#define MSG_EXT_INJECT_CODE  3


#if defined(_BH_LINUX) || defined(_BH_DARWIN)

static i32 extension_send(CompilerExtension *ext, void *data, i32 len) {
    return write(ext->send_file, data, len);
}

static i32 extension_recv(CompilerExtension *ext, void *buf, i32 maxlen) {
    return read(ext->recv_file, buf, maxlen);
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

static char *extension_recv_str(CompilerExtension *ext) {
    i32 len = extension_recv_int(ext);
    bh_allocator a = bh_arena_allocator(&ext->arena);
    char *buf = bh_alloc(a, len + 1);

    i32 bytes_read = 0; 
    while (bytes_read < len) {
        bytes_read += extension_recv(ext, buf, len);
    }

    buf[bytes_read] = '\0';
    return buf;
}

static void extension_kill(CompilerExtension *ext) {
    kill(ext->pid, SIGKILL);
    int status;
    waitpid(ext->pid, &status, 0);
}

static b32 compiler_extension_negotiate_capabilities(CompilerExtension *ext) {
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
    char *extension_name           = extension_recv_str(ext);
    
    printf("Extension '%s' loaded at version %d with proc macros:\n", extension_name, extension_protocol_version);

    int extension_proc_macro_count = extension_recv_int(ext);
    fori (i, 0, extension_proc_macro_count) {
        char *proc_macro = extension_recv_str(ext);
        printf("    %s\n", proc_macro);
    }

    bh_arena_clear(&ext->arena);
    return 1;
}


i32 compiler_extension_start(const char *name) {
    i32 ext_to_comp[2];
    i32 comp_to_ext[2];

    if (pipe(ext_to_comp) || pipe(comp_to_ext)) {
        return -1;
    }

    u32 pid;
    switch (pid = fork()) {
        case -1:
            close(ext_to_comp[0]);
            close(ext_to_comp[1]);
            close(comp_to_ext[0]);
            close(comp_to_ext[1]);
            return -1;

        case 0:
            close(ext_to_comp[0]);
            close(comp_to_ext[1]);

            dup2(comp_to_ext[0], 0);
            dup2(ext_to_comp[1], 1);

            execlp("onyx", "onyx", "run", name, NULL);
            exit(1);
            break;

        default:
            close(ext_to_comp[1]);
            close(comp_to_ext[0]);
            break;
    }

    CompilerExtension ext;
    ext.pid = pid;
    ext.send_file = comp_to_ext[1];
    ext.recv_file = ext_to_comp[0];
    bh_arena_init(&ext.arena, global_heap_allocator, 32 * 1024);

    b32 negotiated = compiler_extension_negotiate_capabilities(&ext);
    if (!negotiated) {
        return -1;
    }

    bh_arr_push(context.extensions, ext);
    return bh_arr_length(context.extensions) -1;
}

#endif



#ifdef _BH_WINDOWS

b32 compiler_extension_start(const char *name) {
    printf("Compiler extensions are currently not support on Windows. Sorry! :(\n");
    return 0;
}

#endif