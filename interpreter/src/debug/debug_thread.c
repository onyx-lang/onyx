
#include "ovm_debug.h"
#include "vm.h"

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <assert.h>
#include <poll.h>


#define CMD_NOP 0
#define CMD_RES 1
#define CMD_PAUSE 2
#define CMD_BRK 3
#define CMD_CLR_BRK 4
#define CMD_STEP 5
#define CMD_TRACE 6
#define CMD_THREADS 7
#define CMD_VARS 8
#define CMD_MEM_R 9
#define CMD_MEM_W 10
#define CMD_DISASM 11

#define EVT_NOP 0
#define EVT_BRK_HIT 1
#define EVT_PAUSE 2
#define EVT_RESPONSE 0xffffffff

struct msg_parse_ctx_t {
    char *data;
    unsigned int offset;
    unsigned int bytes_read;
};

static void send_response_header(debug_state_t *debug, unsigned int message_number) {
    unsigned int RESPONSE_HEADER = EVT_RESPONSE;
    bh_buffer_write_u32(&debug->send_buffer, EVT_RESPONSE);
    bh_buffer_write_u32(&debug->send_buffer, message_number);
}

static void send_string(debug_state_t *debug, const char *str) {
    if (!str) {
        bh_buffer_write_u32(&debug->send_buffer, 0);
    } else {
        unsigned int len = strlen(str);
        bh_buffer_write_u32(&debug->send_buffer, len);
        bh_buffer_append(&debug->send_buffer, str, len);
    }
}

static void send_bytes(debug_state_t *debug, const unsigned char *bytes, unsigned int len) {
    bh_buffer_write_u32(&debug->send_buffer, len);
    bh_buffer_append(&debug->send_buffer, bytes, len);
}

static void send_int(debug_state_t *debug, unsigned int x) {
    bh_buffer_write_u32(&debug->send_buffer, x);
}

static void send_bool(debug_state_t *debug, bool x) {
    bool v = x ? 1 : 0;
    bh_buffer_write_byte(&debug->send_buffer, v);
}

static int parse_int(debug_state_t *debug, struct msg_parse_ctx_t *ctx) {
    int i = *(unsigned int *) &ctx->data[ctx->offset];
    ctx->offset += sizeof(unsigned int);
    return i;
}

static char *parse_bytes(debug_state_t *debug, struct msg_parse_ctx_t *ctx, u32 *len) {
    *len = parse_int(debug, ctx);

    char *buf = bh_alloc_array(debug->tmp_alloc, char, *len);
    memcpy(buf, &ctx->data[ctx->offset], *len);
    ctx->offset += *len;

    return buf;
}

static char *parse_string(debug_state_t *debug, struct msg_parse_ctx_t *ctx) {
    int len = parse_int(debug, ctx);

    char *buf = bh_alloc_array(debug->tmp_alloc, char, len + 1);
    memcpy(buf, &ctx->data[ctx->offset], len);
    ctx->offset += len;

    buf[len] = 0;
    return buf;
}

static void resume_thread(debug_thread_state_t *thread) {
    thread->run_count = -1;
    semaphore_post(thread->wait_semaphore);
}

static void resume_thread_slow(debug_thread_state_t *thread) {
    semaphore_post(thread->wait_semaphore);
}

static u32 get_stack_frame_instruction_pointer(debug_state_t *debug, debug_thread_state_t *thread, ovm_stack_frame_t *frame) {
    ovm_func_t *func = frame->func;

    u32 instr;
    if (frame == &bh_arr_last(thread->ovm_state->stack_frames)) {
        instr = thread->ovm_state->pc;
    } else {
        instr = (frame + 1)->return_address;
    }

    return instr;
}

static void get_stack_frame_location(debug_state_t *debug,
    debug_func_info_t *func_info, debug_file_info_t *file_info, debug_loc_info_t *loc_info,
    debug_thread_state_t *thread, ovm_stack_frame_t *frame) {

    ovm_func_t *func = frame->func;

    u32 instr = get_stack_frame_instruction_pointer(debug, thread, frame);

    assert(debug_info_lookup_func(debug->info, func->id, func_info));

    while (!debug_info_lookup_location(debug->info, instr, loc_info))
        instr++;

    assert(debug_info_lookup_file(debug->info, loc_info->file_id, file_info));
}



//
// Command handling
//

#define DEBUG_COMMAND_HANDLER(name) \
    void name(debug_state_t *debug, struct msg_parse_ctx_t *ctx, u32 msg_id)

typedef DEBUG_COMMAND_HANDLER((* debug_command_handler_t));

#define ON_THREAD(tid) \
    bh_arr_each(debug_thread_state_t *, thread, debug->threads) \
        if ((*thread)->id == tid)


static DEBUG_COMMAND_HANDLER(debug_command_nop) {
    send_response_header(debug, msg_id);
}

static DEBUG_COMMAND_HANDLER(debug_command_res) {
    u32 thread_id = parse_int(debug, ctx);

    bool resumed_a_thread = false;

    // Release the thread(s)
    bh_arr_each(debug_thread_state_t *, thread, debug->threads) {
        if (thread_id == 0xffffffff || (*thread)->id == thread_id) {
            resume_thread(*thread);
            resumed_a_thread = true;
        }
    }

    send_response_header(debug, msg_id);
    send_bool(debug, resumed_a_thread);
}

static DEBUG_COMMAND_HANDLER(debug_command_pause) {
    u32 thread_id = parse_int(debug, ctx);

    ON_THREAD(thread_id) {
        (*thread)->run_count = 0;
    }

    send_response_header(debug, msg_id);
}

static DEBUG_COMMAND_HANDLER(debug_command_brk) {
    char    *filename = parse_string(debug, ctx);
    unsigned int line = parse_int(debug, ctx);

    i32 instr = debug_info_lookup_instr_by_file_line(debug->info, filename, line);
    if (instr < 0) goto brk_send_error;

    printf("[INFO ] Setting breakpoint at %s:%d (%x)\n", filename, line, instr);

    debug_file_info_t file_info;
    debug_info_lookup_file_by_name(debug->info, filename, &file_info);

    debug_breakpoint_t bp;
    bp.id = debug->next_breakpoint_id++;
    bp.instr = instr;
    bp.file_id = file_info.file_id;
    bp.line = line;
    bh_arr_push(debug->breakpoints, bp);

    send_response_header(debug, msg_id);
    send_bool(debug, true);
    send_int(debug, bp.id);
    send_int(debug, line);
    return;

  brk_send_error:
    printf("[WARN ] Failed to set breakpoint at %s:%d (%x)\n", filename, line, instr);

    send_response_header(debug, msg_id);
    send_bool(debug, false);
    send_int(debug, -1);
    send_int(debug, 0);
}

static DEBUG_COMMAND_HANDLER(debug_command_clr_brk) {
    char *filename = parse_string(debug, ctx);

    debug_file_info_t file_info;
    bool file_found = debug_info_lookup_file_by_name(debug->info, filename, &file_info);
    if (!file_found) {
        goto clr_brk_send_error;
    }

    bh_arr_each(debug_breakpoint_t, bp, debug->breakpoints) {
        if (bp->file_id == file_info.file_id) {
            // This is kind of hacky but it does successfully delete
            // a single element from the array and move the iterator.
            bh_arr_fastdelete(debug->breakpoints, bp - debug->breakpoints);
            bp--;
        }
    }

    send_response_header(debug, msg_id);
    send_bool(debug, true);
    return;

  clr_brk_send_error:
    send_response_header(debug, msg_id);
    send_bool(debug, false);
}

static DEBUG_COMMAND_HANDLER(debug_command_step) {
    u32 granularity = parse_int(debug, ctx);
    u32 thread_id = parse_int(debug, ctx);

    if (granularity == 1) {
        ON_THREAD(thread_id) {
            (*thread)->pause_at_next_line = true;
            (*thread)->pause_within = -1;
            resume_thread(*thread);
        }
    }

    if (granularity == 2) {
        ON_THREAD(thread_id) {
            (*thread)->run_count = 1;
            resume_thread_slow(*thread);
        }
    }

    if (granularity == 3) {
        ON_THREAD(thread_id) {
            ovm_stack_frame_t *last_frame = &bh_arr_last((*thread)->ovm_state->stack_frames);
            (*thread)->pause_at_next_line = true;
            (*thread)->pause_within = last_frame->func->id;
            (*thread)->extra_frames_since_last_pause = 0;
            resume_thread(*thread);
        }
    }

    if (granularity == 4) {
        ON_THREAD(thread_id) {
            if (bh_arr_length((*thread)->ovm_state->stack_frames) == 1) {
                (*thread)->pause_within = -1;
            } else {
                ovm_stack_frame_t *last_frame = &bh_arr_last((*thread)->ovm_state->stack_frames);
                (*thread)->pause_within = (last_frame - 1)->func->id;
            }

            (*thread)->pause_at_next_line = true;
            (*thread)->extra_frames_since_last_pause = 0;
            resume_thread(*thread);
        }
    }

    send_response_header(debug, msg_id);
}

static DEBUG_COMMAND_HANDLER(debug_command_trace) {
    unsigned int thread_id = parse_int(debug, ctx);

    debug_thread_state_t *thread = NULL;
    bh_arr_each(debug_thread_state_t *, pthread, debug->threads) {
        if ((*pthread)->id == thread_id) {
            thread = *pthread;
            break;
        }
    }

    if (thread == NULL) {
        send_response_header(debug, msg_id);
        send_int(debug, 0);
        return;
    }

    bh_arr(ovm_stack_frame_t) frames = thread->ovm_state->stack_frames;

    send_response_header(debug, msg_id);
    send_int(debug, bh_arr_length(frames));

    bh_arr_rev_each(ovm_stack_frame_t, frame, frames) {
        debug_func_info_t func_info;
        debug_file_info_t file_info;
        debug_loc_info_t  loc_info;

        get_stack_frame_location(debug, &func_info, &file_info, &loc_info, thread, frame);

        send_string(debug, func_info.name);
        send_string(debug, file_info.name);
        send_int(debug, loc_info.line);
        send_int(debug, get_stack_frame_instruction_pointer(debug, thread, frame));
    }
}

static DEBUG_COMMAND_HANDLER(debug_command_threads) {
    bh_arr(debug_thread_state_t *) threads = debug->threads;

    send_response_header(debug, msg_id);
    send_int(debug, bh_arr_length(threads));

    char buf[128];
    bh_arr_each(debug_thread_state_t *, thread, threads) {
        send_int(debug, (*thread)->id);

        snprintf(buf, 128, "thread #%d", (*thread)->id);
        send_string(debug, buf);
    }
}

static DEBUG_COMMAND_HANDLER(debug_command_vars) {
    i32 stack_frame = parse_int(debug, ctx);
    u32 thread_id   = parse_int(debug, ctx);
    u32 layers      = parse_int(debug, ctx);

    debug_thread_state_t **thread = NULL;
    bh_arr_each(debug_thread_state_t *, t, debug->threads)
        if ((*t)->id == thread_id)
            thread = t;

    if (thread == NULL) {
        goto vars_error;
    }

    bh_arr(ovm_stack_frame_t) frames = (*thread)->ovm_state->stack_frames;
    if (stack_frame >= bh_arr_length(frames)) {
        goto vars_error;
    }

    ovm_stack_frame_t *frame = &frames[bh_arr_length(frames) - 1 - stack_frame];

    debug_func_info_t func_info;
    debug_file_info_t file_info;
    debug_loc_info_t  loc_info;

    get_stack_frame_location(debug, &func_info, &file_info, &loc_info, *thread, frame);

    send_response_header(debug, msg_id);

    debug_runtime_value_builder_t builder;
    builder.state = debug;
    builder.info = debug->info;
    builder.ovm_state = (*thread)->ovm_state;
    builder.ovm_frame = frame;
    builder.func_info = func_info;
    builder.file_info = file_info;
    builder.loc_info = loc_info;
    debug_runtime_value_build_init(&builder, bh_heap_allocator());

    //
    // The first layer specified is the symbol id to drill into.
    // Therefore, the first layer is peeled off here before the
    // following while loop. This makes the assumption that no
    // symbol will have the id 0xffffffff. This is probably safe
    // to assume, especially since just one more symbol and this
    // whole system crashes...
    u32 sym_id_to_match = 0xffffffff;
    if (layers > 0) {
        sym_id_to_match = parse_int(debug, ctx);
        layers--;
    }

    i32 symbol_scope = loc_info.symbol_scope;
    while (symbol_scope != -1) {
        debug_sym_scope_t sym_scope = debug->info->symbol_scopes[symbol_scope];
        builder.sym_scope = sym_scope;

        bh_arr_each(u32, sym_id, sym_scope.symbols) {
            debug_sym_info_t *sym = &debug->info->symbols[*sym_id];
            debug_runtime_value_build_set_location(&builder, sym->loc_kind, sym->loc, sym->type, sym->name);

            //
            // If we are drilling to a particular symbol, and this is that symbol,
            // we have to do the generation a little differently. We first have to
            // pull the other layer queries in, and descend into those layers.
            // Then, we loop through each value at that layer and print their values.
            if (sym->sym_id == sym_id_to_match && sym_id_to_match != 0xffffffff) {
                while (layers--) {
                    u32 desc = parse_int(debug, ctx);
                    debug_runtime_value_build_descend(&builder, desc);
                }

                while (debug_runtime_value_build_step(&builder)) {
                    debug_runtime_value_build_string(&builder);
                    debug_type_info_t *type = &debug->info->types[builder.it_type];

                    send_int(debug, 0);
                    send_string(debug, builder.it_name);
                    send_bytes(debug, builder.output.data, builder.output.length);
                    send_string(debug, type->name);
                    send_int(debug, builder.it_index - 1); // CLEANUP This should be 0 indexed, but because this is after the while loop condition, it is 1 indexed.
                    send_bool(debug, builder.it_has_children);
                    send_int(debug, debug_runtime_value_get_it_addr(&builder));

                    debug_runtime_value_build_clear(&builder);
                }

                // This is important, as when doing a layered query, only one symbol
                // should be considered, and once found, should immediate stop.
                goto syms_done;

            } else if (sym_id_to_match == 0xffffffff) {
                // Otherwise, we simply print the value of the symbol as is.
                debug_type_info_t *type = &debug->info->types[sym->type];

                debug_runtime_value_build_string(&builder);

                send_int(debug, 0);
                send_string(debug, sym->name);
                send_bytes(debug, builder.output.data, builder.output.length);
                send_string(debug, type->name);
                send_int(debug, sym->sym_id);
                send_bool(debug, builder.it_has_children);
                send_int(debug, debug_runtime_value_get_it_addr(&builder));

                debug_runtime_value_build_clear(&builder);
            }
        }

        symbol_scope = sym_scope.parent;
    }

  syms_done:
    send_int(debug, 1);
    debug_runtime_value_build_free(&builder);
    return;

  vars_error:
    send_response_header(debug, msg_id);
    send_int(debug, 1);
}

static DEBUG_COMMAND_HANDLER(debug_command_memory_read) {
    u32 addr = parse_int(debug, ctx);
    u32 count = parse_int(debug, ctx);

    count = bh_min(count, 2048);

    send_response_header(debug, msg_id);
    send_bytes(debug, bh_pointer_add(debug->ovm_engine->memory, addr), count);
}

static DEBUG_COMMAND_HANDLER(debug_command_memory_write) {
    u32 addr = parse_int(debug, ctx);
    u32 count;

    u8 *data = (u8 *)parse_bytes(debug, ctx, &count);
    memcpy(bh_pointer_add(debug->ovm_engine->memory, addr), data, count);

    send_response_header(debug, msg_id);
    send_int(debug, count);
}

static DEBUG_COMMAND_HANDLER(debug_command_disassmble) {
    u32 addr = parse_int(debug, ctx);
    u32 count = parse_int(debug, ctx);

    send_response_header(debug, msg_id);

    bh_buffer instr_buf;
    bh_buffer_init(&instr_buf, debug->tmp_alloc, 1024);

    // This is kind of a hack, but currently there is no
    // easy way to access the program text for the current
    // program without going through a thread. So I just go
    // through the first thread.
    ovm_program_t *prog = debug->threads[0]->ovm_state->program;

    debug_loc_info_t loc_info;
    u32 last_file_id = 0xffffffff;

    while (addr < bh_arr_length(prog->code) && count--) {
        send_int(debug, 0);

        ovm_disassemble(prog, addr, &instr_buf);

        send_bytes(debug, instr_buf.data, instr_buf.length);
        bh_buffer_clear(&instr_buf);

        debug_info_lookup_location(debug->info, addr, &loc_info);
        send_int(debug, loc_info.line);

        if (loc_info.file_id != last_file_id) {
            send_bool(debug, true);
            send_string(debug, debug->info->files[loc_info.file_id].name);

            last_file_id = loc_info.file_id;
        } else {
            send_bool(debug, false);
        }

        addr += 1;
    }

    send_int(debug, 1);
    bh_buffer_free(&instr_buf);
}

static debug_command_handler_t command_handlers[] = {
    [CMD_NOP]     = debug_command_nop,
    [CMD_RES]     = debug_command_res,
    [CMD_PAUSE]   = debug_command_pause,
    [CMD_BRK]     = debug_command_brk,
    [CMD_CLR_BRK] = debug_command_clr_brk,
    [CMD_STEP]    = debug_command_step,
    [CMD_TRACE]   = debug_command_trace,
    [CMD_THREADS] = debug_command_threads,
    [CMD_VARS]    = debug_command_vars,
    [CMD_MEM_R]   = debug_command_memory_read,
    [CMD_MEM_W]   = debug_command_memory_write,
    [CMD_DISASM]  = debug_command_disassmble,
};

static void process_command(debug_state_t *debug, struct msg_parse_ctx_t *ctx) {
    u32 msg_id     = parse_int(debug, ctx);
    u32 command_id = parse_int(debug, ctx);

    if (command_id >= sizeof(command_handlers) / sizeof(command_handlers[0])) {
        printf("[ERROR] Unrecognized command id %x\n", command_id);
        send_response_header(debug, msg_id);
        return;
    }

    command_handlers[command_id](debug, ctx, msg_id);
}

static void process_message(debug_state_t *debug, char *msg, unsigned int bytes_read) {
    struct msg_parse_ctx_t ctx;
    ctx.data = msg;
    ctx.offset = 0;
    ctx.bytes_read = bytes_read;

    while (ctx.offset < ctx.bytes_read) {
        u32 msg_id = *(u32 *) &ctx.data[ctx.offset];
        assert(msg_id != 0xffffffff);

        process_command(debug, &ctx);
    }
}

static void debug_session_handler(debug_state_t *debug) {
    // Disable blocking reads and write in the client socket
    // Alternatively, a MSG_DONTWAIT could be used below
    fcntl(debug->client_fd, F_SETFL, O_NONBLOCK);
    fcntl(debug->state_change_pipes[0], F_SETFL, O_NONBLOCK);

    printf("[INFO ] Client connected\n");

    struct pollfd poll_fds[2];
    poll_fds[0].fd = debug->state_change_pipes[0];
    poll_fds[0].events = POLLIN;
    poll_fds[1].fd = debug->client_fd;
    poll_fds[1].events = POLLIN;

    char command[4096];
    while (debug->debug_thread_running) {
        poll(poll_fds, 2, 1000);

        //
        // If there are no functions on the main thread,
        // assume the program has exited and do not continue to
        // do anything.
        if (debug->threads[0]->ovm_state->call_depth <= 0) {
            debug->debug_thread_running = false;
            return;
        }

        //
        // Try to read commands from the client.
        // If an error was returned, bail out of this thread.
        i32 bytes_read = recv(debug->client_fd, command, 4096, 0);
        if (bytes_read == 0) {
            printf("[INFO ] OVM Debugger connection closed by peer.\n");

            // Resume all threads when the debugger detaches
            bh_arr_each(debug_thread_state_t *, pthread, debug->threads) {
                resume_thread(*pthread);
            }
            return;
        }

        if (bytes_read == -1) {
            switch (errno) {
                case EAGAIN: break;

                case ECONNRESET:
                    printf("[ERROR] OVM Debugger connection closed by peer.\n");
                    return;

                default:
                    printf("[ERROR] OVM Debugger crashed when reading from UNIX socket.\n");
                    return;
            }
        }

        //
        // If data was returned, process the commands that are inside of it.
        if (bytes_read > 0) {
            process_message(debug, command, bytes_read);
        }

        //
        // Check the state of the running program and report any changes.

        char buf;
        int dummy = read(debug->state_change_pipes[0], &buf, 1);

        bh_arr_each(debug_thread_state_t *, thread, debug->threads) {
            if ((*thread)->state == debug_state_hit_breakpoint) {
                (*thread)->state = debug_state_paused;

                i32 instr = -1, bp_id = (*thread)->last_breakpoint_hit;
                bh_arr_each(debug_breakpoint_t, bp, debug->breakpoints) {
                    if (bp->id == bp_id) {
                        instr = bp->instr;
                        break;
                    }
                }

                if (instr == -1) continue;

                debug_loc_info_t loc_info;
                debug_info_lookup_location(debug->info, instr, &loc_info);

                debug_file_info_t file_info;
                debug_info_lookup_file(debug->info, loc_info.file_id, &file_info);

                send_int(debug, EVT_BRK_HIT);
                send_int(debug, bp_id);
                send_int(debug, (*thread)->id);
            }

            if ((*thread)->state == debug_state_pausing) {
                (*thread)->state = debug_state_paused;

                send_int(debug, EVT_PAUSE);
                send_int(debug, (*thread)->id);
                send_int(debug, (*thread)->pause_reason);
            }
        }

        if (debug->send_buffer.length > 0) {
            send(debug->client_fd, debug->send_buffer.data, debug->send_buffer.length, 0);
            bh_buffer_clear(&debug->send_buffer);
        }

        bh_arena_clear(&debug->tmp_arena);
    }

    printf("[INFO ] Session closed\n");
}

void *__debug_thread_entry(void * data) {
    debug_state_t *debug = data;
    debug->debug_thread_running = true;

    // Set up socket listener
    // Wait for initial connection/handshake before entering loop.

    debug->listen_socket_fd = socket(AF_UNIX, SOCK_STREAM, 0);

    struct sockaddr_un local_addr, remote_addr;
    local_addr.sun_family = AF_UNIX;
    strcpy(local_addr.sun_path, debug->listen_path); // TODO: Make this dynamic so mulitple servers can exist at a time.
    unlink(local_addr.sun_path);                     // TODO: Remove this line for the same reason.
    int len = strlen(local_addr.sun_path) + 1 + sizeof(local_addr.sun_family);
    bind(debug->listen_socket_fd, (struct sockaddr *)&local_addr, len);

    //
    // Currently, there can only be 1 connected debugger instance at a time.
    listen(debug->listen_socket_fd, 16);

    bh_buffer_init(&debug->send_buffer, bh_heap_allocator(), 1024);

    while (debug->debug_thread_running) {
        len = sizeof(struct sockaddr_un);
        debug->client_fd = accept(debug->listen_socket_fd, (void * restrict)&remote_addr, (socklen_t * restrict)&len);

        debug_session_handler(debug);

        close(debug->client_fd);
    }

    close(debug->listen_socket_fd);
    unlink(local_addr.sun_path);
    return NULL;
}
