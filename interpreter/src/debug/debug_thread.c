
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

#define EVT_NOP 0
#define EVT_BRK_HIT 1
#define EVT_PAUSE 2
#define EVT_NEW_THREAD 3 // Not Implemented
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

static void send_bytes(debug_state_t *debug, const char *bytes, unsigned int len) {
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

static char *parse_bytes(debug_state_t *debug, struct msg_parse_ctx_t *ctx) {
    int len = parse_int(debug, ctx);

    char *buf = bh_alloc_array(debug->tmp_alloc, char, len);
    memcpy(buf, &ctx->data[ctx->offset], len);
    ctx->offset += len;

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
    sem_post(&thread->wait_semaphore);
}

static void get_stack_frame_location(debug_state_t *debug,
    debug_func_info_t *func_info, debug_file_info_t *file_info, debug_loc_info_t *loc_info,
    debug_thread_state_t *thread, ovm_stack_frame_t *frame) {

    ovm_func_t *func = frame->func;

    assert(debug_info_lookup_func(debug->info, func->id, func_info));

    u32 instr;
    if (frame == &bh_arr_last(thread->ovm_state->stack_frames)) {
        instr = thread->ovm_state->pc;
    } else {
        instr = (frame + 1)->return_address;
    }

    while (!debug_info_lookup_location(debug->info, instr, loc_info))
        instr++;

    assert(debug_info_lookup_file(debug->info, loc_info->file_id, file_info));
}

static void process_command(debug_state_t *debug, struct msg_parse_ctx_t *ctx) {
#define ON_THREAD(tid) \
    bh_arr_each(debug_thread_state_t *, thread, debug->threads) \
        if ((*thread)->id == tid)

    u32 msg_id     = parse_int(debug, ctx);
    u32 command_id = parse_int(debug, ctx);

    // printf("[INFO ] Recv command: %d\n", command_id);

    switch (command_id) {
        case CMD_NOP: {
            send_response_header(debug, msg_id);
            break;
        }

        case CMD_RES: {
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
            break;
        }

        case CMD_PAUSE: {
            u32 thread_id = parse_int(debug, ctx);

            ON_THREAD(thread_id) {
                (*thread)->run_count = 0;
            }

            send_response_header(debug, msg_id);
            break;
        }

        case CMD_BRK: {
            char    *filename = parse_string(debug, ctx);
            unsigned int line = parse_int(debug, ctx);

            //
            // TODO: This translation logic will have to be abstracted
            debug_file_info_t file_info;
            bool file_found = debug_info_lookup_file_by_name(debug->info, filename, &file_info);
            if (!file_found) {
                goto brk_send_error;
            }

            if (line > file_info.line_count) {
                goto brk_send_error;
            }

            u32 instr;
            while ((instr = debug->info->line_to_instruction[file_info.line_buffer_offset + line]) == 0) {
                line += 1;

                if (line > file_info.line_count) {
                    goto brk_send_error;
                }
            }

            printf("[INFO ] Setting breakpoint at %s:%d (%xd)\n", filename, line, instr);
            
            debug_breakpoint_t bp;
            bp.id = debug->next_breakpoint_id++;
            bp.instr = instr;
            bp.file_id = file_info.file_id;
            bp.line = line;
            bh_arr_each(debug_thread_state_t *, thread, debug->threads) {
                bh_arr_push((*thread)->breakpoints, bp);
            }

            send_response_header(debug, msg_id);
            send_bool(debug, true);
            send_int(debug, bp.id);    // TODO: This should be a unique breakpoint ID
            send_int(debug, line);
            break;

          brk_send_error:
            send_response_header(debug, msg_id);
            send_bool(debug, false);
            send_int(debug, -1);
            send_int(debug, 0);
            break;
        }

        case CMD_CLR_BRK: {
            char *filename = parse_string(debug, ctx);

            debug_file_info_t file_info;
            bool file_found = debug_info_lookup_file_by_name(debug->info, filename, &file_info);
            if (!file_found) {
                goto clr_brk_send_error;
            }

            bh_arr_each(debug_thread_state_t *, thread, debug->threads) {
                bh_arr_each(debug_breakpoint_t, bp, (*thread)->breakpoints) {
                    if (bp->file_id == file_info.file_id) {
                        // This is kind of hacky but it does successfully delete
                        // a single element from the array and move the iterator.
                        bh_arr_fastdelete((*thread)->breakpoints, bp - (*thread)->breakpoints);
                        bp--;
                    }
                }
            }

            send_response_header(debug, msg_id);
            send_bool(debug, true);
            break;

          clr_brk_send_error:
            send_response_header(debug, msg_id);
            send_bool(debug, false);
            break;
        }

        case CMD_STEP: {
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
                    resume_thread(*thread);
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
            break;
        }

        case CMD_TRACE: {
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
                break;
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
            }

            break;
        }

        case CMD_THREADS: {
            bh_arr(debug_thread_state_t *) threads = debug->threads;

            send_response_header(debug, msg_id);
            send_int(debug, bh_arr_length(threads));

            char buf[128];
            bh_arr_each(debug_thread_state_t *, thread, threads) {
                send_int(debug, (*thread)->id);

                snprintf(buf, 128, "thread #%d", (*thread)->id);
                send_string(debug, buf);
            }

            break;
        }

        case CMD_VARS: {
            i32 stack_frame = parse_int(debug, ctx);
            u32 thread_id   = parse_int(debug, ctx);

            ON_THREAD(thread_id) {
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

                i32 symbol_scope = loc_info.symbol_scope;
                while (symbol_scope != -1) {
                    debug_sym_scope_t sym_scope = debug->info->symbol_scopes[symbol_scope];

                    bh_arr_each(u32, sym_id, sym_scope.symbols) {
                        debug_sym_info_t *sym = &debug->info->symbols[*sym_id];

                        send_int(debug, 0);
                        send_string(debug, sym->name);

                        debug_runtime_value_builder_t builder;
                        builder.state = debug;
                        builder.info = debug->info;
                        builder.ovm_state = (*thread)->ovm_state;
                        builder.ovm_frame = frame;
                        builder.sym_scope = sym_scope;
                        builder.func_info = func_info;
                        builder.file_info = file_info;
                        builder.loc_info = loc_info;
                        builder.sym_info = *sym;

                        debug_runtime_value_build_init(&builder, bh_heap_allocator());
                        debug_runtime_value_build_string(&builder);
                        send_bytes(debug, builder.output.data, builder.output.length);
                        debug_runtime_value_build_free(&builder);

                        debug_type_info_t *type = &debug->info->types[sym->type];
                        send_string(debug, type->name);
                    }

                    symbol_scope = sym_scope.parent;
                }
                
                send_int(debug, 1);
                return;
            }

          vars_error:
            send_response_header(debug, msg_id);
            send_int(debug, 1);
            break;
        }
    }
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

void *__debug_thread_entry(void * data) {
    debug_state_t *debug = data;
    debug->debug_thread_running = true;

    // Set up socket listener
    // Wait for initial connection/handshake before entering loop.
    
    debug->listen_socket_fd = socket(AF_UNIX, SOCK_STREAM, 0);

    struct sockaddr_un local_addr, remote_addr;
    local_addr.sun_family = AF_UNIX;
    strcpy(local_addr.sun_path, "/tmp/ovm-debug.0000"); // TODO: Make this dynamic so mulitple servers can exist at a time.
    unlink(local_addr.sun_path);                        // TODO: Remove this line for the same reason.
    int len = strlen(local_addr.sun_path) + sizeof(local_addr.sun_family);
    bind(debug->listen_socket_fd, (struct sockaddr *)&local_addr, len);
    
    //
    // Currently, there can only be 1 connected debugger instance at a time.
    listen(debug->listen_socket_fd, 1);

    len = sizeof(struct sockaddr_un);
    debug->client_fd = accept(debug->listen_socket_fd, (void * restrict)&remote_addr, &len);

    close(debug->listen_socket_fd);

    // Disable blocking reads and write in the client socket
    // Alternatively, a MSG_DONTWAIT could be used below
    fcntl(debug->client_fd, F_SETFL, O_NONBLOCK);
    fcntl(debug->state_change_pipes[0], F_SETFL, O_NONBLOCK);

    printf("[INFO ] Client connected\n");

    bh_buffer_init(&debug->send_buffer, bh_heap_allocator(), 1024);

    struct pollfd poll_fds[2];
    poll_fds[0].fd = debug->state_change_pipes[0];
    poll_fds[0].events = POLLIN;
    poll_fds[1].fd = debug->client_fd;
    poll_fds[1].events = POLLIN;

    char command[4096];
    while (debug->debug_thread_running) {
        poll(poll_fds, 2, 1000);

        //
        // Try to read commands from the client.
        // If an error was returned, bail out of this thread.
        i32 bytes_read = recv(debug->client_fd, command, 4096, 0);
        if (bytes_read == -1) {
            switch (errno) {
                case EAGAIN: break;

                case ECONNRESET:
                    printf("[ERROR] OVM Debugger connection closed by peer.\n");
                    debug->debug_thread_running = false;
                    break;

                default:
                    printf("[ERROR] OVM Debugger crashed when reading from UNIX socket.\n");
                    debug->debug_thread_running = false;
                    break;
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
                bh_arr_each(debug_breakpoint_t, bp, (*thread)->breakpoints) {
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

        send(debug->client_fd, debug->send_buffer.data, debug->send_buffer.length, 0);
        bh_buffer_clear(&debug->send_buffer);

        bh_arena_clear(&debug->tmp_arena);
    }

    close(debug->client_fd);
    printf("[INFO ] Session closed\n");

    unlink(local_addr.sun_path);
    return NULL;
}
