

#include "ovm_debug.h"


void debug_info_builder_init(debug_info_builder_t *builder, debug_info_t *info) {
    memset(builder, 0, sizeof(*builder));

    builder->info = info;
}

void debug_info_builder_prepare(debug_info_builder_t *builder, u8 *data) {
    builder->data = data;
    builder->reader_offset = 0;
    builder->current_file_id = 0;
    builder->current_line = 0;
    builder->current_scope = -1;
    builder->next_file_line_offset = 0;
    builder->remaining_reps = 0;
}

static i32 debug_info_builder_push_scope(debug_info_builder_t *builder) {
    debug_sym_scope_t scope;
    scope.symbols = NULL;
    bh_arr_new(builder->info->alloc, scope.symbols, 4);
    scope.parent = builder->current_scope;

    i32 scope_id = bh_arr_length(builder->info->symbol_scopes);
    builder->current_scope = scope_id;

    bh_arr_push(builder->info->symbol_scopes, scope);
    return scope_id;
}

static i32 debug_info_builder_pop_scope(debug_info_builder_t *builder) {
    if (builder->current_scope == -1) return -1;

    debug_sym_scope_t *scope = &builder->info->symbol_scopes[builder->current_scope];
    builder->current_scope = scope->parent;
    return builder->current_scope;
}

static void debug_info_builder_add_symbol(debug_info_builder_t *builder, u32 sym_id) {
    if (builder->current_scope == -1) return;

    debug_sym_scope_t *scope = &builder->info->symbol_scopes[builder->current_scope];
    bh_arr_push(scope->symbols, sym_id);
}

static void debug_info_builder_parse(debug_info_builder_t *builder) {
    u32 count = 0;

    while (!builder->locked) {
        u8 instr = builder->data[builder->reader_offset++];
        switch (instr & 0b11000000) {
            case 0b00000000:
                instr &= 0b00111111;
                switch (instr) {
                    case 0: builder->locked = 1; break; // Early return!
                    case 1:
                        builder->current_file_id = uleb128_to_uint(builder->data, (i32 *)&builder->reader_offset);
                        builder->current_line    = uleb128_to_uint(builder->data, (i32 *)&builder->reader_offset);
                        break;

                    case 2:
                        debug_info_builder_push_scope(builder);
                        break;

                    case 3:
                        debug_info_builder_pop_scope(builder);
                        break;

                    case 4:
                        debug_info_builder_add_symbol(
                            builder,
                            uleb128_to_uint(builder->data, (i32 *)&builder->reader_offset));
                        break;
                }
                break;

            case 0b01000000:
                count = instr & 0x3f;
                builder->current_line += count + 1;
                builder->remaining_reps = 1;
                return;

            case 0b10000000:
                count = instr & 0x3f;
                builder->current_line -= count + 1;
                builder->remaining_reps = 1;
                return;

            case 0b11000000:
                count = instr & 0x3f;
                builder->remaining_reps = count + 1;
                return;
        }
    }
}

void debug_info_builder_step(debug_info_builder_t *builder) {
    if (builder->data == NULL) return;

    while (builder->remaining_reps == 0 && !builder->locked) {
        debug_info_builder_parse(builder);

        debug_loc_info_t info;
        info.file_id      = builder->current_file_id;
        info.line         = builder->current_line;
        info.symbol_scope = builder->current_scope;
        bh_arr_push(builder->info->line_info, info);

        debug_file_info_t *file_info = &builder->info->files[info.file_id];
        if (file_info->line_buffer_offset == -1) {
            file_info->line_buffer_offset = builder->next_file_line_offset;
            builder->next_file_line_offset += file_info->line_count;
        }

        i32 line_index = file_info->line_buffer_offset + builder->current_line;
        u32 target     = bh_arr_length(builder->info->instruction_reducer);

        if (line_index >= bh_arr_capacity(builder->info->line_to_instruction) ||
            builder->info->line_to_instruction[line_index] == 0) {
            bh_arr_set_at(builder->info->line_to_instruction, line_index, target);
        }
    }

    if (builder->locked) return;

    assert(builder->remaining_reps);
    builder->remaining_reps -= 1;
    return;
}

void debug_info_builder_emit_location(debug_info_builder_t *builder) {
    bh_arr_push(builder->info->instruction_reducer, bh_arr_length(builder->info->line_info) - 1);
}

void debug_info_builder_begin_func(debug_info_builder_t *builder, i32 func_idx) {
    if (!builder->data) return;
    if (func_idx >= bh_arr_length(builder->info->funcs)) return;

    debug_func_info_t *func_info = &builder->info->funcs[func_idx];

    builder->reader_offset = func_info->debug_op_offset;
    assert(builder->data[builder->reader_offset] == 2);
    assert(builder->data[builder->reader_offset+1] == 1);
    builder->remaining_reps = 0;
    builder->locked = 0;
}

void debug_info_builder_end_func(debug_info_builder_t *builder) {
    if (!builder->data) return;

    assert(!builder->locked);
    debug_info_builder_step(builder);
    assert(builder->locked);
}
