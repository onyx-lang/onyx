
#include "ovm_debug.h"
#include "vm.h"

static char write_buf[4096];

#define WRITE(str) do {    \
        bh_buffer_write_string(&builder->output, str); \
    } while (0);

#define WRITE_FORMAT(format, ...) do {    \
        u32 len = snprintf(write_buf, 4096, format, __VA_ARGS__); \
        bh_buffer_append(&builder->output, write_buf, len); \
    } while (0);

static bool lookup_register_in_frame(ovm_state_t *state, ovm_stack_frame_t *frame, u32 reg, ovm_value_t *out) {

    u32 val_num_base;
    if (frame == &bh_arr_last(state->stack_frames)) {
        val_num_base = state->value_number_offset;
    } else {
        val_num_base = (frame + 1)->value_number_base;
    }

    *out = state->numbered_values[val_num_base + reg];
    return true;
}

static void append_value_from_memory_with_type(debug_runtime_value_builder_t *builder, void *base, u32 type_id) {
    debug_type_info_t *type = &builder->info->types[type_id];

    switch (type->kind) {
        case debug_type_kind_primitive:
            switch (type->primitive.primitive_kind) {
                case debug_type_primitive_kind_void: WRITE("void"); break;
                case debug_type_primitive_kind_signed_integer:
                    switch (type->size) {
                        case 1: WRITE_FORMAT("%hhd", *(i8 *)  base); break;
                        case 2: WRITE_FORMAT("%hd",  *(i16 *) base); break;
                        case 4: WRITE_FORMAT("%d",   *(i32 *) base); break;
                        case 8: WRITE_FORMAT("%ld",  *(i64 *) base); break;
                        default: WRITE("(err)"); break;
                    }
                    break;

                case debug_type_primitive_kind_unsigned_integer:
                    switch (type->size) {
                        case 1: WRITE_FORMAT("%hhu", *(u8 *) base); break;
                        case 2: WRITE_FORMAT("%hu",  *(u16 *) base); break;
                        case 4: WRITE_FORMAT("%u",   *(u32 *) base); break;
                        case 8: WRITE_FORMAT("%lu",  *(u64 *) base); break;
                        default: WRITE("(err)"); break;
                    }
                    break;

                case debug_type_primitive_kind_float:
                    switch (type->size) {
                        case 4: WRITE_FORMAT("%f",   *(f32 *) base); break;
                        case 8: WRITE_FORMAT("%f",   *(f64 *) base); break;
                        default: WRITE("(err)"); break;
                    }
                    break;

                case debug_type_primitive_kind_boolean:
                    if ((*(u8 *) base) != 0) { WRITE("true"); }
                    else                     { WRITE("false"); }
                    break;

                default:
                    WRITE("(err)");
            }
            break;

        case debug_type_kind_modifier:
            switch (type->modifier.modifier_kind) {
                case debug_type_modifier_kind_pointer:
                    switch (type->size) {
                        case 4: WRITE_FORMAT("0x%x",   *(u32 *) base); break;
                        case 8: WRITE_FORMAT("0x%lx",  *(u64 *) base); break;
                        default: WRITE("(err)"); break;
                    }
                    break;

                default:
                    append_value_from_memory_with_type(builder, base, type->modifier.modified_type);
                    break;
            }
            break;

        case debug_type_kind_alias:
            append_value_from_memory_with_type(builder, base, type->alias.aliased_type);
            break;

        case debug_type_kind_function:
            WRITE_FORMAT("func[%d]", *(u32 *) base);
            break;

        case debug_type_kind_structure: {
            WRITE("{ ");

            fori (i, 0, (i32) type->structure.member_count) {
                if (i != 0) WRITE(", ");

                WRITE_FORMAT("%s=", type->structure.members[i].name);

                u32 offset  = type->structure.members[i].offset;
                u32 type_id = type->structure.members[i].type;
                append_value_from_memory_with_type(builder, bh_pointer_add(base, offset), type_id);
            }

            WRITE(" }");
            break;
        }

        case debug_type_kind_array: {
            WRITE("[");

            debug_type_info_t *elem_type = &builder->info->types[type->array.type];
            fori (i, 0, (i32) type->array.count) {
                if (i != 0) WRITE(", ");

                append_value_from_memory_with_type(builder, bh_pointer_add(base, i * elem_type->size), elem_type->id);
            }

            WRITE("]");
            break;
        }

        default: WRITE("(unknown)"); break;
    }
}

static void append_ovm_value_with_type(debug_runtime_value_builder_t *builder, ovm_value_t value, u32 type_id) {
    debug_type_info_t *type = &builder->info->types[type_id];

    switch (type->kind) {
        case debug_type_kind_primitive:
            switch (type->primitive.primitive_kind) {
                case debug_type_primitive_kind_void: WRITE("void"); break;
                case debug_type_primitive_kind_signed_integer:
                    switch (type->size) {
                        case 1: WRITE_FORMAT("%hhd", value.i8); break;
                        case 2: WRITE_FORMAT("%hd",  value.i16); break;
                        case 4: WRITE_FORMAT("%d",   value.i32); break;
                        case 8: WRITE_FORMAT("%ld",  value.i64); break;
                        default: WRITE("(err)"); break;
                    }
                    break;

                case debug_type_primitive_kind_unsigned_integer:
                    switch (type->size) {
                        case 1: WRITE_FORMAT("%hhu", value.u8); break;
                        case 2: WRITE_FORMAT("%hu",  value.u16); break;
                        case 4: WRITE_FORMAT("%u",   value.u32); break;
                        case 8: WRITE_FORMAT("%lu",  value.u64); break;
                        default: WRITE("(err)"); break;
                    }
                    break;

                case debug_type_primitive_kind_float:
                    switch (type->size) {
                        case 4: WRITE_FORMAT("%f",   value.f32); break;
                        case 8: WRITE_FORMAT("%f",   value.f64); break;
                        default: WRITE("(err)"); break;
                    }
                    break;

                case debug_type_primitive_kind_boolean:
                    if (value.u64 != 0) { WRITE("true"); }
                    else                { WRITE("false"); }
                    break;

                default:
                    WRITE("(err)");
            }
            break;

        case debug_type_kind_modifier:
            switch (type->modifier.modifier_kind) {
                case debug_type_modifier_kind_pointer:
                    switch (type->size) {
                        case 4: WRITE_FORMAT("0x%x",   value.u32); break;
                        case 8: WRITE_FORMAT("0x%lx",  value.u64); break;
                        default: WRITE("(err)"); break;
                    }
                    break;

                default:
                    append_ovm_value_with_type(builder, value, type->modifier.modified_type);
                    break;
            }
            break;

        case debug_type_kind_alias:
            append_ovm_value_with_type(builder, value, type->alias.aliased_type);
            break;

        case debug_type_kind_function:
            WRITE_FORMAT("func[%d]", value.u32);
            break;

        case debug_type_kind_array: {
            void *base = bh_pointer_add(builder->state->ovm_engine->memory, value.u32);
            append_value_from_memory_with_type(builder, base, type_id);
            break;
        }

        default: WRITE("(unknown)"); break;
    }
}

static void append_value_from_stack(debug_runtime_value_builder_t *builder, u32 offset, u32 type_id) {
    ovm_value_t stack_ptr;
    if (!lookup_register_in_frame(builder->ovm_state, builder->ovm_frame, builder->func_info.stack_ptr_idx, &stack_ptr)) {
        WRITE("(no stack ptr)");
        return;
    }

    void *base = bh_pointer_add(builder->state->ovm_engine->memory, stack_ptr.u32 + offset);

    append_value_from_memory_with_type(builder, base, type_id);
}

static void append_value_from_register(debug_runtime_value_builder_t *builder, u32 reg, u32 type_id) {
    ovm_value_t value;

    debug_type_info_t *type = &builder->info->types[type_id];
    if (type->kind == debug_type_kind_structure) {
        WRITE("{ ");

        fori (i, 0, (i32) type->structure.member_count) {
            if (i != 0) WRITE(", ");

            WRITE_FORMAT("%s=", type->structure.members[i].name);

            if (!lookup_register_in_frame(builder->ovm_state, builder->ovm_frame, reg + i, &value)) {
                WRITE("(err)")
                continue;
            }

            append_ovm_value_with_type(builder, value, type->structure.members[i].type);
        }

        WRITE(" }");
        return;
    }

    if (!lookup_register_in_frame(builder->ovm_state, builder->ovm_frame, reg, &value)) {
        WRITE("(err)")
        return;
    }

    append_ovm_value_with_type(builder, value, type_id);
}




void debug_runtime_value_build_init(debug_runtime_value_builder_t *builder, bh_allocator alloc) {
    bh_buffer_init(&builder->output, alloc, 1024);
}

void debug_runtime_value_build_free(debug_runtime_value_builder_t *builder) {
    bh_buffer_free(&builder->output);
}

void debug_runtime_value_build_string(debug_runtime_value_builder_t *builder) {
    if (builder->sym_info.loc_kind == debug_sym_loc_register) {
        append_value_from_register(builder, builder->sym_info.loc, builder->sym_info.type);
        return;
    }

    if (builder->sym_info.loc_kind == debug_sym_loc_stack) {
        append_value_from_stack(builder, builder->sym_info.loc, builder->sym_info.type);
        return;
    }

    WRITE("(location unknown)");
}
