
#include "ovm_debug.h"
#include "vm.h"

#include <ctype.h>

#define MAX_SLICE_LENGTH 64

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
        val_num_base = frame->value_number_base;
    }

    *out = state->numbered_values[val_num_base + reg];
    return true;
}

static bool lookup_stack_pointer(debug_runtime_value_builder_t *builder, u32 *out) {
    ovm_value_t stack_ptr;
    if (!lookup_register_in_frame(builder->ovm_state, builder->ovm_frame, builder->func_info.stack_ptr_idx, &stack_ptr)) {
        return false;
    }

    *out = stack_ptr.u32;
    return true;
}

static void append_slice_from_memory(debug_runtime_value_builder_t *builder, void *elem_data, u32 count, u32 type_id);

static void append_value_from_memory_with_type(debug_runtime_value_builder_t *builder, void *base, u32 type_id) {
    debug_type_info_t *type = &builder->info->types[type_id];

    switch (type->kind) {
        case debug_type_kind_primitive:
            switch (type->primitive.primitive_kind) {
                case debug_type_primitive_kind_void: WRITE("void"); break;
                case debug_type_primitive_kind_signed_integer:
                    switch (type->size) {
                        case 1: WRITE_FORMAT("%hhd",  *(i8 *)  base); break;
                        case 2: WRITE_FORMAT("%hd",   *(i16 *) base); break;
                        case 4: WRITE_FORMAT("%d",    *(i32 *) base); break;
                        case 8: WRITE_FORMAT("%lld",  *(i64 *) base); break;
                        default: WRITE("(err)"); break;
                    }
                    break;

                case debug_type_primitive_kind_unsigned_integer:
                    switch (type->size) {
                        case 1: WRITE_FORMAT("%hhu",  *(u8 *) base); break;
                        case 2: WRITE_FORMAT("%hu",   *(u16 *) base); break;
                        case 4: WRITE_FORMAT("%u",    *(u32 *) base); break;
                        case 8: WRITE_FORMAT("%llu",  *(u64 *) base); break;
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

                case debug_type_primitive_kind_character: {
                    unsigned char c = *(u8 *) base;
                    if (!iscntrl(c) && isascii(c)) {
                        WRITE_FORMAT("%c", c);
                    } else {
                        WRITE_FORMAT("\\x%02hhx", c);
                    }
                    break;
                }

                default:
                    WRITE("(err)");
            }
            break;

        case debug_type_kind_modifier:
            switch (type->modifier.modifier_kind) {
                case debug_type_modifier_kind_pointer:
                    switch (type->size) {
                        case 4: WRITE_FORMAT("0x%x",    *(u32 *) base); break;
                        case 8: WRITE_FORMAT("0x%llx",  *(u64 *) base); break;
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

        case debug_type_kind_slice: {
            void *elem_data = bh_pointer_add(builder->state->ovm_engine->memory, *(u32 *) base);
            u32 count = *(u32 *) bh_pointer_add(base, 4);
            u32 type_id = type->slice.type;

            append_slice_from_memory(builder, elem_data, count, type_id);
            break;
        }

        case debug_type_kind_enum: {
            debug_type_info_t *backing_type = &builder->info->types[type->enumeration.backing_type];

            u64 value = 0;
            switch (backing_type->size) {
                case 1: value = *(u8  *) base; break;
                case 2: value = *(u16 *) base; break;
                case 4: value = *(u32 *) base; break;
                case 8: value = *(u64 *) base; break;
            }

            char *name = debug_info_type_enum_find_name(builder->info, type_id, value);
            if (name) {
                WRITE(name);
            } else {
                WRITE_FORMAT("%llu", value);
            }

            break;
        }

        case debug_type_kind_union: {
            u32 variant = *(u32 *) base;
            if (variant >= type->onion.variant_count) {
                WRITE("unknown_variant");

            } else {
                debug_type_union_variant_t *uv = &type->onion.variants[variant];

                WRITE_FORMAT("%s(", uv->name);
                append_value_from_memory_with_type(builder, bh_pointer_add(base, type->onion.tag_size), uv->type);
                WRITE(")");
            }
            break;
        }

        default: WRITE("(unknown)"); break;
    }
}

static void append_slice_from_memory(debug_runtime_value_builder_t *builder, void *elem_data, u32 count, u32 type_id) {
    debug_type_info_t *elem_type = &builder->info->types[type_id];

    b32 count_overflowed = 0;
    if (count > MAX_SLICE_LENGTH) {
        count = MAX_SLICE_LENGTH;
        count_overflowed = 1;
    }

    if (elem_type->kind == debug_type_kind_primitive &&
        elem_type->primitive.primitive_kind == debug_type_primitive_kind_character) {
        WRITE("\"");

        fori (i, 0, (i32) count) {
            append_value_from_memory_with_type(builder, bh_pointer_add(elem_data, i * elem_type->size), elem_type->id);
        }

        if (count_overflowed) WRITE("...");
        WRITE("\"");
        return;
    }

    WRITE("[");

    fori (i, 0, (i32) count) {
        if (i != 0) WRITE(", ");
        append_value_from_memory_with_type(builder, bh_pointer_add(elem_data, i * elem_type->size), elem_type->id);
    }

    if (count_overflowed) WRITE("...");
    WRITE("]");
}


static void append_ovm_value_with_type(debug_runtime_value_builder_t *builder, ovm_value_t value, u32 type_id) {
    debug_type_info_t *type = &builder->info->types[type_id];

    switch (type->kind) {
        case debug_type_kind_primitive:
            switch (type->primitive.primitive_kind) {
                case debug_type_primitive_kind_void: WRITE("void"); break;
                case debug_type_primitive_kind_signed_integer:
                    switch (type->size) {
                        case 1: WRITE_FORMAT("%hhd",  value.i8); break;
                        case 2: WRITE_FORMAT("%hd",   value.i16); break;
                        case 4: WRITE_FORMAT("%d",    value.i32); break;
                        case 8: WRITE_FORMAT("%lld",  value.i64); break;
                        default: WRITE("(err)"); break;
                    }
                    break;

                case debug_type_primitive_kind_unsigned_integer:
                    switch (type->size) {
                        case 1: WRITE_FORMAT("%hhu",  value.u8); break;
                        case 2: WRITE_FORMAT("%hu",   value.u16); break;
                        case 4: WRITE_FORMAT("%u",    value.u32); break;
                        case 8: WRITE_FORMAT("%llu",  value.u64); break;
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
                        case 8: WRITE_FORMAT("0x%llx", value.u64); break;
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

        // TODO: Should this output all of the members?
        // Is this guaranteed to only have 1 member?
        case debug_type_kind_structure: {
            append_ovm_value_with_type(builder, value, type->structure.members[0].type);
            break;
        }

        case debug_type_kind_enum: {
            char *name = debug_info_type_enum_find_name(builder->info, type_id, value.u64);
            if (name == NULL) {
                WRITE_FORMAT("%llu", value.u64);
            } else {
                WRITE(name);
            }

            break;
        }

        default: WRITE("(unknown)"); break;
    }
}

static void append_value_from_stack(debug_runtime_value_builder_t *builder, u32 offset, u32 type_id) {
    u32 stack_ptr;
    if (!lookup_stack_pointer(builder, &stack_ptr)) {
        WRITE("(no stack ptr)");
        return;
    }

    void *base = bh_pointer_add(builder->state->ovm_engine->memory, stack_ptr + offset);

    append_value_from_memory_with_type(builder, base, type_id);
}

static void append_value_from_register(debug_runtime_value_builder_t *builder, u32 reg, u32 type_id) {
    ovm_value_t value;

    debug_type_info_t *type = &builder->info->types[type_id];
    if (type->kind == debug_type_kind_structure) {
        if (!type->structure.simple) {
            if (lookup_register_in_frame(builder->ovm_state, builder->ovm_frame, builder->base_loc, &value)) {
                void *base = bh_pointer_add(builder->state->ovm_engine->memory, value.u32);
                append_value_from_memory_with_type(builder, base, type_id);
            }

            return;
        }

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

    if (type->kind == debug_type_kind_slice) {
        ovm_value_t base_reg;
        ovm_value_t count_reg;

        if (!lookup_register_in_frame(builder->ovm_state, builder->ovm_frame, reg, &base_reg)) return;
        if (!lookup_register_in_frame(builder->ovm_state, builder->ovm_frame, reg + 1, &count_reg)) return;

        void *elem_data = bh_pointer_add(builder->state->ovm_engine->memory, base_reg.u32);
        u32 count = count_reg.u32;

        append_slice_from_memory(builder, elem_data, count, type->slice.type);
        return;
    }

    if (!lookup_register_in_frame(builder->ovm_state, builder->ovm_frame, reg, &value)) {
        WRITE("(err)")
        return;
    }

    append_ovm_value_with_type(builder, value, type_id);
}

static u32 get_subvalues_for_type(debug_runtime_value_builder_t *builder, u32 type) {
    debug_type_info_t *t = &builder->info->types[type];
    switch (t->kind) {
        case debug_type_kind_primitive: return 0;
        case debug_type_kind_function:  return 0;
        case debug_type_kind_enum:      return 0;

        case debug_type_kind_modifier:
            if (t->modifier.modifier_kind == debug_type_modifier_kind_pointer) return 1;
            return 0;

        case debug_type_kind_alias:
            return get_subvalues_for_type(builder, t->alias.aliased_type);

        case debug_type_kind_structure:
            return t->structure.member_count;

        case debug_type_kind_array: return t->array.count;

        case debug_type_kind_slice: {
            // :Refactor
            u32 count = 0;
            if (builder->base_loc_kind == debug_sym_loc_register) {
                ovm_value_t value;
                if (!lookup_register_in_frame(builder->ovm_state, builder->ovm_frame, builder->base_loc + 1, &value)) {
                    return 0;
                }

                count = value.u32;
            }

            else if (builder->base_loc_kind == debug_sym_loc_stack) {
                u32 stack_ptr;
                if (!lookup_stack_pointer(builder, &stack_ptr)) {
                    return 0;
                }

                u32 *ptr_loc = bh_pointer_add(builder->state->ovm_engine->memory, stack_ptr + builder->base_loc + 4);
                count = *ptr_loc;
            }

            else if (builder->base_loc_kind == debug_sym_loc_global) {
                u32 *ptr_loc = bh_pointer_add(builder->state->ovm_engine->memory, builder->base_loc + 4);
                count = *ptr_loc;
            }

            if (count > MAX_SLICE_LENGTH) {
                count = MAX_SLICE_LENGTH;
            }

            return count;
        }

        default: return 0;
    }
}




void debug_runtime_value_build_init(debug_runtime_value_builder_t *builder, bh_allocator alloc) {
    bh_buffer_init(&builder->output, alloc, 1024);
}

void debug_runtime_value_build_set_location(debug_runtime_value_builder_t *builder,
    debug_sym_loc_kind_t loc_kind, u32 loc, u32 type, char *name) {
    builder->base_loc_kind = loc_kind;
    builder->base_loc = loc;
    builder->base_type = type;

    builder->max_index = get_subvalues_for_type(builder, type);
    builder->it_index = 0;
    builder->it_name = name;
    builder->it_loc = loc;
    builder->it_type = type;
    builder->it_loc_kind = loc_kind;
    builder->it_has_children = builder->max_index > 0;
}

void debug_runtime_value_build_descend(debug_runtime_value_builder_t *builder, u32 index) {
    debug_type_info_t *type = &builder->info->types[builder->base_type];

    if (type->kind == debug_type_kind_modifier && type->modifier.modifier_kind == debug_type_modifier_kind_pointer) {
        if (index > 0) {
            goto bad_case;
        }

        builder->base_type = type->modifier.modified_type;

        if (builder->base_loc_kind == debug_sym_loc_register) {
            ovm_value_t value;
            if (!lookup_register_in_frame(builder->ovm_state, builder->ovm_frame, builder->base_loc, &value)) {
                goto bad_case;
            }

            builder->base_loc_kind = debug_sym_loc_global;
            builder->base_loc = value.u32;
        }

        else if (builder->base_loc_kind == debug_sym_loc_stack) {
            u32 stack_ptr;
            if (!lookup_stack_pointer(builder, &stack_ptr)) {
                goto bad_case;
            }

            u32 *ptr_loc = bh_pointer_add(builder->state->ovm_engine->memory, stack_ptr + builder->base_loc);
            builder->base_loc = *ptr_loc;
            builder->base_loc_kind = debug_sym_loc_global;
        }

        else if (builder->base_loc_kind == debug_sym_loc_global) {
            u32 *ptr_loc = bh_pointer_add(builder->state->ovm_engine->memory, builder->base_loc);
            builder->base_loc = *ptr_loc;
        }
    }

    if (type->kind == debug_type_kind_structure) {
        if (index >= type->structure.member_count) {
            goto bad_case;
        }

        debug_type_structure_member_t *mem = &type->structure.members[index];
        builder->base_type = mem->type;
        builder->it_name = mem->name;

        if (builder->base_loc_kind == debug_sym_loc_register) {
            if (type->structure.simple) {
                builder->base_loc += index;

            } else {
                ovm_value_t value;
                if (!lookup_register_in_frame(builder->ovm_state, builder->ovm_frame, builder->base_loc, &value)) {
                    goto bad_case;
                }

                builder->base_loc_kind = debug_sym_loc_global;
                builder->base_loc = value.u32 + mem->offset;
            }
        }

        else if (builder->base_loc_kind == debug_sym_loc_stack || builder->base_loc_kind == debug_sym_loc_global) {
            builder->base_loc += mem->offset;
        }
    }

    if (type->kind == debug_type_kind_array) {
        builder->base_type = type->array.type;

        debug_type_info_t *sub_type = &builder->info->types[builder->base_type];

        if (builder->base_loc_kind == debug_sym_loc_register) {
            ovm_value_t value;
            if (!lookup_register_in_frame(builder->ovm_state, builder->ovm_frame, builder->base_loc, &value)) {
                goto bad_case;
            }

            builder->base_loc_kind = debug_sym_loc_global;
            builder->base_loc = value.u32 + sub_type->size * index;
        }

        else if (builder->base_loc_kind == debug_sym_loc_stack || builder->base_loc_kind == debug_sym_loc_global) {
            builder->base_loc += sub_type->size * index;
        }
    }

    if (type->kind == debug_type_kind_slice) {
        builder->base_type = type->slice.type;
        debug_type_info_t *sub_type = &builder->info->types[builder->base_type];

        if (builder->base_loc_kind == debug_sym_loc_register) {
            ovm_value_t value;
            if (!lookup_register_in_frame(builder->ovm_state, builder->ovm_frame, builder->base_loc, &value)) {
                goto bad_case;
            }

            builder->base_loc_kind = debug_sym_loc_global;
            builder->base_loc = value.u32 + sub_type->size * index;
        }

        else if (builder->base_loc_kind == debug_sym_loc_stack) {
            u32 stack_ptr;
            if (!lookup_stack_pointer(builder, &stack_ptr)) {
                goto bad_case;
            }

            u32 *data_loc = bh_pointer_add(builder->state->ovm_engine->memory, stack_ptr + builder->base_loc);

            builder->base_loc_kind = debug_sym_loc_global;
            builder->base_loc = *data_loc + index * sub_type->size;
        }

        else if (builder->base_loc_kind == debug_sym_loc_global) {
            u32 *data_loc = bh_pointer_add(builder->state->ovm_engine->memory, builder->base_loc);
            builder->base_loc = *data_loc + index * sub_type->size;
        }
    }

    builder->max_index = get_subvalues_for_type(builder, builder->base_type);
    builder->it_index = 0;
    return;

  bad_case:
    builder->base_loc_kind = debug_sym_loc_unknown;
    return;
}

bool debug_runtime_value_build_step(debug_runtime_value_builder_t *builder) {
    if (builder->it_index >= builder->max_index) return false;

    debug_type_info_t *type = &builder->info->types[builder->base_type];
    static char name_buffer[2048];
    static char tmp_buffer[2048];

    if (type->kind == debug_type_kind_modifier && type->modifier.modifier_kind == debug_type_modifier_kind_pointer) {
        // Double buffering here so if there are multiple
        // pointer descentions, the names don't get mangled.
        snprintf(tmp_buffer, 2048, "*%s", builder->it_name);
        strncpy(name_buffer, tmp_buffer, 2048);

        builder->it_name = name_buffer;
        builder->it_type = type->modifier.modified_type;
        builder->it_has_children = get_subvalues_for_type(builder, builder->it_type) > 0;

        if (builder->base_loc_kind == debug_sym_loc_register) {
            ovm_value_t value;
            if (lookup_register_in_frame(builder->ovm_state, builder->ovm_frame, builder->base_loc, &value)) {
                builder->it_loc_kind = debug_sym_loc_global;
                builder->it_loc = value.u32;
            }
        }

        if (builder->base_loc_kind == debug_sym_loc_stack) {
            u32 stack_ptr;
            if (!lookup_stack_pointer(builder, &stack_ptr)) {
                return false;
            }

            u32 *data_loc = bh_pointer_add(builder->state->ovm_engine->memory, stack_ptr + builder->base_loc);

            builder->it_loc_kind = debug_sym_loc_global;
            builder->it_loc = *data_loc;
        }

        if (builder->base_loc_kind == debug_sym_loc_global) {
            u32 *data_loc = bh_pointer_add(builder->state->ovm_engine->memory, builder->base_loc);

            builder->it_loc_kind = debug_sym_loc_global;
            builder->it_loc = *data_loc;
        }
    }

    if (type->kind == debug_type_kind_structure) {
        debug_type_structure_member_t *mem = &type->structure.members[builder->it_index];
        builder->it_name = mem->name;
        builder->it_has_children = get_subvalues_for_type(builder, mem->type) > 0;
        builder->it_type = mem->type;

        if (builder->base_loc_kind == debug_sym_loc_register) {
            if (type->structure.simple) {
                builder->it_loc_kind = debug_sym_loc_register;
                builder->it_loc = builder->base_loc + builder->it_index;

            } else {
                ovm_value_t value;
                if (!lookup_register_in_frame(builder->ovm_state, builder->ovm_frame, builder->base_loc, &value)) {
                    return false;
                }

                builder->it_loc_kind = debug_sym_loc_global;
                builder->it_loc = value.u32 + mem->offset;
            }
        }

        if (builder->base_loc_kind == debug_sym_loc_stack) {
            builder->it_loc_kind = debug_sym_loc_stack;
            builder->it_loc = builder->base_loc + mem->offset;
        }

        if (builder->base_loc_kind == debug_sym_loc_global) {
            builder->it_loc_kind = debug_sym_loc_global;
            builder->it_loc = builder->base_loc + mem->offset;
        }
    }

    if (type->kind == debug_type_kind_array) {
        snprintf(tmp_buffer, 2048, "[%d]", builder->it_index);
        strncpy(name_buffer, tmp_buffer, 2048);
        builder->it_name = name_buffer;
        builder->it_type = type->array.type;
        builder->it_has_children = get_subvalues_for_type(builder, builder->it_type) > 0;

        debug_type_info_t *sub_type = &builder->info->types[builder->it_type];

        if (builder->base_loc_kind == debug_sym_loc_register) {
            ovm_value_t value;
            if (lookup_register_in_frame(builder->ovm_state, builder->ovm_frame, builder->base_loc, &value)) {
                builder->base_loc_kind = debug_sym_loc_global;
                builder->base_loc = value.u32 + sub_type->size * builder->it_index;
            }
        }

        if (builder->base_loc_kind == debug_sym_loc_stack) {
            builder->it_loc_kind = debug_sym_loc_stack;
            builder->it_loc = builder->base_loc + sub_type->size * builder->it_index;
        }

        if (builder->base_loc_kind == debug_sym_loc_global) {
            builder->it_loc_kind = debug_sym_loc_global;
            builder->it_loc = builder->base_loc + sub_type->size * builder->it_index;
        }
    }

    if (type->kind == debug_type_kind_slice) {
        snprintf(tmp_buffer, 2048, "[%d]", builder->it_index);
        strncpy(name_buffer, tmp_buffer, 2048);
        builder->it_name = name_buffer;
        builder->it_type = type->slice.type;
        builder->it_has_children = get_subvalues_for_type(builder, builder->it_type) > 0;

        debug_type_info_t *sub_type = &builder->info->types[builder->it_type];

        if (builder->base_loc_kind == debug_sym_loc_register) {
            ovm_value_t value;
            if (lookup_register_in_frame(builder->ovm_state, builder->ovm_frame, builder->base_loc, &value)) {
                builder->it_loc_kind = debug_sym_loc_global;
                builder->it_loc = value.u32 + sub_type->size * builder->it_index;
            }
        }

        if (builder->base_loc_kind == debug_sym_loc_stack) {
            u32 stack_ptr;
            if (!lookup_stack_pointer(builder, &stack_ptr)) {
                return false;
            }

            u32 *data_loc = bh_pointer_add(builder->state->ovm_engine->memory, stack_ptr + builder->base_loc);

            builder->it_loc_kind = debug_sym_loc_global;
            builder->it_loc = *data_loc + sub_type->size * builder->it_index;
        }

        if (builder->base_loc_kind == debug_sym_loc_global) {
            u32 *data_loc = bh_pointer_add(builder->state->ovm_engine->memory, builder->base_loc);

            builder->it_loc_kind = debug_sym_loc_global;
            builder->it_loc = *data_loc + sub_type->size * builder->it_index;
        }
    }

    builder->it_index++;
    return true;
}

u32 debug_runtime_value_get_it_addr(debug_runtime_value_builder_t *builder) {
    if (builder->it_loc_kind == debug_sym_loc_register) {
        debug_type_info_t *type = &builder->info->types[builder->it_type];

        if (type->kind == debug_type_kind_slice) {
            ovm_value_t value;
            if (lookup_register_in_frame(builder->ovm_state, builder->ovm_frame, builder->it_loc, &value)) {
                return value.u32;
            }
        }

        return 0;
    }

    if (builder->it_loc_kind == debug_sym_loc_stack) {
        u32 stack_ptr;
        if (!lookup_stack_pointer(builder, &stack_ptr)) {
            return 0;
        }

        return builder->it_loc + stack_ptr;
    }

    if (builder->it_loc_kind == debug_sym_loc_global) {
        return builder->it_loc;
    }

    return 0;
}

void debug_runtime_value_build_string(debug_runtime_value_builder_t *builder) {
    if (builder->it_loc_kind == debug_sym_loc_register) {
        append_value_from_register(builder, builder->it_loc, builder->it_type);
        return;
    }

    if (builder->it_loc_kind == debug_sym_loc_stack) {
        append_value_from_stack(builder, builder->it_loc, builder->it_type);
        return;
    }

    if (builder->it_loc_kind == debug_sym_loc_global) {
        void *base = bh_pointer_add(builder->state->ovm_engine->memory, builder->it_loc);
        append_value_from_memory_with_type(builder, base, builder->it_type);
        return;
    }

    WRITE("(location unknown)");
}

void debug_runtime_value_build_clear(debug_runtime_value_builder_t *builder) {
    bh_buffer_clear(&builder->output);
}

void debug_runtime_value_build_free(debug_runtime_value_builder_t *builder) {
    bh_buffer_free(&builder->output);
}
