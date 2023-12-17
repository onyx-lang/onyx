
#include "ovm_debug.h"

void debug_info_init(debug_info_t *info) {
    memset(info, 0, sizeof(*info));

    info->alloc = bh_heap_allocator();
    info->has_debug_info = false;
    bh_arr_new(info->alloc, info->funcs, 16);
    bh_arr_new(info->alloc, info->line_info, 1024);
    bh_arr_new(info->alloc, info->instruction_reducer, 4096);
    bh_arr_new(info->alloc, info->files, 16);
    bh_arr_new(info->alloc, info->line_to_instruction, 1024);
    bh_arr_new(info->alloc, info->symbols, 128);
    bh_arr_new(info->alloc, info->symbol_scopes, 128);
}

void debug_info_free(debug_info_t *info) {
    bh_arr_free(info->funcs);
    bh_arr_free(info->line_info);
    bh_arr_free(info->instruction_reducer);

    bh_arr_each(debug_file_info_t, file, info->files) {
        bh_free(info->alloc, file->name);
    }
    bh_arr_free(info->files);
}

void debug_info_import_file_info(debug_info_t *info, u8 *data, u32 len) {
    i32 offset = 0;
    info->has_debug_info = true;

    i32 count = uleb128_to_uint(data, &offset);
    fori (i, 0, (i32) count) {
        debug_file_info_t file_info;
        file_info.line_buffer_offset = -1;

        u32 file_id = uleb128_to_uint(data, &offset);
        file_info.file_id = file_id;
        file_info.line_count = uleb128_to_uint(data, &offset);

        u32 name_length = uleb128_to_uint(data, &offset);
        file_info.name = bh_alloc_array(info->alloc, char, name_length + 1);
        memcpy(file_info.name, data + offset, name_length);
        file_info.name[name_length] = 0;
        offset += name_length;

        bh_arr_set_at(info->files, file_id, file_info);
    }

    assert(offset == len);
}

void debug_info_import_func_info(debug_info_t *info, u8 *data, u32 len) {
    i32 offset = 0;
    info->has_debug_info = true;

    i32 count = uleb128_to_uint(data, &offset);
    fori (i, 0, (i32) count) {
        debug_func_info_t func_info;
        func_info.func_id = uleb128_to_uint(data, &offset);
        func_info.file_id = uleb128_to_uint(data, &offset);
        func_info.line    = uleb128_to_uint(data, &offset);

        u32 name_length = uleb128_to_uint(data, &offset);
        if (name_length == 0) {
            func_info.name = NULL;
        } else {
            func_info.name = bh_alloc_array(info->alloc, char, name_length + 1);
            memcpy(func_info.name, data + offset, name_length);
            func_info.name[name_length] = 0;
            offset += name_length;
        }

        func_info.internal = data[offset++] != 0;
        func_info.debug_op_offset = uleb128_to_uint(data, &offset);
        func_info.stack_ptr_idx = uleb128_to_uint(data, &offset);

        uleb128_to_uint(data, &offset);

        bh_arr_set_at(info->funcs, func_info.func_id, func_info);
    }

    assert(offset == len);
}

void debug_info_import_sym_info(debug_info_t *info, u8 *data, u32 len) {
    i32 offset = 0;
    info->has_debug_info = true;

    i32 count = uleb128_to_uint(data, &offset);
    fori (i, 0, count) {
        debug_sym_info_t sym_info;
        sym_info.sym_id = uleb128_to_uint(data, &offset);

        u32 name_length = uleb128_to_uint(data, &offset);
        if (name_length == 0) {
            sym_info.name = NULL;
        } else {
            sym_info.name = bh_alloc_array(info->alloc, char, name_length + 1);
            memcpy(sym_info.name, data + offset, name_length);
            sym_info.name[name_length] = 0;
            offset += name_length;
        }

        sym_info.loc_kind = uleb128_to_uint(data, &offset);
        sym_info.loc = uleb128_to_uint(data, &offset);
        sym_info.type = uleb128_to_uint(data, &offset);

        bh_arr_set_at(info->symbols, sym_info.sym_id, sym_info);
    }

    assert(offset == len);
}

void debug_info_import_type_info(debug_info_t *info, u8 *data, u32 len) {
    i32 offset = 0;
    info->has_debug_info = true;

    i32 count = uleb128_to_uint(data, &offset);
    fori (i, 0, count) {
        debug_type_info_t type;
        type.id = uleb128_to_uint(data, &offset);

        u32 name_length = uleb128_to_uint(data, &offset);
        if (name_length == 0) {
            type.name = NULL;
        } else {
            type.name = bh_alloc_array(info->alloc, char, name_length + 1);
            memcpy(type.name, data + offset, name_length);
            type.name[name_length] = 0;
            offset += name_length;
        }

        type.size = uleb128_to_uint(data, &offset);
        type.kind = uleb128_to_uint(data, &offset);

        switch (type.kind) {
            case debug_type_kind_primitive:
                type.primitive.primitive_kind = uleb128_to_uint(data, &offset);
                break;

            case debug_type_kind_modifier:
                type.modifier.modifier_kind = uleb128_to_uint(data, &offset);
                type.modifier.modified_type = uleb128_to_uint(data, &offset);
                break;

            case debug_type_kind_structure:
                type.structure.simple = uleb128_to_uint(data, &offset);

                type.structure.member_count = uleb128_to_uint(data, &offset);
                type.structure.members = bh_alloc_array(info->alloc, debug_type_structure_member_t, type.structure.member_count);

                fori (i, 0, type.structure.member_count) {
                    type.structure.members[i].offset = uleb128_to_uint(data, &offset);
                    type.structure.members[i].type   = uleb128_to_uint(data, &offset);

                    u32 name_length = uleb128_to_uint(data, &offset);
                    type.structure.members[i].name = bh_alloc_array(info->alloc, char, name_length + 1);
                    memcpy(type.structure.members[i].name, data + offset, name_length);
                    type.structure.members[i].name[name_length] = 0;
                    offset += name_length;
                }
                break;

            case debug_type_kind_array:
                type.array.count = uleb128_to_uint(data, &offset);
                type.array.type  = uleb128_to_uint(data, &offset);
                break;

            case debug_type_kind_alias:
                type.alias.alias_kind   = uleb128_to_uint(data, &offset);
                type.alias.aliased_type = uleb128_to_uint(data, &offset);
                break;

            case debug_type_kind_function:
                type.function.param_count = uleb128_to_uint(data, &offset);
                type.function.param_types = bh_alloc_array(info->alloc, u32, type.function.param_count);

                fori (i, 0, type.function.param_count) {
                    type.function.param_types[i] = uleb128_to_uint(data, &offset);
                }

                type.function.return_type = uleb128_to_uint(data, &offset);
                break;

            case debug_type_kind_slice:
                type.slice.type = uleb128_to_uint(data, &offset);
                break;

            case debug_type_kind_enum:
                type.enumeration.backing_type = uleb128_to_uint(data, &offset);
                type.enumeration.value_count = uleb128_to_uint(data, &offset);
                type.enumeration.values = bh_alloc_array(info->alloc, debug_type_enum_value_t, type.enumeration.value_count);

                fori (i, 0, type.enumeration.value_count) {
                    type.enumeration.values[i].value = uleb128_to_uint(data, &offset);

                    u32 name_length = uleb128_to_uint(data, &offset);
                    type.enumeration.values[i].name = bh_alloc_array(info->alloc, char, name_length + 1);
                    memcpy(type.enumeration.values[i].name, data + offset, name_length);
                    type.enumeration.values[i].name[name_length] = 0;
                    offset += name_length;
                }
                break;

            case debug_type_kind_union:
                type.onion.tag_size = uleb128_to_uint(data, &offset);
                type.onion.variant_count = uleb128_to_uint(data, &offset);
                type.onion.variants = bh_alloc_array(info->alloc, debug_type_union_variant_t, type.onion.variant_count);

                fori (i, 0, type.onion.variant_count) {
                    u32 name_length = uleb128_to_uint(data, &offset);
                    type.onion.variants[i].name = bh_alloc_array(info->alloc, char, name_length + 1);
                    memcpy(type.onion.variants[i].name, data + offset, name_length);
                    type.onion.variants[i].name[name_length] = 0;
                    offset += name_length;

                    type.onion.variants[i].type = uleb128_to_uint(data, &offset);
                }
                break;

            // Error handling
            default: assert(0 && "Unrecognized type kind");
        }

        bh_arr_set_at(info->types, type.id, type);
    }

    assert(offset == len);
}

bool debug_info_lookup_location(debug_info_t *info, u32 instruction, debug_loc_info_t *out) {
    if (!info || !info->has_debug_info) return false;

    if (instruction > (u32) bh_arr_length(info->instruction_reducer)) return false;
    i32 loc = info->instruction_reducer[instruction];
    if (loc < 0) return false;

    *out = info->line_info[loc];
    return true;
}

bool debug_info_lookup_file(debug_info_t *info, u32 file_id, debug_file_info_t *out) {
    if (!info || !info->has_debug_info) return false;

    if (file_id > (u32) bh_arr_length(info->files)) return false;
    *out = info->files[file_id];
    return true;
}

i32 debug_info_lookup_instr_by_file_line(debug_info_t *info, char *filename, u32 line) {
    if (!info || !info->has_debug_info) return 0;

    debug_file_info_t file_info;
    bool file_found = debug_info_lookup_file_by_name(info, filename, &file_info);
    if (!file_found) {
        return -1;
    }

    if (line > file_info.line_count) {
        return -1;
    }

    u32 instr;
    while ((instr = info->line_to_instruction[file_info.line_buffer_offset + line]) == 0) {
        line += 1;

        if (line > file_info.line_count) {
            return -1;
        }
    }

    return instr;
}

char *debug_info_type_enum_find_name(debug_info_t *info, u32 enum_type, u64 value) {
    debug_type_info_t *type = &info->types[enum_type];
    if (type->kind != debug_type_kind_enum) return NULL;

    fori (i, 0, type->enumeration.value_count) {
        if (type->enumeration.values[i].value == value) {
            return type->enumeration.values[i].name;
        }
    }

    return NULL;
}

//
// For now, this is going to compare the strings exactly. In the future, it might be a good
// to do a levenschtein distance or something, so the full path isn't needed.
bool debug_info_lookup_file_by_name(debug_info_t *info, char *name, debug_file_info_t *out) {
    if (!info || !info->has_debug_info) return false;

    bh_arr_each(debug_file_info_t, file, info->files) {
        if (bh_str_ends_with(file->name, name)) {
            *out = *file;
            return true;
        }
    }

    return false;
}

bool debug_info_lookup_func(debug_info_t *info, u32 func_id, debug_func_info_t *out) {
    if (!info || !info->has_debug_info) return false;

    if (func_id > (u32) bh_arr_length(info->funcs)) return false;
    *out = info->funcs[func_id];
    return true;
}
