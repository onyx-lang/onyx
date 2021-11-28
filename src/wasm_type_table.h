// This file is directly included in src/onxywasm.c
// It is here purely to decrease the amount of clutter in the main file.


u64 build_type_table(OnyxWasmModule* module) {

    bh_arr(u32) base_patch_locations=NULL;
    bh_arr_new(global_heap_allocator, base_patch_locations, 256);

#define PATCH (bh_arr_push(base_patch_locations, table_buffer.length))

    // This is the data behind the "type_table" slice in type_info.onyx
    u32 type_count = bh_arr_length(type_map.entries) + 1;
    u64* table_info = bh_alloc_array(global_heap_allocator, u64, type_count); // HACK
    memset(table_info, 0, type_count * sizeof(u64));

    bh_buffer table_buffer;
    bh_buffer_init(&table_buffer, global_heap_allocator, 4096);

    // Write a "NULL" at the beginning so nothing will have to point to the first byte of the buffer.
    bh_buffer_write_u64(&table_buffer, 0);

    bh_arr_each(bh__imap_entry, type_entry, type_map.entries) {
        u64 type_idx = type_entry->key;
        Type* type = (Type *) type_entry->value;

        switch (type->kind) {
            case Type_Kind_Basic: {
                table_info[type_idx] = table_buffer.length;
                bh_buffer_write_u32(&table_buffer, type->kind);
                bh_buffer_write_u32(&table_buffer, type_size_of(type));
                bh_buffer_write_u32(&table_buffer, type_alignment_of(type));
                bh_buffer_write_u32(&table_buffer, type->Basic.kind);
                break;
            }

            case Type_Kind_Pointer: {
                table_info[type_idx] = table_buffer.length;
                bh_buffer_write_u32(&table_buffer, type->kind);
                bh_buffer_write_u32(&table_buffer, type_size_of(type));
                bh_buffer_write_u32(&table_buffer, type_alignment_of(type));
                bh_buffer_write_u32(&table_buffer, type->Pointer.elem->id);
                break;
            }

            case Type_Kind_Array: {
                table_info[type_idx] = table_buffer.length;
                bh_buffer_write_u32(&table_buffer, type->kind);
                bh_buffer_write_u32(&table_buffer, type_size_of(type));
                bh_buffer_write_u32(&table_buffer, type_alignment_of(type));
                bh_buffer_write_u32(&table_buffer, type->Array.elem->id);
                bh_buffer_write_u32(&table_buffer, type->Array.count);
                break;
            }

            case Type_Kind_Slice: {
                table_info[type_idx] = table_buffer.length;
                bh_buffer_write_u32(&table_buffer, type->kind);
                bh_buffer_write_u32(&table_buffer, type_size_of(type));
                bh_buffer_write_u32(&table_buffer, type_alignment_of(type));
                bh_buffer_write_u32(&table_buffer, type->Slice.elem->id);
                break;
            }

            case Type_Kind_DynArray: {
                table_info[type_idx] = table_buffer.length;
                bh_buffer_write_u32(&table_buffer, type->kind);
                bh_buffer_write_u32(&table_buffer, type_size_of(type));
                bh_buffer_write_u32(&table_buffer, type_alignment_of(type));
                bh_buffer_write_u32(&table_buffer, type->DynArray.elem->id);
                break;
            }

            case Type_Kind_VarArgs: {
                table_info[type_idx] = table_buffer.length;
                bh_buffer_write_u32(&table_buffer, type->kind);
                bh_buffer_write_u32(&table_buffer, type_size_of(type));
                bh_buffer_write_u32(&table_buffer, type_alignment_of(type));
                bh_buffer_write_u32(&table_buffer, type->VarArgs.elem->id);
                break;
            }

            case Type_Kind_Compound: {
                u32 components_base = table_buffer.length;

                u32 components_count = type->Compound.count;
                fori (i, 0, components_count) {
                    u32 type_idx = type->Compound.types[i]->id;
                    bh_buffer_write_u32(&table_buffer, type_idx);
                }

                bh_buffer_align(&table_buffer, 8);
                table_info[type_idx] = table_buffer.length;
                bh_buffer_write_u32(&table_buffer, type->kind);
                bh_buffer_write_u32(&table_buffer, type_size_of(type));
                bh_buffer_write_u32(&table_buffer, type_alignment_of(type));
                bh_buffer_align(&table_buffer, 8);
                PATCH;
                bh_buffer_write_u64(&table_buffer, components_base);
                bh_buffer_write_u64(&table_buffer, components_count);
                break;
            }

            case Type_Kind_Function: {
                u32 parameters_base = table_buffer.length;

                u32 parameters_count = type->Function.param_count;
                fori (i, 0, parameters_count) {
                    u32 type_idx = type->Function.params[i]->id;
                    bh_buffer_write_u32(&table_buffer, type_idx);
                }

                table_info[type_idx] = table_buffer.length;
                bh_buffer_write_u32(&table_buffer, type->kind);
                bh_buffer_write_u32(&table_buffer, type_size_of(type));
                bh_buffer_write_u32(&table_buffer, type_alignment_of(type));
                bh_buffer_write_u32(&table_buffer, type->Function.return_type->id);

                PATCH;
                bh_buffer_write_u64(&table_buffer, parameters_base);
                bh_buffer_write_u64(&table_buffer, parameters_count);

                bh_buffer_write_u32(&table_buffer, type->Function.vararg_arg_pos > 0 ? 1 : 0);
                break;
            }

            case Type_Kind_Enum: {
                AstEnumType* ast_enum = (AstEnumType *) type->ast_type;
                u32 member_count = bh_arr_length(ast_enum->values);
                u32* name_locations = bh_alloc_array(global_scratch_allocator, u32, member_count);

                u32 i = 0;
                bh_arr_each(AstEnumValue *, value, ast_enum->values) {
                    name_locations[i++] = table_buffer.length;

                    bh_buffer_append(&table_buffer, (*value)->token->text, (*value)->token->length);
                }
                bh_buffer_align(&table_buffer, 8);

                u32 member_base = table_buffer.length;
                i = 0;
                bh_arr_each(AstEnumValue *, value, ast_enum->values) {
                    u32 name_loc = name_locations[i++];

                    bh_buffer_align(&table_buffer, 8);
                    PATCH;
                    bh_buffer_write_u64(&table_buffer, name_loc);
                    bh_buffer_write_u64(&table_buffer, (*value)->token->length);

                    assert((*value)->value->kind == Ast_Kind_NumLit);
                    AstNumLit *num = (AstNumLit *) (*value)->value;
                    bh_buffer_write_u64(&table_buffer, num->value.l);
                }

                u32 name_base = table_buffer.length;
                u32 name_length = strlen(type->Enum.name);
                bh_buffer_append(&table_buffer, type->Enum.name, name_length);
                bh_buffer_align(&table_buffer, 8);

                table_info[type_idx] = table_buffer.length;
                bh_buffer_write_u32(&table_buffer, type->kind);
                bh_buffer_write_u32(&table_buffer, type_size_of(type));
                bh_buffer_write_u32(&table_buffer, type_alignment_of(type));
                bh_buffer_write_u32(&table_buffer, type->Enum.backing->id);
                PATCH;
                bh_buffer_write_u64(&table_buffer, name_base);
                bh_buffer_write_u64(&table_buffer, name_length);
                PATCH;
                bh_buffer_write_u64(&table_buffer, member_base);
                bh_buffer_write_u64(&table_buffer, member_count);
                bh_buffer_write_u32(&table_buffer, type->Enum.is_flags ? 1 : 0);
                break;
            }

            case Type_Kind_Struct: {
                TypeStruct* s = &type->Struct;
                u32* name_locations = bh_alloc_array(global_scratch_allocator, u32, s->mem_count);
                u32* param_locations = bh_alloc_array(global_scratch_allocator, u32, bh_arr_length(s->poly_sln));
                u32* value_locations = bh_alloc_array(global_scratch_allocator, u32, s->mem_count);
                u32* meta_locations = bh_alloc_array(global_scratch_allocator, u32, s->mem_count);
                u32* struct_tag_locations = bh_alloc_array(global_scratch_allocator, u32, bh_arr_length(s->meta_tags));
                memset(value_locations, 0, s->mem_count * sizeof(u32));
                memset(meta_locations, 0, s->mem_count * sizeof(u32));
                memset(struct_tag_locations, 0, bh_arr_length(s->meta_tags) * sizeof(u32));

                u32 i = 0;
                bh_arr_each(StructMember*, pmem, s->memarr) {
                    StructMember* mem = *pmem;

                    name_locations[i++] = table_buffer.length;
                    bh_buffer_append(&table_buffer, mem->name, strlen(mem->name));
                }

                bh_buffer_align(&table_buffer, 8);

                i = 0;
                bh_arr_each(AstPolySolution, sln, s->poly_sln) {
                    bh_buffer_align(&table_buffer, 8);
                    param_locations[i++] = table_buffer.length;

                    switch (sln->kind) {
                        case PSK_Type: {
                            // NOTE: This assumes a little endian compiler (which is assumed in other part of the code too)
                            bh_buffer_append(&table_buffer, &sln->type->id, 4);
                            break;
                        }

                        case PSK_Value: {
                            assert(sln->value->type);
                            u32 size = type_size_of(sln->value->type);

                            bh_buffer_grow(&table_buffer, table_buffer.length + size);
                            u8* buffer = table_buffer.data + table_buffer.length;
                            emit_raw_data(module, buffer, sln->value);
                            table_buffer.length += size;
                            break;
                        }

                        default: {
                            // Set to null if this is not known how to encode
                            param_locations[i-1] = 0;
                            break;
                        }
                    }
                }

                bh_buffer_align(&table_buffer, 8);

                i = 0;
                bh_arr_each(StructMember*, pmem, s->memarr) {
                    StructMember* mem = *pmem;

                    if (mem->initial_value == NULL || *mem->initial_value == NULL) {
                        i++;
                        continue;
                    }

                    AstTyped* value = *mem->initial_value;
                    assert(value->type);

                    if ((value->flags & Ast_Flag_Comptime) == 0) {
                        // onyx_report_warning(value->token->pos, "Warning: skipping generating default value for '%s' in '%s' because it is not compile-time known.\n", mem->name, s->name);
                        i++;
                        continue;
                    }

                    u32 size = type_size_of(value->type);
                    bh_buffer_align(&table_buffer, type_alignment_of(value->type));

                    bh_buffer_grow(&table_buffer, table_buffer.length + size);
                    u8* buffer = table_buffer.data + table_buffer.length;

                    if (!emit_raw_data_(module, buffer, value)) {
                        // Failed to generate raw data
                        // onyx_report_warning(value->token->pos, "Warning: failed to generate default value for '%s' in '%s'.\n", mem->name, s->name);
                        value_locations[i++] = 0;

                    } else {
                        // Success 
                        value_locations[i++] = table_buffer.length;
                        table_buffer.length += size;
                    }
                }

                i = 0;
                bh_arr_each(StructMember*, pmem, s->memarr) {
                    StructMember* mem = *pmem;

                    if (mem->meta_tags == NULL) {
                        i += 1;
                        continue;
                    }

                    bh_arr(AstTyped *) meta_tags = mem->meta_tags;
                    assert(meta_tags);

                    bh_arr(u64) meta_tag_locations=NULL;
                    bh_arr_new(global_heap_allocator, meta_tag_locations, bh_arr_length(meta_tags));

                    int j = 0;
                    bh_arr_each(AstTyped *, meta, meta_tags) {
                        AstTyped* value = *meta;                        
                        assert(value->flags & Ast_Flag_Comptime);
                        assert(value->type);

                        u32 size = type_size_of(value->type);
                        bh_buffer_align(&table_buffer, type_alignment_of(value->type));
                        meta_tag_locations[j] = table_buffer.length;

                        bh_buffer_grow(&table_buffer, table_buffer.length + size);
                        u8* buffer = table_buffer.data + table_buffer.length;

                        assert(emit_raw_data_(module, buffer, value));
                        table_buffer.length += size;

                        j += 1;
                    }

                    bh_buffer_align(&table_buffer, 8);
                    meta_locations[i] = table_buffer.length;

                    fori (k, 0, bh_arr_length(meta_tags)) {
                        PATCH;
                        bh_buffer_write_u64(&table_buffer, meta_tag_locations[k]);
                        bh_buffer_write_u64(&table_buffer, meta_tags[k]->type->id);
                    }

                    bh_arr_free(meta_tag_locations);
                    i += 1;
                }

                bh_buffer_align(&table_buffer, 8);
                u32 members_base = table_buffer.length;

                i = 0;
                bh_arr_each(StructMember*, pmem, s->memarr) {
                    StructMember* mem = *pmem;

                    u32 name_loc = name_locations[i];
                    u32 value_loc = value_locations[i];
                    u32 meta_loc = meta_locations[i++];

                    bh_buffer_align(&table_buffer, 8);
                    PATCH;
                    bh_buffer_write_u64(&table_buffer, name_loc);
                    bh_buffer_write_u64(&table_buffer, strlen(mem->name));
                    bh_buffer_write_u32(&table_buffer, mem->offset);
                    bh_buffer_write_u32(&table_buffer, mem->type->id);
                    bh_buffer_write_byte(&table_buffer, mem->used ? 1 : 0);
                    
                    bh_buffer_align(&table_buffer, 8);
                    PATCH;
                    bh_buffer_write_u64(&table_buffer, value_loc);

                    PATCH;
                    bh_buffer_write_u64(&table_buffer, meta_loc);
                    bh_buffer_write_u64(&table_buffer, bh_arr_length(mem->meta_tags));
                }

                bh_buffer_align(&table_buffer, 8);
                u32 params_base = table_buffer.length;

                i = 0;
                bh_arr_each(AstPolySolution, sln, s->poly_sln) {
                    bh_buffer_align(&table_buffer, 8);
                    PATCH;
                    bh_buffer_write_u64(&table_buffer, param_locations[i++]);

                    if (sln->kind == PSK_Type) bh_buffer_write_u32(&table_buffer, basic_types[Basic_Kind_Type_Index].id);
                    else                       bh_buffer_write_u32(&table_buffer, sln->value->type->id);
                }

                i = 0;
                bh_arr_each(AstTyped *, tag, s->meta_tags) {
                    AstTyped* value = *tag;                        
                    assert(value->flags & Ast_Flag_Comptime);
                    assert(value->type);

                    u32 size = type_size_of(value->type);
                    bh_buffer_align(&table_buffer, type_alignment_of(value->type));
                    struct_tag_locations[i] = table_buffer.length;

                    bh_buffer_grow(&table_buffer, table_buffer.length + size);
                    u8* buffer = table_buffer.data + table_buffer.length;

                    assert(emit_raw_data_(module, buffer, value));
                    table_buffer.length += size;

                    i += 1;
                }

                bh_buffer_align(&table_buffer, 8);
                u32 struct_tag_base = table_buffer.length;

                fori (i, 0, bh_arr_length(s->meta_tags)) {
                    PATCH;
                    bh_buffer_write_u64(&table_buffer, struct_tag_locations[i]);
                    bh_buffer_write_u64(&table_buffer, s->meta_tags[i]->type->id);
                }

                u32 name_base = 0;
                u32 name_length = 0;
                if (s->name) {
                    name_length = strlen(s->name);
                    name_base = table_buffer.length;
                    bh_buffer_append(&table_buffer, s->name, name_length);
                }

                bh_buffer_align(&table_buffer, 8);
                table_info[type_idx] = table_buffer.length;
                bh_buffer_write_u32(&table_buffer, type->kind);
                bh_buffer_write_u32(&table_buffer, type_size_of(type));
                bh_buffer_write_u32(&table_buffer, type_alignment_of(type));

                if (type->Struct.constructed_from != NULL) {
                    bh_buffer_write_u32(&table_buffer, type->Struct.constructed_from->type_id);
                } else {
                    bh_buffer_write_u32(&table_buffer, 0);
                }

                PATCH;
                bh_buffer_write_u64(&table_buffer, name_base);
                bh_buffer_write_u64(&table_buffer, name_length);
                PATCH;
                bh_buffer_write_u64(&table_buffer, members_base);
                bh_buffer_write_u64(&table_buffer, s->mem_count);
                PATCH;
                bh_buffer_write_u64(&table_buffer, params_base);
                bh_buffer_write_u64(&table_buffer, bh_arr_length(s->poly_sln));
                PATCH;
                bh_buffer_write_u64(&table_buffer, struct_tag_base);
                bh_buffer_write_u64(&table_buffer, bh_arr_length(s->meta_tags));

                break;
            }

            case Type_Kind_PolyStruct: {
                u32* tag_locations = bh_alloc_array(global_scratch_allocator, u32, bh_arr_length(type->PolyStruct.meta_tags));
                memset(tag_locations, 0, sizeof(u32) * bh_arr_length(type->PolyStruct.meta_tags));

                u32 name_base = table_buffer.length;
                u32 name_length = strlen(type->PolyStruct.name);
                bh_buffer_append(&table_buffer, type->PolyStruct.name, name_length);

                i32 i = 0;
                bh_arr_each(AstTyped *, tag, type->PolyStruct.meta_tags) {
                    AstTyped* value = *tag;                        
                    assert(value->flags & Ast_Flag_Comptime);
                    assert(value->type);

                    u32 size = type_size_of(value->type);
                    bh_buffer_align(&table_buffer, type_alignment_of(value->type));
                    tag_locations[i] = table_buffer.length;

                    bh_buffer_grow(&table_buffer, table_buffer.length + size);
                    u8* buffer = table_buffer.data + table_buffer.length;

                    assert(emit_raw_data_(module, buffer, value));
                    table_buffer.length += size;

                    i += 1;
                }

                bh_buffer_align(&table_buffer, 8);
                u32 tags_base = table_buffer.length;
                u32 tags_count = bh_arr_length(type->PolyStruct.meta_tags);

                fori (i, 0, tags_count) {
                    PATCH;
                    bh_buffer_write_u64(&table_buffer, tag_locations[i]);
                    bh_buffer_write_u64(&table_buffer, type->PolyStruct.meta_tags[i]->type->id);
                }

                bh_buffer_align(&table_buffer, 8);
                table_info[type_idx] = table_buffer.length;
                bh_buffer_write_u32(&table_buffer, type->kind);
                bh_buffer_write_u32(&table_buffer, 0);
                bh_buffer_write_u32(&table_buffer, 0);
                bh_buffer_write_u32(&table_buffer, 0);
                PATCH;
                bh_buffer_write_u64(&table_buffer, name_base);
                bh_buffer_write_u64(&table_buffer, name_length);
                PATCH;
                bh_buffer_write_u64(&table_buffer, tags_base);
                bh_buffer_write_u64(&table_buffer, tags_count);

                break;
            }
        
            case Type_Kind_Distinct: {
                u32 name_base = table_buffer.length;
                u32 name_length = strlen(type->Distinct.name);
                bh_buffer_append(&table_buffer, type->Distinct.name, name_length);
                bh_buffer_align(&table_buffer, 8);

                table_info[type_idx] = table_buffer.length;
                bh_buffer_write_u32(&table_buffer, type->kind);
                bh_buffer_write_u32(&table_buffer, type_size_of(type));
                bh_buffer_write_u32(&table_buffer, type_alignment_of(type));
                bh_buffer_write_u32(&table_buffer, type->Distinct.base_type->id);
                PATCH;
                bh_buffer_write_u64(&table_buffer, name_base);
                bh_buffer_write_u64(&table_buffer, name_length);
                break;
            }
        }
    }

    if (context.options->verbose_output == 1) {
        bh_printf("Type table size: %d bytes.\n", table_buffer.length);
    }

    u32 offset = module->next_datum_offset;
    bh_align(offset, 8);

    u64 type_table_location = offset;

    WasmDatum type_table_data = {
        .offset = offset,
        .length = type_count * 8,
        .data = table_info,
    };
    bh_arr_push(module->data, type_table_data);

    offset += type_table_data.length;

    fori (i, 0, type_count) {
        table_info[i] += offset;
    }

    bh_arr_each(u32, patch_loc, base_patch_locations) {
        u64* loc = bh_pointer_add(table_buffer.data, *patch_loc);
        if (*loc == 0) continue;
        
        *loc += offset;
    }

    WasmDatum type_info_data = {
        .offset = offset,
        .length = table_buffer.length,
        .data = table_buffer.data,
    };
    bh_arr_push(module->data, type_info_data);
    offset += type_info_data.length;

    u64 global_data_ptr = offset;

    u64* tmp_data = bh_alloc(global_heap_allocator, 16);
    tmp_data[0] = type_table_location;
    tmp_data[1] = type_count;
    WasmDatum type_table_global_data = {
        .offset = offset,
        .length = 16,
        .data = tmp_data,
    };
    bh_arr_push(module->data, type_table_global_data);
    offset += type_table_global_data.length;

    module->next_datum_offset = offset;

    return global_data_ptr;

#undef PATCH
}
