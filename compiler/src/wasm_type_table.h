// This file is directly included in src/onxywasm.c
// It is here purely to decrease the amount of clutter in the main file.

typedef struct StructMethodData {
    u32 name_loc;
    u32 name_len;
    u32 type;
    u32 data_loc;
} StructMethodData;

static void build_polymorphic_solutions_array(
        bh_arr(AstPolySolution) slns,
        bh_buffer *table_buffer,
        ConstExprContext *constexpr_ctx,
        u32 *param_locations
) {
    u32 i = 0;
    bh_arr_each(AstPolySolution, sln, slns) {
        bh_buffer_align(table_buffer, 8);
        param_locations[i++] = table_buffer->length;

        switch (sln->kind) {
            case PSK_Type: {
                // NOTE: This assumes a little endian compiler (which is assumed in other part of the code too)
                bh_buffer_append(table_buffer, &sln->type->id, 4);
                ensure_type_has_been_submitted_for_emission(constexpr_ctx->module, sln->type);
                break;
            }

            case PSK_Value: {
                assert(sln->value->type);
                u32 size = type_size_of(sln->value->type);

                bh_buffer_grow(table_buffer, table_buffer->length + size);
                constexpr_ctx->data = table_buffer->data;
                if (emit_constexpr_(constexpr_ctx, sln->value, table_buffer->length)) {
                    table_buffer->length += size;
                    break;
                }

                // fallthrough
            }

            default: {
                // Set to null if this is not known how to encode
                param_locations[i-1] = 0;
                break;
            }
        }
    }
}

static u32 build_constexpr(
        AstTyped *value,
        bh_buffer *buffer,
        ConstExprContext *constexpr_ctx
) {
    if ((value->flags & Ast_Flag_Comptime) == 0) {
        return 0;
    }

    u32 size = type_size_of(value->type);
    bh_buffer_align(buffer, type_alignment_of(value->type));

    bh_buffer_grow(buffer, buffer->length + size);
    constexpr_ctx->data = buffer->data;
    if (!emit_constexpr_(constexpr_ctx, value, buffer->length)) {
        return 0;

    } else {
        buffer->length += size;
        return buffer->length - size;
    }
}

#if (POINTER_SIZE == 4)
    #define Table_Info_Type u32
#else
    #error "Expected POINTER_SIZE to be 4"
#endif


struct TypeBuilderContext {
    bh_buffer buffer;
    bh_arr(u32) patches;

    OnyxWasmModule *module;
    ConstExprContext constexpr_ctx;
};

#define PATCH (bh_arr_push(ctx->patches, ctx->buffer.length))

#define WRITE_PTR(val) \
    bh_buffer_align(&ctx->buffer, POINTER_SIZE); \
    PATCH; \
    bh_buffer_write_u32(&ctx->buffer, val);

#define WRITE_SLICE(ptr, count) \
    WRITE_PTR(ptr); \
    bh_buffer_write_u32(&ctx->buffer, count);


static i32 build_type_info_for_basic(struct TypeBuilderContext *ctx, Type *type) {
    bh_buffer_write_u32(&ctx->buffer, type->kind);
    bh_buffer_write_u32(&ctx->buffer, type_size_of(type));
    bh_buffer_write_u32(&ctx->buffer, type_alignment_of(type));
    bh_buffer_write_u32(&ctx->buffer, type->Basic.kind);
    return 0;
}

static i32 build_type_info_for_pointer(struct TypeBuilderContext *ctx, Type *type) {
    bh_buffer_write_u32(&ctx->buffer, type->kind);
    bh_buffer_write_u32(&ctx->buffer, type_size_of(type));
    bh_buffer_write_u32(&ctx->buffer, type_alignment_of(type));
    bh_buffer_write_u32(&ctx->buffer, type->Pointer.elem->id);
    ensure_type_has_been_submitted_for_emission(ctx->module, type->Pointer.elem);
    return 0;
}

static i32 build_type_info_for_multipointer(struct TypeBuilderContext *ctx, Type *type) {
    bh_buffer_write_u32(&ctx->buffer, type->kind);
    bh_buffer_write_u32(&ctx->buffer, type_size_of(type));
    bh_buffer_write_u32(&ctx->buffer, type_alignment_of(type));
    bh_buffer_write_u32(&ctx->buffer, type->MultiPointer.elem->id);
    ensure_type_has_been_submitted_for_emission(ctx->module, type->MultiPointer.elem);
    return 0;
}

static i32 build_type_info_for_array(struct TypeBuilderContext *ctx, Type *type) {
    bh_buffer_write_u32(&ctx->buffer, type->kind);
    bh_buffer_write_u32(&ctx->buffer, type_size_of(type));
    bh_buffer_write_u32(&ctx->buffer, type_alignment_of(type));
    bh_buffer_write_u32(&ctx->buffer, type->Array.elem->id);
    bh_buffer_write_u32(&ctx->buffer, type->Array.count);
    ensure_type_has_been_submitted_for_emission(ctx->module, type->Array.elem);
    return 0;
}

static i32 build_type_info_for_slice(struct TypeBuilderContext *ctx, Type *type) {
    bh_buffer_write_u32(&ctx->buffer, type->kind);
    bh_buffer_write_u32(&ctx->buffer, type_size_of(type));
    bh_buffer_write_u32(&ctx->buffer, type_alignment_of(type));
    bh_buffer_write_u32(&ctx->buffer, type->Slice.elem->id);
    ensure_type_has_been_submitted_for_emission(ctx->module, type->Slice.elem);
    return 0;
}

static i32 build_type_info_for_dynarray(struct TypeBuilderContext *ctx, Type *type) {
    bh_buffer_write_u32(&ctx->buffer, type->kind);
    bh_buffer_write_u32(&ctx->buffer, type_size_of(type));
    bh_buffer_write_u32(&ctx->buffer, type_alignment_of(type));
    bh_buffer_write_u32(&ctx->buffer, type->DynArray.elem->id);
    ensure_type_has_been_submitted_for_emission(ctx->module, type->DynArray.elem);
    return 0;
}

static i32 build_type_info_for_varargs(struct TypeBuilderContext *ctx, Type *type) {
    bh_buffer_write_u32(&ctx->buffer, type->kind);
    bh_buffer_write_u32(&ctx->buffer, type_size_of(type));
    bh_buffer_write_u32(&ctx->buffer, type_alignment_of(type));
    bh_buffer_write_u32(&ctx->buffer, type->VarArgs.elem->id);
    ensure_type_has_been_submitted_for_emission(ctx->module, type->VarArgs.elem);
    return 0;
}

static i32 build_type_info_for_compound(struct TypeBuilderContext *ctx, Type *type) {
    u32 components_count = type->Compound.count;
    fori (i, 0, components_count) {
        u32 type_idx = type->Compound.types[i]->id;
        bh_buffer_write_u32(&ctx->buffer, type_idx);
        ensure_type_has_been_submitted_for_emission(ctx->module, type->Compound.types[i]);
    }

    bh_buffer_align(&ctx->buffer, 8);
    i32 offset = ctx->buffer.length;

    bh_buffer_write_u32(&ctx->buffer, type->kind);
    bh_buffer_write_u32(&ctx->buffer, type_size_of(type));
    bh_buffer_write_u32(&ctx->buffer, type_alignment_of(type));
    WRITE_SLICE(0, components_count);

    return offset;
}

static i32 build_type_info_for_function(struct TypeBuilderContext *ctx, Type *type) {
    u32 parameters_count = type->Function.param_count;
    fori (i, 0, parameters_count) {
        u32 type_idx = type->Function.params[i]->id;
        bh_buffer_write_u32(&ctx->buffer, type_idx);
        ensure_type_has_been_submitted_for_emission(ctx->module, type->Function.params[i]);
    }

    i32 offset = ctx->buffer.length;
    bh_buffer_write_u32(&ctx->buffer, type->kind);
    bh_buffer_write_u32(&ctx->buffer, type_size_of(type));
    bh_buffer_write_u32(&ctx->buffer, type_alignment_of(type));
    bh_buffer_write_u32(&ctx->buffer, type->Function.return_type->id);
    ensure_type_has_been_submitted_for_emission(ctx->module, type->Function.return_type);

    WRITE_SLICE(0, parameters_count);

    bh_buffer_write_u32(&ctx->buffer, type->Function.vararg_arg_pos > 0 ? 1 : 0);

    return offset;
}

static i32 build_type_info_for_enum(struct TypeBuilderContext *ctx, Type *type) {
    AstEnumType* ast_enum = (AstEnumType *) type->ast_type;
    u32  member_count   = bh_arr_length(ast_enum->values);
    u32* name_locations = bh_alloc_array(global_heap_allocator, u32, member_count);

    u32 i = 0;
    bh_arr_each(AstEnumValue *, value, ast_enum->values) {
        name_locations[i++] = ctx->buffer.length;

        bh_buffer_append(&ctx->buffer, (*value)->token->text, (*value)->token->length);
    }
    bh_buffer_align(&ctx->buffer, 8);

    u32 member_base = ctx->buffer.length;
    i = 0;
    bh_arr_each(AstEnumValue *, value, ast_enum->values) {
        u32 name_loc = name_locations[i++];

        bh_buffer_align(&ctx->buffer, 8);
        WRITE_SLICE(name_loc, (*value)->token->length);

        assert((*value)->value->kind == Ast_Kind_NumLit);
        AstNumLit *num = (AstNumLit *) (*value)->value;
        bh_buffer_write_u64(&ctx->buffer, num->value.l);
    }

    u32 name_base = ctx->buffer.length;
    u32 name_length = strlen(type->Enum.name);
    bh_buffer_append(&ctx->buffer, type->Enum.name, name_length);
    bh_buffer_align(&ctx->buffer, 8);

    i32 offset = ctx->buffer.length;
    bh_buffer_write_u32(&ctx->buffer, type->kind);
    bh_buffer_write_u32(&ctx->buffer, type_size_of(type));
    bh_buffer_write_u32(&ctx->buffer, type_alignment_of(type));
    bh_buffer_write_u32(&ctx->buffer, type->Enum.backing->id);
    ensure_type_has_been_submitted_for_emission(ctx->module, type->Enum.backing);
    WRITE_SLICE(name_base, name_length);
    WRITE_SLICE(member_base, member_count);
    bh_buffer_write_u32(&ctx->buffer, type->Enum.is_flags ? 1 : 0);

    return offset;
}

static i32 build_type_info_for_struct(struct TypeBuilderContext *ctx, Type *type) {
    TypeStruct* s = &type->Struct;
    u32* name_locations = bh_alloc_array(global_scratch_allocator, u32, s->mem_count);
    u32* param_locations = bh_alloc_array(global_scratch_allocator, u32, bh_arr_length(s->poly_sln));
    u32* value_locations = bh_alloc_array(global_scratch_allocator, u32, s->mem_count);
    u32* meta_locations = bh_alloc_array(global_scratch_allocator, u32, s->mem_count);
    u32* struct_tag_locations = bh_alloc_array(global_scratch_allocator, u32, bh_arr_length(s->meta_tags));
    memset(value_locations, 0, s->mem_count * sizeof(u32));
    memset(meta_locations, 0, s->mem_count * sizeof(u32));
    memset(struct_tag_locations, 0, bh_arr_length(s->meta_tags) * sizeof(u32));

    // Member names
    u32 i = 0;
    bh_arr_each(StructMember*, pmem, s->memarr) {
        StructMember* mem = *pmem;

        name_locations[i++] = ctx->buffer.length;
        bh_buffer_append(&ctx->buffer, mem->name, strlen(mem->name));
    }

    bh_buffer_align(&ctx->buffer, 8);

    // Polymorphic solutions
    build_polymorphic_solutions_array(s->poly_sln, &ctx->buffer, &ctx->constexpr_ctx, param_locations);

    bh_buffer_align(&ctx->buffer, 8);

    // Member default values
    i = 0;
    bh_arr_each(StructMember*, pmem, s->memarr) {
        StructMember* mem = *pmem;

        if (mem->initial_value == NULL || *mem->initial_value == NULL) {
            i++;
            continue;
        }

        AstTyped* value = *mem->initial_value;
        assert(value->type);

        value_locations[i++] = build_constexpr(value, &ctx->buffer, &ctx->constexpr_ctx);
    }

    // Member tags
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

            meta_tag_locations[j++] = build_constexpr(value, &ctx->buffer, &ctx->constexpr_ctx);
        }

        bh_buffer_align(&ctx->buffer, 8);
        meta_locations[i] = ctx->buffer.length;

        fori (k, 0, bh_arr_length(meta_tags)) {
            WRITE_SLICE(meta_tag_locations[k], meta_tags[k]->type->id);
            ensure_type_has_been_submitted_for_emission(ctx->module, meta_tags[k]->type);
        }

        bh_arr_free(meta_tag_locations);
        i += 1;
    }

    bh_buffer_align(&ctx->buffer, 8);
    u32 members_base = ctx->buffer.length;

    // Member array
    i = 0;
    bh_arr_each(StructMember*, pmem, s->memarr) {
        StructMember* mem = *pmem;

        u32 name_loc = name_locations[i];
        u32 value_loc = value_locations[i];
        u32 meta_loc = meta_locations[i++];

        WRITE_SLICE(name_loc, strlen(mem->name));
        bh_buffer_write_u32(&ctx->buffer, mem->offset);
        bh_buffer_write_u32(&ctx->buffer, mem->type->id);
        ensure_type_has_been_submitted_for_emission(ctx->module, mem->type);
        bh_buffer_write_byte(&ctx->buffer, mem->used ? 1 : 0);
        
        WRITE_PTR(value_loc);

        WRITE_SLICE(meta_loc, bh_arr_length(mem->meta_tags));
    }

    bh_buffer_align(&ctx->buffer, 8);
    u32 params_base = ctx->buffer.length;

    // Polymorphic solution any array
    i = 0;
    bh_arr_each(AstPolySolution, sln, s->poly_sln) {
        WRITE_PTR(param_locations[i++]);

        if (sln->kind == PSK_Type) {
            bh_buffer_write_u32(&ctx->buffer, basic_types[Basic_Kind_Type_Index].id);
            ensure_type_has_been_submitted_for_emission(ctx->module, &basic_types[Basic_Kind_Type_Index]);
        } else {
            bh_buffer_write_u32(&ctx->buffer, sln->value->type->id);
            ensure_type_has_been_submitted_for_emission(ctx->module, sln->value->type);
        }
    }

    // Struct tag array
    i = 0;
    bh_arr_each(AstTyped *, tag, s->meta_tags) {
        AstTyped* value = *tag;                        
        assert(value->flags & Ast_Flag_Comptime);
        assert(value->type);

        struct_tag_locations[i++] = build_constexpr(value, &ctx->buffer, &ctx->constexpr_ctx);
    }

    // Struct methods
    bh_arr(StructMethodData) method_data=NULL;

    AstType *ast_type = type->ast_type;
    if (!context.options->generate_method_info) {
        goto no_methods;
    }

    if (ast_type && ast_type->kind == Ast_Kind_Struct_Type) {
        AstStructType *struct_type  = (AstStructType *) ast_type;
        Scope*         struct_scope = struct_type->scope;

        if (struct_scope == NULL) goto no_methods;

        fori (i, 0, shlen(struct_scope->symbols)) {
            AstFunction* node = (AstFunction *) strip_aliases(struct_scope->symbols[i].value);
            if (node->kind != Ast_Kind_Function) continue;
            assert(node->entity);
            assert(node->entity->function == node);

            // Name
            char *name = struct_scope->symbols[i].key;
            u32 name_loc = ctx->buffer.length;
            u32 name_len = strlen(name);
            bh_buffer_append(&ctx->buffer, name, name_len);

            // any data member
            bh_buffer_align(&ctx->buffer, 4);
            u32 data_loc = ctx->buffer.length;
            u32 func_idx = get_element_idx(ctx->module, node);
            bh_buffer_write_u32(&ctx->buffer, func_idx);
            bh_buffer_write_u32(&ctx->buffer, 0);
            
            bh_arr_push(method_data, ((StructMethodData) {
                .name_loc = name_loc,
                .name_len = name_len,
                .type     = node->type->id,
                .data_loc = data_loc,
            }));
            ensure_type_has_been_submitted_for_emission(ctx->module, node->type);
        }
    }

    no_methods:

    bh_buffer_align(&ctx->buffer, 4);
    u32 method_data_base = ctx->buffer.length;

    i = 0;
    bh_arr_each(StructMethodData, method, method_data) {
        WRITE_SLICE(method->name_loc, method->name_len);
        WRITE_PTR(method->data_loc); 
        bh_buffer_write_u32(&ctx->buffer, method->type);
    }

    bh_buffer_align(&ctx->buffer, 8);
    u32 struct_tag_base = ctx->buffer.length;

    fori (i, 0, bh_arr_length(s->meta_tags)) {
        WRITE_SLICE(struct_tag_locations[i], s->meta_tags[i]->type->id);
        ensure_type_has_been_submitted_for_emission(ctx->module, s->meta_tags[i]->type);
    }

    // Struct name
    u32 name_base = 0;
    u32 name_length = 0;
    if (s->name) {
        name_length = strlen(s->name);
        name_base = ctx->buffer.length;
        bh_buffer_append(&ctx->buffer, s->name, name_length);
    }

    bh_buffer_align(&ctx->buffer, 8);

    i32 offset = ctx->buffer.length;
    bh_buffer_write_u32(&ctx->buffer, type->kind);
    bh_buffer_write_u32(&ctx->buffer, type_size_of(type));
    bh_buffer_write_u32(&ctx->buffer, type_alignment_of(type));

    if (type->Struct.constructed_from != NULL) {
        bh_buffer_write_u32(&ctx->buffer, type->Struct.constructed_from->type_id);

        Type *constructed_from = type_lookup_by_id(type->Struct.constructed_from->type_id);
        ensure_type_has_been_submitted_for_emission(ctx->module, constructed_from);
    } else {
        bh_buffer_write_u32(&ctx->buffer, 0);
    }

    WRITE_SLICE(name_base, name_length);
    WRITE_SLICE(members_base, s->mem_count);
    WRITE_SLICE(params_base, bh_arr_length(s->poly_sln));
    WRITE_SLICE(struct_tag_base, bh_arr_length(s->meta_tags));
    WRITE_SLICE(method_data_base, bh_arr_length(method_data));

    bh_arr_free(method_data);

    return offset;
}

static i32 build_type_info_for_polystruct(struct TypeBuilderContext *ctx, Type *type) {
    u32* tag_locations = bh_alloc_array(global_scratch_allocator, u32, bh_arr_length(type->PolyStruct.meta_tags));
    memset(tag_locations, 0, sizeof(u32) * bh_arr_length(type->PolyStruct.meta_tags));

    u32 name_base = ctx->buffer.length;
    u32 name_length = strlen(type->PolyStruct.name);
    bh_buffer_append(&ctx->buffer, type->PolyStruct.name, name_length);

    u32 tags_count = bh_arr_length(type->PolyStruct.meta_tags);
    i32 i = 0;
    bh_arr_each(AstTyped *, tag, type->PolyStruct.meta_tags) {
        AstTyped* value = *tag;                        

        // Polymorphic structs are weird in this case, because the tag might not be constructed generically for
        // the polymorphic structure so it should only be constructed for actual solidified structures.
        // See core/containers/map.onyx with Custom_Format for an example.
        if (!(value->flags & Ast_Flag_Comptime)) {
            tags_count--;
            continue;
        }

        assert(value->type);

        u32 size = type_size_of(value->type);
        bh_buffer_align(&ctx->buffer, type_alignment_of(value->type));
        tag_locations[i] = ctx->buffer.length;

        bh_buffer_grow(&ctx->buffer, ctx->buffer.length + size);

        ctx->constexpr_ctx.data = ctx->buffer.data;
        assert(emit_constexpr_(&ctx->constexpr_ctx, value, ctx->buffer.length));
        ctx->buffer.length += size;

        i += 1;
    }

    bh_buffer_align(&ctx->buffer, 8);
    u32 tags_base = ctx->buffer.length;

    fori (i, 0, tags_count) {
        WRITE_SLICE(tag_locations[i], type->PolyStruct.meta_tags[i]->type->id);
        ensure_type_has_been_submitted_for_emission(ctx->module, type->PolyStruct.meta_tags[i]->type);
    }

    bh_buffer_align(&ctx->buffer, 8);

    i32 offset = ctx->buffer.length;
    bh_buffer_write_u32(&ctx->buffer, type->kind);
    bh_buffer_write_u32(&ctx->buffer, 0);
    bh_buffer_write_u32(&ctx->buffer, 0);
    WRITE_SLICE(name_base, name_length);
    WRITE_SLICE(tags_base, tags_count);

    return offset;
}

static i32 build_type_info_for_distinct(struct TypeBuilderContext *ctx, Type *type) {
    u32 name_length = strlen(type->Distinct.name);
    bh_buffer_append(&ctx->buffer, type->Distinct.name, name_length);
    bh_buffer_align(&ctx->buffer, 8);

    i32 offset = ctx->buffer.length;
    bh_buffer_write_u32(&ctx->buffer, type->kind);
    bh_buffer_write_u32(&ctx->buffer, type_size_of(type));
    bh_buffer_write_u32(&ctx->buffer, type_alignment_of(type));
    bh_buffer_write_u32(&ctx->buffer, type->Distinct.base_type->id);
    WRITE_SLICE(0, name_length);

    ensure_type_has_been_submitted_for_emission(ctx->module, type->Distinct.base_type);

    return offset;
}

static i32 build_type_info_for_union(struct TypeBuilderContext *ctx, Type *type) {
    TypeUnion* u = &type->Union;
    u32 variant_count = bh_arr_length(u->variants_ordered);
    u32* name_locations = bh_alloc_array(global_scratch_allocator, u32, variant_count);
    u32* param_locations = bh_alloc_array(global_scratch_allocator, u32, bh_arr_length(u->poly_sln));
    u32* meta_locations = bh_alloc_array(global_scratch_allocator, u32, variant_count);
    u32* struct_tag_locations = bh_alloc_array(global_scratch_allocator, u32, bh_arr_length(u->meta_tags));
    memset(meta_locations, 0, variant_count * sizeof(u32));
    memset(struct_tag_locations, 0, bh_arr_length(u->meta_tags) * sizeof(u32));

    // Member names
    u32 i = 0;
    bh_arr_each(UnionVariant*, puv, u->variants_ordered) {
        UnionVariant* uv = *puv;

        name_locations[i++] = ctx->buffer.length;
        bh_buffer_append(&ctx->buffer, uv->name, strlen(uv->name));
    }

    bh_buffer_align(&ctx->buffer, 8);

    // Polymorphic solutions
    build_polymorphic_solutions_array(u->poly_sln, &ctx->buffer, &ctx->constexpr_ctx, param_locations);

    bh_buffer_align(&ctx->buffer, 8);

    // Variant tags
    i = 0;
    bh_arr_each(UnionVariant*, puv, u->variants_ordered) {
        UnionVariant* uv = *puv;

        if (uv->meta_tags == NULL) {
            i += 1;
            continue;
        }

        bh_arr(AstTyped *) meta_tags = uv->meta_tags;
        assert(meta_tags);

        bh_arr(u64) meta_tag_locations=NULL;
        bh_arr_new(global_heap_allocator, meta_tag_locations, bh_arr_length(meta_tags));

        int j = 0;
        bh_arr_each(AstTyped *, meta, meta_tags) {
            AstTyped* value = *meta;                        
            assert(value->flags & Ast_Flag_Comptime);
            assert(value->type);

            meta_tag_locations[j++] = build_constexpr(value, &ctx->buffer, &ctx->constexpr_ctx);
        }

        bh_buffer_align(&ctx->buffer, 8);
        meta_locations[i] = ctx->buffer.length;

        fori (k, 0, bh_arr_length(meta_tags)) {
            WRITE_SLICE(meta_tag_locations[k], meta_tags[k]->type->id);
            ensure_type_has_been_submitted_for_emission(ctx->module, meta_tags[k]->type);
        }

        bh_arr_free(meta_tag_locations);
        i += 1;
    }

    bh_buffer_align(&ctx->buffer, 8);
    u32 variants_base = ctx->buffer.length;

    // Variants array
    i = 0;
    bh_arr_each(UnionVariant*, puv, u->variants_ordered) {
        UnionVariant* uv = *puv;

        u32 name_loc = name_locations[i];
        u32 meta_loc = meta_locations[i++];

        WRITE_SLICE(name_loc, strlen(uv->name));
        bh_buffer_write_u32(&ctx->buffer, uv->tag_value);
        bh_buffer_write_u32(&ctx->buffer, uv->type->id);
        ensure_type_has_been_submitted_for_emission(ctx->module, uv->type);

        WRITE_SLICE(meta_loc, bh_arr_length(uv->meta_tags));
    }

    bh_buffer_align(&ctx->buffer, 8);
    u32 params_base = ctx->buffer.length;

    // Polymorphic solution any array
    i = 0;
    bh_arr_each(AstPolySolution, sln, u->poly_sln) {
        WRITE_PTR(param_locations[i++]);

        if (sln->kind == PSK_Type) {
            bh_buffer_write_u32(&ctx->buffer, basic_types[Basic_Kind_Type_Index].id);
            ensure_type_has_been_submitted_for_emission(ctx->module, &basic_types[Basic_Kind_Type_Index]);
        } else {
            bh_buffer_write_u32(&ctx->buffer, sln->value->type->id);
            ensure_type_has_been_submitted_for_emission(ctx->module, sln->value->type);
        }
    }

    // Union tag array
    i = 0;
    bh_arr_each(AstTyped *, tag, u->meta_tags) {
        AstTyped* value = *tag;                        
        assert(value->flags & Ast_Flag_Comptime);
        assert(value->type);

        struct_tag_locations[i++] = build_constexpr(value, &ctx->buffer, &ctx->constexpr_ctx);
    }

    // Union methods
    bh_arr(StructMethodData) method_data=NULL;

    AstType *ast_type = type->ast_type;
    if (!context.options->generate_method_info) {
        goto no_methods;
    }

    if (ast_type && ast_type->kind == Ast_Kind_Union_Type) {
        AstUnionType *union_type  = (AstUnionType *) ast_type;
        Scope*        union_scope = union_type->scope;

        if (union_scope == NULL) goto no_methods;

        fori (i, 0, shlen(union_scope->symbols)) {
            AstFunction* node = (AstFunction *) strip_aliases(union_scope->symbols[i].value);
            if (node->kind != Ast_Kind_Function) continue;
            assert(node->entity);
            assert(node->entity->function == node);

            // Name
            char *name = union_scope->symbols[i].key;
            u32 name_loc = ctx->buffer.length;
            u32 name_len = strlen(name);
            bh_buffer_append(&ctx->buffer, name, name_len);

            // any data member
            bh_buffer_align(&ctx->buffer, 4);
            u32 data_loc = ctx->buffer.length;
            u32 func_idx = get_element_idx(ctx->module, node);
            bh_buffer_write_u32(&ctx->buffer, func_idx);
            bh_buffer_write_u32(&ctx->buffer, 0);
            
            bh_arr_push(method_data, ((StructMethodData) {
                .name_loc = name_loc,
                .name_len = name_len,
                .type     = node->type->id,
                .data_loc = data_loc,
            }));
            ensure_type_has_been_submitted_for_emission(ctx->module, node->type);
        }
    }

    no_methods:

    bh_buffer_align(&ctx->buffer, 4);
    u32 method_data_base = ctx->buffer.length;

    bh_arr_each(StructMethodData, method, method_data) {
        WRITE_SLICE(method->name_loc, method->name_len);
        WRITE_PTR(method->data_loc); 
        bh_buffer_write_u32(&ctx->buffer, method->type);
    }

    bh_buffer_align(&ctx->buffer, 8);
    
    u32 union_tag_base = ctx->buffer.length;
    fori (i, 0, bh_arr_length(u->meta_tags)) {
        WRITE_SLICE(struct_tag_locations[i], u->meta_tags[i]->type->id);
        ensure_type_has_been_submitted_for_emission(ctx->module, u->meta_tags[i]->type);
    }

    // Union name
    u32 name_base = 0;
    u32 name_length = 0;
    if (u->name) {
        name_length = strlen(u->name);
        name_base = ctx->buffer.length;
        bh_buffer_append(&ctx->buffer, u->name, name_length);
    }

    bh_buffer_align(&ctx->buffer, 8);
    i32 offset = ctx->buffer.length;
    bh_buffer_write_u32(&ctx->buffer, type->kind);
    bh_buffer_write_u32(&ctx->buffer, type_size_of(type));
    bh_buffer_write_u32(&ctx->buffer, type_alignment_of(type));

    if (type->Union.constructed_from != NULL) {
        bh_buffer_write_u32(&ctx->buffer, type->Union.constructed_from->type_id);
    } else {
        bh_buffer_write_u32(&ctx->buffer, 0);
    }

    bh_buffer_write_u32(&ctx->buffer, type->Union.tag_type->id);
    ensure_type_has_been_submitted_for_emission(ctx->module, type->Union.tag_type);

    WRITE_SLICE(name_base, name_length);
    WRITE_SLICE(variants_base, variant_count);
    WRITE_SLICE(params_base, bh_arr_length(u->poly_sln));
    WRITE_SLICE(union_tag_base, bh_arr_length(u->meta_tags));
    WRITE_SLICE(method_data_base, bh_arr_length(method_data));

    bh_arr_free(method_data);

    return offset;
}

static i32 build_type_info_for_polyunion(struct TypeBuilderContext *ctx, Type *type) {
    u32* tag_locations = bh_alloc_array(global_scratch_allocator, u32, bh_arr_length(type->PolyUnion.meta_tags));
    memset(tag_locations, 0, sizeof(u32) * bh_arr_length(type->PolyUnion.meta_tags));

    u32 name_base = ctx->buffer.length;
    u32 name_length = strlen(type->PolyUnion.name);
    bh_buffer_append(&ctx->buffer, type->PolyUnion.name, name_length);

    u32 tags_count = bh_arr_length(type->PolyUnion.meta_tags);
    i32 i = 0;
    bh_arr_each(AstTyped *, tag, type->PolyUnion.meta_tags) {
        AstTyped* value = *tag;                        

        tag_locations[i] = build_constexpr(value, &ctx->buffer, &ctx->constexpr_ctx);
        if (tag_locations[i] == 0) {
            // Polymorphic structs are weird in this case, because the tag might not be constructed generically for
            // the polymorphic structure so it should only be constructed for actual solidified structures.
            // See core/containers/map.onyx with Custom_Format for an example.
            tags_count--;
        } else {
            i++;
        }
    }

    bh_buffer_align(&ctx->buffer, 8);
    u32 tags_base = ctx->buffer.length;

    fori (i, 0, tags_count) {
        WRITE_SLICE(tag_locations[i], type->PolyUnion.meta_tags[i]->type->id);
        ensure_type_has_been_submitted_for_emission(ctx->module, type->PolyUnion.meta_tags[i]->type);
    }

    bh_buffer_align(&ctx->buffer, 8);

    i32 offset = ctx->buffer.length;
    bh_buffer_write_u32(&ctx->buffer, type->kind);
    bh_buffer_write_u32(&ctx->buffer, 0);
    bh_buffer_write_u32(&ctx->buffer, 0);
    WRITE_SLICE(name_base, name_length);
    WRITE_SLICE(tags_base, tags_count);

    return offset;
}

static void build_type_info_for_type(OnyxWasmModule *module, Type *type) {
    bh_buffer buffer;

    struct TypeBuilderContext ctx = {0};
    ctx.module = module;

    bh_buffer_init(&ctx.buffer, global_heap_allocator, 512);
    bh_arr_new(global_heap_allocator, ctx.patches, 16);

    u32 type_table_info_data_id = NEXT_DATA_ID(module);

    ctx.constexpr_ctx.module = module;
    ctx.constexpr_ctx.data_id = type_table_info_data_id;

    i32 offset = 0;

    switch (type->kind) {
        case Type_Kind_Basic:            offset = build_type_info_for_basic(&ctx, type); break;
        case Type_Kind_Pointer:          offset = build_type_info_for_pointer(&ctx, type); break;
        case Type_Kind_MultiPointer:     offset = build_type_info_for_multipointer(&ctx, type); break;
        case Type_Kind_Array:            offset = build_type_info_for_array(&ctx, type); break;
        case Type_Kind_Slice:            offset = build_type_info_for_slice(&ctx, type); break;
        case Type_Kind_DynArray:         offset = build_type_info_for_dynarray(&ctx, type); break;
        case Type_Kind_VarArgs:          offset = build_type_info_for_varargs(&ctx, type); break;
        case Type_Kind_Compound:         offset = build_type_info_for_compound(&ctx, type); break;
        case Type_Kind_Function:         offset = build_type_info_for_function(&ctx, type); break;
        case Type_Kind_Enum:             offset = build_type_info_for_enum(&ctx, type); break;
        case Type_Kind_Struct:           offset = build_type_info_for_struct(&ctx, type); break;
        case Type_Kind_PolyStruct:       offset = build_type_info_for_polystruct(&ctx, type); break;
        case Type_Kind_Distinct:         offset = build_type_info_for_distinct(&ctx, type); break;
        case Type_Kind_Union:            offset = build_type_info_for_union(&ctx, type); break;
        case Type_Kind_PolyUnion:        offset = build_type_info_for_polyunion(&ctx, type); break;
    
        case Type_Kind_Invalid:
        case Type_Kind_Count:
            break;
    }

    WasmDatum type_info_data = {
        .alignment = 8,
        .length = ctx.buffer.length,
        .data = ctx.buffer.data,
    };
    emit_data_entry(module, &type_info_data);
    assert(type_info_data.id == type_table_info_data_id);

    bh_arr_each(u32, patch_loc, ctx.patches) {
        DatumPatchInfo patch;
        patch.kind = Datum_Patch_Relative;
        patch.data_id = type_info_data.id;
        patch.offset = 0;
        patch.index = type_info_data.id;
        patch.location = *patch_loc;
        bh_arr_push(module->data_patches, patch);
    }

    WasmDatum *type_table_data = &module->data[module->global_type_table_data_id - 1];
    i32 next_location = type_table_data->length;
    type_table_data->length += 2 * POINTER_SIZE;
    *(i32 *) &((u8 *) type_table_data->data)[next_location] = type->id;

    DatumPatchInfo patch;
    patch.kind = Datum_Patch_Data;
    patch.data_id = type_info_data.id;
    patch.offset = offset;
    patch.index = module->global_type_table_data_id;
    patch.location = next_location + POINTER_SIZE;
    bh_arr_push(module->data_patches, patch);

    *module->type_info_entry_count += 1;
    module->type_info_size += type_info_data.length;
    module->type_info_size += 8;
}

static u64 prepare_type_table(OnyxWasmModule* module) {
    // This is the data behind the "type_table" slice in runtime/info/types.onyx
    u32 type_count = bh_arr_length(type_map.entries) + 1;
    void* table_info = bh_alloc_array(global_heap_allocator, u8, 2 * type_count * POINTER_SIZE);
    memset(table_info, 0, 2 * type_count * POINTER_SIZE);

    WasmDatum type_table_data = {
        .alignment = POINTER_SIZE,
        .length = 0,
        .data = table_info,
    };
    emit_data_entry(module, &type_table_data);
    module->global_type_table_data_id = type_table_data.id;

    Table_Info_Type* tmp_data = bh_alloc(global_heap_allocator, 2 * POINTER_SIZE);

    tmp_data[0] = 0;
    tmp_data[1] = 0;
    module->type_info_entry_count = &tmp_data[1];

    WasmDatum type_table_global_data = {
        .alignment = POINTER_SIZE,
        .length = 2 * POINTER_SIZE,
        .data = tmp_data,
    };
    emit_data_entry(module, &type_table_global_data);

    DatumPatchInfo patch;
    patch.kind = Datum_Patch_Data;
    patch.data_id = type_table_data.id;
    patch.offset = 0;
    patch.index = type_table_global_data.id;
    patch.location = 0;
    bh_arr_push(module->data_patches, patch);

    return type_table_global_data.id;

#undef WRITE_SLICE
#undef WRITE_PTR
#undef PATCH
}



static u64 build_foreign_blocks(OnyxWasmModule* module) {
    bh_arr(u32) base_patch_locations=NULL;
    bh_arr_new(global_heap_allocator, base_patch_locations, 256);

#define PATCH (bh_arr_push(base_patch_locations, foreign_buffer.length))
#define PATCH_AT(x) (bh_arr_push(base_patch_locations, (x)))
#define WRITE_PTR(val) \
    bh_buffer_align(&foreign_buffer, POINTER_SIZE); \
    PATCH; \
    if (POINTER_SIZE == 4) bh_buffer_write_u32(&foreign_buffer, val); \
    if (POINTER_SIZE == 8) bh_buffer_write_u64(&foreign_buffer, val); 
#define WRITE_SLICE(ptr, count) \
    WRITE_PTR(ptr); \
    if (POINTER_SIZE == 4) bh_buffer_write_u32(&foreign_buffer, count); \
    if (POINTER_SIZE == 8) bh_buffer_write_u64(&foreign_buffer, count); 

    // This is the data behind the "type_table" slice in type_info.onyx
    #if (POINTER_SIZE == 4)
        #define Foreign_Block_Type u32
    #else
        #define Foreign_Block_Type u64
    #endif
    u32 block_count = bh_arr_length(module->foreign_blocks);
    Foreign_Block_Type* foreign_info = bh_alloc_array(global_heap_allocator, Foreign_Block_Type, block_count); // HACK
    memset(foreign_info, 0, block_count * sizeof(Foreign_Block_Type));

    bh_buffer foreign_buffer;
    bh_buffer_init(&foreign_buffer, global_heap_allocator, 4096);

    u32 foreign_info_data_id = NEXT_DATA_ID(module);

    ConstExprContext constexpr_ctx;
    constexpr_ctx.module = module;
    constexpr_ctx.data_id = foreign_info_data_id;

    // 
    // This is necessary because 0 is an invalid offset to store in this
    // buffer, as 0 will map to NULL. This could be a single byte insertion,
    // but 64 bytes keeps better alignment.
    bh_buffer_write_u64(&foreign_buffer, 0);

    u32 index = 0;
    bh_arr_each(AstForeignBlock *, pfb, module->foreign_blocks) {
        AstForeignBlock *fb = *pfb;

        u32 funcs_length = 0;

        u32 *name_offsets = bh_alloc_array(global_scratch_allocator, u32, shlen(fb->scope->symbols));
        u32 *name_lengths = bh_alloc_array(global_scratch_allocator, u32, shlen(fb->scope->symbols));
        u32 *func_types   = bh_alloc_array(global_scratch_allocator, u32, shlen(fb->scope->symbols));
        u32 *tag_offsets  = bh_alloc_array(global_scratch_allocator, u32, shlen(fb->scope->symbols));
        u32 *tag_lengths  = bh_alloc_array(global_scratch_allocator, u32, shlen(fb->scope->symbols));

        fori (i, 0, shlen(fb->scope->symbols)) {
            AstFunction *func = (AstFunction *) fb->scope->symbols[i].value;
            if (func->kind != Ast_Kind_Function) continue;

            OnyxToken *import_name = func->foreign.import_name->token;
            u32 func_name_base = foreign_buffer.length;
            u32 func_name_length = import_name->length;
            bh_buffer_append(&foreign_buffer, import_name->text, func_name_length);

            tag_offsets[funcs_length] = 0;
            tag_lengths[funcs_length] = 0;
            if (func->tags) {
                u32 tag_count = bh_arr_length(func->tags);

                bh_buffer_grow(&foreign_buffer, foreign_buffer.length + POINTER_SIZE * 2 * tag_count);
                u32 tag_array_offset = foreign_buffer.length;
                u32 *tag_array = (u32 *) (foreign_buffer.data + tag_array_offset);
                foreign_buffer.length += POINTER_SIZE * 2 * tag_count;

                int i = 0;
                bh_arr_each(AstTyped *, ptag, func->tags) {
                    AstTyped *tag = *ptag;
                    assert(tag);
                    assert(tag->type);

                    u32 size = type_size_of(tag->type);
                    bh_buffer_align(&foreign_buffer, type_alignment_of(tag->type));
                    tag_array[i * 2 + 0] = foreign_buffer.length;
                    tag_array[i * 2 + 1] = tag->type->id;
                    ensure_type_has_been_submitted_for_emission(module, tag->type);
                    PATCH_AT(tag_array_offset + i * POINTER_SIZE * 2);
                    
                    bh_buffer_grow(&foreign_buffer, foreign_buffer.length + size);
                    constexpr_ctx.data = foreign_buffer.data;
                    assert(emit_constexpr_(&constexpr_ctx, tag, foreign_buffer.length));
                    foreign_buffer.length += size;

                    i++;
                }

                tag_offsets[funcs_length] = tag_array_offset;
                tag_lengths[funcs_length] = tag_count;
            }

            name_offsets[funcs_length] = func_name_base;
            name_lengths[funcs_length] = func_name_length;
            func_types[funcs_length]   = func->type->id;
            ensure_type_has_been_submitted_for_emission(module, func->type);
            funcs_length++;
        }

        bh_buffer_align(&foreign_buffer, 8);
        u32 funcs_base = foreign_buffer.length;

        fori (i, 0, (i64) funcs_length) {
            bh_buffer_align(&foreign_buffer, POINTER_SIZE);
            WRITE_SLICE(name_offsets[i], name_lengths[i]);
            bh_buffer_write_u32(&foreign_buffer, func_types[i]);
            WRITE_SLICE(tag_offsets[i], tag_lengths[i]);
        }

        OnyxToken *module_name = fb->module_name->token;
        u32 name_base = foreign_buffer.length;
        u32 name_length = module_name->length;
        bh_buffer_append(&foreign_buffer, module_name->text, name_length);
        bh_buffer_align(&foreign_buffer, 8);

        foreign_info[index] = foreign_buffer.length;
        WRITE_SLICE(name_base, name_length);
        WRITE_SLICE(funcs_base, funcs_length);
        index++;
    }


    if (context.options->verbose_output == 1) {
        bh_printf("Foreign blocks size: %d bytes.\n", foreign_buffer.length);
    }

    WasmDatum foreign_info_data = {
        .alignment = 8,
        .length = foreign_buffer.length,
        .data = foreign_buffer.data,
    };
    emit_data_entry(module, &foreign_info_data);
    assert(foreign_info_data.id == foreign_info_data_id);

    bh_arr_each(u32, patch_loc, base_patch_locations) {
        DatumPatchInfo patch;
        patch.kind = Datum_Patch_Relative;
        patch.data_id = foreign_info_data.id;
        patch.offset = 0;
        patch.index = foreign_info_data.id;
        patch.location = *patch_loc;
        bh_arr_push(module->data_patches, patch);
    }

    WasmDatum foreign_table_data = {
        .alignment = POINTER_SIZE,
        .length = block_count * POINTER_SIZE,
        .data = foreign_info,
    };
    emit_data_entry(module, &foreign_table_data);

    fori (i, 0, block_count) {
        DatumPatchInfo patch;
        patch.kind = Datum_Patch_Data;
        patch.data_id = foreign_info_data.id;
        patch.offset = foreign_info[i];
        patch.index = foreign_table_data.id;
        patch.location = i * POINTER_SIZE;
        bh_arr_push(module->data_patches, patch);
    }

    Foreign_Block_Type* tmp_data = bh_alloc(global_heap_allocator, 2 * POINTER_SIZE);
    tmp_data[0] = 0;
    tmp_data[1] = block_count;
    WasmDatum foreign_table_global_data = {
        .alignment = POINTER_SIZE,
        .length = 2 * POINTER_SIZE,
        .data = tmp_data,
    };
    emit_data_entry(module, &foreign_table_global_data);

    {
        DatumPatchInfo patch;
        patch.kind = Datum_Patch_Data;
        patch.data_id = foreign_table_data.id;
        patch.location = 0;
        patch.index = foreign_table_global_data.id;
        patch.offset = 0;
        bh_arr_push(module->data_patches, patch);
    }

    return foreign_table_global_data.id;

#undef WRITE_SLICE
#undef WRITE_PTR
#undef PATCH
}



static u64 build_tagged_procedures(OnyxWasmModule *module) {
    bh_arr(u32) base_patch_locations=NULL;
    bh_arr_new(global_heap_allocator, base_patch_locations, 256);

#define PATCH (bh_arr_push(base_patch_locations, tag_proc_buffer.length))
#define WRITE_PTR(val) \
    bh_buffer_align(&tag_proc_buffer, POINTER_SIZE); \
    PATCH; \
    if (POINTER_SIZE == 4) bh_buffer_write_u32(&tag_proc_buffer, val); \
    if (POINTER_SIZE == 8) bh_buffer_write_u64(&tag_proc_buffer, val); 
#define WRITE_SLICE(ptr, count) \
    WRITE_PTR(ptr); \
    if (POINTER_SIZE == 4) bh_buffer_write_u32(&tag_proc_buffer, count); \
    if (POINTER_SIZE == 8) bh_buffer_write_u64(&tag_proc_buffer, count); 

    #if (POINTER_SIZE == 4)
        #define Tagged_Procedure_Type u32
    #else
        #define Tagged_Procedure_Type u64
    #endif
    u32 proc_count = bh_arr_length(module->procedures_with_tags);
    Tagged_Procedure_Type* tag_proc_info = bh_alloc_array(global_heap_allocator, Tagged_Procedure_Type, proc_count); // HACK
    memset(tag_proc_info, 0, proc_count * sizeof(Tagged_Procedure_Type));

    bh_buffer tag_proc_buffer;
    bh_buffer_init(&tag_proc_buffer, global_heap_allocator, 4096);

    u32 proc_info_data_id = NEXT_DATA_ID(module);

    ConstExprContext constexpr_ctx;
    constexpr_ctx.module = module;
    constexpr_ctx.data_id = proc_info_data_id;

    // 
    // This is necessary because 0 is an invalid offset to store in this
    // buffer, as 0 will map to NULL. This could be a single byte insertion,
    // but 64 bytes keeps better alignment.
    bh_buffer_write_u64(&tag_proc_buffer, 0);

    u32 index = 0;
    bh_arr_each(AstFunction *, pfunc, module->procedures_with_tags) {
        AstFunction *func = *pfunc;

        u32 tag_count = bh_arr_length(func->tags);
        u32 *tag_data_offsets = bh_alloc_array(global_scratch_allocator, u32, tag_count);
        u32 *tag_data_types   = bh_alloc_array(global_scratch_allocator, u32, tag_count);

        u32 tag_index = 0;
        bh_arr_each(AstTyped *, ptag, func->tags) {
            AstTyped *tag = *ptag;
            bh_buffer_align(&tag_proc_buffer, type_alignment_of(tag->type));

            tag_data_offsets[tag_index  ] = tag_proc_buffer.length;
            tag_data_types  [tag_index++] = tag->type->id;
            ensure_type_has_been_submitted_for_emission(module, tag->type);

            u32 size = type_size_of(tag->type);
            bh_buffer_grow(&tag_proc_buffer, tag_proc_buffer.length + size);

            constexpr_ctx.data = tag_proc_buffer.data;
            emit_constexpr(&constexpr_ctx, tag, tag_proc_buffer.length);
            tag_proc_buffer.length += size;
        }

        bh_buffer_align(&tag_proc_buffer, 4);
        u32 tag_array_base = tag_proc_buffer.length;
        fori (i, 0, tag_count) {
            PATCH;
            bh_buffer_write_u32(&tag_proc_buffer, tag_data_offsets[i]);
            bh_buffer_write_u32(&tag_proc_buffer, tag_data_types[i]);
        }

        bh_buffer_align(&tag_proc_buffer, 4);
        tag_proc_info[index++] = tag_proc_buffer.length;

        assert(func->entity && func->entity->package);

        bh_buffer_write_u32(&tag_proc_buffer, get_element_idx(module, func));
        bh_buffer_write_u32(&tag_proc_buffer, 0);
        bh_buffer_write_u32(&tag_proc_buffer, func->type->id);
        ensure_type_has_been_submitted_for_emission(module, func->type);
        WRITE_SLICE(tag_array_base, tag_count);
        bh_buffer_write_u32(&tag_proc_buffer, func->entity->package->id);
    }

    if (context.options->verbose_output == 1) {
        bh_printf("Tagged procedure size: %d bytes.\n", tag_proc_buffer.length);
    }

    WasmDatum proc_info_data = {
        .alignment = 8,
        .length = tag_proc_buffer.length,
        .data = tag_proc_buffer.data,
    };
    emit_data_entry(module, &proc_info_data);
    assert(proc_info_data.id == proc_info_data_id);

    bh_arr_each(u32, patch_loc, base_patch_locations) {
        DatumPatchInfo patch;
        patch.kind = Datum_Patch_Relative;
        patch.data_id = proc_info_data.id;
        patch.location = *patch_loc;
        patch.index = proc_info_data.id;
        patch.offset = 0;
        bh_arr_push(module->data_patches, patch);
    }

    WasmDatum proc_table_data = {
        .alignment = POINTER_SIZE,
        .length = proc_count * POINTER_SIZE,
        .data = tag_proc_info,
    };
    emit_data_entry(module, &proc_table_data);

    fori (i, 0, proc_count) {
        DatumPatchInfo patch;
        patch.kind = Datum_Patch_Data;
        patch.data_id = proc_info_data.id;
        patch.offset = tag_proc_info[i];
        patch.index = proc_table_data.id;
        patch.location = i * POINTER_SIZE;
        bh_arr_push(module->data_patches, patch);
    }

    Tagged_Procedure_Type* tmp_data = bh_alloc(global_heap_allocator, 2 * POINTER_SIZE);
    tmp_data[0] = 0;
    tmp_data[1] = proc_count;
    WasmDatum proc_table_global_data = {
        .alignment = POINTER_SIZE,
        .length = 2 * POINTER_SIZE,
        .data = tmp_data,
    };
    emit_data_entry(module, &proc_table_global_data);

    {
        DatumPatchInfo patch;
        patch.kind = Datum_Patch_Data;
        patch.offset = 0;
        patch.data_id = proc_table_data.id;
        patch.index = proc_table_global_data.id;
        patch.location = 0;
        bh_arr_push(module->data_patches, patch);
    }

    return proc_table_global_data.id;

#undef WRITE_SLICE
#undef WRITE_PTR
#undef PATCH
}


static u64 build_tagged_globals(OnyxWasmModule *module) {
    bh_arr(u32) base_patch_locations=NULL;
    bh_arr_new(global_heap_allocator, base_patch_locations, 256);

#define PATCH (bh_arr_push(base_patch_locations, tag_global_buffer.length))
#define WRITE_PTR(val) \
    bh_buffer_align(&tag_global_buffer, POINTER_SIZE); \
    PATCH; \
    if (POINTER_SIZE == 4) bh_buffer_write_u32(&tag_global_buffer, val); \
    if (POINTER_SIZE == 8) bh_buffer_write_u64(&tag_global_buffer, val); 
#define WRITE_SLICE(ptr, count) \
    WRITE_PTR(ptr); \
    if (POINTER_SIZE == 4) bh_buffer_write_u32(&tag_global_buffer, count); \
    if (POINTER_SIZE == 8) bh_buffer_write_u64(&tag_global_buffer, count); 

    #if (POINTER_SIZE == 4)
        #define Tagged_Global_Type u32
    #else
        #define Tagged_Global_Type u64
    #endif
    u32 global_count = bh_arr_length(module->globals_with_tags);
    Tagged_Global_Type* tag_global_info = bh_alloc_array(global_heap_allocator, Tagged_Global_Type, global_count); // HACK
    memset(tag_global_info, 0, global_count * sizeof(Tagged_Global_Type));

    bh_buffer tag_global_buffer;
    bh_buffer_init(&tag_global_buffer, global_heap_allocator, 4096);

    u32 global_info_data_id = NEXT_DATA_ID(module);

    ConstExprContext constexpr_ctx;
    constexpr_ctx.module = module;
    constexpr_ctx.data_id = global_info_data_id;

    // 
    // This is necessary because 0 is an invalid offset to store in this
    // buffer, as 0 will map to NULL. This could be a single byte insertion,
    // but 64 bytes keeps better alignment.
    bh_buffer_write_u64(&tag_global_buffer, 0);

    u32 index = 0;
    bh_arr_each(AstMemRes *, pmemres, module->globals_with_tags) {
        AstMemRes *memres = *pmemres;

        u32 tag_count = bh_arr_length(memres->tags);
        u32 *tag_data_offsets = bh_alloc_array(global_scratch_allocator, u32, tag_count);
        u32 *tag_data_types   = bh_alloc_array(global_scratch_allocator, u32, tag_count);

        u32 tag_index = 0;
        bh_arr_each(AstTyped *, ptag, memres->tags) {
            AstTyped *tag = *ptag;
            bh_buffer_align(&tag_global_buffer, type_alignment_of(tag->type));

            tag_data_offsets[tag_index  ] = tag_global_buffer.length;
            tag_data_types  [tag_index++] = tag->type->id;
            ensure_type_has_been_submitted_for_emission(module, tag->type);

            u32 size = type_size_of(tag->type);
            bh_buffer_grow(&tag_global_buffer, tag_global_buffer.length + size);

            constexpr_ctx.data = tag_global_buffer.data;
            emit_constexpr(&constexpr_ctx, tag, tag_global_buffer.length);
            tag_global_buffer.length += size;
        }

        bh_buffer_align(&tag_global_buffer, 4);
        u32 tag_array_base = tag_global_buffer.length;
        fori (i, 0, tag_count) {
            PATCH;
            bh_buffer_write_u32(&tag_global_buffer, tag_data_offsets[i]);
            bh_buffer_write_u32(&tag_global_buffer, tag_data_types[i]);
        }

        bh_buffer_align(&tag_global_buffer, 4);
        tag_global_info[index++] = tag_global_buffer.length;

        assert(memres->entity && memres->entity->package);

        DatumPatchInfo patch;
        patch.kind = Datum_Patch_Data;
        patch.index = global_info_data_id;
        patch.location = tag_global_buffer.length;
        patch.offset = 0;
        patch.data_id = 0;
        patch.node_to_use_if_data_id_is_null = (AstNode *) memres;
        bh_arr_push(module->data_patches, patch);

        bh_buffer_write_u32(&tag_global_buffer, 0);
        bh_buffer_write_u32(&tag_global_buffer, memres->type->id);
        ensure_type_has_been_submitted_for_emission(module, memres->type);
        WRITE_SLICE(tag_array_base, tag_count);
        bh_buffer_write_u32(&tag_global_buffer, memres->entity->package->id);
    }

    if (context.options->verbose_output == 1) {
        bh_printf("Tagged global size: %d bytes.\n", tag_global_buffer.length);
    }

    WasmDatum global_info_data = {
        .alignment = 8,
        .length = tag_global_buffer.length,
        .data = tag_global_buffer.data,
    };
    emit_data_entry(module, &global_info_data);
    assert(global_info_data.id == global_info_data_id);

    bh_arr_each(u32, patch_loc, base_patch_locations) {
        DatumPatchInfo patch;
        patch.kind = Datum_Patch_Relative;
        patch.data_id = global_info_data.id;
        patch.location = *patch_loc;
        patch.index = global_info_data.id;
        patch.offset = 0;
        bh_arr_push(module->data_patches, patch);
    }

    WasmDatum global_table_data = {
        .alignment = POINTER_SIZE,
        .length = global_count * POINTER_SIZE,
        .data = tag_global_info,
    };
    emit_data_entry(module, &global_table_data);

    fori (i, 0, global_count) {
        DatumPatchInfo patch;
        patch.kind = Datum_Patch_Data;
        patch.data_id = global_info_data.id;
        patch.offset = tag_global_info[i];
        patch.index = global_table_data.id;
        patch.location = i * POINTER_SIZE;
        bh_arr_push(module->data_patches, patch);
    }

    Tagged_Procedure_Type* tmp_data = bh_alloc(global_heap_allocator, 2 * POINTER_SIZE);
    tmp_data[0] = 0;
    tmp_data[1] = global_count;
    WasmDatum global_table_global_data = {
        .alignment = POINTER_SIZE,
        .length = 2 * POINTER_SIZE,
        .data = tmp_data,
    };
    emit_data_entry(module, &global_table_global_data);

    {
        DatumPatchInfo patch;
        patch.kind = Datum_Patch_Data;
        patch.offset = 0;
        patch.data_id = global_table_data.id;
        patch.index = global_table_global_data.id;
        patch.location = 0;
        bh_arr_push(module->data_patches, patch);
    }

    return global_table_global_data.id;
}

