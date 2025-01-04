#define BH_DEBUG
#include "stb_ds.h"
#include "types.h"
#include "astnodes.h"
#include "utils.h"
#include "errors.h"
#include "parser.h"

static Type* type_create(Context *context, TypeKind kind, u32 extra_type_pointer_count) {
    Type* type = bh_alloc(context->ast_alloc, sizeof(Type) + sizeof(Type *) * extra_type_pointer_count);
    memset(type, 0, sizeof(Type));

    type->kind = kind;
    return type;
}

static void type_register(Context *context, Type* type) {
    type->id = ++context->next_type_id;
    if (type->ast_type) type->ast_type->type_id = type->id;

    bh_imap_put(&context->types.type_map, type->id, (u64) type);
}

void types_init(Context *context) {
#define MAKE_MAP(x) (memset(&x, 0, sizeof(x)), bh_imap_init(&x, context->gp_alloc, 255))
    MAKE_MAP(context->types.type_map);
    MAKE_MAP(context->types.pointer_map);
    MAKE_MAP(context->types.multi_pointer_map);
    MAKE_MAP(context->types.array_map);
    MAKE_MAP(context->types.slice_map);
    MAKE_MAP(context->types.dynarr_map);
    MAKE_MAP(context->types.vararg_map);
#undef MAKE_MAP

    context->types.func_map = NULL;
    sh_new_arena(context->types.func_map);

    fori (i, 0, Basic_Kind_Count) {
        context->types.basic[i] = bh_alloc_item(context->ast_alloc, Type);
    }

    *context->types.basic[Basic_Kind_Void]          = ((Type) { Type_Kind_Basic, 0, 0, (AstType *) &context->basic_types.type_void,      { Basic_Kind_Void,                    0,                       0,  1, "void"   } });
    *context->types.basic[Basic_Kind_Bool]          = ((Type) { Type_Kind_Basic, 0, 0, (AstType *) &context->basic_types.type_bool,      { Basic_Kind_Bool,   Basic_Flag_Boolean,                       1,  1, "bool"   } });
    *context->types.basic[Basic_Kind_Int_Unsized]   = ((Type) { Type_Kind_Basic, 0, 0, NULL,                                             { Basic_Kind_Int_Unsized, Basic_Flag_Integer,                  0,  0, "unsized int" } });
    *context->types.basic[Basic_Kind_I8]            = ((Type) { Type_Kind_Basic, 0, 0, (AstType *) &context->basic_types.type_i8,        { Basic_Kind_I8,     Basic_Flag_Integer,                       1,  1, "i8"     } });
    *context->types.basic[Basic_Kind_U8]            = ((Type) { Type_Kind_Basic, 0, 0, (AstType *) &context->basic_types.type_u8,        { Basic_Kind_U8,     Basic_Flag_Integer | Basic_Flag_Unsigned, 1,  1, "u8"     } });
    *context->types.basic[Basic_Kind_I16]           = ((Type) { Type_Kind_Basic, 0, 0, (AstType *) &context->basic_types.type_i16,       { Basic_Kind_I16,    Basic_Flag_Integer,                       2,  2, "i16"    } });
    *context->types.basic[Basic_Kind_U16]           = ((Type) { Type_Kind_Basic, 0, 0, (AstType *) &context->basic_types.type_u16,       { Basic_Kind_U16,    Basic_Flag_Integer | Basic_Flag_Unsigned, 2,  2, "u16"    } });
    *context->types.basic[Basic_Kind_I32]           = ((Type) { Type_Kind_Basic, 0, 0, (AstType *) &context->basic_types.type_i32,       { Basic_Kind_I32,    Basic_Flag_Integer,                       4,  4, "i32"    } });
    *context->types.basic[Basic_Kind_U32]           = ((Type) { Type_Kind_Basic, 0, 0, (AstType *) &context->basic_types.type_u32,       { Basic_Kind_U32,    Basic_Flag_Integer | Basic_Flag_Unsigned, 4,  4, "u32"    } });
    *context->types.basic[Basic_Kind_I64]           = ((Type) { Type_Kind_Basic, 0, 0, (AstType *) &context->basic_types.type_i64,       { Basic_Kind_I64,    Basic_Flag_Integer,                       8,  8, "i64"    } });
    *context->types.basic[Basic_Kind_U64]           = ((Type) { Type_Kind_Basic, 0, 0, (AstType *) &context->basic_types.type_u64,       { Basic_Kind_U64,    Basic_Flag_Integer | Basic_Flag_Unsigned, 8,  8, "u64"    } });
    *context->types.basic[Basic_Kind_Float_Unsized] = ((Type) { Type_Kind_Basic, 0, 0, NULL,                                             { Basic_Kind_Float_Unsized, Basic_Flag_Float,                  0,  0, "unsized float" } });
    *context->types.basic[Basic_Kind_F32]           = ((Type) { Type_Kind_Basic, 0, 0, (AstType *) &context->basic_types.type_f32,       { Basic_Kind_F32,    Basic_Flag_Float,                         4,  4, "f32"    } });
    *context->types.basic[Basic_Kind_F64]           = ((Type) { Type_Kind_Basic, 0, 0, (AstType *) &context->basic_types.type_f64,       { Basic_Kind_F64,    Basic_Flag_Float,                         8,  4, "f64"    } });
    *context->types.basic[Basic_Kind_Rawptr]        = ((Type) { Type_Kind_Basic, 0, 0, (AstType *) &context->basic_types.type_rawptr,    { Basic_Kind_Rawptr, Basic_Flag_Pointer,                       POINTER_SIZE,  POINTER_SIZE, "rawptr" } });
    *context->types.basic[Basic_Kind_I8X16]         = ((Type) { Type_Kind_Basic, 0, 0, (AstType *) &context->basic_types.type_i8x16,     { Basic_Kind_I8X16,  Basic_Flag_SIMD,                          16, 16, "i8x16" } });
    *context->types.basic[Basic_Kind_I16X8]         = ((Type) { Type_Kind_Basic, 0, 0, (AstType *) &context->basic_types.type_i16x8,     { Basic_Kind_I16X8,  Basic_Flag_SIMD,                          16, 16, "i16x8" } });
    *context->types.basic[Basic_Kind_I32X4]         = ((Type) { Type_Kind_Basic, 0, 0, (AstType *) &context->basic_types.type_i32x4,     { Basic_Kind_I32X4,  Basic_Flag_SIMD,                          16, 16, "i32x4" } });
    *context->types.basic[Basic_Kind_I64X2]         = ((Type) { Type_Kind_Basic, 0, 0, (AstType *) &context->basic_types.type_i64x2,     { Basic_Kind_I64X2,  Basic_Flag_SIMD,                          16, 16, "i64x2" } });
    *context->types.basic[Basic_Kind_F32X4]         = ((Type) { Type_Kind_Basic, 0, 0, (AstType *) &context->basic_types.type_f32x4,     { Basic_Kind_F32X4,  Basic_Flag_SIMD,                          16, 16, "f32x4" } });
    *context->types.basic[Basic_Kind_F64X2]         = ((Type) { Type_Kind_Basic, 0, 0, (AstType *) &context->basic_types.type_f64x2,     { Basic_Kind_F64X2,  Basic_Flag_SIMD,                          16, 16, "f64x2" } });
    *context->types.basic[Basic_Kind_V128]          = ((Type) { Type_Kind_Basic, 0, 0, (AstType *) &context->basic_types.type_v128,      { Basic_Kind_V128,   Basic_Flag_SIMD,                          16, 16, "v128"  } });
    *context->types.basic[Basic_Kind_Type_Index]    = ((Type) { Type_Kind_Basic, 0, 0, (AstType *) &context->basic_types.type_type_expr, { Basic_Kind_Type_Index, Basic_Flag_Type_Index,                4,  4,   "type_expr" } });

    fori (i, 0, Basic_Kind_Count) {
        type_register(context, context->types.basic[i]);
    }
}

void types_dump_type_info(Context *context) {
    bh_arr_each(bh__imap_entry, entry, context->types.type_map.entries) {
        bh_printf("%d -> %s\n", entry->key, type_get_name(context, (Type *) entry->value));
    }
}

Type* type_lookup_by_id(Context *context, u32 id) {
    if (bh_imap_has(&context->types.type_map, id)) {
        return (Type *) bh_imap_get(&context->types.type_map, id);
    }

    return NULL;
}

b32 types_are_compatible(Context *context, Type* t1, Type* t2) {
    // NOTE: If they are pointing to the same thing,
    // it is safe to assume they are the same type
    if (t1 == t2) return 1;
    if (t1 == NULL || t2 == NULL) return 0;
    if (t1->id == t2->id) return 1;

    if (t1 == context->types.auto_return || t2 == context->types.auto_return) {
        return 0;
    }

    switch (t1->kind) {
        case Type_Kind_Basic:
            if (t2->kind == Type_Kind_Basic) {
                // Signedness of an integer doesn't matter.
                if ((t1->Basic.flags & Basic_Flag_Integer) && (t2->Basic.flags & Basic_Flag_Integer)) {
                    return t1->Basic.size == t2->Basic.size;
                }

                if (t1->Basic.kind == Basic_Kind_V128 || t2->Basic.kind == Basic_Kind_V128) return 1;
            }

            if (t1->Basic.kind == Basic_Kind_Rawptr && (type_is_pointer(t2) || type_is_multi_pointer(t2))) {
                return 1;
            }
            break;

        case Type_Kind_Pointer: {
            if (t2->kind == Type_Kind_Pointer) {
                if (types_are_compatible(context, t1->Pointer.elem, t2->Pointer.elem)) return 1;

                if (t1->Pointer.elem->kind == Type_Kind_Struct && t2->Pointer.elem->kind == Type_Kind_Struct) {
                    Type* t1_struct = t1->Pointer.elem;
                    Type* t2_struct = t2->Pointer.elem;

                    bh_arr(StructMember *) members = t1_struct->Struct.memarr;
                    if (bh_arr_length(members) > 0 && members[0]->used)
                        return types_are_compatible(context, t2_struct,members[0]->type);
                }
            }

            // Pointers promote to multi-pointers
            // &u8 -> [&] u8
            if (t2->kind == Type_Kind_MultiPointer) {
                if (types_are_compatible(context,t1->Pointer.elem, t2->Pointer.elem)) return 1;
            }

            // Pointer decays to rawptr
            if (t2->kind == Type_Kind_Basic && t2->Basic.kind == Basic_Kind_Rawptr) return 1;

            break;
        }

        case Type_Kind_MultiPointer: {
            // Multi-pointer decays to pointer
            // [&] u8 -> &u8
            if (t2->kind == Type_Kind_Pointer) {
                if (types_are_compatible(context, t1->MultiPointer.elem, t2->Pointer.elem)) return 1;
            }

            // Multi-pointer decays to rawptr
            if (t2->kind == Type_Kind_Basic && t2->Basic.kind == Basic_Kind_Rawptr) return 1;

            break;
        }

        case Type_Kind_Array: {
            if (t2->kind != Type_Kind_Array) return 0;

            if (t1->Array.count != 0)
                if (t1->Array.count != t2->Array.count) return 0;

            return types_are_compatible(context, t1->Array.elem, t2->Array.elem);
        }

        case Type_Kind_Struct: {
            // NOTE: The check above for t1 == t2 would already catch this.

            // if (t2->kind != Type_Kind_Struct) return 0;
            // if (t1->Struct.unique_id != t2->Struct.unique_id) return 0;
            // if (t1->Struct.mem_count != t2->Struct.mem_count) return 0;
            return 0;
        }

        case Type_Kind_Enum: {
            // NOTE: The check above for t1 == t2 would already catch this.
            return 0;
        }

        case Type_Kind_Function: {
            if (t2->kind != Type_Kind_Function) return 0;
            if (t1->Function.param_count != t2->Function.param_count) return 0;

            if (!types_are_compatible(context, t1->Function.return_type, t2->Function.return_type)) return 0;

            if (t1->Function.param_count > 0) {
                fori (i, 0, t1->Function.param_count) {
                    if (!types_are_compatible(context, t1->Function.params[i], t2->Function.params[i])) return 0;
                }
            }

            return 1;
        }

        case Type_Kind_Slice: {
            if (t2->kind != Type_Kind_Slice) return 0;
            return types_are_compatible(context, t1->Slice.elem, t2->Slice.elem);
        }

        case Type_Kind_VarArgs: {
            if (t2->kind != Type_Kind_VarArgs) return 0;
            return types_are_compatible(context, t1->VarArgs.elem, t2->VarArgs.elem);
        }

        case Type_Kind_DynArray: {
            if (t2->kind != Type_Kind_DynArray) return 0;
            return types_are_compatible(context, t1->DynArray.elem, t2->DynArray.elem);
        }

        case Type_Kind_Compound: {
            if (t2->kind != Type_Kind_Compound) return 0;
            if (t1->Compound.count != t2->Compound.count) return 0;

            fori (i, 0, (i64) t1->Compound.count) {
                if (!types_are_compatible(context, t1->Compound.types[i], t2->Compound.types[i])) return 0;
            }

            return 1;
        }

        case Type_Kind_Distinct:
            // If the above cases didn't catch it, then these distinct types are not compatible.
            return 0;

        case Type_Kind_Union:
            // If the above cases didn't catch it, then these union types are not compatible.
            return 0;

        case Type_Kind_Invalid:
            // I'm not 100% sure when this can happen, but if this happens, the types will automatically not match.
            return 0;

        default:
            assert("Invalid type" && 0);
            break;
    }

    return 0;
}

u32 type_size_of(Type* type) {
    if (type == NULL) return 0;

    switch (type->kind) {
        case Type_Kind_Basic:    return type->Basic.size;
        case Type_Kind_MultiPointer:
        case Type_Kind_Pointer:  return POINTER_SIZE;
        case Type_Kind_Function: return 2 * POINTER_SIZE;
        case Type_Kind_Array:    return type->Array.size;
        case Type_Kind_Struct:   return type->Struct.size;
        case Type_Kind_Enum:     return type_size_of(type->Enum.backing);
        case Type_Kind_Slice:    return POINTER_SIZE * 2; // HACK: These should not have to be 16 bytes in size, they should only have to be 12,
        case Type_Kind_VarArgs:  return POINTER_SIZE * 2; // but there are alignment issues right now with that so I decided to not fight it and just make them 16 bytes in size.
        case Type_Kind_DynArray: return POINTER_SIZE * 3 + POINTER_SIZE * 2 + POINTER_SIZE; // data (8), count (4), capacity (4), allocator { func (4 + 4 + 8), data (8) }
        case Type_Kind_Compound: return type->Compound.size;
        case Type_Kind_Distinct: return type_size_of(type->Distinct.base_type);
        case Type_Kind_Union:    return type->Union.size;
        default:                 return 0;
    }
}

u32 type_alignment_of(Type* type) {
    if (type == NULL) return 1;

    switch (type->kind) {
        case Type_Kind_Basic:    return type->Basic.alignment;
        case Type_Kind_MultiPointer:
        case Type_Kind_Pointer:  return POINTER_SIZE;
        case Type_Kind_Function: return POINTER_SIZE;
        case Type_Kind_Array:    return type_alignment_of(type->Array.elem);
        case Type_Kind_Struct:   return type->Struct.alignment;
        case Type_Kind_Enum:     return type_alignment_of(type->Enum.backing);
        case Type_Kind_Slice:    return POINTER_SIZE;
        case Type_Kind_VarArgs:  return POINTER_SIZE;
        case Type_Kind_DynArray: return POINTER_SIZE;
        case Type_Kind_Compound: return 4; // HACK
        case Type_Kind_Distinct: return type_alignment_of(type->Distinct.base_type);
        case Type_Kind_Union:    return type->Union.alignment;
        default: return 1;
    }
}

static b32 type_is_ready_to_be_used_in_construction(Type *t) {
    switch (t->kind) {
        case Type_Kind_Struct: return t->Struct.status != SPS_Start; break;
        case Type_Kind_Union:  return t->Union.status != SPS_Start; break;
        default: return 1;
    }
}

static Type* type_build_from_ast_inner(Context *context, AstType* type_node, b32 accept_partial_types) {
    if (type_node == NULL) return NULL;

    bh_allocator alloc = context->ast_alloc;

    switch (type_node->kind) {
        case Ast_Kind_Pointer_Type: {
            Type *inner_type = type_build_from_ast_inner(context, ((AstPointerType *) type_node)->elem, accept_partial_types);
            Type *ptr_type = type_make_pointer(context, inner_type);
            if (ptr_type) ptr_type->ast_type = type_node;
            return ptr_type;
        }

        case Ast_Kind_Multi_Pointer_Type: {
            Type *inner_type = type_build_from_ast_inner(context, ((AstMultiPointerType *) type_node)->elem, accept_partial_types);
            Type *ptr_type = type_make_multi_pointer(context, inner_type);
            if (ptr_type) ptr_type->ast_type = type_node;
            return ptr_type;
        }

        case Ast_Kind_Function_Type: {
            AstFunctionType* ftype_node = (AstFunctionType *) type_node;
            u64 param_count = ftype_node->param_count;

            Type* return_type = type_build_from_ast_inner(context, ftype_node->return_type, accept_partial_types);
            if (return_type == NULL) return NULL;

            Type* func_type = type_create(context, Type_Kind_Function, param_count);
            func_type->ast_type = type_node;
            func_type->Function.param_count = param_count;
            func_type->Function.needed_param_count = param_count;
            func_type->Function.vararg_arg_pos = -1;
            func_type->Function.return_type = return_type;

            if (param_count > 0) {
                fori (i, 0, (i64) param_count) {
                    func_type->Function.params[i] = type_build_from_ast_inner(context, ftype_node->params[i], accept_partial_types);

                    // LEAK LEAK LEAK
                    if (func_type->Function.params[i] == NULL) return NULL;
                }
            }

            char* name = (char *) type_get_unique_name(context, func_type);
            if (func_type->Function.return_type != context->types.auto_return) {
                i32 index = shgeti(context->types.func_map, name);
                if (index != -1) {
                    u64 id = context->types.func_map[index].value;
                    Type* existing_type = (Type *) bh_imap_get(&context->types.type_map, id);

                    // LEAK LEAK LEAK the func_type that is created
                    return existing_type;
                }
            }

            type_register(context, func_type);
            shput(context->types.func_map, name, func_type->id);

            return func_type;
        }

        case Ast_Kind_Array_Type: {
            AstArrayType* a_node = (AstArrayType *) type_node;

            Type *elem_type = type_build_from_ast_inner(context, a_node->elem, accept_partial_types);
            if (elem_type == NULL)  return NULL;

            u32 count = 0;
            if (a_node->count_expr) {
                if (a_node->count_expr->type == NULL)
                    a_node->count_expr->type = type_build_from_ast(context, a_node->count_expr->type_node);

                if (node_is_auto_cast((AstNode *) a_node->count_expr)) {
                    a_node->count_expr = ((AstUnaryOp *) a_node)->expr;
                }

                resolve_expression_type(context, a_node->count_expr);

                // NOTE: Currently, the count_expr has to be an I32 literal
                if (a_node->count_expr->type->kind != Type_Kind_Basic
                    || a_node->count_expr->type->Basic.kind != Basic_Kind_I32) {
                    ONYX_ERROR(type_node->token->pos, Error_Critical, "Array type expects type 'i32' for size, got '%s'.",
                        type_get_name(context, a_node->count_expr->type));
                    return NULL;
                }

                b32 valid = 0;
                count = get_expression_integer_value(context, a_node->count_expr, &valid);

                if (!valid) {
                    if (!(a_node->count_expr->flags & Ast_Flag_Comptime)) {
                        ONYX_ERROR(a_node->token->pos, Error_Critical, "Array type size must be a constant.");
                    }
                    else {
                        ONYX_ERROR(a_node->token->pos, Error_Critical, "Array type size expression must be 'i32', got '%s'.",
                            type_get_name(context, a_node->count_expr->type));
                    }

                    return NULL;
                }

                if ((i32)count < 0) {
                    ONYX_ERROR(a_node->token->pos, Error_Critical, "Array type size must be a positive integer.");
                    return NULL;
                }
            }

            Type* array_type = type_make_array(context, elem_type, count);
            if (array_type) array_type->ast_type = type_node;
            return array_type;
        }

        case Ast_Kind_Struct_Type: {
            AstStructType* s_node = (AstStructType *) type_node;
            if (s_node->stcache != NULL) return s_node->stcache;
            if (s_node->pending_type != NULL && s_node->pending_type_is_valid) return s_node->pending_type;
            if (!s_node->ready_to_build_type) return NULL;

            Type* s_type;
            if (s_node->pending_type == NULL) {
                s_type = type_create(context, Type_Kind_Struct, 0);
                s_node->pending_type = s_type;

                s_type->ast_type = type_node;
                s_type->Struct.name = s_node->name;
                s_type->Struct.mem_count = bh_arr_length(s_node->members);
                s_type->Struct.meta_tags = s_node->meta_tags;
                s_type->Struct.constructed_from = NULL;
                s_type->Struct.poly_sln = NULL;
                s_type->Struct.status = SPS_Start;
                s_type->Struct.scope = s_node->scope;
                type_register(context, s_type);

                s_type->Struct.memarr = NULL;
                sh_new_arena(s_type->Struct.members);
                bh_arr_new(context->gp_alloc, s_type->Struct.memarr, s_type->Struct.mem_count);

            } else {
                s_type = s_node->pending_type;
            }

            bh_arr_clear(s_type->Struct.memarr);
            shfree(s_type->Struct.members);
            sh_new_arena(s_type->Struct.members);

            s_node->pending_type_is_valid = 1;

            b32 is_union = s_node->is_union;
            u32 size = 0;
            u32 offset = 0;
            u32 alignment = 1, mem_alignment;
            u32 idx = 0;
            bh_arr_each(AstStructMember *, member, s_node->members) {
                if ((*member)->type == NULL)
                    (*member)->type = type_build_from_ast_inner(context, (*member)->type_node, 1);

                if ((*member)->type == NULL) {
                    if (context->cycle_detected) {
                        ONYX_ERROR((* member)->token->pos, Error_Critical, "Unable to figure out the type of this structure member.");
                    }

                    s_node->pending_type_is_valid = 0;
                    return accept_partial_types ? s_node->pending_type : NULL;
                }

                if (!type_is_ready_to_be_used_in_construction((*member)->type)) {
                    s_node->pending_type_is_valid = 0;
                    return accept_partial_types ? s_node->pending_type : NULL;
                }

                mem_alignment = type_alignment_of((*member)->type);
                if (mem_alignment <= 0) {
                    ONYX_ERROR((*member)->token->pos, Error_Critical, "Invalid member type: %s. Has alignment %d", type_get_name(context, (*member)->type), mem_alignment);
                    return NULL;
                }

                if (mem_alignment > alignment) alignment = mem_alignment;

                if (!s_node->is_packed) {
                    bh_align(offset, mem_alignment);
                }

                token_toggle_end((*member)->token);
                if (shgeti(s_type->Struct.members, (*member)->token->text) != -1) {
                    ONYX_ERROR((*member)->token->pos, Error_Critical, "Duplicate struct member, '%s'.", (*member)->token->text);
                    token_toggle_end((*member)->token);
                    return NULL;
                }

                StructMember* smem = bh_alloc_item(context->ast_alloc, StructMember);
                smem->offset = offset;
                smem->type = (*member)->type;
                smem->idx = idx;
                smem->name = bh_strdup(context->ast_alloc, (*member)->token->text);
                smem->token = (*member)->token;
                smem->initial_value = &(*member)->initial_value;
                smem->meta_tags = (*member)->meta_tags;
                smem->member_node = *member;

                smem->included_through_use = 0;
                smem->used = (*member)->is_used;
                smem->use_through_pointer_index = -1;
                shput(s_type->Struct.members, (*member)->token->text, smem);
                bh_arr_push(s_type->Struct.memarr, smem);
                token_toggle_end((*member)->token);

                u32 type_size = type_size_of((*member)->type);

                if (!is_union) {
                    offset += type_size;
                    size = offset;
                } else {
                    size = bh_max(size, type_size);
                }

                idx++;
            }

            u32 min_alignment = get_expression_integer_value(context, s_node->min_alignment_, NULL);
            alignment = bh_max(min_alignment, alignment);
            if (!s_node->is_packed) {
                bh_align(size, alignment);
            }

            u32 min_size = get_expression_integer_value(context, s_node->min_size_, NULL);
            size = bh_max(min_size, size);

            s_type->Struct.alignment = alignment;
            s_type->Struct.size = size;

            s_type->Struct.status = SPS_Members_Done;
            return s_type;
        }

        case Ast_Kind_Enum_Type: {
            AstEnumType* enum_node = (AstEnumType *) type_node;
            if (enum_node->etcache) return enum_node->etcache;
            if (enum_node->backing_type == NULL) return NULL;

            Type* enum_type = type_create(context, Type_Kind_Enum, 0);
            enum_node->etcache = enum_type;

            enum_type->ast_type = type_node;
            enum_type->Enum.backing = enum_node->backing_type;
            enum_type->Enum.name = enum_node->name;
            enum_type->Enum.is_flags = enum_node->is_flags;

            type_register(context, enum_type);
            return enum_type;
        }

        case Ast_Kind_Slice_Type: {
            Type* slice_type = type_make_slice(context, type_build_from_ast_inner(context, ((AstSliceType *) type_node)->elem, accept_partial_types));
            if (slice_type) slice_type->ast_type = type_node;
            return slice_type;
        }

        case Ast_Kind_DynArr_Type: {
            Type* dynarr_type = type_make_dynarray(context, type_build_from_ast_inner(context, ((AstDynArrType *) type_node)->elem, accept_partial_types));
            if (dynarr_type) dynarr_type->ast_type = type_node;
            return dynarr_type;
        }

        case Ast_Kind_VarArg_Type: {
            Type* va_type = type_make_varargs(context, type_build_from_ast_inner(context, ((AstVarArgType *) type_node)->elem, accept_partial_types));
            if (va_type) va_type->ast_type = type_node;
            return va_type;
        }

        case Ast_Kind_Basic_Type: {
            return ((AstBasicType *) type_node)->basic_type;
        }

        case Ast_Kind_Type_Alias: {
            Type* type = type_build_from_ast_inner(context, ((AstTypeAlias *) type_node)->to, accept_partial_types);
            if (type && type->ast_type) type_node->type_id = type->id;
            return type;
        }

        case Ast_Kind_Type_Raw_Alias:
            return ((AstTypeRawAlias *) type_node)->to;

        case Ast_Kind_Poly_Struct_Type: {
            if (type_node->type_id != 0) return NULL;

            Type* p_type = type_create(context, Type_Kind_PolyStruct, 0);
            p_type->ast_type = type_node;
            p_type->PolyStruct.name = ((AstPolyStructType *) type_node)->name;
            p_type->PolyStruct.meta_tags = ((AstPolyStructType *) type_node)->base_struct->meta_tags;
            p_type->PolyStruct.scope = ((AstPolyStructType *) type_node)->scope;

            type_register(context, p_type);
            return NULL;
        }

        case Ast_Kind_Poly_Union_Type: {
            if (type_node->type_id != 0) return NULL;

            Type* p_type = type_create(context, Type_Kind_PolyUnion, 0);
            p_type->ast_type = type_node;
            p_type->PolyUnion.name = ((AstPolyUnionType *) type_node)->name;
            p_type->PolyUnion.meta_tags = ((AstPolyUnionType *) type_node)->base_union->meta_tags;
            p_type->PolyUnion.scope = ((AstPolyUnionType *) type_node)->scope;

            type_register(context, p_type);
            return NULL;
        }

        case Ast_Kind_Poly_Call_Type: {
            AstPolyCallType* pc_type = (AstPolyCallType *) type_node;
            pc_type->callee = (AstType *) strip_aliases((AstNode *) pc_type->callee);

            if (!(pc_type->callee && (
                    pc_type->callee->kind == Ast_Kind_Poly_Struct_Type ||
                    pc_type->callee->kind == Ast_Kind_Poly_Union_Type
                ))) {

                // If it is an unresolved field access or symbol, just return because an error will be printed elsewhere.
                if (pc_type->callee->kind == Ast_Kind_Field_Access || pc_type->callee->kind == Ast_Kind_Symbol) return NULL;

                ONYX_ERROR(pc_type->token->pos, Error_Critical, "Cannot instantiate a concrete type off of a non-polymorphic type.");
                ONYX_ERROR(pc_type->callee->token->pos, Error_Critical, "Here is the type trying to be instantiated. (%s)", onyx_ast_node_kind_string(pc_type->callee->kind));
                return NULL;
            }

            bh_arr(AstPolySolution) slns = NULL;
            bh_arr_new(context->gp_alloc, slns, bh_arr_length(pc_type->params));
            bh_arr_each(AstNode *, given, pc_type->params) {
                if (node_is_type(*given)) {
                    Type* param_type = type_build_from_ast_inner(context, (AstType *) *given, 1);

                    // LEAK LEAK LEAK
                    if (param_type == NULL) return NULL;

                    bh_arr_push(slns, ((AstPolySolution) {
                        .kind     = PSK_Type,
                        .type     = param_type,
                    }));
                } else {
                    bh_arr_push(slns, ((AstPolySolution) {
                        .kind  = PSK_Value,
                        .value = (AstTyped *) *given,
                    }));
                }
            }

            Type* concrete = NULL;
            if (pc_type->callee->kind == Ast_Kind_Poly_Struct_Type) {
                AstPolyStructType* ps_type = (AstPolyStructType *) pc_type->callee;
                type_build_from_ast_inner(context, (AstType *) ps_type, 0);
                concrete = polymorphic_struct_lookup(context, ps_type, slns, pc_type->token->pos, (pc_type->flags & Ast_Flag_Header_Check_No_Error) == 0);
            }
            else if (pc_type->callee->kind == Ast_Kind_Poly_Union_Type) {
                AstPolyUnionType* pu_type = (AstPolyUnionType *) pc_type->callee;
                type_build_from_ast_inner(context, (AstType *) pu_type, 0);
                concrete = polymorphic_union_lookup(context, pu_type, slns, pc_type->token->pos, (pc_type->flags & Ast_Flag_Header_Check_No_Error) == 0);
            }

            // This should be copied in the previous function.
            // CLEANUP: Maybe don't copy it and just use this one since it is allocated on the heap?
            bh_arr_free(slns);

            if (!concrete) return NULL;
            if (concrete == (Type *) &context->node_that_signals_failure) return concrete;
            pc_type->resolved_type = concrete;
            return concrete;
        }

        case Ast_Kind_Type_Compound: {
            AstCompoundType* ctype = (AstCompoundType *) type_node;

            i64 type_count = bh_arr_length(ctype->types);

            Type* comp_type = type_create(context, Type_Kind_Compound, type_count);
            comp_type->Compound.size = 0;
            comp_type->Compound.count = type_count;

            fori (i, 0, type_count) {
                assert(ctype->types[i] != NULL);
                comp_type->Compound.types[i] = type_build_from_ast_inner(context, ctype->types[i], accept_partial_types);

                // LEAK LEAK LEAK
                if (comp_type->Compound.types[i] == NULL) return NULL;

                comp_type->Compound.size += bh_max(type_size_of(comp_type->Compound.types[i]), 4);
            }

            bh_align(comp_type->Compound.size, 4);

            comp_type->Compound.linear_members = NULL;
            bh_arr_new(context->gp_alloc, comp_type->Compound.linear_members, comp_type->Compound.count);
            build_linear_types_with_offset(context, comp_type, &comp_type->Compound.linear_members, 0);

            type_register(context, comp_type);
            return comp_type;
        }

        case Ast_Kind_Alias: {
            AstAlias* alias = (AstAlias *) type_node;
            return type_build_from_ast_inner(context, (AstType *) alias->alias, accept_partial_types);
        }

        case Ast_Kind_Typeof: {
            AstTypeOf* type_of = (AstTypeOf *) type_node;
            if (type_of->resolved_type != NULL) {
                return type_of->resolved_type;
            }

            return type_of->expr->type;
        }

        case Ast_Kind_Distinct_Type: {
            AstDistinctType* distinct = (AstDistinctType *) type_node;
            if (distinct->dtcache) return distinct->dtcache;

            Type *base_type = type_build_from_ast(context, distinct->base_type);
            if (base_type == NULL) return NULL;

            Type *distinct_type = type_create(context, Type_Kind_Distinct, 0);
            distinct_type->Distinct.base_type = base_type;
            distinct_type->Distinct.name = distinct->name;
            distinct_type->Distinct.scope = distinct->scope;
            distinct_type->ast_type = type_node;
            distinct->dtcache = distinct_type;

            type_register(context, distinct_type);
            return distinct_type;
        }

        case Ast_Kind_Union_Type: {
            AstUnionType* union_ = (AstUnionType *) type_node;
            if (union_->utcache) return union_->utcache;
            if (union_->pending_type && union_->pending_type_is_valid) return union_->pending_type;

            Type *u_type;
            if (union_->pending_type == NULL) {
                u_type = type_create(context, Type_Kind_Union, 0);
                union_->pending_type = u_type;

                u_type->ast_type = type_node;
                u_type->Union.name = union_->name;
                u_type->Union.meta_tags = union_->meta_tags;
                u_type->Union.constructed_from = NULL;
                u_type->Union.status = SPS_Start;
                u_type->Union.scope = union_->scope;
                type_register(context, u_type);

                u_type->Union.variants = NULL;
                u_type->Union.variants_ordered = NULL;
                sh_new_arena(u_type->Union.variants);
                bh_arr_new(context->gp_alloc, u_type->Union.variants_ordered, bh_arr_length(union_->variants));
            } else {
                u_type = union_->pending_type;
            }

            union_->pending_type_is_valid = 1;

            //
            // All variants need to have type know.
            bh_arr_each(AstUnionVariant *, pvariant, union_->variants) {
                AstUnionVariant *variant = *pvariant;
                if (!variant->type) {
                    variant->type = type_build_from_ast_inner(context, variant->type_node, 1);
                }

                if (!variant->type) {
                    if (context->cycle_detected) {
                        ONYX_ERROR(variant->token->pos, Error_Critical, "Unable to figure out the type of this union variant.");
                    }

                    union_->pending_type_is_valid = 0;
                    return accept_partial_types ? union_->pending_type : NULL;
                }

                if (!type_is_ready_to_be_used_in_construction(variant->type)) {
                    union_->pending_type_is_valid = 0;
                    return accept_partial_types ? union_->pending_type : NULL;
                }
            }

            u_type->Union.status = SPS_Members_Done;

            // From this point forward, there is no chance we will return early
            // in a yielding fashion. Everything is either straight success or
            // failure.

            u32 size = 0;
            u32 alignment = 0;
            u32 next_tag_value = 0;

            assert(union_->tag_backing_type);

            AstEnumType* tag_enum_node = onyx_ast_node_new(context->ast_alloc, sizeof(AstEnumType), Ast_Kind_Enum_Type);
            tag_enum_node->token = union_->token;
            tag_enum_node->name = bh_aprintf(context->ast_alloc, "%s.tag_enum", union_->name);
            tag_enum_node->backing_type = type_build_from_ast(context, union_->tag_backing_type);
            bh_arr_new(context->ast_alloc, tag_enum_node->values, bh_arr_length(union_->variants));

            add_entities_for_node(&context->entities, NULL, (AstNode *) tag_enum_node, union_->entity->scope, union_->entity->package);

            //
            // Create variant instances
            bh_arr_each(AstUnionVariant *, pvariant, union_->variants) {
                AstUnionVariant *variant = *pvariant;
                assert(variant->type);

                u32 var_alignment = type_alignment_of(variant->type);
                if (var_alignment <= 0) {
                    ONYX_ERROR(variant->token->pos, Error_Critical, "Invalid variant type '%s', has alignment %d", type_get_name(context, variant->type), var_alignment);
                    return NULL;
                }

                if (var_alignment > alignment) alignment = var_alignment;

                token_toggle_end(variant->token);
                if (shgeti(u_type->Union.variants, variant->token->text) != -1) {
                    ONYX_ERROR(variant->token->pos, Error_Critical, "Duplicate union variant, '%s'.", variant->token->text);
                    token_toggle_end(variant->token);
                    return NULL;
                }

                u32 type_size = type_size_of(variant->type);
                size = bh_max(size, type_size);

                UnionVariant* uv = bh_alloc_item(context->ast_alloc, UnionVariant);
                uv->name = bh_strdup(context->ast_alloc, variant->token->text);
                uv->token = variant->token;
                uv->meta_tags = variant->meta_tags;
                uv->type = variant->type;

                if (variant->explicit_tag_value) {
                    b32 success;
                    uv->tag_value = get_expression_integer_value(context, variant->explicit_tag_value, &success);
                    next_tag_value = uv->tag_value + 1;

                    if (!success) {
                        ONYX_ERROR(variant->token->pos, Error_Critical, "Expected a compile-time known integer for explicit value of variant.");
                        return NULL;
                    }
                } else {
                    uv->tag_value = next_tag_value++;
                }

                shput(u_type->Union.variants, variant->token->text, uv);
                token_toggle_end(variant->token);

                bh_arr_push(u_type->Union.variants_ordered, uv);

                AstEnumValue *ev = onyx_ast_node_new(context->ast_alloc, sizeof(AstEnumValue), Ast_Kind_Enum_Value);
                ev->token = uv->token;
                ev->value = (AstTyped *) make_int_literal(context, uv->tag_value);
                bh_arr_push(tag_enum_node->values, ev);
            }

            alignment = bh_max(alignment, type_alignment_of(tag_enum_node->backing_type));
            bh_align(size, alignment);

            u_type->Union.alignment = alignment;
            u_type->Union.size = size + alignment; // Add the size of the tag
            u_type->Union.tag_type = type_build_from_ast(context, (AstType *) tag_enum_node);
            u_type->Union.status = SPS_Uses_Done;

            return u_type;
        }

        default: break;
    }

    return NULL;
}

// If this function returns NULL, then the caller MUST yield because the type may still be constructed in the future.
// If there was an error constructing the type, then this function will report that directly.
Type *type_build_from_ast(Context *context, AstType* type_node) {
    return type_build_from_ast_inner(context, type_node, 0);
}

// CLEANUP: This needs to be merged with the very similar code from up above.
Type* type_build_function_type(Context *context, AstFunction* func) {
    u64 param_count = bh_arr_length(func->params);

    Type* return_type = type_build_from_ast(context, func->return_type);
    if (return_type == NULL) return NULL;

    Type* func_type = type_create(context, Type_Kind_Function, param_count);
    func_type->Function.param_count = param_count;
    func_type->Function.needed_param_count = 0;
    func_type->Function.vararg_arg_pos = -1;
    func_type->Function.return_type = return_type;

    if (param_count > 0) {
        i32 i = 0;
        bh_arr_each(AstParam, param, func->params) {
            if (param->default_value == NULL && param->vararg_kind == VA_Kind_Not_VA)
                func_type->Function.needed_param_count++;

            if (param->vararg_kind == VA_Kind_Untyped)
                func_type->Function.vararg_arg_pos = i;

            func_type->Function.params[i++] = param->local->type;
        }
    }

    // CopyPaste from above in type_build_from_ast
    char* name = (char *) type_get_unique_name(context, func_type);
    if (func_type->Function.return_type != context->types.auto_return) {
        i32 index = shgeti(context->types.func_map, name);
        if (index != -1) {
            u64 id = context->types.func_map[index].value;
            Type* existing_type = (Type *) bh_imap_get(&context->types.type_map, id);

            // LEAK LEAK LEAK the func_type that is created
            return existing_type;
        }
    }

    type_register(context, func_type);
    shput(context->types.func_map, name, func_type->id);

    return func_type;
}

Type* type_build_compound_type(Context *context, AstCompound* compound) {
    i64 expr_count = bh_arr_length(compound->exprs);
    fori (i, 0, expr_count) {
        if (compound->exprs[i]->type == NULL) return NULL;
        if (compound->exprs[i]->type->kind == Type_Kind_Basic) {
            if (compound->exprs[i]->type->Basic.kind == Basic_Kind_Int_Unsized || compound->exprs[i]->type->Basic.kind == Basic_Kind_Float_Unsized) {
                return NULL;
            }
        }
    }

    Type* comp_type = type_create(context, Type_Kind_Compound, expr_count);
    comp_type->Compound.size = 0;
    comp_type->Compound.count = expr_count;

    fori (i, 0, expr_count) {
        assert(compound->exprs[i]->type != NULL);
        comp_type->Compound.types[i] = compound->exprs[i]->type;
        comp_type->Compound.size += bh_max(type_size_of(comp_type->Compound.types[i]), 4);
    }

    bh_align(comp_type->Compound.size, 4);

    comp_type->Compound.linear_members = NULL;
    bh_arr_new(context->gp_alloc, comp_type->Compound.linear_members, comp_type->Compound.count);
    build_linear_types_with_offset(context, comp_type, &comp_type->Compound.linear_members, 0);

    type_register(context, comp_type);
    return comp_type;
}

Type* type_build_implicit_type_of_struct_literal(Context *context, AstStructLiteral* lit, b32 is_query) {
    if (lit->generated_inferred_type) {
        return lit->generated_inferred_type;
    }

    Type* type = type_create(context, Type_Kind_Struct, 0);
    type->ast_type = NULL;
    type->Struct.name = NULL;
    type->Struct.mem_count = bh_arr_length(lit->args.named_values);
    type->Struct.meta_tags = NULL;
    type->Struct.constructed_from = NULL;
    type->Struct.status = SPS_Start;
    type->Struct.poly_sln = NULL;
    type_register(context, type);

    type->Struct.memarr = NULL;
    sh_new_arena(type->Struct.members);
    bh_arr_new(context->gp_alloc, type->Struct.memarr, type->Struct.mem_count);

    u32 size = 0;
    u32 offset = 0;
    u32 alignment = 1;
    u32 idx = 0;
    bh_arr_each(AstNamedValue *, pnv, lit->args.named_values) {
        AstNamedValue *nv = *pnv;

        Type* member_type = resolve_expression_type(context, nv->value);
        if (member_type == NULL) {
            if (!is_query) {
                ONYX_ERROR(nv->value->token->pos, Error_Critical, "Unable to resolve type of this member when trying to construct an inferred type of the structure literal.");
            }

            return NULL;
        }

        u32 mem_alignment = type_alignment_of(member_type);
        if (mem_alignment <= 0) {
            return NULL;
        }

        alignment = bh_max(alignment, mem_alignment);

        // Should these structs be packed or not?
        bh_align(offset, mem_alignment);

        token_toggle_end(nv->token);
        if (shgeti(type->Struct.members, nv->token->text) != -1) {
            token_toggle_end(nv->token);
            return NULL;
        }

        StructMember *smem = bh_alloc_item(context->ast_alloc, StructMember);
        smem->offset = offset;
        smem->type = member_type;
        smem->idx = idx;
        smem->name = bh_strdup(context->ast_alloc, nv->token->text);
        smem->token = nv->token;
        smem->meta_tags = NULL;
        smem->included_through_use = 0;
        smem->used = 0;
        smem->use_through_pointer_index = -1;
        smem->member_node = NULL;

        // Having this present caused more issues than its
        // worth. I don't think this is necessary, and it allows
        // you to access variables outside of where they are
        // defined.
        // smem->initial_value = &nv->value;
        smem->initial_value = NULL;

        shput(type->Struct.members, nv->token->text, smem);
        bh_arr_push(type->Struct.memarr, smem);
        token_toggle_end(nv->token);

        u32 type_size = type_size_of(member_type);
        offset += type_size;
        size = offset;
        idx++;
    }

    type->Struct.alignment = alignment;
    type->Struct.size = size;

    type->Struct.status = SPS_Uses_Done;
    lit->generated_inferred_type = type;
    return type;
}

Type* type_make_pointer(Context *context, Type* to) {
    if (to == NULL) return NULL;
    if (to == (Type *) &context->node_that_signals_failure) return to;

    assert(to->id > 0);
    u64 ptr_id = bh_imap_get(&context->types.pointer_map, to->id);
    if (ptr_id > 0) {
        Type* ptr_type = (Type *) bh_imap_get(&context->types.type_map, ptr_id);
        return ptr_type;

    } else {
        Type* ptr_type = type_create(context, Type_Kind_Pointer, 0);
        ptr_type->Pointer.base.flags |= Basic_Flag_Pointer;
        ptr_type->Pointer.base.size = POINTER_SIZE;
        ptr_type->Pointer.elem = to;

        type_register(context, ptr_type);
        bh_imap_put(&context->types.pointer_map, to->id, ptr_type->id);

        return ptr_type;
    }
}

Type* type_make_multi_pointer(Context *context, Type* to) {
    if (to == NULL) return NULL;
    if (to == (Type *) &context->node_that_signals_failure) return to;

    assert(to->id > 0);
    u64 ptr_id = bh_imap_get(&context->types.multi_pointer_map, to->id);
    if (ptr_id > 0) {
        Type* ptr_type = (Type *) bh_imap_get(&context->types.type_map, ptr_id);
        return ptr_type;

    } else {
        Type* ptr_type = type_create(context, Type_Kind_MultiPointer, 0);
        ptr_type->MultiPointer.base.flags |= Basic_Flag_Pointer;
        ptr_type->MultiPointer.base.flags |= Basic_Flag_Multi_Pointer;
        ptr_type->MultiPointer.base.size = POINTER_SIZE;
        ptr_type->MultiPointer.elem = to;

        type_register(context, ptr_type);
        bh_imap_put(&context->types.multi_pointer_map, to->id, ptr_type->id);

        return ptr_type;
    }
}

Type* type_make_array(Context *context, Type* to, u32 count) {
    if (to == NULL) return NULL;
    if (to == (Type *) &context->node_that_signals_failure) return to;

    assert(to->id > 0);
    u64 key = ((((u64) to->id) << 32) | (u64) count);
    u64 array_id = bh_imap_get(&context->types.array_map, key);
    if (array_id > 0) {
        Type* array_type = (Type *) bh_imap_get(&context->types.type_map, array_id);
        return array_type;

    } else {
        Type* arr_type = type_create(context, Type_Kind_Array, 0);
        arr_type->Array.count = count;
        arr_type->Array.elem = to;
        arr_type->Array.size = count * type_size_of(to);

        type_register(context, arr_type);
        bh_imap_put(&context->types.array_map, key, arr_type->id);

        return arr_type;
    }
}

Type* type_make_slice(Context *context, Type* of) {
    if (of == NULL) return NULL;
    if (of == (Type *) &context->node_that_signals_failure) return of;

    assert(of->id > 0);
    u64 slice_id = bh_imap_get(&context->types.slice_map, of->id);
    if (slice_id > 0) {
        Type* slice_type = (Type *) bh_imap_get(&context->types.type_map, slice_id);
        return slice_type;

    } else {
        Type* slice_type = type_create(context, Type_Kind_Slice, 0);
        type_register(context, slice_type);
        bh_imap_put(&context->types.slice_map, of->id, slice_type->id);

        type_make_multi_pointer(context, of);
        slice_type->Slice.elem = of;

        AstPolyStructType* pslice_type = (AstPolyStructType *) context->builtins.slice_type;
        OnyxFilePos pos = { 0 };
        slice_type->Slice.scope = scope_create(context, pslice_type->scope, pos);

        return slice_type;
    }
}

Type* type_make_dynarray(Context *context, Type* of) {
    if (of == NULL) return NULL;
    if (of == (Type *) &context->node_that_signals_failure) return of;

    assert(of->id > 0);
    u64 dynarr_id = bh_imap_get(&context->types.dynarr_map, of->id);
    if (dynarr_id > 0) {
        Type* dynarr = (Type *) bh_imap_get(&context->types.type_map, dynarr_id);
        return dynarr;

    } else {
        Type* dynarr = type_create(context, Type_Kind_DynArray, 0);
        type_register(context, dynarr);
        bh_imap_put(&context->types.dynarr_map, of->id, dynarr->id);

        type_make_multi_pointer(context, of);
        dynarr->DynArray.elem = of;

        AstPolyStructType* dynarr_type = (AstPolyStructType *) context->builtins.array_type;
        OnyxFilePos pos = { 0 };
        dynarr->DynArray.scope = scope_create(context, dynarr_type->scope, pos);

        return dynarr;
    }
}

Type* type_make_varargs(Context *context, Type* of) {
    if (of == NULL) return NULL;
    if (of == (Type *) &context->node_that_signals_failure) return of;

    assert(of->id > 0);
    u64 vararg_id = bh_imap_get(&context->types.vararg_map, of->id);
    if (vararg_id > 0) {
        Type* va_type = (Type *) bh_imap_get(&context->types.type_map, vararg_id);
        return va_type;

    } else {
        Type* va_type = type_create(context, Type_Kind_VarArgs, 0);
        type_register(context, va_type);
        bh_imap_put(&context->types.vararg_map, of->id, va_type->id);

        type_make_multi_pointer(context, of);
        va_type->VarArgs.elem = of;

        return va_type;
    }
}

void build_linear_types_with_offset(Context *context, Type* type, bh_arr(TypeWithOffset)* pdest, u32 offset) {
    if (type->kind == Type_Kind_Compound) {
        u32 elem_offset = 0;
        fori (i, 0, type->Compound.count) {
            build_linear_types_with_offset(context, type->Compound.types[i], pdest, offset + elem_offset);
            elem_offset += bh_max(type_size_of(type->Compound.types[i]), 4);
        }

    } else if (type->kind == Type_Kind_Slice || type->kind == Type_Kind_VarArgs || type->kind == Type_Kind_Function) {
        u32 mem_count = type_structlike_mem_count(type);
        StructMember smem = { 0 };
        fori (i, 0, mem_count) {
            type_lookup_member_by_idx(context, type, i, &smem);
            build_linear_types_with_offset(context, smem.type, pdest, offset + smem.offset);
        }

    } else {
        bh_arr(TypeWithOffset) dest = *pdest;

        TypeWithOffset two;
        two.type = type;
        two.offset = offset;
        bh_arr_push(dest, two);

        *pdest = dest;
    }
}

b32 type_struct_member_apply_use(Context *context, Type *s_type, StructMember *smem) {
    Type* used_type = smem->type;

    b32 type_is_pointer = 0;
    if (used_type->kind == Type_Kind_Pointer) {
        type_is_pointer = 1;
        used_type = type_get_contained_type(used_type);
    }

    if (used_type->kind != Type_Kind_Struct) {
        ONYX_ERROR(smem->token->pos, Error_Critical, "Can only use things of structure, or pointer to structure type.");
        return 0;
    }

    if (used_type->Struct.status < SPS_Uses_Done) return 0;

    fori (i, 0, shlen(used_type->Struct.members)) {
        StructMember *nsmem = used_type->Struct.members[i].value;

        //
        // TODO: :Bugfix  Currently, nested use through pointers are not
        // working correctly and I need to rethink them entirely.
        if (nsmem->use_through_pointer_index >= 0) {
            continue;
        }

        if (shgeti(s_type->Struct.members, nsmem->name) != -1) {
            ONYX_ERROR(smem->token->pos, Error_Critical, "Used name '%s' conflicts with existing struct member.", nsmem->name);
            return 0;
        }

        StructMember* new_smem = bh_alloc_item(context->ast_alloc, StructMember);
        new_smem->type   = nsmem->type;
        new_smem->name   = nsmem->name;
        new_smem->meta_tags = nsmem->meta_tags;
        new_smem->used = nsmem->used;
        new_smem->included_through_use = 1;
        new_smem->member_node = nsmem->member_node;

        if (type_is_pointer) {
            new_smem->offset = nsmem->offset;
            new_smem->idx = nsmem->idx;
            new_smem->initial_value = NULL;
            new_smem->use_through_pointer_index = smem->idx;
        } else {
            new_smem->offset = smem->offset + nsmem->offset;
            new_smem->idx    = nsmem->idx; // Dummy value because I don't think this is needed.
            new_smem->initial_value = nsmem->initial_value;
            new_smem->use_through_pointer_index = -1;
        }

        shput(s_type->Struct.members, nsmem->name, new_smem);
    }

    return 1;
}

const char* type_get_unique_name(Context *context, Type* type) {
    if (type == NULL) return "unknown";

    switch (type->kind) {
        case Type_Kind_Basic: return type->Basic.name;
        case Type_Kind_Pointer: return bh_aprintf(context->scratch_alloc, "&%s", type_get_unique_name(context, type->Pointer.elem));
        case Type_Kind_MultiPointer: return bh_aprintf(context->scratch_alloc, "[&] %s", type_get_unique_name(context, type->Pointer.elem));
        case Type_Kind_Array: return bh_aprintf(context->scratch_alloc, "[%d] %s", type->Array.count, type_get_unique_name(context, type->Array.elem));
        case Type_Kind_Struct:
            if (type->Struct.name)
                return bh_aprintf(context->scratch_alloc, "%s@%l", type->Struct.name, type->id);
            else
                return bh_aprintf(context->scratch_alloc, "%s@%l", "<anonymous struct>", type->id);
        case Type_Kind_Enum:
            if (type->Enum.name)
                return bh_aprintf(context->scratch_alloc, "%s@%l", type->Enum.name, type->id);
            else
                return bh_aprintf(context->scratch_alloc, "%s@%l", "<anonymous enum>", type->id);
        case Type_Kind_Union:
            if (type->Union.name)
                return bh_aprintf(context->scratch_alloc, "%s@%l", type->Union.name, type->id);
            else
                return bh_aprintf(context->scratch_alloc, "%s@%l", "<anonymous union>", type->id);

        case Type_Kind_Slice: return bh_aprintf(context->scratch_alloc, "[] %s", type_get_unique_name(context, type->Slice.elem));
        case Type_Kind_VarArgs: return bh_aprintf(context->scratch_alloc, "..%s", type_get_unique_name(context, type->VarArgs.elem));
        case Type_Kind_DynArray: return bh_aprintf(context->scratch_alloc, "[..] %s", type_get_unique_name(context, type->DynArray.elem));

        case Type_Kind_Function: {
            char buf[1024];
            memset(buf, 0, 1024);

            strncat(buf, "(", 1023);
            fori (i, 0, type->Function.param_count) {
                strncat(buf, type_get_unique_name(context, type->Function.params[i]), 1023);

                if (i >= type->Function.needed_param_count)
                    strncat(buf, "?", 1023);

                if (i != type->Function.param_count - 1)
                    strncat(buf, ", ", 1023);
            }

            strncat(buf, ") -> ", 1023);
            strncat(buf, type_get_unique_name(context, type->Function.return_type), 1023);

            return bh_aprintf(context->scratch_alloc, "%s", buf);
        }

        case Type_Kind_Compound: {
            char buf[1024];
            memset(buf, 0, 1024);

            strncat(buf, "(", 1023);
            fori (i, 0, type->Compound.count) {
                strncat(buf, type_get_unique_name(context, type->Compound.types[i]), 1023);
                if (i != type->Compound.count - 1)
                    strncat(buf, ", ", 1023);
            }
            strncat(buf, ")", 1023);

            return bh_aprintf(context->scratch_alloc, "%s", buf);
        }

        case Type_Kind_Distinct: {
            return bh_aprintf(context->scratch_alloc, "%s@%l", type->Distinct.name, type->id);
        }

        default: return "unknown (not null)";
    }
}

const char* type_get_name(Context *context, Type* type) {
    if (type == NULL) return "unknown";

    switch (type->kind) {
        case Type_Kind_Basic: return type->Basic.name;
        case Type_Kind_Pointer: return bh_aprintf(context->scratch_alloc, "&%s", type_get_name(context, type->Pointer.elem));
        case Type_Kind_MultiPointer: return bh_aprintf(context->scratch_alloc, "[&] %s", type_get_name(context, type->Pointer.elem));
        case Type_Kind_Array: return bh_aprintf(context->scratch_alloc, "[%d] %s", type->Array.count, type_get_name(context, type->Array.elem));

        case Type_Kind_PolyStruct:
            return type->PolyStruct.name;

        case Type_Kind_PolyUnion:
            return type->PolyUnion.name;

        case Type_Kind_Struct:
            if (type->Struct.name)
                return type->Struct.name;
            else
                return "<anonymous struct>";

        case Type_Kind_Enum:
            if (type->Enum.name)
                return type->Enum.name;
            else
                return "<anonymous enum>";

        case Type_Kind_Union:
            if (type->Union.name)
                return type->Union.name;
            else
                return "<anonymous union>";

        case Type_Kind_Slice: return bh_aprintf(context->scratch_alloc, "[] %s", type_get_name(context, type->Slice.elem));
        case Type_Kind_VarArgs: return bh_aprintf(context->scratch_alloc, "..%s", type_get_name(context, type->VarArgs.elem));
        case Type_Kind_DynArray: return bh_aprintf(context->scratch_alloc, "[..] %s", type_get_name(context, type->DynArray.elem));

        case Type_Kind_Function: {
            char buf[512];
            fori (i, 0, 512) buf[i] = 0;

            strncat(buf, "(", 511);
            fori (i, 0, type->Function.param_count) {
                strncat(buf, type_get_name(context, type->Function.params[i]), 511);
                if (i != type->Function.param_count - 1)
                    strncat(buf, ", ", 511);
            }

            strncat(buf, ") -> ", 511);
            strncat(buf, type_get_name(context, type->Function.return_type), 511);

            return bh_aprintf(context->scratch_alloc, "%s", buf);
        }

        case Type_Kind_Compound: {
            char buf[512];
            fori (i, 0, 512) buf[i] = 0;

            strncat(buf, "(", 511);
            fori (i, 0, type->Compound.count) {
                strncat(buf, type_get_name(context, type->Compound.types[i]), 511);
                if (i != type->Compound.count - 1)
                    strncat(buf, ", ", 511);
            }
            strncat(buf, ")", 511);

            return bh_aprintf(context->scratch_alloc, "%s", buf);
        }

        case Type_Kind_Distinct: {
            return bh_aprintf(context->scratch_alloc, "%s", type->Distinct.name);
        }

        default: return "unknown";
    }
}

u32 type_get_alignment_log2(Type* type) {
    i32 store_size = type_alignment_of(type);
    if      (store_size == 1) return 0;
    else if (store_size == 2) return 1;
    else if (store_size == 4) return 2;
    else if (store_size == 8) return 3;
    else if (store_size == 16) return 4;
    return 2;
}

Type* type_get_contained_type(Type* type) {
    if (type == NULL) return NULL;
    switch (type->kind) {
        case Type_Kind_Pointer:      return type->Pointer.elem;
        case Type_Kind_MultiPointer: return type->MultiPointer.elem;
        case Type_Kind_Array:        return type->Array.elem;
        case Type_Kind_Slice:        return type->Slice.elem;
        case Type_Kind_DynArray:     return type->DynArray.elem;
        case Type_Kind_VarArgs:      return type->VarArgs.elem;
        default: return NULL;
    }
}

b32 type_is_ready_for_lookup(Type* type) {
    if (type->kind == Type_Kind_Pointer) type = type->Pointer.elem;

    if (type->kind == Type_Kind_Struct) {
        return type->Struct.status == SPS_Uses_Done;
    }

    return 1;
}

static const StructMember slice_members[] = {
    { 0,            0, NULL, "data",   NULL, NULL, -1, 0, 0 },
    { POINTER_SIZE, 1, NULL, "count",  NULL, NULL, -1, 0, 0 },
    { POINTER_SIZE, 1, NULL, "size",   NULL, NULL, -1, 0, 0 },
    { POINTER_SIZE, 1, NULL, "length", NULL, NULL, -1, 0, 0 },
};

static const StructMember array_members[] = {
    { 0,                0, NULL, "data",      NULL, NULL, -1, 0, 0 },
    { POINTER_SIZE,     1, NULL, "count",     NULL, NULL, -1, 0, 0 },
    { POINTER_SIZE + 4, 2, NULL, "capacity",  NULL, NULL, -1, 0, 0 },
    { POINTER_SIZE + 8, 3, NULL, "allocator", NULL, NULL, -1, 0, 0 },
    { POINTER_SIZE,     1, NULL, "size",      NULL, NULL, -1, 0, 0 },
    { POINTER_SIZE,     1, NULL, "length",    NULL, NULL, -1, 0, 0 },
};

static const StructMember func_members[] = {
    { 0,                0, NULL, "__funcidx",    NULL, NULL, -1, 0, 0 },
    { POINTER_SIZE,     1, NULL, "closure",      NULL, NULL, -1, 0, 0 },
};

static const StructMember union_members[] = {
    { 0, 0, NULL, "tag", NULL, NULL, -1, 0, 0 },
};

b32 type_lookup_member(Context *context, Type* type, char* member, StructMember* smem) {
    if (type->kind == Type_Kind_Pointer) type = type->Pointer.elem;

    switch (type->kind) {
        case Type_Kind_Struct: {
            TypeStruct* stype = &type->Struct;

            i32 index = shgeti(stype->members, member);
            if (index == -1) return 0;
            *smem = *stype->members[index].value;
            return 1;
        }

        case Type_Kind_VarArgs:
        case Type_Kind_Slice: {
            fori (i, 0, (i64) (sizeof(slice_members) / sizeof(StructMember))) {
                if (strcmp(slice_members[i].name, member) == 0) {
                    *smem = slice_members[i];
                    if (smem->idx == 0) smem->type = type_make_multi_pointer(context, type->Slice.elem);
                    else                smem->type = context->types.basic[Basic_Kind_U32];

                    return 1;
                }
            }
            return 0;
        }

        case Type_Kind_DynArray: {
            fori (i, 0, (i64) (sizeof(array_members) / sizeof(StructMember))) {
                if (strcmp(array_members[i].name, member) == 0) {
                    *smem = array_members[i];
                    if      (smem->idx == 0) smem->type = type_make_multi_pointer(context, type->DynArray.elem);
                    else if (smem->idx == 3) smem->type = type_build_from_ast(context, context->builtins.allocator_type);
                    else                     smem->type = context->types.basic[Basic_Kind_U32];

                    return 1;
                }
            }
            return 0;
        }

        case Type_Kind_Function: {
            fori (i, 0, (i64) (sizeof(func_members) / sizeof(StructMember))) {
                if (strcmp(func_members[i].name, member) == 0) {
                    *smem = func_members[i];
                    if (smem->idx == 0) smem->type = context->types.basic[Basic_Kind_U32];
                    if (smem->idx == 1) smem->type = context->types.basic[Basic_Kind_Rawptr];

                    return 1;
                }
            }
        }

        case Type_Kind_Union: {
            if (!strcmp(member, "tag")) {
                *smem = union_members[0];
                smem->type = type->Union.tag_type;
                return 1;
            }

            return 0;
        }

        default: return 0;
    }
}

b32 type_lookup_member_by_idx(Context *context, Type* type, i32 idx, StructMember* smem) {
    while (type->kind == Type_Kind_Distinct) type = type->Distinct.base_type;

    if (type->kind == Type_Kind_Pointer) type = type->Pointer.elem;

    switch (type->kind) {
        case Type_Kind_Struct: {
            TypeStruct* stype = &type->Struct;

            if (idx > stype->mem_count) return 0;
            *smem = *stype->memarr[idx];
            return 1;
        }

        // HACK: This relies on the fact that the structures for Slice and VarArgs
        // are identical.                       - brendanfh   2020/09/07
        case Type_Kind_VarArgs:
        case Type_Kind_Slice: {
            if (idx > 2) return 0;

            *smem = slice_members[idx];
            if (smem->idx == 0) smem->type = type_make_multi_pointer(context, type->Slice.elem);
            else                smem->type = context->types.basic[Basic_Kind_U32];

            return 1;
        }

        case Type_Kind_DynArray: {
            if (idx > 4) return 0;

            *smem = array_members[idx];
            if      (idx == 0) smem->type = type_make_multi_pointer(context, type->DynArray.elem);
            else if (idx == 3) smem->type = type_build_from_ast(context, context->builtins.allocator_type);
            else               smem->type = context->types.basic[Basic_Kind_U32];

            return 1;
        }

        case Type_Kind_Function: {
            if (idx > 1) return 0;

            *smem = func_members[idx];
            if (idx == 0) smem->type = context->types.basic[Basic_Kind_U32];
            if (idx == 1) smem->type = context->types.basic[Basic_Kind_Rawptr];
            
            return 1;
        }

        case Type_Kind_Union: {
            if (idx > 2) return 0;

            if (idx == 0) {
                smem->type = type->Union.tag_type;
                smem->offset = 0;
            }

            if (idx == 1) {
                smem->type = NULL;
                smem->offset = type->Union.alignment;
            }

            return 1;
        }

        default: return 0;
    }
}

i32 type_linear_member_count(Type* type) {
    if (!type) return 0;
    switch (type->kind) {
        case Type_Kind_Slice:
        case Type_Kind_VarArgs:  return 2;
        case Type_Kind_Function: return 2;
        case Type_Kind_Compound: return bh_arr_length(type->Compound.linear_members);
        case Type_Kind_Distinct: return type_linear_member_count(type->Distinct.base_type);
        default: return 1;
    }
}

b32 type_linear_member_lookup(Context *context, Type* type, i32 idx, TypeWithOffset* two) {
    while (type->kind == Type_Kind_Distinct) type = type->Distinct.base_type;

    switch (type->kind) {
        case Type_Kind_Slice:
        case Type_Kind_VarArgs: {
            if (idx == 0) {
                two->type = type_make_multi_pointer(context, type->Slice.elem);
                two->offset = 0;
            }
            if (idx == 1) {
                two->type = context->types.basic[Basic_Kind_U32];
                two->offset = POINTER_SIZE;
            }

            return 1;
        }
        case Type_Kind_DynArray: {
            if (idx == 0) {
                two->type = type_make_multi_pointer(context, type->DynArray.elem);
                two->offset = 0;
            }
            if (idx == 1) {
                two->type = context->types.basic[Basic_Kind_U32];
                two->offset = POINTER_SIZE;
            }
            if (idx == 2) {
                two->type = context->types.basic[Basic_Kind_U32];
                two->offset = POINTER_SIZE + 4;
            }
            if (idx == 3 || idx == 4) {
                Type* allocator_type = type_build_from_ast(context, context->builtins.allocator_type);
                type_linear_member_lookup(context, allocator_type, idx - 3, two);
                two->offset += POINTER_SIZE + 8;
            }

            return 1;
        }
        case Type_Kind_Compound: *two = type->Compound.linear_members[idx]; return 1;

        case Type_Kind_Function:
            if (idx == 0) {
                two->type = context->types.basic[Basic_Kind_U32];
                two->offset = 0;
            }
            if (idx == 1) {
                two->type = context->types.basic[Basic_Kind_Rawptr];
                two->offset = POINTER_SIZE;
            }
            return 1;

        default: {
            if (idx > 0) return 0;
            two->offset = 0;
            two->type = type;
            return 1;
        }
    }
}

i32 type_get_idx_of_linear_member_with_offset(Type* type, u32 offset) {
    while (type->kind == Type_Kind_Distinct) type = type->Distinct.base_type;

    switch (type->kind) {
        case Type_Kind_Slice:
        case Type_Kind_VarArgs: {
            if (offset == 0) return 0;
            if (offset == POINTER_SIZE) return 1;
            return -1;
        }
        case Type_Kind_DynArray: {
            if (offset == 0)                    return 0;
            if (offset == POINTER_SIZE)         return 1;
            if (offset == POINTER_SIZE + 4)     return 2;
            if (offset == POINTER_SIZE + 8)     return 3;
            if (offset == POINTER_SIZE * 2 + 8) return 4;
            return -1;
        }
        case Type_Kind_Compound: {
            i32 idx = 0;
            bh_arr_each(TypeWithOffset, two, type->Compound.linear_members) {
                if (two->offset == offset) return idx;
                idx++;
            }

            return -1;
        }
        case Type_Kind_Function: {
            if (offset == 0) return 0;
            if (offset == POINTER_SIZE) return 1;
            return -1;
        }
        default:
            if (offset == 0) return 0;
            return -1;
    }
}

b32 type_is_pointer(Type* type) {
    if (type == NULL) return 0;
    return type->kind == Type_Kind_Pointer;
}

b32 type_is_multi_pointer(Type* type) {
    if (type == NULL) return 0;
    return type->kind == Type_Kind_MultiPointer;
}

b32 type_is_rawptr(Type* type) {
    if (type == NULL) return 0;
    return type->kind == Type_Kind_Basic && type->Basic.kind == Basic_Kind_Rawptr;
}

b32 type_is_array(Type* type) {
    if (type == NULL) return 0;
    return type->kind == Type_Kind_Array;
}

b32 type_is_struct(Type* type) {
    if (type == NULL) return 0;
    if (type->kind == Type_Kind_Struct) return 1;
    if (type->kind == Type_Kind_Pointer)
        if (type->Pointer.elem != NULL && type->Pointer.elem->kind == Type_Kind_Struct) return 1;
    return 0;
}

b32 type_is_bool(Type* type) {
    if (type == NULL) return 0;
    return type != NULL && type->kind == Type_Kind_Basic && type->Basic.kind == Basic_Kind_Bool;
}

b32 type_is_small_integer(Type* type) {
    if (type == NULL) return 0;
    if (type->kind == Type_Kind_Enum) return type_is_small_integer(type->Enum.backing);
    if (type->kind == Type_Kind_Distinct) return type_is_small_integer(type->Distinct.base_type);
    if (type->kind != Type_Kind_Basic) return 0;

    return type->Basic.kind >= Basic_Kind_I8 && type->Basic.kind <= Basic_Kind_U32;
}

b32 type_is_integer(Type* type) {
    if (type == NULL) return 0;
    if (type->kind == Type_Kind_Enum) return type_is_integer(type->Enum.backing);
    if (type->kind == Type_Kind_Distinct) return type_is_integer(type->Distinct.base_type);
    if (type->kind != Type_Kind_Basic) return 0;

    return (type->Basic.kind >= Basic_Kind_I8 && type->Basic.kind <= Basic_Kind_U64)
        || type->Basic.kind == Basic_Kind_Type_Index;
}

b32 type_is_numeric(Type* type) {
    if (type == NULL) return 0;
    if (type->kind == Type_Kind_Enum) return 1;
    if (type->kind == Type_Kind_Distinct) return type_is_numeric(type->Distinct.base_type);
    if (type->kind != Type_Kind_Basic) return 0;

    return type->Basic.kind >= Basic_Kind_Int_Unsized && type->Basic.kind <= Basic_Kind_F64;
}

b32 type_is_compound(Type* type) {
    return type_linear_member_count(type) > 1;
}

b32 type_is_simd(Type* type) {
    if (type == NULL) return 0;
    if (type->kind != Type_Kind_Basic) return 0;
    return type->Basic.flags & Basic_Flag_SIMD;
}

b32 type_results_in_void(Type* type) {
    return (type == NULL)
        || (type->kind == Type_Kind_Basic && type->Basic.kind == Basic_Kind_Void);
}

b32 type_is_array_accessible(Type* type) {
    if (type == NULL) return 0;
    // if (type_is_pointer(type)) return 1;
    if (type_is_multi_pointer(type)) return 1;
    if (type->kind == Type_Kind_Array) return 1;
    if (type->kind == Type_Kind_Slice) return 1;
    if (type->kind == Type_Kind_DynArray) return 1;
    if (type->kind == Type_Kind_VarArgs) return 1;
    return 0;
}

b32 type_is_structlike(Type* type) {
    if (type == NULL) return 0;
    if (type->kind == Type_Kind_Array) return 1;
    if (type->kind == Type_Kind_Struct) return 1;
    if (type->kind == Type_Kind_Slice)  return 1;
    if (type->kind == Type_Kind_Function) return 1;
    if (type->kind == Type_Kind_DynArray) return 1;
    if (type->kind == Type_Kind_VarArgs) return 1;
    if (type->kind == Type_Kind_Union) return 1;
    if (type->kind == Type_Kind_Pointer) {
        if (type->Pointer.elem->kind == Type_Kind_Struct) return 1;
        if (type->Pointer.elem->kind == Type_Kind_Slice)  return 1;
        if (type->Pointer.elem->kind == Type_Kind_DynArray) return 1;
        if (type->Pointer.elem->kind == Type_Kind_Union) return 1;
    }
    if (type->kind == Type_Kind_Distinct) {
        return type_is_structlike(type->Distinct.base_type);
    }
    return 0;
}

b32 type_is_structlike_strict(Type* type) {
    if (type == NULL) return 0;
    if (type->kind == Type_Kind_Struct)   return 1;
    if (type->kind == Type_Kind_Slice)    return 1;
    if (type->kind == Type_Kind_DynArray) return 1;
    if (type->kind == Type_Kind_Function) return 1;
    if (type->kind == Type_Kind_VarArgs)  return 1;
    if (type->kind == Type_Kind_Union)    return 1;
    return 0;
}

b32 type_should_be_passed_like_a_struct(Type *type) {
    if (type == NULL) return 0;
    if (type->kind == Type_Kind_Struct)   return 1;
    if (type->kind == Type_Kind_Array)    return 1;
    if (type->kind == Type_Kind_Slice)    return 1;
    if (type->kind == Type_Kind_DynArray) return 1;
    if (type->kind == Type_Kind_Function) return 1;
    if (type->kind == Type_Kind_VarArgs)  return 1;
    if (type->kind == Type_Kind_Union)    return 1;
    if (type->kind == Type_Kind_Distinct) return type_should_be_passed_like_a_struct(type->Distinct.base_type);
    return 0;
}

u32 type_structlike_mem_count(Type* type) {
    if (type == NULL) return 0;
    switch (type->kind) {
        case Type_Kind_Struct:   return type->Struct.mem_count;
        case Type_Kind_Slice:    return 2;
        case Type_Kind_VarArgs:  return 2;
        case Type_Kind_Function: return 2;
        case Type_Kind_DynArray: return 4;
        case Type_Kind_Distinct: return type_structlike_mem_count(type->Distinct.base_type);
        case Type_Kind_Union:    return 2;
        default: return 0;
    }
}

u32 type_structlike_is_simple(Type* type) {
    if (type == NULL) return 0;
    switch (type->kind) {
        case Type_Kind_Slice:    return 1;
        case Type_Kind_VarArgs:  return 1;
        case Type_Kind_Function: return 1;
        case Type_Kind_Distinct: return type_structlike_is_simple(type->Distinct.base_type);
        default: return 0;
    }
}

b32 type_is_sl_constructable(Type* type) {
    if (type == NULL) return 0;
    switch (type->kind) {
        case Type_Kind_Struct:   return 1;
        case Type_Kind_Slice:    return 1;
        case Type_Kind_DynArray: return 1;
        case Type_Kind_Function: return 1;
        case Type_Kind_Union:    return 1;
        case Type_Kind_Array:    return 1;
        default: return 0;
    }
}

b32 type_constructed_from_poly(Type* base, struct AstType* from) {
    if (base == NULL) return 0;
    if (base->kind == Type_Kind_Struct) {
        return base->Struct.constructed_from == from;
    }

    if (base->kind == Type_Kind_Union) {
        return base->Union.constructed_from == from;
    }

    return 0;
}

Type* type_struct_is_just_one_basic_value(Type *type) {
    if (!type)                                                 return NULL;
    if (type->kind != Type_Kind_Struct)                        return NULL;
    if (bh_arr_length(type->Struct.memarr) != 1)               return NULL;
    if (type->Struct.memarr[0]->type->kind != Type_Kind_Basic) return NULL;
    return type->Struct.memarr[0]->type;
}

u32 type_union_get_variant_count(Type *type) {
    if (!type) return 0;
    switch (type->kind) {
        case Type_Kind_Union: return bh_arr_length(type->Union.variants_ordered);
        case Type_Kind_Pointer: return type_union_get_variant_count(type->Pointer.elem);
        default: return 0;
    }
}

UnionVariant* type_lookup_union_variant_by_idx(Type* type, i32 idx) {
    if (!type) return NULL;
    if (type->kind == Type_Kind_Pointer) type = type->Pointer.elem;
    if (type->kind != Type_Kind_Union) return NULL;
    if (idx < 0 || idx >= bh_arr_length(type->Union.variants_ordered)) return NULL;

    return type->Union.variants_ordered[idx];
}

UnionVariant* type_lookup_union_variant_by_name(Type* type, char *name) {
    if (!type) return NULL;
    if (type->kind == Type_Kind_Pointer) type = type->Pointer.elem;
    if (type->kind != Type_Kind_Union) return NULL;

    i32 index = shgeti(type->Union.variants, name);
    if (index == -1) return NULL;
    return type->Union.variants[index].value;
}

