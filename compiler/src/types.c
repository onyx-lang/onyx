#define BH_DEBUG
#include "stb_ds.h"
#include "types.h"
#include "astnodes.h"
#include "utils.h"
#include "errors.h"

// NOTE: These have to be in the same order as Basic
Type basic_types[] = {
    { Type_Kind_Basic, 0, 0, (AstType *) &basic_type_void, { Basic_Kind_Void,                    0,                       0,  1, "void"   } },

    { Type_Kind_Basic, 0, 0, (AstType *) &basic_type_bool, { Basic_Kind_Bool,   Basic_Flag_Boolean,                       1,  1, "bool"   } },

    { Type_Kind_Basic, 0, 0, NULL,                        { Basic_Kind_Int_Unsized, Basic_Flag_Integer,                  0,  0, "unsized int" } },
    { Type_Kind_Basic, 0, 0, (AstType *) &basic_type_i8,  { Basic_Kind_I8,     Basic_Flag_Integer,                       1,  1, "i8"     } },
    { Type_Kind_Basic, 0, 0, (AstType *) &basic_type_u8,  { Basic_Kind_U8,     Basic_Flag_Integer | Basic_Flag_Unsigned, 1,  1, "u8"     } },
    { Type_Kind_Basic, 0, 0, (AstType *) &basic_type_i16, { Basic_Kind_I16,    Basic_Flag_Integer,                       2,  2, "i16"    } },
    { Type_Kind_Basic, 0, 0, (AstType *) &basic_type_u16, { Basic_Kind_U16,    Basic_Flag_Integer | Basic_Flag_Unsigned, 2,  2, "u16"    } },
    { Type_Kind_Basic, 0, 0, (AstType *) &basic_type_i32, { Basic_Kind_I32,    Basic_Flag_Integer,                       4,  4, "i32"    } },
    { Type_Kind_Basic, 0, 0, (AstType *) &basic_type_u32, { Basic_Kind_U32,    Basic_Flag_Integer | Basic_Flag_Unsigned, 4,  4, "u32"    } },
    { Type_Kind_Basic, 0, 0, (AstType *) &basic_type_i64, { Basic_Kind_I64,    Basic_Flag_Integer,                       8,  8, "i64"    } },
    { Type_Kind_Basic, 0, 0, (AstType *) &basic_type_u64, { Basic_Kind_U64,    Basic_Flag_Integer | Basic_Flag_Unsigned, 8,  8, "u64"    } },

    { Type_Kind_Basic, 0, 0, NULL,                        { Basic_Kind_Float_Unsized, Basic_Flag_Float,                  0,  0, "unsized float" } },
    { Type_Kind_Basic, 0, 0, (AstType *) &basic_type_f32, { Basic_Kind_F32,    Basic_Flag_Float,                         4,  4, "f32"    } },
    { Type_Kind_Basic, 0, 0, (AstType *) &basic_type_f64, { Basic_Kind_F64,    Basic_Flag_Float,                         8,  4, "f64"    } },

    { Type_Kind_Basic, 0, 0, (AstType *) &basic_type_rawptr, { Basic_Kind_Rawptr, Basic_Flag_Pointer,                    POINTER_SIZE,  POINTER_SIZE, "rawptr" } },

    { Type_Kind_Basic, 0, 0, (AstType *) &basic_type_i8x16, { Basic_Kind_I8X16,  Basic_Flag_SIMD,                        16, 16, "i8x16" } },
    { Type_Kind_Basic, 0, 0, (AstType *) &basic_type_i16x8, { Basic_Kind_I16X8,  Basic_Flag_SIMD,                        16, 16, "i16x8" } },
    { Type_Kind_Basic, 0, 0, (AstType *) &basic_type_i32x4, { Basic_Kind_I32X4,  Basic_Flag_SIMD,                        16, 16, "i32x4" } },
    { Type_Kind_Basic, 0, 0, (AstType *) &basic_type_i64x2, { Basic_Kind_I64X2,  Basic_Flag_SIMD,                        16, 16, "i64x2" } },
    { Type_Kind_Basic, 0, 0, (AstType *) &basic_type_f32x4, { Basic_Kind_F32X4,  Basic_Flag_SIMD,                        16, 16, "f32x4" } },
    { Type_Kind_Basic, 0, 0, (AstType *) &basic_type_f64x2, { Basic_Kind_F64X2,  Basic_Flag_SIMD,                        16, 16, "f64x2" } },
    { Type_Kind_Basic, 0, 0, (AstType *) &basic_type_v128,  { Basic_Kind_V128,   Basic_Flag_SIMD,                        16, 16, "v128"  } },

    { Type_Kind_Basic, 0, 0, NULL,                          { Basic_Kind_Type_Index, Basic_Flag_Type_Index,              4,  4, "type_expr" } },
};

// TODO: Document this!!
       bh_imap type_map;
static bh_imap type_pointer_map;
static bh_imap type_array_map;
static bh_imap type_slice_map;
static bh_imap type_dynarr_map;
static bh_imap type_vararg_map;
static Table(u64) type_func_map;

static Type* type_create(TypeKind kind, bh_allocator a, u32 extra_type_pointer_count) {
    Type* type = bh_alloc(a, sizeof(Type) + sizeof(Type *) * extra_type_pointer_count);
    type->kind = kind;
    type->ast_type = NULL;
    return type;
}

static void type_register(Type* type) {
    static u32 next_unique_id = 1;
    type->id = next_unique_id++;
    if (type->ast_type) type->ast_type->type_id = type->id;

    bh_imap_put(&type_map, type->id, (u64) type);
}

void types_init() {
    bh_imap_init(&type_map,         global_heap_allocator, 255);
    bh_imap_init(&type_pointer_map, global_heap_allocator, 255);
    bh_imap_init(&type_array_map,   global_heap_allocator, 255);
    bh_imap_init(&type_slice_map,   global_heap_allocator, 255);
    bh_imap_init(&type_dynarr_map,  global_heap_allocator, 255);
    bh_imap_init(&type_vararg_map,  global_heap_allocator, 255);
    sh_new_arena(type_func_map);

    fori (i, 0, Basic_Kind_Count) type_register(&basic_types[i]);
}

void types_dump_type_info() {
    bh_arr_each(bh__imap_entry, entry, type_map.entries) {
        bh_printf("%d -> %s\n", entry->key, type_get_name((Type *) entry->value));
    }
}

Type* type_lookup_by_id(u32 id) {
    if (bh_imap_has(&type_map, id)) {
        return (Type *) bh_imap_get(&type_map, id);
    }

    return NULL;
}

b32 types_are_compatible_(Type* t1, Type* t2, b32 recurse_pointers) {
    // NOTE: If they are pointing to the same thing,
    // it is safe to assume they are the same type
    if (t1 == t2) return 1;
    if (t1 == NULL || t2 == NULL) return 0;
    if (t1->id == t2->id) return 1;

    if (t1 == &type_auto_return || t2 == &type_auto_return) {
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

            if (t1->Basic.kind == Basic_Kind_Rawptr && type_is_pointer(t2)) {
                return 1;
            }
            break;

        case Type_Kind_Pointer: {
            if (t2->kind == Type_Kind_Pointer) {
                if (!recurse_pointers) return 1;

                if (types_are_compatible(t1->Pointer.elem, t2->Pointer.elem)) return 1;

                if (t1->Pointer.elem->kind == Type_Kind_Struct && t2->Pointer.elem->kind == Type_Kind_Struct) {
                    Type* t1_struct = t1->Pointer.elem;
                    Type* t2_struct = t2->Pointer.elem;

                    bh_arr(StructMember *) members = t1_struct->Struct.memarr;
                    if (bh_arr_length(members) > 0 && members[0]->used)
                        return types_are_compatible(t2_struct,members[0]->type);
                }
            }

            if (t2->kind == Type_Kind_Basic && t2->Basic.kind == Basic_Kind_Rawptr) return 1;

            break;
        }

        case Type_Kind_Array: {
            if (t2->kind != Type_Kind_Array) return 0;

            if (t1->Array.count != 0)
                if (t1->Array.count != t2->Array.count) return 0;

            return types_are_compatible(t1->Array.elem, t2->Array.elem);
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

            if (!types_are_compatible(t1->Function.return_type, t2->Function.return_type)) return 0;

            if (t1->Function.param_count > 0) {
                fori (i, 0, t1->Function.param_count) {
                    if (!types_are_compatible(t1->Function.params[i], t2->Function.params[i])) return 0;
                }
            }

            return 1;
        }

        case Type_Kind_Slice: {
            if (t2->kind != Type_Kind_Slice) return 0;
            return types_are_compatible(t1->Slice.elem, t2->Slice.elem);
        }

        case Type_Kind_VarArgs: {
            if (t2->kind != Type_Kind_VarArgs) return 0;
            return types_are_compatible(t1->VarArgs.elem, t2->VarArgs.elem);
        }

        case Type_Kind_DynArray: {
            if (t2->kind != Type_Kind_DynArray) return 0;
            return types_are_compatible(t1->DynArray.elem, t2->DynArray.elem);
        }

        case Type_Kind_Compound: {
            if (t2->kind != Type_Kind_Compound) return 0;
            if (t1->Compound.count != t2->Compound.count) return 0;

            fori (i, 0, (i64) t1->Compound.count) {
                if (!types_are_compatible(t1->Compound.types[i], t2->Compound.types[i])) return 0;
            }

            return 1;
        }

        case Type_Kind_Distinct:
            // If the above cases didn't catch it, then these distinct types are not compatible.
            return 0;

        default:
            assert(("Invalid type", 0));
            break;
    }

    return 0;
}

b32 types_are_compatible(Type* t1, Type* t2) {
    return types_are_compatible_(t1, t2, 1);
}

u32 type_size_of(Type* type) {
    if (type == NULL) return 0;

    switch (type->kind) {
        case Type_Kind_Basic:    return type->Basic.size;
        case Type_Kind_Pointer:  return POINTER_SIZE;
        case Type_Kind_Function: return 4;
        case Type_Kind_Array:    return type->Array.size;
        case Type_Kind_Struct:   return type->Struct.size;
        case Type_Kind_Enum:     return type_size_of(type->Enum.backing);
        case Type_Kind_Slice:    return POINTER_SIZE * 2; // HACK: These should not have to be 16 bytes in size, they should only have to be 12,
        case Type_Kind_VarArgs:  return POINTER_SIZE * 2; // but there are alignment issues right now with that so I decided to not fight it and just make them 16 bytes in size.
        case Type_Kind_DynArray: return POINTER_SIZE + 8 + 2 * POINTER_SIZE; // data (8), count (4), capacity (4), allocator { func (4), ---(4), data (8) }
        case Type_Kind_Compound: return type->Compound.size;
        case Type_Kind_Distinct: return type_size_of(type->Distinct.base_type);
        default:                 return 0;
    }
}

u32 type_alignment_of(Type* type) {
    if (type == NULL) return 1;

    switch (type->kind) {
        case Type_Kind_Basic:    return type->Basic.alignment;
        case Type_Kind_Pointer:  return POINTER_SIZE;
        case Type_Kind_Function: return 4;
        case Type_Kind_Array:    return type_alignment_of(type->Array.elem);
        case Type_Kind_Struct:   return type->Struct.alignment;
        case Type_Kind_Enum:     return type_alignment_of(type->Enum.backing);
        case Type_Kind_Slice:    return POINTER_SIZE;
        case Type_Kind_VarArgs:  return POINTER_SIZE;
        case Type_Kind_DynArray: return POINTER_SIZE;
        case Type_Kind_Compound: return 4; // HACK
        case Type_Kind_Distinct: return type_alignment_of(type->Distinct.base_type);
        default: return 1;
    }
}

// If this function returns NULL, then the caller MUST yield because the type may still be constructed in the future.
// If there was an error constructing the type, then this function will report that directly.
Type* type_build_from_ast(bh_allocator alloc, AstType* type_node) {
    if (type_node == NULL) return NULL;

    switch (type_node->kind) {
        case Ast_Kind_Pointer_Type: {
            // ((AstPointerType *) type_node)->elem->flags |= type_node->flags & Ast_Flag_Header_Check_No_Error;
            Type* ptr_type = type_make_pointer(alloc, type_build_from_ast(alloc, ((AstPointerType *) type_node)->elem));
            if (ptr_type) ptr_type->ast_type = type_node;
            return ptr_type;
        }

        case Ast_Kind_Function_Type: {
            AstFunctionType* ftype_node = (AstFunctionType *) type_node;
            u64 param_count = ftype_node->param_count;

            Type* return_type = type_build_from_ast(alloc, ftype_node->return_type);
            if (return_type == NULL) return NULL;

            Type* func_type = type_create(Type_Kind_Function, alloc, param_count);
            func_type->ast_type = type_node;
            func_type->Function.param_count = param_count;
            func_type->Function.needed_param_count = param_count;
            func_type->Function.vararg_arg_pos = -1;
            func_type->Function.return_type = return_type;

            if (param_count > 0) {
                fori (i, 0, (i64) param_count) {
                    func_type->Function.params[i] = type_build_from_ast(alloc, ftype_node->params[i]);

                    // LEAK LEAK LEAK
                    if (func_type->Function.params[i] == NULL) return NULL;
                }
            }

            char* name = (char *) type_get_unique_name(func_type);
            if (func_type->Function.return_type != &type_auto_return) {
                i32 index = shgeti(type_func_map, name);
                if (index != -1) {
                    u64 id = type_func_map[index].value;
                    Type* existing_type = (Type *) bh_imap_get(&type_map, id);

                    // LEAK LEAK LEAK the func_type that is created
                    return existing_type;
                }
            }

            type_register(func_type);
            shput(type_func_map, name, func_type->id);

            return func_type;
        }

        case Ast_Kind_Array_Type: {
            AstArrayType* a_node = (AstArrayType *) type_node;

            Type *elem_type = type_build_from_ast(alloc, a_node->elem);
            if (elem_type == NULL)  return NULL;

            u32 count = 0;
            if (a_node->count_expr) {
                if (a_node->count_expr->type == NULL)
                    a_node->count_expr->type = type_build_from_ast(alloc, a_node->count_expr->type_node);

                if (node_is_auto_cast((AstNode *) a_node->count_expr)) {
                    a_node->count_expr = ((AstUnaryOp *) a_node)->expr;
                }

                resolve_expression_type(a_node->count_expr);

                // NOTE: Currently, the count_expr has to be an I32 literal
                if (a_node->count_expr->type->kind != Type_Kind_Basic
                    || a_node->count_expr->type->Basic.kind != Basic_Kind_I32) {
                    onyx_report_error(type_node->token->pos, Error_Critical, "Array type expects type 'i32' for size, got '%s'.",
                        type_get_name(a_node->count_expr->type));
                    return NULL;
                }

                count = get_expression_integer_value(a_node->count_expr, NULL);
            }

            Type* array_type = type_make_array(alloc, elem_type, count);
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
                s_type = type_create(Type_Kind_Struct, alloc, 0);
                s_node->pending_type = s_type;

                s_type->ast_type = type_node;
                s_type->Struct.name = s_node->name;
                s_type->Struct.mem_count = bh_arr_length(s_node->members);
                s_type->Struct.meta_tags = s_node->meta_tags;
                s_type->Struct.constructed_from = NULL;
                s_type->Struct.status = SPS_Start;
                type_register(s_type);

                s_type->Struct.memarr = NULL;
                sh_new_arena(s_type->Struct.members);
                bh_arr_new(global_heap_allocator, s_type->Struct.memarr, s_type->Struct.mem_count);

            } else {
                s_type = s_node->pending_type;
            }

            s_type->Struct.poly_sln = NULL;

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
                    (*member)->type = type_build_from_ast(alloc, (*member)->type_node);

                if ((*member)->type == NULL) {
                    s_node->pending_type_is_valid = 0;
                    return NULL;
                }

                mem_alignment = type_alignment_of((*member)->type);
                if (mem_alignment <= 0) {
                    onyx_report_error((*member)->token->pos, Error_Critical, "Invalid member type: %s. Has alignment %d", type_get_name((*member)->type), mem_alignment); 
                    return NULL;
                }

                if (mem_alignment > alignment) alignment = mem_alignment;

                if (!s_node->is_packed) {
                    bh_align(offset, mem_alignment);
                }

                token_toggle_end((*member)->token);
                if (shgeti(s_type->Struct.members, (*member)->token->text) != -1) {
                    onyx_report_error((*member)->token->pos, Error_Critical, "Duplicate struct member, '%s'.", (*member)->token->text);
                    return NULL;
                }

                StructMember* smem = bh_alloc_item(alloc, StructMember);
                smem->offset = offset;
                smem->type = (*member)->type;
                smem->idx = idx;
                smem->name = bh_strdup(alloc, (*member)->token->text);
                smem->token = (*member)->token;
                smem->initial_value = &(*member)->initial_value;
                smem->meta_tags = (*member)->meta_tags;

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

            u32 min_alignment = get_expression_integer_value(s_node->min_alignment_, NULL);
            alignment = bh_max(min_alignment, alignment);
            if (!s_node->is_packed) {
                bh_align(size, alignment);
            }

            u32 min_size = get_expression_integer_value(s_node->min_size_, NULL);
            size = bh_max(min_size, size);

            s_type->Struct.alignment = alignment;
            s_type->Struct.size = size;

            s_type->Struct.linear_members = NULL;
            bh_arr_new(global_heap_allocator, s_type->Struct.linear_members, s_type->Struct.mem_count);
            build_linear_types_with_offset(s_type, &s_type->Struct.linear_members, 0);

            s_type->Struct.status = SPS_Members_Done;
            return s_type;
        }

        case Ast_Kind_Enum_Type: {
            AstEnumType* enum_node = (AstEnumType *) type_node;
            if (enum_node->etcache) return enum_node->etcache;
            if (enum_node->backing_type == NULL) return NULL;

            Type* enum_type = type_create(Type_Kind_Enum, alloc, 0);
            enum_node->etcache = enum_type;

            enum_type->ast_type = type_node;
            enum_type->Enum.backing = enum_node->backing_type;
            enum_type->Enum.name = enum_node->name;
            enum_type->Enum.is_flags = enum_node->is_flags;

            type_register(enum_type);
            return enum_type;
        }

        case Ast_Kind_Slice_Type: {
            Type* slice_type = type_make_slice(alloc, type_build_from_ast(alloc, ((AstSliceType *) type_node)->elem));
            if (slice_type) slice_type->ast_type = type_node;
            return slice_type;
        }

        case Ast_Kind_DynArr_Type: {
            Type* dynarr_type = type_make_dynarray(alloc, type_build_from_ast(alloc, ((AstDynArrType *) type_node)->elem));
            if (dynarr_type) dynarr_type->ast_type = type_node;
            return dynarr_type;
        }

        case Ast_Kind_VarArg_Type: {
            Type* va_type = type_make_varargs(alloc, type_build_from_ast(alloc, ((AstVarArgType *) type_node)->elem));   
            if (va_type) va_type->ast_type = type_node;
            return va_type;
        }

        case Ast_Kind_Basic_Type: {
            return ((AstBasicType *) type_node)->basic_type;
        }

        case Ast_Kind_Type_Alias: {
            Type* type = type_build_from_ast(alloc, ((AstTypeAlias *) type_node)->to);
            if (type && type->ast_type) type_node->type_id = type->id;
            return type;
        }

        case Ast_Kind_Type_Raw_Alias:
            return ((AstTypeRawAlias *) type_node)->to;

        case Ast_Kind_Poly_Struct_Type: {
            if (type_node->type_id != 0) return NULL;

            Type* p_type = type_create(Type_Kind_PolyStruct, alloc, 0);
            p_type->ast_type = type_node;
            p_type->PolyStruct.name = ((AstPolyStructType *) type_node)->name;
            p_type->PolyStruct.meta_tags = ((AstPolyStructType *) type_node)->base_struct->meta_tags;

            type_register(p_type);
            return NULL;
        }

        case Ast_Kind_Poly_Call_Type: {
            AstPolyCallType* pc_type = (AstPolyCallType *) type_node;
            pc_type->callee = (AstType *) strip_aliases((AstNode *) pc_type->callee);

            if (!(pc_type->callee && pc_type->callee->kind == Ast_Kind_Poly_Struct_Type)) {
                // If it is an unresolved field access or symbol, just return because an error will be printed elsewhere.
                if (pc_type->callee->kind == Ast_Kind_Field_Access || pc_type->callee->kind == Ast_Kind_Symbol) return NULL;

                onyx_report_error(pc_type->token->pos, Error_Critical, "Cannot instantiate a concrete type off of a non-polymorphic type.");
                onyx_report_error(pc_type->callee->token->pos, Error_Critical, "Here is the type trying to be instantiated. (%s)", onyx_ast_node_kind_string(pc_type->callee->kind));
                return NULL;
            }

            AstPolyStructType* ps_type = (AstPolyStructType *) pc_type->callee;

            bh_arr(AstPolySolution) slns = NULL;
            bh_arr_new(global_heap_allocator, slns, bh_arr_length(pc_type->params));
            bh_arr_each(AstNode *, given, pc_type->params) {
                if (node_is_type(*given)) {
                    Type* param_type = type_build_from_ast(alloc, (AstType *) *given);

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

            Type* concrete = polymorphic_struct_lookup(ps_type, slns, pc_type->token->pos, (pc_type->flags & Ast_Flag_Header_Check_No_Error) == 0);

            // This should be copied in the previous function.
            // CLEANUP: Maybe don't copy it and just use this one since it is allocated on the heap?
            bh_arr_free(slns);

            if (!concrete) return NULL;
            if (concrete == (Type *) &node_that_signals_failure) return concrete;
            concrete->Struct.constructed_from = (AstType *) ps_type;
            return concrete;
        }

        case Ast_Kind_Type_Compound: {
            AstCompoundType* ctype = (AstCompoundType *) type_node;

            i64 type_count = bh_arr_length(ctype->types);

            Type* comp_type = type_create(Type_Kind_Compound, alloc, type_count);
            comp_type->Compound.size = 0;
            comp_type->Compound.count = type_count;

            fori (i, 0, type_count) {
                assert(ctype->types[i] != NULL);
                comp_type->Compound.types[i] = type_build_from_ast(alloc, ctype->types[i]);

                // LEAK LEAK LEAK
                if (comp_type->Compound.types[i] == NULL) return NULL;

                comp_type->Compound.size += bh_max(type_size_of(comp_type->Compound.types[i]), 4);
            }

            bh_align(comp_type->Compound.size, 4);

            comp_type->Compound.linear_members = NULL;
            bh_arr_new(global_heap_allocator, comp_type->Compound.linear_members, comp_type->Compound.count);
            build_linear_types_with_offset(comp_type, &comp_type->Compound.linear_members, 0);

            type_register(comp_type);
            return comp_type;
        }

        case Ast_Kind_Alias: {
            AstAlias* alias = (AstAlias *) type_node;
            return type_build_from_ast(alloc, (AstType *) alias->alias);
        }

        case Ast_Kind_Typeof: {
            AstTypeOf* type_of = (AstTypeOf *) type_node;
            if (type_of->resolved_type != NULL) {
                return type_of->resolved_type;
            }
            
            return NULL;
        }

        case Ast_Kind_Distinct_Type: {
            AstDistinctType* distinct = (AstDistinctType *) type_node;
            if (distinct->dtcache) return distinct->dtcache;

            Type *base_type = type_build_from_ast(alloc, distinct->base_type);
            if (base_type == NULL) return NULL;
            if (base_type->kind != Type_Kind_Basic) {
                onyx_report_error(distinct->token->pos, Error_Critical, "Distinct types can only be made out of primitive types. '%s' is not a primitive type.", type_get_name(base_type));
                return NULL;
            }

            Type *distinct_type = type_create(Type_Kind_Distinct, alloc, 0);
            distinct_type->Distinct.base_type = base_type;
            distinct_type->Distinct.name = distinct->name;
            distinct->dtcache = distinct_type;

            type_register(distinct_type);
            return distinct_type;
        }
    }

    return NULL;
}

// CLEANUP: This needs to be merged with the very similar code from up above.
Type* type_build_function_type(bh_allocator alloc, AstFunction* func) {
    u64 param_count = bh_arr_length(func->params);

    Type* return_type = type_build_from_ast(alloc, func->return_type);
    if (return_type == NULL) return NULL;

    Type* func_type = type_create(Type_Kind_Function, alloc, param_count);
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
    char* name = (char *) type_get_unique_name(func_type);
    if (func_type->Function.return_type != &type_auto_return) {
        i32 index = shgeti(type_func_map, name);
        if (index != -1) {
            u64 id = type_func_map[index].value;
            Type* existing_type = (Type *) bh_imap_get(&type_map, id);

            // LEAK LEAK LEAK the func_type that is created
            return existing_type;
        }
    }

    type_register(func_type);
    shput(type_func_map, name, func_type->id);

    return func_type;
}

Type* type_build_compound_type(bh_allocator alloc, AstCompound* compound) {
    i64 expr_count = bh_arr_length(compound->exprs);
    fori (i, 0, expr_count) {
        if (compound->exprs[i]->type == NULL) return NULL;
        if (compound->exprs[i]->type->kind == Type_Kind_Basic) {
            if (compound->exprs[i]->type->Basic.kind == Basic_Kind_Int_Unsized || compound->exprs[i]->type->Basic.kind == Basic_Kind_Float_Unsized) {
                return NULL;
            }
        }
    }

    Type* comp_type = type_create(Type_Kind_Compound, alloc, expr_count);
    comp_type->Compound.size = 0;
    comp_type->Compound.count = expr_count;

    fori (i, 0, expr_count) {
        assert(compound->exprs[i]->type != NULL);
        comp_type->Compound.types[i] = compound->exprs[i]->type;
        comp_type->Compound.size += bh_max(type_size_of(comp_type->Compound.types[i]), 4);
    }
    
    bh_align(comp_type->Compound.size, 4);
    
    comp_type->Compound.linear_members = NULL;
    bh_arr_new(global_heap_allocator, comp_type->Compound.linear_members, comp_type->Compound.count);
    build_linear_types_with_offset(comp_type, &comp_type->Compound.linear_members, 0);

    type_register(comp_type);
    return comp_type;
}

Type* type_build_implicit_type_of_struct_literal(bh_allocator alloc, AstStructLiteral* lit) {
    Type* type = type_create(Type_Kind_Struct, alloc, 0);
    type->ast_type = NULL;
    type->Struct.name = NULL;
    type->Struct.mem_count = bh_arr_length(lit->args.named_values);
    type->Struct.meta_tags = NULL;
    type->Struct.constructed_from = NULL;
    type->Struct.status = SPS_Start;
    type->Struct.poly_sln = NULL;
    type_register(type);

    type->Struct.memarr = NULL;
    sh_new_arena(type->Struct.members);
    bh_arr_new(global_heap_allocator, type->Struct.memarr, type->Struct.mem_count);

    u32 size = 0;
    u32 offset = 0;
    u32 alignment = 1;
    u32 idx = 0;
    bh_arr_each(AstNamedValue *, pnv, lit->args.named_values) {
        AstNamedValue *nv = *pnv;

        Type* member_type = resolve_expression_type(nv->value);
        if (member_type == NULL) {
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

        StructMember *smem = bh_alloc_item(alloc, StructMember);
        smem->offset = offset;
        smem->type = member_type;
        smem->idx = idx;
        smem->name = bh_strdup(alloc, nv->token->text);
        smem->token = nv->token;
        smem->initial_value = &nv->value;
        smem->meta_tags = NULL;
        smem->included_through_use = 0;
        smem->used = 0;
        smem->use_through_pointer_index = -1;
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
    type->Struct.linear_members = NULL;
    bh_arr_new(global_heap_allocator, type->Struct.linear_members, type->Struct.mem_count);
    build_linear_types_with_offset(type, &type->Struct.linear_members, 0);

    type->Struct.status = SPS_Uses_Done;
    return type;
}

Type* type_make_pointer(bh_allocator alloc, Type* to) {
    if (to == NULL) return NULL;
    if (to == (Type *) &node_that_signals_failure) return to;

    assert(to->id > 0);
    u64 ptr_id = bh_imap_get(&type_pointer_map, to->id);
    if (ptr_id > 0) {
        Type* ptr_type = (Type *) bh_imap_get(&type_map, ptr_id);
        return ptr_type;

    } else {
        Type* ptr_type = type_create(Type_Kind_Pointer, alloc, 0);
        ptr_type->Pointer.base.flags |= Basic_Flag_Pointer;
        ptr_type->Pointer.base.size = POINTER_SIZE;
        ptr_type->Pointer.elem = to;

        type_register(ptr_type);
        bh_imap_put(&type_pointer_map, to->id, ptr_type->id);

        return ptr_type;
    }
}

Type* type_make_array(bh_allocator alloc, Type* to, u32 count) {
    if (to == NULL) return NULL;
    if (to == (Type *) &node_that_signals_failure) return to;

    assert(to->id > 0);
    u64 key = ((((u64) to->id) << 32) | (u64) count);
    u64 array_id = bh_imap_get(&type_array_map, key);
    if (array_id > 0) {
        Type* array_type = (Type *) bh_imap_get(&type_map, array_id);
        return array_type;

    } else {
        Type* arr_type = type_create(Type_Kind_Array, alloc, 0);
        arr_type->Array.count = count;
        arr_type->Array.elem = to;
        arr_type->Array.size = count * type_size_of(to);

        type_register(arr_type);
        bh_imap_put(&type_array_map, key, arr_type->id);

        return arr_type;
    }
}

Type* type_make_slice(bh_allocator alloc, Type* of) {
    if (of == NULL) return NULL;
    if (of == (Type *) &node_that_signals_failure) return of;

    assert(of->id > 0);
    u64 slice_id = bh_imap_get(&type_slice_map, of->id);
    if (slice_id > 0) {
        Type* slice_type = (Type *) bh_imap_get(&type_map, slice_id);
        return slice_type;

    } else {
        Type* slice_type = type_create(Type_Kind_Slice, alloc, 0);
        type_register(slice_type);
        bh_imap_put(&type_slice_map, of->id, slice_type->id);

        type_make_pointer(alloc, of);
        slice_type->Slice.elem = of;

        return slice_type;
    }
}

Type* type_make_dynarray(bh_allocator alloc, Type* of) {
    if (of == NULL) return NULL;
    if (of == (Type *) &node_that_signals_failure) return of;

    assert(of->id > 0);
    u64 dynarr_id = bh_imap_get(&type_dynarr_map, of->id);
    if (dynarr_id > 0) {
        Type* dynarr = (Type *) bh_imap_get(&type_map, dynarr_id);
        return dynarr;

    } else {
        Type* dynarr = type_create(Type_Kind_DynArray, alloc, 0);
        type_register(dynarr);
        bh_imap_put(&type_dynarr_map, of->id, dynarr->id);

        type_make_pointer(alloc, of);
        dynarr->DynArray.elem = of;

        return dynarr;
    }
}

Type* type_make_varargs(bh_allocator alloc, Type* of) {
    if (of == NULL) return NULL;
    if (of == (Type *) &node_that_signals_failure) return of;
    
    assert(of->id > 0);
    u64 vararg_id = bh_imap_get(&type_vararg_map, of->id);
    if (vararg_id > 0) {
        Type* va_type = (Type *) bh_imap_get(&type_map, vararg_id);
        return va_type;

    } else {
        Type* va_type = type_create(Type_Kind_VarArgs, alloc, 0);
        type_register(va_type);
        bh_imap_put(&type_vararg_map, of->id, va_type->id);

        type_make_pointer(alloc, of);
        va_type->VarArgs.elem = of;

        return va_type;
    }
}

void build_linear_types_with_offset(Type* type, bh_arr(TypeWithOffset)* pdest, u32 offset) {
    if (type_is_structlike_strict(type)) {
        u32 mem_count = type_structlike_mem_count(type);
        StructMember smem = { 0 };
        fori (i, 0, mem_count) {
            type_lookup_member_by_idx(type, i, &smem);
            build_linear_types_with_offset(smem.type, pdest, offset + smem.offset);
        }

    } else if (type->kind == Type_Kind_Compound) {
        u32 elem_offset = 0;
        fori (i, 0, type->Compound.count) {
            build_linear_types_with_offset(type->Compound.types[i], pdest, offset + elem_offset);
            elem_offset += bh_max(type_size_of(type->Compound.types[i]), 4);
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

b32 type_struct_member_apply_use(bh_allocator alloc, Type *s_type, StructMember *smem) {
    Type* used_type = smem->type;

    b32 type_is_pointer = 0;
    if (used_type->kind == Type_Kind_Pointer) {
        type_is_pointer = 1;
        used_type = type_get_contained_type(used_type);
    }

    if (used_type->kind != Type_Kind_Struct) {
        onyx_report_error(smem->token->pos, Error_Critical, "Can only use things of structure, or pointer to structure type.");
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
            onyx_report_error(smem->token->pos, Error_Critical, "Used name '%s' conflicts with existing struct member.", nsmem->name);
            return 0;
        }

        StructMember* new_smem = bh_alloc_item(alloc, StructMember);
        new_smem->type   = nsmem->type;
        new_smem->name   = nsmem->name;
        new_smem->meta_tags = nsmem->meta_tags;
        new_smem->used = nsmem->used;
        new_smem->included_through_use = 1;

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

const char* type_get_unique_name(Type* type) {
    if (type == NULL) return "unknown";

    switch (type->kind) {
        case Type_Kind_Basic: return type->Basic.name;
        case Type_Kind_Pointer: return bh_aprintf(global_scratch_allocator, "^%s", type_get_unique_name(type->Pointer.elem));
        case Type_Kind_Array: return bh_aprintf(global_scratch_allocator, "[%d] %s", type->Array.count, type_get_unique_name(type->Array.elem));
        case Type_Kind_Struct:
            if (type->Struct.name)
                return bh_aprintf(global_scratch_allocator, "%s@%l", type->Struct.name, type->id);
            else
                return bh_aprintf(global_scratch_allocator, "%s@%l", "<anonymous struct>", type->id);
        case Type_Kind_Enum:
            if (type->Enum.name)
                return bh_aprintf(global_scratch_allocator, "%s@%l", type->Enum.name, type->id);
            else
                return bh_aprintf(global_scratch_allocator, "%s@%l", "<anonymous enum>", type->id);

        case Type_Kind_Slice: return bh_aprintf(global_scratch_allocator, "[] %s", type_get_unique_name(type->Slice.elem));
        case Type_Kind_VarArgs: return bh_aprintf(global_scratch_allocator, "..%s", type_get_unique_name(type->VarArgs.elem));
        case Type_Kind_DynArray: return bh_aprintf(global_scratch_allocator, "[..] %s", type_get_unique_name(type->DynArray.elem));

        case Type_Kind_Function: {
            char buf[1024];
            memset(buf, 0, 1024);

            strncat(buf, "(", 1023);
            fori (i, 0, type->Function.param_count) {
                strncat(buf, type_get_unique_name(type->Function.params[i]), 1023);

                if (i >= type->Function.needed_param_count)
                    strncat(buf, "?", 1023);

                if (i != type->Function.param_count - 1)
                    strncat(buf, ", ", 1023);
            }

            strncat(buf, ") -> ", 1023);
            strncat(buf, type_get_unique_name(type->Function.return_type), 1023);

            return bh_aprintf(global_scratch_allocator, "%s", buf);
        }

        case Type_Kind_Compound: {
            char buf[1024];
            memset(buf, 0, 1024);

            strncat(buf, "(", 1023);
            fori (i, 0, type->Compound.count) {
                strncat(buf, type_get_unique_name(type->Compound.types[i]), 1023);
                if (i != type->Compound.count - 1)
                    strncat(buf, ", ", 1023);
            }
            strncat(buf, ")", 1023);

            return bh_aprintf(global_scratch_allocator, "%s", buf);
        }

        case Type_Kind_Distinct: {
            return bh_aprintf(global_scratch_allocator, "%s@%l", type->Distinct.name, type->id);
        }

        default: return "unknown (not null)";
    }
}

const char* type_get_name(Type* type) {
    if (type == NULL) return "unknown";

    switch (type->kind) {
        case Type_Kind_Basic: return type->Basic.name;
        case Type_Kind_Pointer: return bh_aprintf(global_scratch_allocator, "^%s", type_get_name(type->Pointer.elem));
        case Type_Kind_Array: return bh_aprintf(global_scratch_allocator, "[%d] %s", type->Array.count, type_get_name(type->Array.elem));
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

        case Type_Kind_Slice: return bh_aprintf(global_scratch_allocator, "[] %s", type_get_name(type->Slice.elem));
        case Type_Kind_VarArgs: return bh_aprintf(global_scratch_allocator, "..%s", type_get_name(type->VarArgs.elem));
        case Type_Kind_DynArray: return bh_aprintf(global_scratch_allocator, "[..] %s", type_get_name(type->DynArray.elem));

        case Type_Kind_Function: {
            char buf[512];
            fori (i, 0, 512) buf[i] = 0;

            strncat(buf, "(", 511);
            fori (i, 0, type->Function.param_count) {
                strncat(buf, type_get_name(type->Function.params[i]), 511);
                if (i != type->Function.param_count - 1)
                    strncat(buf, ", ", 511);
            }

            strncat(buf, ") -> ", 511);
            strncat(buf, type_get_name(type->Function.return_type), 511);

            return bh_aprintf(global_scratch_allocator, "%s", buf);
        }

        case Type_Kind_Compound: {
            char buf[512];
            fori (i, 0, 512) buf[i] = 0;

            strncat(buf, "(", 511);
            fori (i, 0, type->Compound.count) {
                strncat(buf, type_get_name(type->Compound.types[i]), 511);
                if (i != type->Compound.count - 1)
                    strncat(buf, ", ", 511);
            }
            strncat(buf, ")", 511);

            return bh_aprintf(global_scratch_allocator, "%s", buf);
        }

        case Type_Kind_Distinct: {
            return bh_aprintf(global_scratch_allocator, "%s", type->Distinct.name);
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
        case Type_Kind_Pointer:  return type->Pointer.elem;
        case Type_Kind_Array:    return type->Array.elem;
        case Type_Kind_Slice:    return type->Slice.elem;
        case Type_Kind_DynArray: return type->DynArray.elem;
        case Type_Kind_VarArgs:  return type->VarArgs.elem;
        default: return NULL;
    }
}

static const StructMember slice_members[] = {
    { 0,            0, NULL,                         "data",   NULL, NULL, -1, 0, 0 },
    { POINTER_SIZE, 1, &basic_types[Basic_Kind_U32], "count",  NULL, NULL, -1, 0, 0 },
    { POINTER_SIZE, 1, &basic_types[Basic_Kind_U32], "size",   NULL, NULL, -1, 0, 0 },
    { POINTER_SIZE, 1, &basic_types[Basic_Kind_U32], "length", NULL, NULL, -1, 0, 0 },
};

static const StructMember array_members[] = {
    { 0,                0, NULL,                         "data",      NULL, NULL, -1, 0, 0 },
    { POINTER_SIZE,     1, &basic_types[Basic_Kind_U32], "count",     NULL, NULL, -1, 0, 0 },
    { POINTER_SIZE + 4, 2, &basic_types[Basic_Kind_U32], "capacity",  NULL, NULL, -1, 0, 0 },
    { POINTER_SIZE + 8, 3, NULL,                         "allocator", NULL, NULL, -1, 0, 0 },
    { POINTER_SIZE,     1, &basic_types[Basic_Kind_U32], "size",      NULL, NULL, -1, 0, 0 },
    { POINTER_SIZE,     1, &basic_types[Basic_Kind_U32], "length",    NULL, NULL, -1, 0, 0 },
};

b32 type_lookup_member(Type* type, char* member, StructMember* smem) {
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
                    if (smem->idx == 0) smem->type = type_make_pointer(context.ast_alloc, type->Slice.elem);

                    return 1;
                }
            }
            return 0;
        }

        case Type_Kind_DynArray: {
            fori (i, 0, (i64) (sizeof(array_members) / sizeof(StructMember))) {
                if (strcmp(array_members[i].name, member) == 0) {
                    *smem = array_members[i];
                    if (smem->idx == 0) smem->type = type_make_pointer(context.ast_alloc, type->DynArray.elem);
                    if (smem->idx == 3) smem->type = type_build_from_ast(context.ast_alloc, builtin_allocator_type);

                    return 1;
                }
            }
            return 0;
        }

        default: return 0;
    }
}

b32 type_lookup_member_by_idx(Type* type, i32 idx, StructMember* smem) {
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
            if (smem->idx == 0) smem->type = type_make_pointer(context.ast_alloc, type->Slice.elem);

            return 1;
        }

        case Type_Kind_DynArray: {
            if (idx > 4) return 0;

            *smem = array_members[idx];
            if (idx == 0) smem->type = type_make_pointer(context.ast_alloc, type->DynArray.elem);
            if (idx == 3) smem->type = type_build_from_ast(context.ast_alloc, builtin_allocator_type);

            return 1;
        }

        default: return 0;
    }
}

i32 type_linear_member_count(Type* type) {
    switch (type->kind) {
        case Type_Kind_Slice:
        case Type_Kind_VarArgs:  return 2;
        case Type_Kind_DynArray: return 5;
        case Type_Kind_Compound: return bh_arr_length(type->Compound.linear_members);
        case Type_Kind_Struct:   return bh_arr_length(type->Struct.linear_members);
        default: return 1; 
    }
}

b32 type_linear_member_lookup(Type* type, i32 idx, TypeWithOffset* two) {
    switch (type->kind) {
        case Type_Kind_Slice:
        case Type_Kind_VarArgs: {
            if (idx == 0) { 
                two->type = type_make_pointer(context.ast_alloc, type->Slice.elem);
                two->offset = 0;
            }
            if (idx == 1) {
                two->type = &basic_types[Basic_Kind_U32];
                two->offset = POINTER_SIZE;
            }

            return 1;
        }
        case Type_Kind_DynArray: {
            if (idx == 0) { 
                two->type = type_make_pointer(context.ast_alloc, type->DynArray.elem);
                two->offset = 0;
            }
            if (idx == 1) {
                two->type = &basic_types[Basic_Kind_U32];
                two->offset = POINTER_SIZE;
            }
            if (idx == 2) {
                two->type = &basic_types[Basic_Kind_U32];
                two->offset = POINTER_SIZE + 4;
            }
            if (idx == 3 || idx == 4) {
                Type* allocator_type = type_build_from_ast(context.ast_alloc, builtin_allocator_type);
                type_linear_member_lookup(allocator_type, idx - 3, two);
                two->offset += POINTER_SIZE + 8;
            }

            return 1;
        }
        case Type_Kind_Compound: *two = type->Compound.linear_members[idx]; return 1;
        case Type_Kind_Struct:   *two = type->Struct.linear_members[idx];   return 1;

        case Type_Kind_Distinct:
            two->type = type->Distinct.base_type;
            two->offset = 0;
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
        case Type_Kind_Struct: {
            i32 idx = 0;
            bh_arr_each(TypeWithOffset, two, type->Struct.linear_members) {
                if (two->offset == offset) return idx;
                idx++;
            }

            return -1;
        }
        default: return -1;
    }
}

b32 type_struct_is_simple(Type* type) {
    if (type->kind != Type_Kind_Struct) return 0;

    b32 is_simple = 1;
    bh_arr_each(StructMember *, mem, type->Struct.memarr) {
        if (type_is_compound((*mem)->type) || (*mem)->type->kind == Type_Kind_Array) {
            is_simple = 0;
            break;
        }
    }

    return is_simple;
}

b32 type_is_pointer(Type* type) {
    if (type == NULL) return 0;
    return type->kind == Type_Kind_Pointer;
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
    if (type == NULL) return 0;

    if (type->kind == Type_Kind_Struct) {
        //
        // This is for the kind of common case where a structure simply wraps a
        // single non-compound value; in this situation, the structure can be
        // "dissolved" at compile-time and turn into the underlying type.
        //
        
        if (bh_arr_length(type->Struct.linear_members) != 1) return 1;
        return type_is_compound(type->Struct.linear_members[0].type);
    }

    return type->kind != Type_Kind_Basic
        && type->kind != Type_Kind_Pointer
        && type->kind != Type_Kind_Enum
        && type->kind != Type_Kind_Function
        && type->kind != Type_Kind_Array
        && type->kind != Type_Kind_Distinct;
}

b32 type_is_simd(Type* type) {
    if (type == NULL) return 0;
    if (type->kind != Type_Kind_Basic) return 0;
    return type->Basic.flags & Basic_Flag_SIMD;
}

b32 type_results_in_void(Type* type) {
    return (type == NULL)
        || (type->kind == Type_Kind_Basic && type->Basic.kind == Basic_Kind_Void)
        || (   (type->kind == Type_Kind_Function)
            && (type->Function.return_type->kind == Type_Kind_Basic)
            && (type->Function.return_type->Basic.kind == Basic_Kind_Void));
}

b32 type_is_array_accessible(Type* type) {
    if (type == NULL) return 0;
    if (type_is_pointer(type)) return 1;
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
    if (type->kind == Type_Kind_DynArray) return 1;
    if (type->kind == Type_Kind_VarArgs) return 1;
    if (type->kind == Type_Kind_Pointer) {
        if (type->Pointer.elem->kind == Type_Kind_Struct) return 1;
        if (type->Pointer.elem->kind == Type_Kind_Slice)  return 1;
        if (type->Pointer.elem->kind == Type_Kind_DynArray) return 1;
    }
    return 0;
}

b32 type_is_structlike_strict(Type* type) {
    if (type == NULL) return 0;
    if (type->kind == Type_Kind_Struct)   return 1;
    if (type->kind == Type_Kind_Slice)    return 1;
    if (type->kind == Type_Kind_DynArray) return 1;
    if (type->kind == Type_Kind_VarArgs)  return 1;
    return 0;
}

u32 type_structlike_mem_count(Type* type) {
    if (type == NULL) return 0;
    switch (type->kind) {
        case Type_Kind_Struct:   return type->Struct.mem_count;
        case Type_Kind_Slice:    return 2;
        case Type_Kind_VarArgs:  return 2;
        case Type_Kind_DynArray: return 4;
        default: return 0;
    }
}

u32 type_structlike_is_simple(Type* type) {
    if (type == NULL) return 0;
    switch (type->kind) {
        case Type_Kind_Struct:   return type_struct_is_simple(type);
        case Type_Kind_Slice:    return 1;
        case Type_Kind_VarArgs:  return 1;
        case Type_Kind_DynArray: return 0;
        default: return 0;
    }
}

b32 type_is_sl_constructable(Type* type) {
    if (type == NULL) return 0;
    switch (type->kind) {
        case Type_Kind_Struct:   return 1;
        case Type_Kind_Slice:    return 1;
        case Type_Kind_DynArray: return 1;
        default: return 0;
    }
}

b32 type_struct_constructed_from_poly_struct(Type* struct_type, struct AstType* from) {
    if (struct_type == NULL) return 0;
    if (struct_type->kind != Type_Kind_Struct) return 0;

    return struct_type->Struct.constructed_from == from;
}
