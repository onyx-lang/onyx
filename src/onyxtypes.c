#define BH_DEBUG
#include "onyxtypes.h"
#include "onyxastnodes.h"
#include "onyxutils.h"
#include "onyxerrors.h"

// NOTE: These have to be in the same order as Basic
Type basic_types[] = {
    { Type_Kind_Basic, 0, { Basic_Kind_Void,                    0,                       0,  1, "void"   } },

    { Type_Kind_Basic, 0, { Basic_Kind_Bool,   Basic_Flag_Boolean,                       1,  1, "bool"   } },

    { Type_Kind_Basic, 0, { Basic_Kind_Int_Unsized, Basic_Flag_Integer,                  0,  0, "unsized int" } },
    { Type_Kind_Basic, 0, { Basic_Kind_I8,     Basic_Flag_Integer,                       1,  1, "i8"     } },
    { Type_Kind_Basic, 0, { Basic_Kind_U8,     Basic_Flag_Integer | Basic_Flag_Unsigned, 1,  1, "u8"     } },
    { Type_Kind_Basic, 0, { Basic_Kind_I16,    Basic_Flag_Integer,                       2,  2, "i16"    } },
    { Type_Kind_Basic, 0, { Basic_Kind_U16,    Basic_Flag_Integer | Basic_Flag_Unsigned, 2,  2, "u16"    } },
    { Type_Kind_Basic, 0, { Basic_Kind_I32,    Basic_Flag_Integer,                       4,  4, "i32"    } },
    { Type_Kind_Basic, 0, { Basic_Kind_U32,    Basic_Flag_Integer | Basic_Flag_Unsigned, 4,  4, "u32"    } },
    { Type_Kind_Basic, 0, { Basic_Kind_I64,    Basic_Flag_Integer,                       8,  8, "i64"    } },
    { Type_Kind_Basic, 0, { Basic_Kind_U64,    Basic_Flag_Integer | Basic_Flag_Unsigned, 8,  8, "u64"    } },

    { Type_Kind_Basic, 0, { Basic_Kind_Float_Unsized, Basic_Flag_Float,                  0,  0, "unsized float" } },
    { Type_Kind_Basic, 0, { Basic_Kind_F32,    Basic_Flag_Float,                         4,  4, "f32"    } },
    { Type_Kind_Basic, 0, { Basic_Kind_F64,    Basic_Flag_Float,                         8,  4, "f64"    } },

    { Type_Kind_Basic, 0, { Basic_Kind_Rawptr, Basic_Flag_Pointer,                       4,  4, "rawptr" } },

    { Type_Kind_Basic, 0, { Basic_Kind_I8X16,  Basic_Flag_SIMD,                          16, 16, "i8x16" } },
    { Type_Kind_Basic, 0, { Basic_Kind_I16X8,  Basic_Flag_SIMD,                          16, 16, "i16x8" } },
    { Type_Kind_Basic, 0, { Basic_Kind_I32X4,  Basic_Flag_SIMD,                          16, 16, "i32x4" } },
    { Type_Kind_Basic, 0, { Basic_Kind_I64X2,  Basic_Flag_SIMD,                          16, 16, "i64x2" } },
    { Type_Kind_Basic, 0, { Basic_Kind_F32X4,  Basic_Flag_SIMD,                          16, 16, "f32x4" } },
    { Type_Kind_Basic, 0, { Basic_Kind_F64X2,  Basic_Flag_SIMD,                          16, 16, "f64x2" } },
    { Type_Kind_Basic, 0, { Basic_Kind_V128,   Basic_Flag_SIMD,                          16, 16, "v128"  } },
};

b32 types_are_surface_compatible(Type* t1, Type* t2) {
    // NOTE: If they are pointing to the same thing,
    // it is safe to assume they are the same type
    if (t1 == t2) return 1;
    if (t1 == NULL || t2 == NULL) return 0;

    switch (t1->kind) {
        case Type_Kind_Basic:
            if (t2->kind == Type_Kind_Basic) {
                // HACK: Not sure if this is right way to check this?
                if (t1 == t2) return 1;

                if ((t1->Basic.flags & Basic_Flag_Integer) && (t2->Basic.flags & Basic_Flag_Integer)) {
                    return t1->Basic.size == t2->Basic.size;
                }

                if (t1->Basic.kind == Basic_Kind_Rawptr && type_is_pointer(t2)) {
                    return 1;
                }
            }
            break;

        case Type_Kind_Pointer:
            if (t2->kind != Type_Kind_Pointer) return 0;

            if (t1->Pointer.elem->kind == Type_Kind_Basic && t2->Pointer.elem->kind == Type_Kind_Basic)
                return types_are_compatible(t1->Pointer.elem, t2->Pointer.elem);

            return 1;

        case Type_Kind_Array: {
            if (t2->kind != Type_Kind_Array) return 0;

            if (t1->Array.count != 0)
                if (t1->Array.count != t2->Array.count) return 0;

            return types_are_compatible(t1->Array.elem, t2->Array.elem);
        }

        case Type_Kind_Struct: {
            if (t2->kind != Type_Kind_Struct) return 0;
            if (t1->Struct.mem_count != t2->Struct.mem_count) return 0;
            if (t1->Struct.name && t2->Struct.name)
                if (strcmp(t1->Struct.name, t2->Struct.name) == 0) return 1;

            b32 works = 1;
            bh_table_each_start(StructMember, t1->Struct.members);
                if (!bh_table_has(StructMember, t2->Struct.members, (char *) key)) return 0;
                StructMember other = bh_table_get(StructMember, t2->Struct.members, (char *) key);
                if (other.offset != value.offset) return 0;

                if (!types_are_compatible(value.type, other.type)) {
                    works = 0;
                    break;
                }
            bh_table_each_end;

            return works;
        }

        case Type_Kind_Enum: {
            if (t2->kind != Type_Kind_Enum) return 0;
            return t1 == t2;
        }

        case Type_Kind_Slice: {
            if (t2->kind != Type_Kind_Slice) return 0;
            return types_are_compatible(t1->Slice.ptr_to_data->Pointer.elem, t2->Slice.ptr_to_data->Pointer.elem);
        }

        case Type_Kind_VarArgs: {
            if (t2->kind != Type_Kind_VarArgs) return 0;
            return types_are_compatible(t1->VarArgs.ptr_to_data->Pointer.elem, t2->VarArgs.ptr_to_data->Pointer.elem);
        }

        case Type_Kind_DynArray: {
            if (t2->kind != Type_Kind_DynArray) return 0;
            return types_are_compatible(t1->DynArray.ptr_to_data->Pointer.elem, t2->DynArray.ptr_to_data->Pointer.elem);
        }

        default:
            assert(("Invalid type", 0));
            break;
    }

    return 0;
}

b32 types_are_compatible(Type* t1, Type* t2) {
    // NOTE: If they are pointing to the same thing,
    // it is safe to assume they are the same type
    if (t1 == t2) return 1;
    if (t1 == NULL || t2 == NULL) return 0;

    switch (t1->kind) {
        case Type_Kind_Basic:
            if (t2->kind == Type_Kind_Basic) {
                // HACK: Not sure if this is right way to check this?
                if (t1 == t2) return 1;

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
                return types_are_compatible(t1->Pointer.elem, t2->Pointer.elem);
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
            if (t2->kind != Type_Kind_Struct) return 0;
            if (t1->Struct.mem_count != t2->Struct.mem_count) return 0;

            b32 works = 1;
            bh_table_each_start(StructMember, t1->Struct.members);
                if (!bh_table_has(StructMember, t2->Struct.members, (char *) key)) return 0;
                StructMember other = bh_table_get(StructMember, t2->Struct.members, (char *) key);
                if (other.offset != value.offset) return 0;

                if (!types_are_surface_compatible(value.type, other.type)) {
                    works = 0;
                    break;
                }
            bh_table_each_end;

            return works;
        }

        case Type_Kind_Enum: {
            return 0;
            // if (t2->kind != Type_Kind_Enum) return 0;
            // return t1 == t2;
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
            return types_are_compatible(t1->Slice.ptr_to_data->Pointer.elem, t2->Slice.ptr_to_data->Pointer.elem);
        }

        case Type_Kind_VarArgs: {
            if (t2->kind != Type_Kind_VarArgs) return 0;
            return types_are_compatible(t1->VarArgs.ptr_to_data->Pointer.elem, t2->VarArgs.ptr_to_data->Pointer.elem);
        }

        case Type_Kind_DynArray: {
            if (t2->kind != Type_Kind_DynArray) return 0;
            return types_are_compatible(t1->DynArray.ptr_to_data->Pointer.elem, t2->DynArray.ptr_to_data->Pointer.elem);
        }

        default:
            assert(("Invalid type", 0));
            break;
    }

    return 0;
}

u32 type_size_of(Type* type) {
    if (type == NULL) return 0;

    switch (type->kind) {
        case Type_Kind_Basic:    return type->Basic.size;
        case Type_Kind_Pointer:  return 4;
        case Type_Kind_Function: return 4;
        case Type_Kind_Array:    return type->Array.size;
        case Type_Kind_Struct:   return type->Struct.size;
        case Type_Kind_Enum:     return type_size_of(type->Enum.backing);
        case Type_Kind_Slice:    return 8;
        case Type_Kind_VarArgs:  return 8;
        case Type_Kind_DynArray: return 12;
        default:                 return 0;
    }
}

u32 type_alignment_of(Type* type) {
    if (type == NULL) return 1;

    switch (type->kind) {
        case Type_Kind_Basic:    return type->Basic.alignment;
        case Type_Kind_Pointer:  return 4;
        case Type_Kind_Function: return 4;
        case Type_Kind_Array:    return type_alignment_of(type->Array.elem);
        case Type_Kind_Struct:   return type->Struct.alignment;
        case Type_Kind_Enum:     return type_alignment_of(type->Enum.backing);
        case Type_Kind_Slice:    return 4;
        case Type_Kind_VarArgs:  return 4;
        case Type_Kind_DynArray: return 4;
        default: return 1;
    }
}

Type* type_build_from_ast(bh_allocator alloc, AstType* type_node) {
    if (type_node == NULL) return NULL;

    switch (type_node->kind) {
        case Ast_Kind_Pointer_Type: {
            return type_make_pointer(alloc, type_build_from_ast(alloc, ((AstPointerType *) type_node)->elem));
        }

        case Ast_Kind_Function_Type: {
            AstFunctionType* ftype_node = (AstFunctionType *) type_node;
            u64 param_count = ftype_node->param_count;

            Type* func_type = bh_alloc(alloc, sizeof(Type) + sizeof(Type *) * param_count);

            func_type->kind = Type_Kind_Function;
            func_type->Function.param_count = param_count;
            func_type->Function.return_type = type_build_from_ast(alloc, ftype_node->return_type);

            if (param_count > 0)
                fori (i, 0, param_count) {
                    func_type->Function.params[i] = type_build_from_ast(alloc, ftype_node->params[i]);
                }

            return func_type;
        }

        case Ast_Kind_Array_Type: {
            AstArrayType* a_node = (AstArrayType *) type_node;

            Type* a_type = bh_alloc(alloc, sizeof(Type));
            a_type->kind = Type_Kind_Array;
            a_type->Array.elem = type_build_from_ast(alloc, a_node->elem);

            u32 count = 0;
            if (a_node->count_expr) {
                if (a_node->count_expr->type == NULL)
                    a_node->count_expr->type = type_build_from_ast(alloc, a_node->count_expr->type_node);

                if (node_is_auto_cast((AstNode *) a_node->count_expr)) {
                    a_node->count_expr = ((AstUnaryOp *) a_node)->expr;
                }

                resolve_expression_type((AstTyped *) a_node->count_expr);

                // NOTE: Currently, the count_expr has to be an I32 literal
                if (a_node->count_expr->kind != Ast_Kind_NumLit
                    || a_node->count_expr->type->kind != Type_Kind_Basic
                    || a_node->count_expr->type->Basic.kind != Basic_Kind_I32) {
                    return NULL;
                }

                count = ((AstNumLit *) a_node->count_expr)->value.i;
            }

            a_type->Array.count = count;
            a_type->Array.size = type_size_of(a_type->Array.elem) * count;

            return a_type;
        }

        case Ast_Kind_Struct_Type: {
            AstStructType* s_node = (AstStructType *) type_node;
            if (s_node->stcache != NULL) return s_node->stcache;

            Type* s_type = bh_alloc(alloc, sizeof(Type));
            s_node->stcache = s_type;
            s_type->kind = Type_Kind_Struct;

            s_type->Struct.name = s_node->name;
            s_type->Struct.mem_count = bh_arr_length(s_node->members);
            s_type->Struct.memarr = NULL;
            bh_table_init(global_heap_allocator, s_type->Struct.members, s_type->Struct.mem_count);
            bh_arr_new(global_heap_allocator, s_type->Struct.memarr, s_type->Struct.mem_count);

            b32 is_union = (s_node->flags & Ast_Flag_Struct_Is_Union) != 0;
            u32 size = 0;
            u32 offset = 0;
            u32 alignment = 1, mem_alignment;
            u32 idx = 0;
            bh_arr_each(AstStructMember *, member, s_node->members) {
                (*member)->type = type_build_from_ast(alloc, (*member)->type_node);

                mem_alignment = type_alignment_of((*member)->type);
                if (mem_alignment <= 0) {
                    onyx_report_error((*member)->token->pos, "Invalid member type: %s", type_get_name((*member)->type)); 
                    return s_type;
                }

                if (mem_alignment > alignment) alignment = mem_alignment;
                if (offset % mem_alignment != 0) {
                    offset += mem_alignment - (offset % mem_alignment);
                }

                token_toggle_end((*member)->token);
                StructMember smem = {
                    .offset = offset,
                    .type = (*member)->type,
                    .idx = idx,
                    .name = bh_strdup(alloc, (*member)->token->text),
                };

                bh_table_put(StructMember, s_type->Struct.members, (*member)->token->text, smem);
                token_toggle_end((*member)->token);

                u32 type_size = type_size_of((*member)->type);
                if (((*member)->flags & Ast_Flag_Struct_Mem_Used) == 0) {
                    if (!is_union) offset += type_size;
                    if (!is_union) size += type_size;
                    else           size =  bh_max(size, type_size);

                    idx++;
                }
            }

            // NOTE: Need to do a second pass because the references to the
            // elements of the table may change if the internal arrays of the
            // table need to be resized.
            s_type->Struct.mem_count = 0;
            bh_arr_each(AstStructMember *, member, s_node->members) {
                if ((*member)->flags & Ast_Flag_Struct_Mem_Used) continue;

                s_type->Struct.mem_count++;

                token_toggle_end((*member)->token);
                bh_arr_push(s_type->Struct.memarr, &bh_table_get(StructMember, s_type->Struct.members, (*member)->token->text));
                token_toggle_end((*member)->token);
            }

            alignment = bh_max(s_node->min_alignment, alignment);
            s_type->Struct.alignment = alignment;

            if (size % alignment != 0) {
                size += alignment - (size % alignment);
            }

            size = bh_max(s_node->min_size, size);
            s_type->Struct.size = size;

            return s_type;
        }

        case Ast_Kind_Enum_Type: {
            AstEnumType* enum_node = (AstEnumType *) type_node;
            if (enum_node->etcache) return enum_node->etcache;

            Type* enum_type = bh_alloc(alloc, sizeof(Type));
            enum_node->etcache = enum_type;

            enum_type->kind = Type_Kind_Enum;
            enum_type->Enum.backing = enum_node->backing_type;
            enum_type->Enum.name = enum_node->name;

            return enum_type;
        }

        case Ast_Kind_Slice_Type: {
            return type_make_slice(alloc, type_build_from_ast(alloc, ((AstSliceType *) type_node)->elem));
        }

        case Ast_Kind_DynArr_Type: {
            return type_make_dynarray(alloc, type_build_from_ast(alloc, ((AstDynArrType *) type_node)->elem));
        }

        case Ast_Kind_VarArg_Type:
            return type_make_varargs(alloc, type_build_from_ast(alloc, ((AstVarArgType *) type_node)->elem));

        case Ast_Kind_Basic_Type:
            return ((AstBasicType *) type_node)->type;

        case Ast_Kind_Type_Alias:
            return type_build_from_ast(alloc, ((AstTypeAlias *) type_node)->to);

        case Ast_Kind_Type_Raw_Alias:
            return ((AstTypeRawAlias *) type_node)->to;

        case Ast_Kind_Poly_Struct_Type:
            // @Cleanup: Replace this with a proper onyx_report_error. - brendanfh 2020/09/14
            assert(("Polymorphic struct used without instantiation.", 0));
            break;

        case Ast_Kind_Poly_Call_Type: {
            AstPolyCallType* pc_type = (AstPolyCallType *) type_node;

            if (!(pc_type->callee && pc_type->callee->kind == Ast_Kind_Poly_Struct_Type)) {
                onyx_report_error(pc_type->token->pos, "Cannot instantiate a concrete type off of a non-polymorphic type.");
                return NULL;
            }

            AstPolyStructType* ps_type = (AstPolyStructType *) pc_type->callee;

            bh_arr(Type *) param_types = NULL;
            bh_arr_new(global_heap_allocator, param_types, bh_arr_length(pc_type->params));
            bh_arr_each(AstType *, ptype, pc_type->params) {
                bh_arr_push(param_types, type_build_from_ast(alloc, *ptype));
            }

            AstStructType* concrete = polymorphic_struct_lookup(ps_type, param_types, pc_type->token->pos);

            bh_arr_free(param_types);

            return type_build_from_ast(alloc, (AstType *) concrete);
        }

        case Ast_Kind_Symbol:
            assert(("symbol node in type expression", 0));
            return NULL;

        default: return NULL;
    }
}

Type* type_build_function_type(bh_allocator alloc, AstFunction* func) {
    u64 param_count = bh_arr_length(func->params);

    Type* func_type = bh_alloc(alloc, sizeof(Type) + sizeof(Type *) * param_count);

    func_type->kind = Type_Kind_Function;
    func_type->Function.param_count = param_count;
    func_type->Function.needed_param_count = 0;
    func_type->Function.return_type = type_build_from_ast(alloc, func->return_type);

    if (param_count > 0) {
        i32 i = 0;
        bh_arr_each(AstParam, param, func->params) {
            if (param->default_value == NULL && param->vararg_kind == VA_Kind_Not_VA)
                func_type->Function.needed_param_count++;
            func_type->Function.params[i++] = param->local->type;
        }
    }

    return func_type;
}

Type* type_make_pointer(bh_allocator alloc, Type* to) {
    Type* ptr_type = bh_alloc_item(alloc, Type);

    ptr_type->kind = Type_Kind_Pointer;
    ptr_type->Pointer.base.flags |= Basic_Flag_Pointer;
    ptr_type->Pointer.base.size = 4;
    ptr_type->Pointer.elem = to;

    return ptr_type;
}

Type* type_make_array(bh_allocator alloc, Type* to, u32 count) {
    Type* arr_type = bh_alloc_item(alloc, Type);

    arr_type->kind = Type_Kind_Array;
    arr_type->Array.count = count;
    arr_type->Array.elem = to;
    arr_type->Array.size = count * type_size_of(to);

    return arr_type;
}

Type* type_make_slice(bh_allocator alloc, Type* of) {
    Type* slice_type = bh_alloc(alloc, sizeof(Type));
    slice_type->kind = Type_Kind_Slice;
    slice_type->Slice.ptr_to_data = type_make_pointer(alloc, of);

    return slice_type;
}

Type* type_make_dynarray(bh_allocator alloc, Type* of) {
    Type* dynarr = bh_alloc(alloc, sizeof(Type));
    dynarr->kind = Type_Kind_DynArray;
    dynarr->DynArray.ptr_to_data = type_make_pointer(alloc, of);

    return dynarr;
}

Type* type_make_varargs(bh_allocator alloc, Type* of) {
    Type* va_type = bh_alloc(alloc, sizeof(Type));
    va_type->kind = Type_Kind_VarArgs;
    va_type->VarArgs.ptr_to_data = type_make_pointer(alloc, of);

    return va_type;
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

        case Type_Kind_Slice: return bh_aprintf(global_scratch_allocator, "[] %s", type_get_name(type->Slice.ptr_to_data->Pointer.elem));
        case Type_Kind_VarArgs: return bh_aprintf(global_scratch_allocator, "..%s", type_get_name(type->VarArgs.ptr_to_data->Pointer.elem));
        case Type_Kind_DynArray: return bh_aprintf(global_scratch_allocator, "[..] %s", type_get_name(type->DynArray.ptr_to_data->Pointer.elem));

        case Type_Kind_Function: {
            char buf[512];
            fori (i, 0, 512) buf[i] = 0;

            strncat(buf, "proc (", 511);
            fori (i, 0, type->Function.param_count) {
                strncat(buf, type_get_name(type->Function.params[i]), 511);
                if (i != type->Function.param_count - 1)
                    strncat(buf, ", ", 511);
            }

            strncat(buf, ") -> ", 511);
            strncat(buf, type_get_name(type->Function.return_type), 511);

            return bh_aprintf(global_scratch_allocator, "%s", buf);
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

b32 type_lookup_member(Type* type, char* member, StructMember* smem) {
    if (type->kind == Type_Kind_Pointer) type = type->Pointer.elem;

    switch (type->kind) {
        case Type_Kind_Struct: {
            TypeStruct* stype = &type->Struct;

            if (!bh_table_has(StructMember, stype->members, member)) return 0;
            *smem = bh_table_get(StructMember, stype->members, member);
            return 1;
        }

        case Type_Kind_VarArgs:
        case Type_Kind_Slice: {
            if (strcmp(member, "data") == 0) {
                smem->idx = 0;
                smem->offset = 0;
                smem->type = type->Slice.ptr_to_data;
                smem->name = "data";
                return 1;
            }
            if (strcmp(member, "count") == 0) {
                smem->idx = 1;
                smem->offset = 4;
                smem->type = &basic_types[Basic_Kind_U32];
                smem->name = "count";
                return 1;
            }

            return 0;
        }

        case Type_Kind_DynArray: {
            if (strcmp(member, "data") == 0) {
                smem->idx = 0;
                smem->offset = 0;
                smem->type = type->DynArray.ptr_to_data;
                smem->name = "data";
                return 1;
            }
            if (strcmp(member, "count") == 0) {
                smem->idx = 1;
                smem->offset = 4;
                smem->type = &basic_types[Basic_Kind_U32];
                smem->name = "count";
                return 1;
            }
            if (strcmp(member, "capacity") == 0) {
                smem->idx = 2;
                smem->offset = 8;
                smem->type = &basic_types[Basic_Kind_U32];
                smem->name = "capacity";
                return 1;
            }
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
            if (idx == 0) {
                smem->idx = 0;
                smem->offset = 0;
                smem->type = type->Slice.ptr_to_data;
                smem->name = "data";
                return 1;
            }
            if (idx == 1) {
                smem->idx = 1;
                smem->offset = 4;
                smem->type = &basic_types[Basic_Kind_U32];
                smem->name = "count";
                return 1;
            }

            return 0;
        }

        case Type_Kind_DynArray: {
            if (idx == 0) {
                smem->idx = 0;
                smem->offset = 0;
                smem->type = type->DynArray.ptr_to_data;
                smem->name = "data";
                return 1;
            }
            if (idx == 1) {
                smem->idx = 1;
                smem->offset = 4;
                smem->type = &basic_types[Basic_Kind_U32];
                smem->name = "count";
                return 1;
            }
            if (idx == 2) {
                smem->idx = 2;
                smem->offset = 8;
                smem->type = &basic_types[Basic_Kind_U32];
                smem->name = "capacity";
                return 1;
            }
        }

        default: return 0;
    }
}

b32 type_struct_is_simple(Type* type) {
    if (type->kind != Type_Kind_Struct) return 0;

    b32 is_simple = 1;
    bh_arr_each(StructMember *, mem, type->Struct.memarr) {
        if ((*mem)->type->kind == Type_Kind_Struct
            || (*mem)->type->kind == Type_Kind_Array
            || (*mem)->type->kind == Type_Kind_Slice
            || (*mem)->type->kind == Type_Kind_DynArray) {
            is_simple = 0;
            break;
        }
    }

    return is_simple;
}

b32 type_is_pointer(Type* type) {
    return type->kind == Type_Kind_Pointer;
}

b32 type_is_rawptr(Type* type) {
    return type->kind == Type_Kind_Basic && type->Basic.kind == Basic_Kind_Rawptr;
}

b32 type_is_array(Type* type) {
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
    return type != NULL && type->kind == Type_Kind_Basic && type->Basic.kind == Basic_Kind_Bool;
}

b32 type_is_small_integer(Type* type) {
    if (type->kind != Type_Kind_Basic) return 0;

    return type->Basic.kind >= Basic_Kind_I8 && type->Basic.kind <= Basic_Kind_U32;
}

b32 type_is_integer(Type* type) {
    if (type->kind != Type_Kind_Basic) return 0;

    return type->Basic.kind >= Basic_Kind_I8 && type->Basic.kind <= Basic_Kind_U64;
}

b32 type_is_numeric(Type* type) {
    if (type->kind == Type_Kind_Enum) return 1;
    if (type->kind != Type_Kind_Basic) return 0;

    return type->Basic.kind >= Basic_Kind_Int_Unsized && type->Basic.kind <= Basic_Kind_F64;
}

b32 type_is_compound(Type* type) {
    return type->kind == Type_Kind_Array
        || type->kind == Type_Kind_Struct;
}

b32 type_is_simd(Type* type) {
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
    if (type_is_pointer(type)) return 1;
    if (type->kind == Type_Kind_Array) return 1;
    if (type->kind == Type_Kind_Slice) return 1;
    if (type->kind == Type_Kind_DynArray) return 1;
    if (type->kind == Type_Kind_VarArgs) return 1;
    return 0;
}

b32 type_is_structlike(Type* type) {
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
    if (type->kind == Type_Kind_Struct)   return 1;
    if (type->kind == Type_Kind_Slice)    return 1;
    if (type->kind == Type_Kind_DynArray) return 1;
    if (type->kind == Type_Kind_VarArgs) return 1;
    return 0;
}

u32 type_structlike_mem_count(Type* type) {
    switch (type->kind) {
        case Type_Kind_Struct:   return type->Struct.mem_count;
        case Type_Kind_Slice:    return 2;
        case Type_Kind_VarArgs:  return 2;
        case Type_Kind_DynArray: return 3;
        default: return 0;
    }
}

u32 type_structlike_is_simple(Type* type) {
    switch (type->kind) {
        case Type_Kind_Struct:   return type_struct_is_simple(type);
        case Type_Kind_Slice:    return 1;
        case Type_Kind_VarArgs:  return 1;
        case Type_Kind_DynArray: return 1;
        default: return 0;
    }
}
