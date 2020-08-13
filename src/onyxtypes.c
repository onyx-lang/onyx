#define BH_DEBUG
#include "onyxtypes.h"
#include "onyxastnodes.h"
#include "onyxutils.h"

// NOTE: These have to be in the same order as Basic
Type basic_types[] = {
    { Type_Kind_Basic, 0, { Basic_Kind_Void,                    0,                       0, 1, "void"   } },

    { Type_Kind_Basic, 0, { Basic_Kind_Bool,   Basic_Flag_Boolean,                       1, 1, "bool"   } },

    { Type_Kind_Basic, 0, { Basic_Kind_I8,     Basic_Flag_Integer,                       1, 1, "i8"     } },
    { Type_Kind_Basic, 0, { Basic_Kind_U8,     Basic_Flag_Integer | Basic_Flag_Unsigned, 1, 1, "u8"     } },
    { Type_Kind_Basic, 0, { Basic_Kind_I16,    Basic_Flag_Integer,                       2, 2, "i16"    } },
    { Type_Kind_Basic, 0, { Basic_Kind_U16,    Basic_Flag_Integer | Basic_Flag_Unsigned, 2, 2, "u16"    } },
    { Type_Kind_Basic, 0, { Basic_Kind_I32,    Basic_Flag_Integer,                       4, 4, "i32"    } },
    { Type_Kind_Basic, 0, { Basic_Kind_U32,    Basic_Flag_Integer | Basic_Flag_Unsigned, 4, 4, "u32"    } },
    { Type_Kind_Basic, 0, { Basic_Kind_I64,    Basic_Flag_Integer,                       8, 8, "i64"    } },
    { Type_Kind_Basic, 0, { Basic_Kind_U64,    Basic_Flag_Integer | Basic_Flag_Unsigned, 8, 8, "u64"    } },

    { Type_Kind_Basic, 0, { Basic_Kind_F32,    Basic_Flag_Float,                         4, 4, "f32"    } },
    { Type_Kind_Basic, 0, { Basic_Kind_F64,    Basic_Flag_Float,                         8, 4, "f64"    } },

    { Type_Kind_Basic, 0, { Basic_Kind_Rawptr, Basic_Flag_Pointer,                       4, 4, "rawptr" } },
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
            if (t2->kind == Type_Kind_Pointer) return 1;
            break;

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
            }

            if (t1->Basic.kind == Basic_Kind_Rawptr && type_is_pointer(t2)) {
                return 1;
            }
            break;

        case Type_Kind_Pointer: {
            if (t2->kind == Type_Kind_Pointer) {
                return types_are_compatible(t1->Pointer.elem, t2->Pointer.elem);
            }

            if (t2->kind == Type_Kind_Basic && t2->Basic.kind == Basic_Kind_Rawptr)
                return 1;

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
            if (t1->Struct.name && t2->Struct.name)
                if (strcmp(t1->Struct.name, t2->Struct.name) == 0) return 1;

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
                fori (i, 0, t1->Function.param_count - 1) {
                    if (!types_are_compatible(t1->Function.params[i], t2->Function.params[i])) return 0;
                }
            }

            return 1;
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
        case Type_Kind_Struct:   return type->Struct.aligment;
        case Type_Kind_Enum:     return type_alignment_of(type->Enum.backing);
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
                fori (i, 0, param_count - 1) {
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
                a_node->count_expr->type = type_build_from_ast(alloc, a_node->count_expr->type_node);

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

            u32 offset = 0;
            u32 alignment = 1, mem_alignment;
            u32 idx = 0;
            bh_arr_each(AstStructMember *, member, s_node->members) {
                (*member)->type = type_build_from_ast(alloc, (*member)->type_node);

                // TODO: Add alignment checking here
                mem_alignment = type_alignment_of((*member)->type);
                if (mem_alignment > alignment) alignment = mem_alignment;
                if (offset % alignment != 0) {
                    offset += alignment - (offset % alignment);
                }

                StructMember smem = {
                    .offset = offset,
                    .type = (*member)->type,
                    .idx = idx,
                };

                token_toggle_end((*member)->token);
                bh_table_put(StructMember, s_type->Struct.members, (*member)->token->text, smem);
                bh_arr_push(s_type->Struct.memarr, &bh_table_get(StructMember, s_type->Struct.members, (*member)->token->text));
                token_toggle_end((*member)->token);

                offset += type_size_of((*member)->type);
                idx++;
            }

            s_type->Struct.aligment = alignment;

            if (offset % alignment != 0) {
                offset += alignment - (offset % alignment);
            }
            s_type->Struct.size = offset;

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

        case Ast_Kind_Basic_Type:
            return ((AstBasicType *) type_node)->type;

        case Ast_Kind_Type_Alias:
            return type_build_from_ast(alloc, ((AstTypeAlias *) type_node)->to);

        case Ast_Kind_Symbol:
            assert(("symbol node in type expression", 0));
            return NULL;

        default:
            assert(("Node is not a type node", 0));
            return NULL;
    }
}

// NOTE: Kinda hacky way of building the functions type
Type* type_build_function_type(bh_allocator alloc, AstFunction* func, AstType* return_type) {
    u64 param_count = 0;
    for (AstLocal* param = func->params;
            param != NULL;
            param = (AstLocal *) param->next)
        param_count++;

    AstFunctionType* old_ftype = (AstFunctionType *) func->type_node;
    Type* func_type = bh_alloc(alloc, sizeof(Type) + sizeof(Type *) * param_count);

    func_type->kind = Type_Kind_Function;
    func_type->Function.param_count = param_count;
    func_type->Function.return_type = type_build_from_ast(alloc, return_type);

    if (param_count > 0) {
        i32 i = 0;
        for (AstLocal* param = func->params;
                param != NULL;
                param = (AstLocal *) param->next) {
            func_type->Function.params[i++] = param->type;
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

        case Type_Kind_Function: return bh_aprintf(global_scratch_allocator, "proc (...) -> %s", type_get_name(type->Function.return_type));

        default: return "unknown";
    }
}

u32 type_get_alignment_log2(Type* type) {
    i32 store_size = type_size_of(type);
    if      (store_size == 1) return 0;
    else if (store_size == 2) return 1;
    else if (store_size == 4) return 2;
    else if (store_size == 8) return 3;
    return 2;
}

b32 type_struct_lookup_member(Type* type, char* member, StructMember* smem) {
    if (!type_is_struct(type)) return 0;
    if (type->kind == Type_Kind_Pointer) type = type->Pointer.elem;

    TypeStruct* stype = &type->Struct;

    if (!bh_table_has(StructMember, stype->members, member)) return 0;
    *smem = bh_table_get(StructMember, stype->members, member);
    return 1;
}

b32 type_struct_is_simple(Type* type) {
    if (type->kind != Type_Kind_Struct) return 0;

    b32 is_simple = 1;
    bh_table_each_start(StructMember, type->Struct.members);
        if (value.type->kind == Type_Kind_Struct
            || value.type->kind == Type_Kind_Array) {
            is_simple = 0;
            break;
        }
    bh_table_each_end;

    return is_simple;
}

b32 type_is_pointer(Type* type) {
    return (type->kind == Type_Kind_Pointer)
        || (type->kind == Type_Kind_Array);
}

b32 type_is_rawptr(Type* type) {
    return type->kind == Type_Kind_Basic && type->Basic.kind == Basic_Kind_Rawptr;
}

b32 type_is_array(Type* type) {
    return type->kind == Type_Kind_Array;
}

b32 type_is_struct(Type* type) {
    if (type->kind == Type_Kind_Struct) return 1;
    if (type->kind == Type_Kind_Pointer && type->Pointer.elem->kind == Type_Kind_Struct) return 1;
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
    if (type->kind != Type_Kind_Basic) return 0;

    return type->Basic.kind >= Basic_Kind_I8 && type->Basic.kind <= Basic_Kind_F64;
}

b32 type_is_compound(Type* type) {
    return type->kind == Type_Kind_Array
        || type->kind == Type_Kind_Struct;
}

b32 type_results_in_void(Type* type) {
    return (type == NULL)
        || (type->kind == Type_Kind_Basic && type->Basic.kind == Basic_Kind_Void)
        || (   (type->kind == Type_Kind_Function)
            && (type->Function.return_type->kind == Type_Kind_Basic)
            && (type->Function.return_type->Basic.kind == Basic_Kind_Void));
}
