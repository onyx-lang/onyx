#include "onyxtypes.h"
#include "onyxastnodes.h"
#include "onyxutils.h"

// NOTE: These have to be in the same order as Basic
Type basic_types[] = {
    { Type_Kind_Basic, { Basic_Kind_Void,                    0,                       0, "void"   } },

    { Type_Kind_Basic, { Basic_Kind_Bool,   Basic_Flag_Boolean,                       1, "bool"   } },

    { Type_Kind_Basic, { Basic_Kind_I8,     Basic_Flag_Integer,                       1, "i8"     } },
    { Type_Kind_Basic, { Basic_Kind_U8,     Basic_Flag_Integer | Basic_Flag_Unsigned, 1, "u8"     } },
    { Type_Kind_Basic, { Basic_Kind_I16,    Basic_Flag_Integer,                       2, "i16"    } },
    { Type_Kind_Basic, { Basic_Kind_U16,    Basic_Flag_Integer | Basic_Flag_Unsigned, 2, "u16"    } },
    { Type_Kind_Basic, { Basic_Kind_I32,    Basic_Flag_Integer,                       4, "i32"    } },
    { Type_Kind_Basic, { Basic_Kind_U32,    Basic_Flag_Integer | Basic_Flag_Unsigned, 4, "u32"    } },
    { Type_Kind_Basic, { Basic_Kind_I64,    Basic_Flag_Integer,                       8, "i64"    } },
    { Type_Kind_Basic, { Basic_Kind_U64,    Basic_Flag_Integer | Basic_Flag_Unsigned, 8, "u64"    } },

    { Type_Kind_Basic, { Basic_Kind_F32,    Basic_Flag_Float,                         4, "f32"    } },
    { Type_Kind_Basic, { Basic_Kind_F64,    Basic_Flag_Float,                         8, "f64"    } },

    { Type_Kind_Basic, { Basic_Kind_Rawptr, Basic_Flag_Pointer,                       4, "rawptr" } },
};

b32 types_are_compatible(Type* t1, Type* t2) {
    // NOTE: If they are pointing to the same thing,
    // it is safe to assume they are the same type
    if (t1 == t2) return 1;
    if (t1 == NULL || t2 == NULL) return 0;

    switch (t1->kind) {
        case Type_Kind_Basic:
            if (t2->kind == Type_Kind_Basic) {
                // HACK: Not sure if this is right way to check this?
                return t1 == t2;
            }
            break;

        case Type_Kind_Pointer:
            if (t2->kind == Type_Kind_Pointer) {
                return types_are_compatible(t1->Pointer.elem, t2->Pointer.elem);
            }
            break;

        default:
            assert(("Invalid type", 0));
            break;
    }

    return 0;
}

Type* type_build_from_ast(bh_allocator alloc, AstType* type_node) {
    if (type_node == NULL) return NULL;

    switch (type_node->kind) {
        case Ast_Kind_Pointer_Type: {
            Type* ptr_type = bh_alloc_item(alloc, Type);

            ptr_type->kind = Type_Kind_Pointer;
            ptr_type->Pointer.base.flags |= Basic_Flag_Pointer;
            ptr_type->Pointer.base.size = 4;
            ptr_type->Pointer.elem = type_build_from_ast(alloc, ((AstPointerType *) type_node)->elem);

            return (Type *) ptr_type;
        }

        case Ast_Kind_Basic_Type:
            return ((AstBasicType *) type_node)->type;

        default:
            assert(("Node is not a type node", 0));
            return NULL;
    }
}

const char* type_get_name(Type* type) {
    if (type == NULL) return "unknown";

    switch (type->kind) {
        case Type_Kind_Basic: return type->Basic.name;
        case Type_Kind_Pointer: return bh_aprintf(global_scratch_allocator, "^%s", type_get_name(type->Pointer.elem));
        default: return "unknown";
    }
}
