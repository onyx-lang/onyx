#ifndef ONYXTYPES_H
#define ONYXTYPES_H

#include "bh.h"

enum BasicKind {
    Basic_Kind_Void,

    Basic_Kind_Bool,

    Basic_Kind_Int_Unsized,
    Basic_Kind_I8,
    Basic_Kind_U8,
    Basic_Kind_I16,
    Basic_Kind_U16,
    Basic_Kind_I32,
    Basic_Kind_U32,
    Basic_Kind_I64,
    Basic_Kind_U64,

    Basic_Kind_Float_Unsized,
    Basic_Kind_F32,
    Basic_Kind_F64,

    Basic_Kind_Rawptr,

    Basic_Kind_I8X16,
    Basic_Kind_I16X8,
    Basic_Kind_I32X4,
    Basic_Kind_I64X2,
    Basic_Kind_F32X4,
    Basic_Kind_F64X2,
    Basic_Kind_V128,

    Basic_Kind_Type_Index,

    Basic_Kind_Count,
};

enum BasicFlag {
    Basic_Flag_Boolean          = BH_BIT(0),
    Basic_Flag_Integer          = BH_BIT(1),
    Basic_Flag_Unsigned         = BH_BIT(2),
    Basic_Flag_Float            = BH_BIT(3),
    Basic_Flag_Pointer          = BH_BIT(4),

    Basic_Flag_SIMD             = BH_BIT(5),

    Basic_Flag_Type_Index       = BH_BIT(6),

    Basic_Flag_Numeric          = Basic_Flag_Integer | Basic_Flag_Float,
    Basic_Flag_Ordered          = Basic_Flag_Integer | Basic_Flag_Float | Basic_Flag_Pointer,
    Basic_Flag_Equality         = Basic_Flag_Ordered | Basic_Flag_Type_Index | Basic_Flag_Boolean,
    Basic_Flag_Constant_Type    = Basic_Flag_Boolean | Basic_Flag_Numeric | Basic_Flag_Pointer,
    Basic_Flag_Numeric_Ordered  = Basic_Flag_Numeric | Basic_Flag_Ordered,
};

typedef struct TypeBasic {
    enum BasicKind kind;
    u32 flags;
    u32 size, alignment; // NOTE: In bytes
    const char* name;
} TypeBasic;

// NOTE: Forward declaration for some of the types below
typedef struct Type Type;

typedef struct StructMember {
    u32 offset, idx;
    Type *type;

    // NOTE: Since the name is stored here, it may not be necessary to,
    // store a hash table of struct members. There realistically will not
    // be many struct members, and iterating through an array would be
    // easier and less costly.                  - brendanfh 2020/09/17
    char *name;

    struct AstTyped** initial_value;
    b32 included_through_use : 1;
    b32 used : 1;
} StructMember;

typedef struct TypeWithOffset TypeWithOffset;
struct TypeWithOffset {
    Type* type;
    u32   offset;
};

#define TYPE_KINDS \
    TYPE_KIND(Basic, TypeBasic)                                   \
    TYPE_KIND(Pointer, struct { TypeBasic base; Type *elem; })    \
    TYPE_KIND(Function, struct {                                  \
        Type *return_type;                                        \
        u16 param_count;                                          \
        u16 needed_param_count;                                   \
        i16 vararg_arg_pos;                                       \
        Type* params[];                                           \
    })                                                            \
    TYPE_KIND(Struct, struct {                                    \
        char* name;                                               \
        u32 size;                                                 \
        u16 alignment, mem_count;                                 \
        bh_table(StructMember) members;                           \
        bh_arr(StructMember *) memarr;                            \
        bh_arr(struct AstPolySolution) poly_sln;                  \
        bh_arr(TypeWithOffset) linear_members;                    \
        struct AstType *constructed_from;                         \
    })                                                            \
    TYPE_KIND(Compound, struct {                                  \
        u32 count;                                                \
        u32 size;                                                 \
        bh_arr(TypeWithOffset) linear_members;                    \
        Type* types[];                                            \
    })                                                            \
    TYPE_KIND(Array, struct { u32 size; u32 count; Type *elem; }) \
    TYPE_KIND(Slice, struct { Type *ptr_to_data; })               \
    TYPE_KIND(DynArray, struct { Type *ptr_to_data; })            \
    TYPE_KIND(VarArgs, struct { Type *ptr_to_data; })             \
    TYPE_KIND(Enum, struct {                                      \
        char* name;                                               \
        Type* backing;                                            \
        b32   is_flags;                                           \
    })


typedef enum TypeKind {
    Type_Kind_Invalid,

#define TYPE_KIND(k, ...) Type_Kind_##k,
    TYPE_KINDS
#undef TYPE_KIND

    Type_Kind_Count,
} TypeKind;

#define TYPE_KIND(k, ...) typedef __VA_ARGS__ Type ## k;
    TYPE_KINDS
#undef TYPE_KIND

enum TypeFlag {
    Type_Flag_Default
};

struct Type {
    TypeKind kind;

    u32 id;
    u32 flags;

    // NOTE(Brendan Hansen): The abstract syntax tree node used to create
    // the type. Primarily used to look up symbols in scopes that are embedded
    // in the type.
    struct AstType* ast_type;

    union {
#define TYPE_KIND(k, ...) Type##k k;
        TYPE_KINDS
#undef TYPE_KIND
    };
};

extern bh_imap type_map;

extern Type basic_types[];

struct AstType;
struct AstFunction;
struct AstCompound;

void types_init();
void types_dump_type_info();

b32 types_are_compatible(Type* t1, Type* t2);
u32 type_size_of(Type* type);
u32 type_alignment_of(Type* type);
Type* type_build_from_ast(bh_allocator alloc, struct AstType* type_node);

Type* type_build_function_type(bh_allocator alloc, struct AstFunction* func);
Type* type_build_compound_type(bh_allocator alloc, struct AstCompound* compound);

Type* type_make_pointer(bh_allocator alloc, Type* to);
Type* type_make_array(bh_allocator alloc, Type* to, u32 count);
Type* type_make_slice(bh_allocator alloc, Type* of);
Type* type_make_dynarray(bh_allocator alloc, Type* of);
Type* type_make_varargs(bh_allocator alloc, Type* of);

void build_linear_types_with_offset(Type* type, bh_arr(TypeWithOffset)* pdest, u32 offset);

const char* type_get_unique_name(Type* type);
const char* type_get_name(Type* type);
u32 type_get_alignment_log2(Type* type);

b32 type_lookup_member(Type* type, char* member, StructMember* smem);
b32 type_lookup_member_by_idx(Type* type, i32 idx, StructMember* smem);

i32 type_linear_member_count(Type* type);
b32 type_linear_member_lookup(Type* type, i32 idx, TypeWithOffset* two);
i32 type_get_idx_of_linear_member_with_offset(Type* type, u32 offset);

b32 type_struct_is_simple(Type* type);

b32 type_is_pointer(Type* type);
b32 type_is_rawptr(Type* type);
b32 type_is_array(Type* tyoe);
b32 type_is_struct(Type* type);
b32 type_is_bool(Type* type);
b32 type_is_small_integer(Type* type);
b32 type_is_integer(Type* type);
b32 type_is_numeric(Type* type);
b32 type_is_compound(Type* type);
b32 type_is_simd(Type* type);
b32 type_results_in_void(Type* type);
b32 type_is_array_accessible(Type* type);
b32 type_is_structlike(Type* type);
b32 type_is_structlike_strict(Type* type);
u32 type_structlike_mem_count(Type* type);
u32 type_structlike_is_simple(Type* type);
b32 type_is_sl_constructable(Type* type);
b32 type_struct_constructed_from_poly_struct(Type* struct_type, struct AstType* from);

#endif // #ifndef ONYX_TYPES
