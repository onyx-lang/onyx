#ifndef ONYXTYPES_H
#define ONYXTYPES_H

#include "bh.h"

#define POINTER_SIZE 4

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
    Basic_Flag_Multi_Pointer    = BH_BIT(7),

    Basic_Flag_SIMD             = BH_BIT(5),

    Basic_Flag_Type_Index       = BH_BIT(6),

    Basic_Flag_Numeric          = Basic_Flag_Integer | Basic_Flag_Float,
    Basic_Flag_Ordered          = Basic_Flag_Integer | Basic_Flag_Float | Basic_Flag_Pointer | Basic_Flag_Multi_Pointer,
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
    struct OnyxToken* token;

    struct AstTyped** initial_value;
    i32 use_through_pointer_index;
    b32 included_through_use : 1;
    b32 used : 1;
    b32 use_processed : 1;

    bh_arr(struct AstTyped *) meta_tags;

    // Used in reporting errors and symbol information.
    struct AstStructMember *member_node;
} StructMember;

typedef struct TypeWithOffset TypeWithOffset;
struct TypeWithOffset {
    Type* type;
    u32   offset;
};

typedef enum StructProcessingStatus {
    SPS_Start,
    SPS_Members_Done,
    SPS_Uses_Done,
} StructProcessingStatus;

typedef struct UnionVariant {
    char *name;
    Type *type;
    u32 tag_value;
    bh_arr(struct AstTyped *) meta_tags;
    struct OnyxToken *token;
} UnionVariant;

#define TYPE_KINDS \
    TYPE_KIND(Basic, TypeBasic)                                   \
    TYPE_KIND(Pointer, struct {                                   \
        TypeBasic base;                                           \
        Type *elem;                                               \
    })                                                            \
    TYPE_KIND(MultiPointer, struct {                              \
        TypeBasic base;                                           \
        Type *elem;                                               \
    })                                                            \
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
        Table(StructMember *) members;                            \
        bh_arr(StructMember *) memarr;                            \
        bh_arr(struct AstPolySolution) poly_sln;                  \
        struct AstType *constructed_from;                         \
        bh_arr(struct AstTyped *) meta_tags;                      \
        StructProcessingStatus status;                            \
        struct Scope* scope;                                      \
    })                                                            \
    TYPE_KIND(PolyStruct, struct {                                \
        char* name;                                               \
        bh_arr(struct AstTyped *) meta_tags;                      \
        struct Scope* scope;                                      \
    })                                                            \
    TYPE_KIND(Compound, struct {                                  \
        u32 count;                                                \
        u32 size;                                                 \
        bh_arr(TypeWithOffset) linear_members;                    \
        Type* types[];                                            \
    })                                                            \
    TYPE_KIND(Array, struct { Type* elem; u32 size; u32 count; }) \
    TYPE_KIND(Slice, struct { Type *elem; struct Scope *scope; }) \
    TYPE_KIND(DynArray, struct {                                  \
        Type *elem;                                               \
        struct Scope *scope;                                      \
    })                                                            \
    TYPE_KIND(VarArgs, struct { Type *elem; })                    \
    TYPE_KIND(Enum, struct {                                      \
        char* name;                                               \
        Type* backing;                                            \
        b32   is_flags;                                           \
    })                                                            \
    TYPE_KIND(Distinct, struct {                                  \
        char* name;                                               \
        Type* base_type;                                          \
        struct Scope* scope;                                      \
    })                                                            \
    TYPE_KIND(Union, struct {                                     \
        u32 size;                                                 \
        u32 alignment;                                            \
        char* name;                                               \
        Type* tag_type;                                           \
        Table(UnionVariant *) variants;                           \
        bh_arr(UnionVariant *) variants_ordered;                  \
        bh_arr(struct AstPolySolution) poly_sln;                  \
        struct AstType *constructed_from;                         \
        bh_arr(struct AstTyped *) meta_tags;                      \
        StructProcessingStatus status;                            \
        struct Scope* scope;                                      \
    })                                                            \
    TYPE_KIND(PolyUnion, struct {                                 \
        char* name;                                               \
        bh_arr(struct AstTyped *) meta_tags;                      \
        struct Scope* scope;                                      \
    })                                                            \



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

struct Context;
struct AstType;
struct AstFunction;
struct AstCompound;
struct AstStructLiteral;

void types_init(struct Context *context);
void types_dump_type_info(struct Context *context);
Type* type_lookup_by_id(struct Context *context, u32 id);

b32 types_are_compatible(struct Context *context, Type* t1, Type* t2);
u32 type_size_of(Type* type);
u32 type_alignment_of(Type* type);
Type* type_build_from_ast(struct Context *context, struct AstType* type_node);
Type* type_build_implicit_type_of_struct_literal(struct Context *context, struct AstStructLiteral* lit, b32 is_query);

Type* type_build_function_type(struct Context *context, struct AstFunction* func);
Type* type_build_compound_type(struct Context *context, struct AstCompound* compound);

Type* type_make_pointer(struct Context *context, Type* to);
Type* type_make_multi_pointer(struct Context *context, Type* to);
Type* type_make_array(struct Context *context, Type* to, u32 count);
Type* type_make_slice(struct Context *context, Type* of);
Type* type_make_dynarray(struct Context *context, Type* of);
Type* type_make_varargs(struct Context *context, Type* of);
Type* type_make_optional(struct Context *context, Type* of);

void build_linear_types_with_offset(struct Context *context, Type* type, bh_arr(TypeWithOffset)* pdest, u32 offset);
b32  type_struct_member_apply_use(struct Context *context, Type *s_type, StructMember *smem);

const char* type_get_unique_name(struct Context *context, Type* type);
const char* type_get_name(struct Context *context, Type* type);
u32 type_get_alignment_log2(Type* type);
Type* type_get_contained_type(Type* type);

b32 type_is_ready_for_lookup(Type* type);
b32 type_lookup_member(struct Context *context, Type* type, char* member, StructMember* smem);
b32 type_lookup_member_by_idx(struct Context *context, Type* type, i32 idx, StructMember* smem);

i32 type_linear_member_count(Type* type);
b32 type_linear_member_lookup(struct Context *context, Type* type, i32 idx, TypeWithOffset* two);
i32 type_get_idx_of_linear_member_with_offset(Type* type, u32 offset);

b32 type_struct_is_simple(Type* type);

b32 type_is_pointer(Type* type);
b32 type_is_multi_pointer(Type* type);
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
b32 type_should_be_passed_like_a_struct(Type *type);
b32 type_is_sl_constructable(Type* type);
b32 type_constructed_from_poly(Type* base, struct AstType* from);
Type* type_struct_is_just_one_basic_value(Type *type);
u32 type_union_get_variant_count(Type *type);
UnionVariant* type_lookup_union_variant_by_idx(Type* type, i32 idx);
UnionVariant* type_lookup_union_variant_by_name(Type* type, char *name);

#endif // #ifndef ONYX_TYPES
