#ifndef ONYXASTNODES_H
#define ONYXASTNODES_H

#include "onyxlex.h"
#include "onyxtypes.h"

typedef struct AstNode AstNode;
typedef struct AstTyped AstTyped;

typedef struct AstBinaryOp AstBinaryOp;
typedef struct AstUnaryOp AstUnaryOp;
typedef struct AstNumLit AstNumLit;
typedef struct AstStrLit AstStrLit;
typedef struct AstLocal AstLocal;
typedef struct AstCall AstCall;
typedef struct AstIntrinsicCall AstIntrinsicCall;
typedef struct AstArgument AstArgument;
typedef struct AstAddressOf AstAddressOf;
typedef struct AstDereference AstDereference;
typedef struct AstArrayAccess AstArrayAccess;
typedef struct AstFieldAccess AstFieldAccess;
typedef struct AstSizeOf AstSizeOf;
typedef struct AstAlignOf AstAlignOf;
typedef struct AstFileContents AstFileContents;
typedef struct AstStructLiteral AstStructLiteral;
typedef struct AstArrayLiteral AstArrayLiteral;
typedef struct AstRangeLiteral AstRangeLiteral;

typedef struct AstDirectiveSolidify AstDirectiveSolidify;

typedef struct AstReturn AstReturn;
typedef struct AstJump AstJump;
typedef struct AstUse AstUse;

typedef struct AstBlock AstBlock;
typedef struct AstIfWhile AstIfWhile;
typedef struct AstFor AstFor;
typedef struct AstDefer AstDefer;
typedef struct AstSwitchCase AstSwitchCase;
typedef struct AstSwitch AstSwitch;

typedef struct AstType AstType;
typedef struct AstBasicType AstBasicType;
typedef struct AstPointerType AstPointerType;
typedef struct AstFunctionType AstFunctionType;
typedef struct AstArrayType AstArrayType;
typedef struct AstSliceType AstSliceType;
typedef struct AstDynArrType AstDynArrType;
typedef struct AstVarArgType AstVarArgType;
typedef struct AstStructType AstStructType;
typedef struct AstStructMember AstStructMember;
typedef struct AstPolyStructType AstPolyStructType;
typedef struct AstPolyStructParam AstPolyStructParam;
typedef struct AstPolyCallType AstPolyCallType;
typedef struct AstEnumType AstEnumType;
typedef struct AstEnumValue AstEnumValue;
typedef struct AstTypeAlias AstTypeAlias;
typedef struct AstTypeRawAlias AstTypeRawAlias;

typedef struct AstBinding AstBinding;
typedef struct AstMemRes AstMemRes;
typedef struct AstInclude AstInclude;
typedef struct AstUsePackage AstUsePackage;
typedef struct AstAlias AstAlias;
typedef struct AstGlobal AstGlobal;
typedef struct AstParam AstParam;
typedef struct AstFunction AstFunction;
typedef struct AstOverloadedFunction AstOverloadedFunction;

typedef struct AstPolyParam AstPolyParam;
typedef struct AstPolySolution AstPolySolution;
typedef struct AstPolyProc AstPolyProc;

typedef struct AstPackage AstPackage;
typedef struct Package Package;

typedef struct Scope {
    struct Scope *parent;
    OnyxFilePos created_at;
    bh_table(AstNode *) symbols;
} Scope;

extern Scope* scope_create(bh_allocator a, Scope* parent, OnyxFilePos created_at);


typedef enum AstKind {
    Ast_Kind_Error,
    Ast_Kind_Program,
    Ast_Kind_Package,
    Ast_Kind_Load_File,
    Ast_Kind_Load_Path,
    Ast_Kind_Use_Package,
    Ast_Kind_Alias,
    Ast_Kind_Memres,

    Ast_Kind_Binding,
    Ast_Kind_Function,
    Ast_Kind_Overloaded_Function,
    Ast_Kind_Polymorphic_Proc,
    Ast_Kind_Block,
    Ast_Kind_Local_Group,
    Ast_Kind_Local,
    Ast_Kind_Global,
    Ast_Kind_Symbol,

    Ast_Kind_Unary_Op,
    Ast_Kind_Binary_Op,

    Ast_Kind_Type_Start,
    Ast_Kind_Type,
    Ast_Kind_Basic_Type,
    Ast_Kind_Pointer_Type,
    Ast_Kind_Function_Type,
    Ast_Kind_Array_Type,
    Ast_Kind_Slice_Type,
    Ast_Kind_DynArr_Type,
    Ast_Kind_VarArg_Type,
    Ast_Kind_Struct_Type,
    Ast_Kind_Poly_Struct_Type,
    Ast_Kind_Poly_Call_Type,
    Ast_Kind_Enum_Type,
    Ast_Kind_Type_Alias,
    Ast_Kind_Type_Raw_Alias,
    Ast_Kind_Type_End,

    Ast_Kind_Struct_Member,
    Ast_Kind_Enum_Value,

    Ast_Kind_NumLit,
    Ast_Kind_StrLit,
    Ast_Kind_Param,
    Ast_Kind_Argument,
    Ast_Kind_Call,
    Ast_Kind_Intrinsic_Call,
    Ast_Kind_Return,
    Ast_Kind_Address_Of,
    Ast_Kind_Dereference,
    Ast_Kind_Array_Access,
    Ast_Kind_Slice,
    Ast_Kind_Field_Access,
    Ast_Kind_Pipe,
    Ast_Kind_Range_Literal,
    Ast_Kind_Size_Of,
    Ast_Kind_Align_Of,
    Ast_Kind_File_Contents,
    Ast_Kind_Struct_Literal,
    Ast_Kind_Array_Literal,

    Ast_Kind_If,
    Ast_Kind_For,
    Ast_Kind_While,
    Ast_Kind_Jump,
    Ast_Kind_Use,
    Ast_Kind_Defer,
    Ast_Kind_Switch,
    Ast_Kind_Switch_Case,

    Ast_Kind_Directive_Solidify,

    Ast_Kind_Count
} AstKind;

// NOTE: Some of these flags will overlap since there are
// only 32-bits of flags to play with
typedef enum AstFlags {
    // Top-level flags
    Ast_Flag_Exported              = BH_BIT(1),
    Ast_Flag_Foreign               = BH_BIT(2),
    Ast_Flag_Const                 = BH_BIT(3),
    Ast_Flag_Comptime              = BH_BIT(4),
    Ast_Flag_Private_Package       = BH_BIT(5),
    Ast_Flag_Private_File          = BH_BIT(6),

    // Global flags
    Ast_Flag_Global_Stack_Top      = BH_BIT(7),
    Ast_Flag_Global_Stack_Base     = BH_BIT(8),

    // Function flags
    Ast_Flag_Intrinsic             = BH_BIT(10),
    Ast_Flag_Function_Used         = BH_BIT(11),

    // Expression flags
    Ast_Flag_Expr_Ignored          = BH_BIT(13),
    Ast_Flag_Param_Use             = BH_BIT(14),
    Ast_Flag_Address_Taken         = BH_BIT(15),

    // Type flags
    Ast_Flag_Type_Is_Resolved      = BH_BIT(16),

    // Enum flags
    Ast_Flag_Enum_Is_Flags         = BH_BIT(17),

    // Struct flags
    Ast_Flag_Struct_Is_Union       = BH_BIT(18),

    Ast_Flag_No_Clone              = BH_BIT(19),

    Ast_Flag_Cannot_Take_Addr      = BH_BIT(20),

    Ast_Flag_Struct_Mem_Used       = BH_BIT(21),

    // HACK: NullProcHack
    Ast_Flag_Proc_Is_Null          = BH_BIT(22),

    Ast_Flag_From_Polymorphism     = BH_BIT(23),
} AstFlags;

typedef enum UnaryOp {
    Unary_Op_Negate,
    Unary_Op_Not,
    Unary_Op_Bitwise_Not,
    Unary_Op_Cast,
    Unary_Op_Auto_Cast,
} UnaryOp;

typedef enum BinaryOp {
    Binary_Op_Add             = 0,
    Binary_Op_Minus           = 1,
    Binary_Op_Multiply        = 2,
    Binary_Op_Divide          = 3,
    Binary_Op_Modulus         = 4,

    Binary_Op_Equal           = 5,
    Binary_Op_Not_Equal       = 6,
    Binary_Op_Less            = 7,
    Binary_Op_Less_Equal      = 8,
    Binary_Op_Greater         = 9,
    Binary_Op_Greater_Equal   = 10,

    Binary_Op_And             = 11,
    Binary_Op_Or              = 12,
    Binary_Op_Xor             = 13,
    Binary_Op_Shl             = 14,
    Binary_Op_Shr             = 15,
    Binary_Op_Sar             = 16,

    Binary_Op_Bool_And        = 17,
    Binary_Op_Bool_Or         = 18,

    Binary_Op_Assign_Start    = 19,
    Binary_Op_Assign          = 20,
    Binary_Op_Assign_Add      = 21,
    Binary_Op_Assign_Minus    = 22,
    Binary_Op_Assign_Multiply = 23,
    Binary_Op_Assign_Divide   = 24,
    Binary_Op_Assign_Modulus  = 25,
    Binary_Op_Assign_And      = 26,
    Binary_Op_Assign_Or       = 27,
    Binary_Op_Assign_Xor      = 28,
    Binary_Op_Assign_Shl      = 29,
    Binary_Op_Assign_Shr      = 30,
    Binary_Op_Assign_Sar      = 31,
    Binary_Op_Assign_End      = 32,

    Binary_Op_Pipe            = 33,
    Binary_Op_Range           = 34,

    Binary_Op_Count
} BinaryOp;

extern const char *binaryop_string[Binary_Op_Count];

typedef enum OnyxIntrinsic {
    ONYX_INTRINSIC_UNDEFINED,

    ONYX_INTRINSIC_MEMORY_SIZE, ONYX_INTRINSIC_MEMORY_GROW,

    ONYX_INTRINSIC_I32_CLZ,   ONYX_INTRINSIC_I32_CTZ, ONYX_INTRINSIC_I32_POPCNT,
    ONYX_INTRINSIC_I32_AND,   ONYX_INTRINSIC_I32_OR,  ONYX_INTRINSIC_I32_XOR,
    ONYX_INTRINSIC_I32_SHL,   ONYX_INTRINSIC_I32_SLR, ONYX_INTRINSIC_I32_SAR,
    ONYX_INTRINSIC_I32_ROTL,  ONYX_INTRINSIC_I32_ROTR,

    ONYX_INTRINSIC_I64_CLZ,   ONYX_INTRINSIC_I64_CTZ, ONYX_INTRINSIC_I64_POPCNT,
    ONYX_INTRINSIC_I64_AND,   ONYX_INTRINSIC_I64_OR,  ONYX_INTRINSIC_I64_XOR,
    ONYX_INTRINSIC_I64_SHL,   ONYX_INTRINSIC_I64_SLR, ONYX_INTRINSIC_I64_SAR,
    ONYX_INTRINSIC_I64_ROTL,  ONYX_INTRINSIC_I64_ROTR,

    ONYX_INTRINSIC_F32_ABS,   ONYX_INTRINSIC_F32_SQRT,
    ONYX_INTRINSIC_F32_CEIL,  ONYX_INTRINSIC_F32_FLOOR,
    ONYX_INTRINSIC_F32_TRUNC, ONYX_INTRINSIC_F32_NEAREST,
    ONYX_INTRINSIC_F32_MIN,   ONYX_INTRINSIC_F32_MAX,
    ONYX_INTRINSIC_F32_COPYSIGN,

    ONYX_INTRINSIC_F64_ABS,   ONYX_INTRINSIC_F64_SQRT,
    ONYX_INTRINSIC_F64_CEIL,  ONYX_INTRINSIC_F64_FLOOR,
    ONYX_INTRINSIC_F64_TRUNC, ONYX_INTRINSIC_F64_NEAREST,
    ONYX_INTRINSIC_F64_MIN,   ONYX_INTRINSIC_F64_MAX,
    ONYX_INTRINSIC_F64_COPYSIGN,



    ONYX_INTRINSIC_V128_CONST,
    ONYX_INTRINSIC_I8X16_CONST, ONYX_INTRINSIC_I16X8_CONST,
    ONYX_INTRINSIC_I32X4_CONST, ONYX_INTRINSIC_I64X2_CONST,
    ONYX_INTRINSIC_F32X4_CONST, ONYX_INTRINSIC_F64X2_CONST,
    ONYX_INTRINSIC_I8X16_SHUFFLE,

    ONYX_INTRINSIC_I8X16_EXTRACT_LANE_S, ONYX_INTRINSIC_I8X16_EXTRACT_LANE_U, ONYX_INTRINSIC_I8X16_REPLACE_LANE,
    ONYX_INTRINSIC_I16X8_EXTRACT_LANE_S, ONYX_INTRINSIC_I16X8_EXTRACT_LANE_U, ONYX_INTRINSIC_I16X8_REPLACE_LANE,
    ONYX_INTRINSIC_I32X4_EXTRACT_LANE, ONYX_INTRINSIC_I32X4_REPLACE_LANE,
    ONYX_INTRINSIC_I64X2_EXTRACT_LANE, ONYX_INTRINSIC_I64X2_REPLACE_LANE,
    ONYX_INTRINSIC_F32X4_EXTRACT_LANE, ONYX_INTRINSIC_F32X4_REPLACE_LANE,
    ONYX_INTRINSIC_F64X2_EXTRACT_LANE, ONYX_INTRINSIC_F64X2_REPLACE_LANE,

    ONYX_INTRINSIC_I8X16_SWIZZLE,
    ONYX_INTRINSIC_I8X16_SPLAT, ONYX_INTRINSIC_I16X8_SPLAT,
    ONYX_INTRINSIC_I32X4_SPLAT, ONYX_INTRINSIC_I64X2_SPLAT,
    ONYX_INTRINSIC_F32X4_SPLAT, ONYX_INTRINSIC_F64X2_SPLAT,

    ONYX_INTRINSIC_I8X16_EQ, ONYX_INTRINSIC_I8X16_NEQ,
    ONYX_INTRINSIC_I8X16_LT_S, ONYX_INTRINSIC_I8X16_LT_U,
    ONYX_INTRINSIC_I8X16_GT_S, ONYX_INTRINSIC_I8X16_GT_U,
    ONYX_INTRINSIC_I8X16_LE_S, ONYX_INTRINSIC_I8X16_LE_U,
    ONYX_INTRINSIC_I8X16_GE_S, ONYX_INTRINSIC_I8X16_GE_U,

    ONYX_INTRINSIC_I16X8_EQ, ONYX_INTRINSIC_I16X8_NEQ,
    ONYX_INTRINSIC_I16X8_LT_S, ONYX_INTRINSIC_I16X8_LT_U,
    ONYX_INTRINSIC_I16X8_GT_S, ONYX_INTRINSIC_I16X8_GT_U,
    ONYX_INTRINSIC_I16X8_LE_S, ONYX_INTRINSIC_I16X8_LE_U,
    ONYX_INTRINSIC_I16X8_GE_S, ONYX_INTRINSIC_I16X8_GE_U,

    ONYX_INTRINSIC_I32X4_EQ, ONYX_INTRINSIC_I32X4_NEQ,
    ONYX_INTRINSIC_I32X4_LT_S, ONYX_INTRINSIC_I32X4_LT_U,
    ONYX_INTRINSIC_I32X4_GT_S, ONYX_INTRINSIC_I32X4_GT_U,
    ONYX_INTRINSIC_I32X4_LE_S, ONYX_INTRINSIC_I32X4_LE_U,
    ONYX_INTRINSIC_I32X4_GE_S, ONYX_INTRINSIC_I32X4_GE_U,

    ONYX_INTRINSIC_F32X4_EQ, ONYX_INTRINSIC_F32X4_NEQ,
    ONYX_INTRINSIC_F32X4_LT, ONYX_INTRINSIC_F32X4_GT,
    ONYX_INTRINSIC_F32X4_LE, ONYX_INTRINSIC_F32X4_GE,

    ONYX_INTRINSIC_F64X2_EQ, ONYX_INTRINSIC_F64X2_NEQ,
    ONYX_INTRINSIC_F64X2_LT, ONYX_INTRINSIC_F64X2_GT,
    ONYX_INTRINSIC_F64X2_LE, ONYX_INTRINSIC_F64X2_GE,

    ONYX_INTRINSIC_V128_NOT, ONYX_INTRINSIC_V128_AND, ONYX_INTRINSIC_V128_ANDNOT,
    ONYX_INTRINSIC_V128_OR, ONYX_INTRINSIC_V128_XOR, ONYX_INTRINSIC_V128_BITSELECT,

    ONYX_INTRINSIC_I8X16_ABS, ONYX_INTRINSIC_I8X16_NEG,
    ONYX_INTRINSIC_I8X16_ANY_TRUE, ONYX_INTRINSIC_I8X16_ALL_TRUE,
    ONYX_INTRINSIC_I8X16_BITMASK,
    ONYX_INTRINSIC_I8X16_NARROW_I16X8_S, ONYX_INTRINSIC_I8X16_NARROW_I16X8_U,
    ONYX_INTRINSIC_I8X16_SHL, ONYX_INTRINSIC_I8X16_SHR_S, ONYX_INTRINSIC_I8X16_SHR_U,
    ONYX_INTRINSIC_I8X16_ADD, ONYX_INTRINSIC_I8X16_ADD_SAT_S, ONYX_INTRINSIC_I8X16_ADD_SAT_U,
    ONYX_INTRINSIC_I8X16_SUB, ONYX_INTRINSIC_I8X16_SUB_SAT_S, ONYX_INTRINSIC_I8X16_SUB_SAT_U,
    ONYX_INTRINSIC_I8X16_MIN_S, ONYX_INTRINSIC_I8X16_MIN_U,
    ONYX_INTRINSIC_I8X16_MAX_S, ONYX_INTRINSIC_I8X16_MAX_U,
    ONYX_INTRINSIC_I8X16_AVGR_U,

    ONYX_INTRINSIC_I16X8_ABS, ONYX_INTRINSIC_I16X8_NEG,
    ONYX_INTRINSIC_I16X8_ANY_TRUE, ONYX_INTRINSIC_I16X8_ALL_TRUE,
    ONYX_INTRINSIC_I16X8_BITMASK,
    ONYX_INTRINSIC_I16X8_NARROW_I32X4_S, ONYX_INTRINSIC_I16X8_NARROW_I32X4_U,
    ONYX_INTRINSIC_I16X8_WIDEN_LOW_I8X16_S, ONYX_INTRINSIC_I16X8_WIDEN_HIGH_I8X16_S,
    ONYX_INTRINSIC_I16X8_WIDEN_LOW_I8X16_U, ONYX_INTRINSIC_I16X8_WIDEN_HIGH_I8X16_U,
    ONYX_INTRINSIC_I16X8_SHL, ONYX_INTRINSIC_I16X8_SHR_S, ONYX_INTRINSIC_I16X8_SHR_U,
    ONYX_INTRINSIC_I16X8_ADD, ONYX_INTRINSIC_I16X8_ADD_SAT_S, ONYX_INTRINSIC_I16X8_ADD_SAT_U,
    ONYX_INTRINSIC_I16X8_SUB, ONYX_INTRINSIC_I16X8_SUB_SAT_S, ONYX_INTRINSIC_I16X8_SUB_SAT_U,
    ONYX_INTRINSIC_I16X8_MUL,
    ONYX_INTRINSIC_I16X8_MIN_S, ONYX_INTRINSIC_I16X8_MIN_U,
    ONYX_INTRINSIC_I16X8_MAX_S, ONYX_INTRINSIC_I16X8_MAX_U,
    ONYX_INTRINSIC_I16X8_AVGR_U,

    ONYX_INTRINSIC_I32X4_ABS, ONYX_INTRINSIC_I32X4_NEG,
    ONYX_INTRINSIC_I32X4_ANY_TRUE, ONYX_INTRINSIC_I32X4_ALL_TRUE,
    ONYX_INTRINSIC_I32X4_BITMASK,
    ONYX_INTRINSIC_I32X4_WIDEN_LOW_I16X8_S, ONYX_INTRINSIC_I32X4_WIDEN_HIGH_I16X8_S,
    ONYX_INTRINSIC_I32X4_WIDEN_LOW_I16X8_U, ONYX_INTRINSIC_I32X4_WIDEN_HIGH_I16X8_U,
    ONYX_INTRINSIC_I32X4_SHL, ONYX_INTRINSIC_I32X4_SHR_S, ONYX_INTRINSIC_I32X4_SHR_U,
    ONYX_INTRINSIC_I32X4_ADD, ONYX_INTRINSIC_I32X4_SUB, ONYX_INTRINSIC_I32X4_MUL,
    ONYX_INTRINSIC_I32X4_MIN_S, ONYX_INTRINSIC_I32X4_MIN_U,
    ONYX_INTRINSIC_I32X4_MAX_S, ONYX_INTRINSIC_I32X4_MAX_U,

    ONYX_INTRINSIC_I64X2_NEG, ONYX_INTRINSIC_I64X2_SHL,
    ONYX_INTRINSIC_I64X2_SHR_S, ONYX_INTRINSIC_I64X2_SHR_U,
    ONYX_INTRINSIC_I64X2_ADD, ONYX_INTRINSIC_I64X2_SUB, ONYX_INTRINSIC_I64X2_MUL,

    ONYX_INTRINSIC_F32X4_ABS, ONYX_INTRINSIC_F32X4_NEG, ONYX_INTRINSIC_F32X4_SQRT,
    ONYX_INTRINSIC_F32X4_ADD, ONYX_INTRINSIC_F32X4_SUB,
    ONYX_INTRINSIC_F32X4_MUL, ONYX_INTRINSIC_F32X4_DIV,
    ONYX_INTRINSIC_F32X4_MIN, ONYX_INTRINSIC_F32X4_MAX,

    ONYX_INTRINSIC_F64X2_ABS, ONYX_INTRINSIC_F64X2_NEG, ONYX_INTRINSIC_F64X2_SQRT,
    ONYX_INTRINSIC_F64X2_ADD, ONYX_INTRINSIC_F64X2_SUB,
    ONYX_INTRINSIC_F64X2_MUL, ONYX_INTRINSIC_F64X2_DIV,
    ONYX_INTRINSIC_F64X2_MIN, ONYX_INTRINSIC_F64X2_MAX,

    ONYX_INTRINSIC_I32X4_TRUNC_SAT_F32X4_S,
    ONYX_INTRINSIC_I32X4_TRUNC_SAT_F32X4_U,
    ONYX_INTRINSIC_F32X4_CONVERT_I32X4_S,
    ONYX_INTRINSIC_F32X4_CONVERT_I32X4_U,
} OnyxIntrinsic;

typedef enum CallingConvention {
    CC_Undefined,
    CC_Return_Wasm,
    CC_Return_Stack
} CallingConvention;

typedef enum JumpType {
    Jump_Type_Break,
    Jump_Type_Continue,
    Jump_Type_Fallthrough,

    Jump_Type_Count,
} JumpType;

typedef enum ForLoopType {
    For_Loop_Invalid,
    For_Loop_Range,
    For_Loop_Array,
    For_Loop_Slice,
    For_Loop_DynArr,
} ForLoopType;

typedef enum ParamPassType {
    Param_Pass_Invalid,
    Param_Pass_By_Value,
    Param_Pass_By_VarArg,
    Param_Pass_By_Implicit_Pointer,
} ParamPassType;

typedef enum VarArgKind {
    VA_Kind_Not_VA,
    VA_Kind_Typed,
    VA_Kind_Untyped,
} VarArgKind;

// Base Nodes
#define AstNode_base \
    AstKind kind;             \
    u32 flags;                \
    OnyxToken *token;         \
    AstNode *next
struct AstNode { AstNode_base; };

// NOTE: 'type_node' is filled out by the parser.
// For a type such as '^^i32', the tree would look something like
//
//      Typed Thing -> AstPointerType -> AstPointerType -> AstNode (symbol node)
//
// The symbol node will be filled out during symbol resolution.
// It will end up pointing to an AstBasicType that corresponds to
// the underlying type.
//
// 'type' is filled out afterwards. If it is NULL, the Type* is built
// using the type_node. This can then be used to typecheck this node.
#define AstTyped_base       \
    AstNode_base;           \
    AstType *type_node;     \
    Type *type
struct AstTyped { AstTyped_base; };

// Expression Nodes
struct AstBinaryOp      { AstTyped_base; BinaryOp operation; AstTyped *left, *right; };
struct AstUnaryOp       { AstTyped_base; UnaryOp operation; AstTyped *expr; };
struct AstNumLit        { AstTyped_base; union { i32 i; i64 l; f32 f; f64 d; } value; };
struct AstStrLit        { AstTyped_base; u64 addr; u64 length; };
struct AstLocal         { AstTyped_base; };
struct AstArgument      { AstTyped_base; AstTyped *value; VarArgKind va_kind; };
struct AstAddressOf     { AstTyped_base; AstTyped *expr; };
struct AstDereference   { AstTyped_base; AstTyped *expr; };
struct AstArrayAccess   { AstTyped_base; AstTyped *addr; AstTyped *expr; u64 elem_size; };
struct AstFieldAccess   { AstTyped_base; AstTyped *expr; u32 offset; u32 idx; char* field; }; // If token is null, defer to field
struct AstSizeOf        { AstTyped_base; AstType *so_ast_type; Type *so_type; u64 size; };
struct AstAlignOf       { AstTyped_base; AstType *ao_ast_type; Type *ao_type; u64 alignment; };
struct AstFileContents  { AstTyped_base; OnyxToken *filename; u32 addr, size; };
struct AstStructLiteral {
    AstTyped_base;

    AstTyped *stnode;

    bh_arr(AstStructMember *) named_values;
    bh_arr(AstTyped *) values;
};
struct AstArrayLiteral {
    AstTyped_base;

    AstTyped *atnode;

    bh_arr(AstTyped *) values;
};
struct AstRangeLiteral {
    AstTyped_base; 

    // HACK: Currently, range literals are parsed as binary operators, which means
    // the first sizeof(AstBinaryOp) bytes of this structure must match that of
    // AstBinaryOp, which means I need this dummy field here.
    //                                              - brendanfh 2020/12/23
    BinaryOp __unused_operation;
    AstTyped *low, *high;

    // Currently, there is no way to specify this in the grammar, but it is set
    // to be the initial value of the `step` member of the range structure in
    // core/builtin.onyx.
    //                                              - brendanfh 2020/12/23
    AstTyped *step;
};
struct AstCall {
    AstTyped_base;

    u64 arg_count;
    bh_arr(AstArgument *) arg_arr;

    AstTyped *callee;

    VarArgKind va_kind;
};
struct AstIntrinsicCall {
    AstTyped_base;

    u64 arg_count;
    bh_arr(AstArgument *) arg_arr;

    OnyxIntrinsic intrinsic;

    VarArgKind va_kind;
};

struct AstDirectiveSolidify {
    AstTyped_base;

    AstPolyProc* poly_proc;
    bh_arr(AstPolySolution) known_polyvars;

    AstNode* resolved_proc;
};

// Intruction Node
struct AstReturn        { AstNode_base; AstTyped* expr; };
struct AstJump          { AstNode_base; JumpType jump; u32 count; };
struct AstUse           { AstNode_base; AstTyped* expr; };

// Structure Nodes
struct AstBlock         {
    AstNode_base;
    AstNode *body;
    Scope *scope;

    bh_arr(AstTyped *)   allocate_exprs;

    Scope *binding_scope;
};
struct AstDefer         { AstNode_base; AstNode *stmt; };
struct AstFor           {
    AstNode_base;

    // NOTE: Stores the iteration variable
    Scope *scope;

    // NOTE: Local defining the iteration variable
    AstLocal* var;

    // NOTE: This can be any expression, but it is checked that
    // it is of a type that we know how to iterate over.
    AstTyped* iter;
    ForLoopType loop_type;

    AstBlock *stmt;

    // ROBUSTNESS: This should be able to be set by a compile time variable at some point.
    // But for now, this will do.
    b32 by_pointer : 1;
};
struct AstIfWhile {
    AstNode_base;

    Scope *scope;
    AstLocal *local;
    AstBinaryOp *assignment;

    AstTyped *cond;

    AstBlock *true_stmt;
    AstBlock *false_stmt;
};
struct AstSwitchCase {
    // NOTE: All expressions that end up in this block
    bh_arr(AstTyped *) values;
    
    AstBlock *block;
};
struct AstSwitch {
    AstNode_base;

    Scope *scope;
    AstLocal *local;
    AstBinaryOp *assignment;

    AstTyped *expr;

    bh_arr(AstSwitchCase) cases;
    AstBlock *default_case;

    // NOTE: This is a mapping from the compile time known case value
    // to a pointer to the block that it is associated with.
    bh_imap case_map;
    u64 min_case, max_case;
};

// Type Nodes
// NOTE: This node is very similar to an AstNode, just
// without the 'next' member. This is because types
// can't be in expressions so a 'next' thing
// doesn't make sense.
#define AstType_base    \
    AstKind kind;       \
    u32 flags;          \
    OnyxToken* token;   \
    char* name
struct AstType { AstType_base; };

struct AstBasicType     { AstType_base; Type* type; };
struct AstPointerType   { AstType_base; AstType* elem; };
struct AstFunctionType  { AstType_base; AstType* return_type; u64 param_count; AstType* params[]; };
struct AstArrayType     { AstType_base; AstType* elem; AstTyped *count_expr; };
struct AstSliceType     { AstType_base; AstType* elem; };
struct AstDynArrType    { AstType_base; AstType* elem; };
struct AstVarArgType    { AstType_base; AstType* elem; };
struct AstStructType {
    AstType_base;

    bh_arr(AstStructMember *) members;

    u32 min_alignment, min_size;

    // NOTE: Used to cache the actual type, since building
    // a struct type is kind of complicated and should
    // only happen once.
    Type *stcache;
};
struct AstStructMember {
    AstTyped_base;
    AstTyped* initial_value;
};
struct AstPolyStructParam {
    AstTyped_base;
};
struct AstPolyStructType {
    AstType_base;

    Scope *scope;
    bh_arr(AstPolyStructParam) poly_params;
    bh_table(AstStructType *) concrete_structs;

    AstStructType* base_struct;
};
struct AstPolyCallType {
    AstType_base;

    AstType* callee;

    // NOTE: These nodes can be either AstTypes, or AstTyped expressions.
    bh_arr(AstNode *) params;
};
struct AstEnumType {
    AstType_base;
    Scope *scope;

    AstType *backing;
    Type    *backing_type;

    bh_arr(AstEnumValue *) values;

    // NOTE: Used to cache the actual type for the same reason as above.
    Type *etcache;
};
struct AstEnumValue    { AstTyped_base; AstNumLit* value; };
struct AstTypeAlias    { AstType_base; AstType* to; };
struct AstTypeRawAlias { AstType_base; Type* to; };

// Top level nodes
struct AstBinding       { AstTyped_base; AstNode* node; };
struct AstMemRes        { AstTyped_base; u64 addr; AstTyped *initial_value; };
struct AstInclude       { AstNode_base; char* name; };
struct AstUsePackage    {
    AstNode_base;

    AstPackage *package;
    OnyxToken *alias;
    bh_arr(AstAlias *) only;
};
struct AstAlias         {
    AstNode_base;
    OnyxToken *alias;
};
struct AstGlobal        {
    AstTyped_base;

    OnyxToken* name;

    union {
        // NOTE: Used when a global is exported with a specific name
        OnyxToken* exported_name;

        // NOTE: Used when the global is declared as foreign
        struct {
            OnyxToken* foreign_module;
            OnyxToken* foreign_name;
        };
    };
};
struct AstParam {
    // HACK CLEANUP: This does not need to have a local buried inside of it.
    // Convert this to be AstTyped_base and pull the ParamPassType from AstLocal
    // to here.                                        - brendanfh 2020/09/18
    AstLocal *local;
    AstTyped *default_value;

    VarArgKind vararg_kind;
};
struct AstFunction {
    AstTyped_base;

    Scope *scope;

    bh_arr(AstParam) params;
    AstType* return_type;

    AstBlock *body;
    bh_arr(AstTyped *) allocate_exprs;

    // NOTE: used by the #add_overload directive. Initially set to a symbol,
    // then resolved to an overloaded function.
    AstNode *overloaded_function;

    // NOTE: set to -1 if the function is not an operator overload;
    // set to a BinaryOp value if it is.
    BinaryOp operator_overload;

    OnyxToken* name;

    union {
        // NOTE: Used when a function is exported with a specific name
        OnyxToken* exported_name;
        OnyxToken* intrinsic_name;

        // NOTE: Used when the function is declared as foreign
        struct {
            OnyxToken* foreign_module;
            OnyxToken* foreign_name;
        };
    };
};
struct AstPolyParam {
    // The symbol node that represents the polymorphic variable.
    AstNode* poly_sym;

    // The type expression that contains `poly_sym` in it somewhere.
    // Matching polymorphic variables does a parallel tree traversal
    // to find the pairing of the actual type and polymorphic variable
    // symbol.
    AstType* type_expr;

    // The parameter index where the polymorphic variable occurs.
    u64 idx;
};

typedef enum PolySolutionKind {
    PSK_Undefined,
    PSK_Type,
    PSK_Value,
} PolySolutionKind;

struct AstPolySolution {
    PolySolutionKind kind;
    AstNode* poly_sym;

    union {
        struct {
            // If `type` is null, it is filled in with this type.
            AstType* ast_type;
            Type*    type;
        };

        AstTyped* value;
    };
};
struct AstPolyProc {
    AstNode_base;

    Scope *poly_scope;
    bh_arr(AstPolyParam) poly_params;

    bh_arr(AstPolySolution) known_slns;

    AstFunction* base_func;
    bh_table(AstFunction *) concrete_funcs;
};
struct AstOverloadedFunction {
    AstTyped_base;

    bh_arr(AstTyped *) overloads;
};
struct AstPackage {
    AstNode_base;

    Package* package;
};

extern AstNode empty_node;

typedef enum EntityState {
    Entity_State_Error,
    
    Entity_State_Parse_Builtin,
    Entity_State_Parse,
    Entity_State_Resolve_Symbols,
    Entity_State_Check_Types,
    Entity_State_Code_Gen,
    Entity_State_Finalized,

    Entity_State_Count,
} EntityState;

extern const char* entity_state_strings[Entity_State_Count];

// NOTE: An Entity represents something will need to be
// processed later down the pipeline.
typedef enum EntityType {
    Entity_Type_Unknown,

    Entity_Type_Load_Path,
    Entity_Type_Load_File,
    Entity_Type_Use_Package,
    Entity_Type_String_Literal,
    Entity_Type_File_Contents,
    Entity_Type_Enum,
    Entity_Type_Type_Alias,
    Entity_Type_Memory_Reservation_Type,
    Entity_Type_Use,
    Entity_Type_Polymorphic_Proc,
    Entity_Type_Foreign_Function_Header,
    Entity_Type_Foreign_Global_Header,
    Entity_Type_Function_Header,
    Entity_Type_Global_Header,
    Entity_Type_Struct_Member_Default,
    Entity_Type_Memory_Reservation,
    Entity_Type_Expression,
    Entity_Type_Global,
    Entity_Type_Overloaded_Function,
    Entity_Type_Function,

    Entity_Type_Count,
} EntityType;

extern const char* entity_type_strings[Entity_Type_Count];

typedef struct Entity {
    EntityType type;
    EntityState state;
    Package *package;
    Scope *scope;

    union {
        AstInclude            *include;
        AstUsePackage         *use_package;
        AstFunction           *function;
        AstOverloadedFunction *overloaded_function;
        AstGlobal             *global;
        AstTyped              *expr;
        AstStrLit             *strlit;
        AstFileContents       *file_contents;
        AstType               *type_alias;
        AstEnumType           *enum_type;
        AstMemRes             *mem_res;
        AstPolyProc           *poly_proc;
        AstUse                *use;
    };
} Entity;

typedef struct EntityHeap {
    bh_arr(Entity) entities;

    i32 state_count[Entity_State_Count];
} EntityHeap;

void entity_heap_insert(EntityHeap* entities, Entity e);
Entity entity_heap_top(EntityHeap* entities);
void entity_heap_change_top(EntityHeap* entities, Entity new_top);
void entity_heap_remove_top(EntityHeap* entities);

void entity_bring_to_state(Entity* ent, EntityState state);
void symres_entity(Entity* ent);
void check_entity(Entity* ent);
void emit_entity(Entity* ent);

struct Package {
    char *name;

    Scope *scope;
    Scope *private_scope;
};

// NOTE: Simple data structure for storing what comes out of the parser
typedef struct ProgramInfo {
    Scope *global_scope;

    bh_table(Package *)   packages;
    EntityHeap            entities;

    u32 foreign_global_count;
} ProgramInfo;

// NOTE: Basic internal types constructed in the parser
extern AstBasicType basic_type_void;
extern AstBasicType basic_type_bool;
extern AstBasicType basic_type_i8;
extern AstBasicType basic_type_u8;
extern AstBasicType basic_type_i16;
extern AstBasicType basic_type_u16;
extern AstBasicType basic_type_i32;
extern AstBasicType basic_type_u32;
extern AstBasicType basic_type_i64;
extern AstBasicType basic_type_u64;
extern AstBasicType basic_type_f32;
extern AstBasicType basic_type_f64;
extern AstBasicType basic_type_rawptr;

extern AstBasicType basic_type_int_unsized;
extern AstBasicType basic_type_float_unsized;

// :TypeExprHack
extern AstNode type_expr_symbol;

extern AstNode   builtin_package_node;
extern AstNumLit builtin_heap_start;
extern AstGlobal builtin_stack_top;
extern AstType  *builtin_string_type;
extern AstType  *builtin_range_type;
extern Type     *builtin_range_type_type;
extern AstType  *builtin_vararg_type;
extern Type     *builtin_vararg_type_type;
extern AstTyped *builtin_context_variable;

typedef struct BuiltinSymbol {
    char*    package;
    char*    sym;
    AstNode* node;
} BuiltinSymbol;

extern const BuiltinSymbol builtin_symbols[];

typedef struct IntrinsicMap {
    char*         name;
    OnyxIntrinsic intrinsic;
} IntrinsicMap;

extern bh_table(OnyxIntrinsic) intrinsic_table;

extern bh_arr(AstTyped *) operator_overloads[Binary_Op_Count];

void initialize_builtins(bh_allocator a, ProgramInfo* prog);


// NOTE: Useful not inlined functions
AstTyped* ast_reduce(bh_allocator a, AstTyped* node);
AstNode* ast_clone(bh_allocator a, void* n);
AstFunction* clone_function_header(bh_allocator a, AstFunction* func);

void promote_numlit_to_larger(AstNumLit* num);
b32 convert_numlit_to_type(AstNumLit* num, Type* type);

b32 type_check_or_auto_cast(AstTyped** pnode, Type* type);
Type* resolve_expression_type(AstTyped* node);

b32 cast_is_legal(Type* from_, Type* to_, char** err_msg);
char* get_function_name(AstFunction* func);

AstNumLit* make_int_literal(bh_allocator a, i64 value);
AstNumLit* make_float_literal(bh_allocator a, f64 value);
AstBinaryOp* make_binary_op(bh_allocator a, BinaryOp operation, AstTyped* left, AstTyped* right);
AstArgument* make_argument(bh_allocator a, AstTyped* value);
AstFieldAccess* make_field_access(bh_allocator a, AstTyped* node, char* field);

typedef enum PolyProcLookupMethod {
    PPLM_By_Call,
    PPLM_By_Function_Type,
    PPLM_By_Value_Array,
} PolyProcLookupMethod;
AstFunction* polymorphic_proc_lookup(AstPolyProc* pp, PolyProcLookupMethod pp_lookup, ptr actual, OnyxFilePos pos);
AstFunction* polymorphic_proc_solidify(AstPolyProc* pp, bh_arr(AstPolySolution) slns, OnyxFilePos pos);
AstNode* polymorphic_proc_try_solidify(AstPolyProc* pp, bh_arr(AstPolySolution) slns, OnyxFilePos pos);
AstFunction* polymorphic_proc_build_only_header(AstPolyProc* pp, PolyProcLookupMethod pp_lookup, ptr actual);


AstStructType* polymorphic_struct_lookup(AstPolyStructType* ps_type, bh_arr(AstPolySolution) slns, OnyxFilePos pos);

// NOTE: Useful inlined functions
static inline b32 is_lval(AstNode* node) {
    return (node->kind == Ast_Kind_Local)
        || (node->kind == Ast_Kind_Param)
        || (node->kind == Ast_Kind_Global)
        || (node->kind == Ast_Kind_Dereference)
        || (node->kind == Ast_Kind_Array_Access)
        || (node->kind == Ast_Kind_Field_Access)
        || (node->kind == Ast_Kind_Memres);
}

static inline b32 binop_is_assignment(BinaryOp binop) {
    return (binop >= Binary_Op_Assign_Start && binop <= Binary_Op_Assign_End);
}

static inline b32 binop_is_compare(BinaryOp binop) {
    return (binop >= Binary_Op_Equal && binop <= Binary_Op_Greater_Equal);
}

static inline b32 node_is_type(AstNode* node) {
    return (node->kind > Ast_Kind_Type_Start) && (node->kind < Ast_Kind_Type_End);
}

static inline b32 node_is_auto_cast(AstNode* node) {
    return (node->kind == Ast_Kind_Unary_Op) && (((AstUnaryOp *) node)->operation == Unary_Op_Auto_Cast);
}

static inline CallingConvention type_function_get_cc(Type* type) {
    if (type == NULL) return CC_Undefined;
    if (type->kind != Type_Kind_Function) return CC_Undefined;
    if (type->Function.return_type->kind == Type_Kind_Struct) return CC_Return_Stack;
    if (type->Function.return_type->kind == Type_Kind_Slice) return CC_Return_Stack;
    if (type->Function.return_type->kind == Type_Kind_DynArray) return CC_Return_Stack;
    return CC_Return_Wasm;
}

static inline ParamPassType type_get_param_pass(Type* type) {
    if (type_is_structlike_strict(type) && !type_structlike_is_simple(type)) return Param_Pass_By_Implicit_Pointer;
    return Param_Pass_By_Value;
}

#endif // #ifndef ONYXASTNODES_H
