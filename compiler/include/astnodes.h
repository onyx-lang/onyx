#ifndef ONYXASTNODES_H
#define ONYXASTNODES_H

#define VERSION_MAJOR 0
#define VERSION_MINOR 1
#define VERSION_PATCH 14
#define VERSION_SUFFIX "-preview"

#include "stb_ds.h"
#include "lex.h"
#include "types.h"
#include "errors.h"

#define AST_NODES              \
    NODE(Node)                 \
    NODE(Typed)                \
                               \
    NODE(NamedValue)           \
    NODE(BinaryOp)             \
    NODE(UnaryOp)              \
    NODE(NumLit)               \
    NODE(StrLit)               \
    NODE(Local)                \
    NODE(Call)                 \
    NODE(Argument)             \
    NODE(AddressOf)            \
    NODE(Dereference)          \
    NODE(Subscript)            \
    NODE(FieldAccess)          \
    NODE(UnaryFieldAccess)     \
    NODE(SizeOf)               \
    NODE(AlignOf)              \
    NODE(FileContents)         \
    NODE(StructLiteral)        \
    NODE(ArrayLiteral)         \
    NODE(RangeLiteral)         \
    NODE(Compound)             \
    NODE(IfExpression)         \
    NODE(DoBlock)              \
                               \
    NODE(DirectiveSolidify)    \
    NODE(DirectiveError)       \
    NODE(DirectiveAddOverload) \
    NODE(DirectiveOperator)    \
    NODE(DirectiveExport)      \
    NODE(DirectiveDefined)     \
    NODE(DirectiveInit)        \
    NODE(DirectiveLibrary)     \
    NODE(DirectiveRemove)      \
    NODE(DirectiveFirst)       \
    NODE(DirectiveExportName)  \
    NODE(DirectiveWasmSection) \
                               \
    NODE(Return)               \
    NODE(Jump)                 \
                               \
    NODE(Block)                \
    NODE(IfWhile)              \
    NODE(For)                  \
    NODE(Defer)                \
    NODE(SwitchCase)           \
    NODE(Switch)               \
                               \
    NODE(Type)                 \
    NODE(BasicType)            \
    NODE(PointerType)          \
    NODE(MultiPointerType)     \
    NODE(FunctionType)         \
    NODE(ArrayType)            \
    NODE(SliceType)            \
    NODE(DynArrType)           \
    NODE(VarArgType)           \
    NODE(StructType)           \
    NODE(StructMember)         \
    NODE(PolyStructType)       \
    NODE(PolyStructParam)      \
    NODE(PolyCallType)         \
    NODE(UnionType)            \
    NODE(UnionVariant)         \
    NODE(PolyUnionType)        \
    NODE(EnumType)             \
    NODE(EnumValue)            \
    NODE(TypeAlias)            \
    NODE(TypeRawAlias)         \
    NODE(CompoundType)         \
    NODE(TypeOf)               \
    NODE(DistinctType)         \
                               \
    NODE(Binding)              \
    NODE(Alias)                \
    NODE(Injection)            \
    NODE(MemRes)               \
    NODE(Include)              \
    NODE(UsePackage)           \
    NODE(Global)               \
    NODE(Param)                \
    NODE(Function)             \
    NODE(OverloadedFunction)   \
    NODE(Interface)            \
    NODE(Constraint)           \
                               \
    NODE(PolyParam)            \
    NODE(PolySolution)         \
    NODE(SolidifiedFunction)   \
    NODE(PolyProc)             \
    NODE(PolyQuery)            \
                               \
    NODE(CallSite)             \
                               \
    NODE(CodeBlock)            \
    NODE(DirectiveInsert)      \
    NODE(Macro)                \
                               \
    NODE(CaptureBlock)         \
    NODE(CaptureLocal)         \
                               \
    NODE(ForeignBlock)         \
                               \
    NODE(Package)              \
    NODE(Import)               \
                               \
    NODE(ZeroValue)            \
                               \
    NODE(JsNode)               \
                               \
    NODE(CompilerExtension)    \
    NODE(ProceduralMacro)      \
    NODE(ProceduralExpansion)

#define NODE(name) typedef struct Ast ## name Ast ## name;
AST_NODES
#undef NODE

typedef struct Package Package;

typedef struct Scope {
    u64 id;
    struct Scope *parent;
    OnyxFilePos created_at;
    char* name;
    Table(AstNode *) symbols;
} Scope;


typedef enum AstKind {
    Ast_Kind_Error,
    Ast_Kind_Package,
    Ast_Kind_Load_File,
    Ast_Kind_Load_Path,
    Ast_Kind_Load_All,
    Ast_Kind_Library_Path,
    Ast_Kind_Import,
    Ast_Kind_Memres,

    Ast_Kind_Binding,
    Ast_Kind_Alias,
    Ast_Kind_Injection,
    Ast_Kind_Function,
    Ast_Kind_Overloaded_Function,
    Ast_Kind_Polymorphic_Proc,
    Ast_Kind_Polymorph_Query,
    Ast_Kind_Interface,
    Ast_Kind_Constraint,
    Ast_Kind_Constraint_Sentinel,
    Ast_Kind_Block,
    Ast_Kind_Local,
    Ast_Kind_Global,
    Ast_Kind_Symbol,

    Ast_Kind_Unary_Op,
    Ast_Kind_Binary_Op,

    Ast_Kind_Compound,
    Ast_Kind_Named_Value,

    Ast_Kind_Type_Start,
    Ast_Kind_Type,
    Ast_Kind_Basic_Type,
    Ast_Kind_Pointer_Type,
    Ast_Kind_Multi_Pointer_Type,
    Ast_Kind_Function_Type,
    Ast_Kind_Array_Type,
    Ast_Kind_Slice_Type,
    Ast_Kind_DynArr_Type,
    Ast_Kind_VarArg_Type,
    Ast_Kind_Struct_Type,
    Ast_Kind_Poly_Struct_Type,
    Ast_Kind_Poly_Call_Type,
    Ast_Kind_Union_Type,
    Ast_Kind_Poly_Union_Type,
    Ast_Kind_Enum_Type,
    Ast_Kind_Type_Alias,
    Ast_Kind_Type_Raw_Alias,
    Ast_Kind_Type_Compound,
    Ast_Kind_Typeof,
    Ast_Kind_Distinct_Type,
    Ast_Kind_Type_End,

    Ast_Kind_Struct_Member,
    Ast_Kind_Union_Variant,
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
    Ast_Kind_Subscript,
    Ast_Kind_Slice,
    Ast_Kind_Field_Access,
    Ast_Kind_Unary_Field_Access,
    Ast_Kind_Pipe,
    Ast_Kind_Method_Call,
    Ast_Kind_Range_Literal,
    Ast_Kind_Size_Of,
    Ast_Kind_Align_Of,
    Ast_Kind_File_Contents,
    Ast_Kind_Struct_Literal,
    Ast_Kind_Array_Literal,
    Ast_Kind_If_Expression,

    Ast_Kind_If,
    Ast_Kind_For,
    Ast_Kind_While,
    Ast_Kind_Jump,
    Ast_Kind_Defer,
    Ast_Kind_Switch,
    Ast_Kind_Switch_Case,

    Ast_Kind_Directive_Solidify,
    Ast_Kind_Static_If,
    Ast_Kind_Directive_Error,
    Ast_Kind_Directive_Add_Overload,
    Ast_Kind_Directive_Operator,
    Ast_Kind_Directive_Export,
    Ast_Kind_Directive_Defined,
    Ast_Kind_Directive_Init,
    Ast_Kind_Directive_Library,
    Ast_Kind_Directive_Remove,
    Ast_Kind_Directive_First,
    Ast_Kind_Directive_Export_Name,
    Ast_Kind_Directive_This_Package,
    Ast_Kind_Directive_Wasm_Section,
    Ast_Kind_Call_Site,

    Ast_Kind_Code_Block,
    Ast_Kind_Directive_Insert,
    Ast_Kind_Macro,
    Ast_Kind_Do_Block,

    Ast_Kind_Capture_Block,
    Ast_Kind_Capture_Local,

    Ast_Kind_Foreign_Block,

    Ast_Kind_Zero_Value,

    Ast_Kind_Js_Code,

    Ast_Kind_Compiler_Extension,
    Ast_Kind_Procedural_Macro,
    Ast_Kind_Procedural_Expansion,

    Ast_Kind_Count
} AstKind;

// NOTE: Some of these flags will overlap since there are
// only 32-bits of flags to play with
typedef enum AstFlags {
    // Top-level flags
    Ast_Flag_Const                 = BH_BIT(1),
    Ast_Flag_Comptime              = BH_BIT(2),
    Ast_Flag_Private_Package       = BH_BIT(3),
    Ast_Flag_Private_File          = BH_BIT(4),

    Ast_Flag_Block_Returns         = BH_BIT(5),

    // Expression flags
    Ast_Flag_Expr_Ignored          = BH_BIT(6),
    Ast_Flag_Address_Taken         = BH_BIT(7),

    // Type flags
    Ast_Flag_No_Clone              = BH_BIT(9),

    Ast_Flag_Cannot_Take_Addr      = BH_BIT(10),

    // HACK: NullProcHack
    Ast_Flag_Proc_Is_Null          = BH_BIT(11),

    Ast_Flag_From_Polymorphism     = BH_BIT(12),

    Ast_Flag_Incomplete_Body       = BH_BIT(13),

    Ast_Flag_Array_Literal_Typed   = BH_BIT(14),

    Ast_Flag_Has_Been_Symres       = BH_BIT(15),

    Ast_Flag_Has_Been_Checked      = BH_BIT(16),

    Ast_Flag_Static_If_Resolved    = BH_BIT(17),

    Ast_Flag_Symbol_Invisible      = BH_BIT(18),

    Ast_Flag_Header_Check_No_Error = BH_BIT(19),

    Ast_Flag_Decl_Followed_By_Init = BH_BIT(20),

    Ast_Flag_Param_Symbol_Dirty    = BH_BIT(21),

    Ast_Flag_Dead                  = BH_BIT(22),

    Ast_Flag_Extra_Field_Access    = BH_BIT(23),

    Ast_Flag_Symbol_Is_PolyVar     = BH_BIT(24),
    Ast_Flag_Poly_Call_From_Auto   = BH_BIT(24),

    Ast_Flag_Binding_Isnt_Captured = BH_BIT(25),

    Ast_Flag_Function_Is_Lambda    = BH_BIT(26),
    Ast_Flag_Function_Is_Lambda_Inside_PolyProc = BH_BIT(27),

    Ast_Flag_Constraint_Is_Expression = BH_BIT(28),

    Ast_Flag_Has_Been_Scheduled_For_Emit = BH_BIT(29),
} AstFlags;

typedef enum UnaryOp {
    Unary_Op_Negate,
    Unary_Op_Not,
    Unary_Op_Bitwise_Not,
    Unary_Op_Cast,
    Unary_Op_Auto_Cast,
    Unary_Op_Try,
    Unary_Op_Unwrap,

    Unary_Op_Count,
} UnaryOp;

typedef enum BinaryOp {
    Binary_Op_Add              = 0,
    Binary_Op_Minus            = 1,
    Binary_Op_Multiply         = 2,
    Binary_Op_Divide           = 3,
    Binary_Op_Modulus          = 4,

    Binary_Op_Equal            = 5,
    Binary_Op_Not_Equal        = 6,
    Binary_Op_Less             = 7,
    Binary_Op_Less_Equal       = 8,
    Binary_Op_Greater          = 9,
    Binary_Op_Greater_Equal    = 10,

    Binary_Op_And              = 11,
    Binary_Op_Or               = 12,
    Binary_Op_Xor              = 13,
    Binary_Op_Shl              = 14,
    Binary_Op_Shr              = 15,
    Binary_Op_Sar              = 16,

    Binary_Op_Bool_And         = 17,
    Binary_Op_Bool_Or          = 18,

    Binary_Op_Assign_Start     = 19,
    Binary_Op_Assign           = 20,
    Binary_Op_Assign_Add       = 21,
    Binary_Op_Assign_Minus     = 22,
    Binary_Op_Assign_Multiply  = 23,
    Binary_Op_Assign_Divide    = 24,
    Binary_Op_Assign_Modulus   = 25,
    Binary_Op_Assign_And       = 26,
    Binary_Op_Assign_Or        = 27,
    Binary_Op_Assign_Xor       = 28,
    Binary_Op_Assign_Shl       = 29,
    Binary_Op_Assign_Shr       = 30,
    Binary_Op_Assign_Sar       = 31,
    Binary_Op_Assign_End       = 32,

    Binary_Op_Pipe             = 33,
    Binary_Op_Range            = 34,
    Binary_Op_Range_Equal      = 35,
    Binary_Op_Method_Call      = 36,

    Binary_Op_Subscript        = 37,
    Binary_Op_Subscript_Equals = 38,
    Binary_Op_Ptr_Subscript    = 39,

    Binary_Op_Coalesce         = 40,

    Binary_Op_Count
} BinaryOp;

extern const char *binaryop_string[Binary_Op_Count];

typedef enum OnyxIntrinsic {
    ONYX_INTRINSIC_UNDEFINED,

    ONYX_INTRINSIC_UNREACHABLE,

    ONYX_INTRINSIC_MEMORY_SIZE, ONYX_INTRINSIC_MEMORY_GROW,
    ONYX_INTRINSIC_MEMORY_COPY, ONYX_INTRINSIC_MEMORY_FILL,
    ONYX_INTRINSIC_MEMORY_EQUAL,

    ONYX_INTRINSIC_INITIALIZE,

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

    ONYX_INTRINSIC_I32_REINTERPRET_F32,
    ONYX_INTRINSIC_I64_REINTERPRET_F64,
    ONYX_INTRINSIC_F32_REINTERPRET_I32,
    ONYX_INTRINSIC_F64_REINTERPRET_I64,


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

    ONYX_INTRINSIC_ATOMIC_WAIT,
    ONYX_INTRINSIC_ATOMIC_NOTIFY,

    ONYX_INTRINSIC_ATOMIC_FENCE,

    ONYX_INTRINSIC_ATOMIC_LOAD,
    ONYX_INTRINSIC_ATOMIC_STORE,
    ONYX_INTRINSIC_ATOMIC_ADD,
    ONYX_INTRINSIC_ATOMIC_SUB,
    ONYX_INTRINSIC_ATOMIC_AND,
    ONYX_INTRINSIC_ATOMIC_OR,
    ONYX_INTRINSIC_ATOMIC_XOR,
    ONYX_INTRINSIC_ATOMIC_XCHG,
    ONYX_INTRINSIC_ATOMIC_CMPXCHG,
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
    Jump_Type_Return,

    Jump_Type_Count,
} JumpType;

typedef enum ForLoopType {
    For_Loop_Invalid,
    For_Loop_Range,
    For_Loop_Array,
    For_Loop_Slice,
    For_Loop_DynArr,
    For_Loop_Iterator,
} ForLoopType;

typedef enum ParamPassType {
    Param_Pass_Invalid,
    Param_Pass_By_Value,
    Param_Pass_By_Multiple_Values,
    Param_Pass_By_Implicit_Pointer,
} ParamPassType;

typedef enum VarArgKind {
    VA_Kind_Not_VA,
    VA_Kind_Typed,
    VA_Kind_Any,
    VA_Kind_Untyped,
} VarArgKind;

typedef enum BlockRule {
    Block_Rule_New_Scope         = BH_BIT(1),
    Block_Rule_Emit_Instructions = BH_BIT(2),
    Block_Rule_Clear_Defer       = BH_BIT(3),
    Block_Rule_Override_Return   = BH_BIT(4),

    Block_Rule_Normal     = Block_Rule_New_Scope | Block_Rule_Clear_Defer | Block_Rule_Emit_Instructions,
    Block_Rule_Macro      = Block_Rule_New_Scope,
    Block_Rule_Code_Block = Block_Rule_New_Scope,
    Block_Rule_Do_Block   = Block_Rule_New_Scope | Block_Rule_Clear_Defer | Block_Rule_Override_Return | Block_Rule_Emit_Instructions,
} BlockRule;

typedef enum ConstraintCheckStatus {
    Constraint_Check_Status_Queued,
    Constraint_Check_Status_Failed,
    Constraint_Check_Status_Success,
} ConstraintCheckStatus;

typedef struct ConstraintContext {
    bh_arr(AstConstraint *) constraints;
    ConstraintCheckStatus *constraint_checks;
    b32 constraints_met : 1;
    b32 produce_errors  : 1;
} ConstraintContext;


typedef struct Arguments Arguments;
struct Arguments {
    bh_arr(AstTyped *) values;
    bh_arr(AstNamedValue *) named_values;

    // How many arguments were not baked.
    i32 used_argument_count;
};

typedef struct ForeignReference {
    AstTyped *module_name;
    AstTyped *import_name;
} ForeignReference;

typedef struct ValueWithOffset {
    AstTyped *value;
    u32 offset;
} ValueWithOffset;


// Base Nodes
#define AstNode_base \
    AstKind kind;             \
    u32 flags;                \
    OnyxToken *token;         \
    struct Entity* entity;    \
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
struct AstNamedValue    { AstTyped_base; AstTyped* value; };
struct AstStrLit        { AstTyped_base; u64 data_id; u64 length; b32 is_cstr: 1; };
struct AstLocal         { AstTyped_base; b32 auto_dispose : 1; };
struct AstDereference   { AstTyped_base; AstTyped *expr; };
struct AstSizeOf        { AstTyped_base; AstType *so_ast_type; Type *so_type; u64 size; };
struct AstAlignOf       { AstTyped_base; AstType *ao_ast_type; Type *ao_type; u64 alignment; };
struct AstNumLit        {
    AstTyped_base;
    union {
        i32 i;
        i64 l;
        f32 f;
        f64 d;
    } value;

    b32 was_hex_literal : 1;
    b32 was_char_literal : 1;
};
struct AstUnaryOp       {
    AstTyped_base;
    UnaryOp operation;

    AstTyped *expr;

    Arguments *overload_args;
};
struct AstBinaryOp      {
    AstTyped_base;
    BinaryOp operation;
    AstTyped *left, *right;

    Arguments *overload_args; // This is set of the binary operator is attempted to be overloaded
                              // but isnt successful yet.
    AstBinaryOp *potential_substitute;
};
struct AstAddressOf     {
    AstTyped_base;
    AstTyped *expr;

    AstBinaryOp *potential_substitute;

    // This is set by check_method_call.
    // If set, the address of node can be removed if the
    // type checking does not pass for it. This makes it
    // possible to have something that will optionally
    // have its address taken, if necessary.
    b32 can_be_removed : 1;
};
struct AstArgument      {
    AstTyped_base;

    AstTyped *value;

    VarArgKind va_kind;
    b32 is_baked : 1;
    b32 pass_as_any : 1;
    b32 used_as_lval_of_method_call : 1;
};
struct AstSubscript   {
    AstTyped_base;
    BinaryOp __unused_operation; // This will be set to Binary_Op_Subscript
    AstTyped *addr;
    AstTyped *expr;

    Arguments *overload_args; // This is set of the binary operator is attempted to be overloaded
                              // but isnt successful yet.
    AstBinaryOp *potential_substitute;

    u64 elem_size;
};
struct AstFieldAccess   {
    AstTyped_base;
    AstTyped *expr;
    u32 offset;
    u32 idx;
    char* field; // If token is null, defer to field

    b32 is_union_variant_access : 1;
};
struct AstFileContents  {
    AstTyped_base;

    AstTyped *filename_expr;
    char *filename; // The parsed file name, with '\' sequences removed and resolved to a particular file if possible.

    u32 data_id, size;
};
struct AstUnaryFieldAccess {
    AstTyped_base;

    // "token" represents the field. This does not need an "offset" or an "index"
    // because this node is meant to be replaced.
};
struct AstStructLiteral {
    AstTyped_base;

    AstTyped *stnode;

    Arguments args;

    Type *generated_inferred_type;

    // Value used when you do .{ ..value, val = 123 }
    AstTyped *extension_value;

    bh_arr(ValueWithOffset) values_to_initialize;
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
    union {
        BinaryOp __unused_operation;
        b32 inclusive: 1;
    };
    AstTyped *low, *high;

    // Currently, there is no way to specify this in the grammar, but it is set
    // to be the initial value of the `step` member of the range structure in
    // core/builtin.onyx.
    //                                              - brendanfh 2020/12/23
    AstTyped *step;
};
struct AstCall {
    AstTyped_base;

    Arguments args;
    i32 placeholder_argument_position;

    union {
        AstTyped *callee;
        OnyxIntrinsic intrinsic;
    };

    VarArgKind va_kind;
    i32 ignored_return_value_count;
};
struct AstCompound {
    AstTyped_base;

    bh_arr(AstTyped *) exprs;
};
struct AstIfExpression {
    AstTyped_base;

    AstTyped* cond;
    AstTyped* true_expr;
    AstTyped* false_expr;
};
struct AstDoBlock {
    AstTyped_base;

    AstBlock* block;
    bh_arr(AstLocal *) named_return_locals;
};
struct AstZeroValue {
    AstTyped_base;
};

struct AstDirectiveSolidify {
    AstTyped_base;

    AstFunction* poly_proc;
    bh_arr(AstPolySolution) known_polyvars;

    AstNode* resolved_proc;
};

// Intruction Node
struct AstReturn        { AstNode_base; AstTyped* expr; u32 count; b32 from_proc: 1; }; // Note: This count is one less than it should be, because internal codegen with macros would have to know about this and that is error prone.
struct AstJump          { AstNode_base; JumpType jump; u32 count; };

// Structure Nodes
struct AstBlock         {
    AstNode_base;

    AstNode *body;

    Scope *scope;
    Scope *binding_scope;
    Scope *quoted_block_capture_scope;
    BlockRule rules;

    u32 statement_idx;

    OnyxToken *macro_generated_from;
};
struct AstDefer         { AstNode_base; AstNode *stmt; };
struct AstFor           {
    AstNode_base;

    // NOTE: Stores the iteration variable
    Scope *scope;

    bh_arr(AstLocal *) indexing_variables;

    // NOTE: This can be any expression, but it is checked that
    // it is of a type that we know how to iterate over.
    AstTyped* iter;
    ForLoopType loop_type;

    AstBlock *stmt;

    // NOTE: This is set when a for-loop isn't over a primitive type
    // and instead is over a custom type, such as `Iterator` or `Map`.
    // To properly invoke `__for_expansion`, we need to store the prepared
    // call, as there could be overloads we have to wait for by yielding.
    AstCall *intermediate_macro_expansion;

    // ROBUSTNESS: This should be able to be set by a compile time variable at some point.
    // But for now, this will do.
    b32 by_pointer : 1;
    b32 no_close   : 1;
};

struct AstIfWhile {
    AstNode_base;

    Scope *scope;
    AstNode* initialization;

    AstTyped *cond;

    AstBlock *true_stmt;
    AstBlock *false_stmt;

    union {
        // Used by Static_If
        struct {
            Scope *defined_in_scope;
            bh_arr(struct Entity *) true_entities;
            bh_arr(struct Entity *) false_entities;
        };

        // Used by While
        struct {
            // NOTE: This is used by the AstDirectiveFirst node for this
            // for node to know which local variable to use.
            u64 first_local;
            b32 has_first;

            b32 bottom_test;
        };
    };
};
typedef struct AstIfWhile AstIf;
typedef struct AstIfWhile AstWhile;

typedef enum SwitchKind {
    Switch_Kind_Integer,
    Switch_Kind_Use_Equals,
    Switch_Kind_Union,
} SwitchKind;

typedef struct CaseToBlock {
    AstTyped *original_value;
    AstBinaryOp *comparison;
    AstSwitchCase *casestmt;
} CaseToBlock;

struct AstSwitchCase {
    AstNode_base;

    // NOTE: All expressions that end up in this block
    bh_arr(AstTyped *) values;

    union {
        AstBlock *block;
        AstTyped *expr;
    };

    AstLocal *capture;
    Scope *scope; // Scope for the capture

    b32 is_default: 1; // Could this be inferred by the values array being null?
    b32 capture_is_by_pointer: 1;
    b32 body_is_expr : 1;
};

struct AstSwitch {
    AstTyped_base;

    Scope *scope;
    AstNode* initialization;

    AstTyped *expr;

    AstBlock *case_block;

    bh_arr(AstSwitchCase *) cases;
    AstBlock *default_case;

    i32 yield_return_index;
    SwitchKind switch_kind;

    // NOTE: This is an array of "bools" that says which union variants have
    // been handled.
    u8 *union_variants_handled;

    b32 is_expr;

    union {
        struct {
            // NOTE: This is a mapping from the compile time known case value
            // to a pointer to the block that it is associated with.
            bh_imap case_map;
            u64 min_case, max_case;
        };

        struct {
            // NOTE: This is a mapping from the '==' binary op node to
            // a pointer to the block that it is associated with.
            bh_arr(CaseToBlock) case_exprs;
        };
    };
};

// Type Nodes
// NOTE: This node is very similar to an AstNode, just
// without the 'next' member. This is because types
// can't be in expressions so a 'next' thing
// doesn't make sense.
#define AstType_base       \
    AstKind kind;          \
    u32 flags;             \
    OnyxToken* token;      \
    struct Entity* entity; \
    void* next;            \
    u64 type_id;           \
    Type* type
struct AstType { AstType_base; };

struct AstBasicType     { AstType_base; Type* basic_type; Scope *scope; };
struct AstPointerType   { AstType_base; AstType* elem; };
struct AstArrayType     { AstType_base; AstType* elem; AstTyped *count_expr; };
struct AstSliceType     { AstType_base; AstType* elem; };
struct AstDynArrType    { AstType_base; AstType* elem; };
struct AstVarArgType    { AstType_base; AstType* elem; };
struct AstMultiPointerType { AstType_base; AstType* elem; };
struct AstFunctionType  {
    AstType_base;

    // Used in a rare case in solve_for_polymorphic_param_type.
    Type *partial_function_type;

    AstType* return_type;

    u64 param_count;
    AstType* params[];
};
struct AstStructType {
    AstType_base;
    char *name;

    bh_arr(AstStructMember *) members;
    bh_arr(AstTyped *) meta_tags;

    // u32 min_alignment, min_size;
    AstTyped *min_alignment_, *min_size_;

    // NOTE: Used to cache the actual type, since building
    // a struct type is kind of complicated and should
    // only happen once.
    Type *stcache;

    // NOTE: This type is used when the structure has not been
    // completely generated, but is a valid pointer to where the
    // type will be generated to.
    Type *pending_type;

    // NOTE: Used to store statically bound expressions in the struct.
    Scope* scope;

    struct Entity* entity_type;
    struct Entity* entity_defaults;

    OnyxFilePos polymorphic_error_loc;
    ConstraintContext constraints;

    bh_arr(AstType *)       polymorphic_argument_types;
    bh_arr(AstPolySolution) polymorphic_arguments;

    b32 pending_type_is_valid : 1;
    b32 is_union              : 1;
    b32 is_packed             : 1;
    b32 ready_to_build_type   : 1;
};
struct AstStructMember {
    AstTyped_base;
    AstTyped* initial_value;

    bh_arr(AstTyped *) meta_tags;

    b32 is_used : 1;
};
struct AstPolyStructParam {
    AstTyped_base;
};
struct AstPolyStructType {
    AstType_base;
    char *name;

    Scope *scope;
    bh_arr(AstPolyStructParam) poly_params;
    Table(AstStructType *) concrete_structs;

    AstStructType* base_struct;
};
struct AstPolyCallType {
    AstType_base;

    AstType* callee;
    Type *resolved_type;

    // NOTE: These nodes can be either AstTypes, or AstTyped expressions.
    bh_arr(AstNode *) params;
};
struct AstUnionType {
    AstType_base;
    char *name;

    bh_arr(AstUnionVariant *) variants;
    bh_arr(AstTyped *) meta_tags;

    AstType *tag_backing_type;

    // NOTE: Used to cache the actual type, since building
    // a union type is kind of complicated and should
    // only happen once.
    Type *utcache;

    // NOTE: This type is used when the union has not been
    // completely generated, but is a valid pointer to where the
    // type will be generated to.
    Type *pending_type;

    // NOTE: Used to store statically bound expressions in the union.
    Scope* scope;

    OnyxFilePos polymorphic_error_loc;
    ConstraintContext constraints;

    bh_arr(AstType *)       polymorphic_argument_types;
    bh_arr(AstPolySolution) polymorphic_arguments;

    b32 pending_type_is_valid : 1;
    // b32 ready_to_build_type   : 1;
};
struct AstUnionVariant {
    AstTyped_base;
    bh_arr(AstTyped *) meta_tags;

    AstTyped *explicit_tag_value;
};
struct AstPolyUnionType {
    AstType_base;
    char *name;

    Scope *scope;
    bh_arr(AstPolyStructParam) poly_params;
    Table(AstUnionType *) concrete_unions;

    AstUnionType* base_union;
};
struct AstEnumType {
    AstType_base;
    char *name;
    Scope *scope;

    AstType *backing;
    Type    *backing_type;

    bh_arr(AstEnumValue *) values;

    // NOTE: Used to cache the actual type for the same reason as above.
    Type *etcache;

    b32 is_flags : 1;
};
struct AstEnumValue    { AstTyped_base; AstTyped* value; };
struct AstTypeAlias    { AstType_base; AstType* to; };
struct AstTypeRawAlias { AstType_base; Type* to; };
struct AstCompoundType {
    AstType_base;

    bh_arr(AstType *) types;
};
struct AstTypeOf {
    AstType_base;

    AstTyped* expr;
    Type* resolved_type;
};
struct AstDistinctType {
    AstType_base;
    char *name;
    AstType *base_type;
    Type *dtcache;
    Scope *scope;
};

// Top level nodes
struct AstAlias         { AstTyped_base; AstTyped* alias; };
struct AstInclude       { AstNode_base;  AstTyped* name_node; char* name; b32 recursive: 1; };
struct AstBinding       {
    AstTyped_base;
    AstNode* node;

    // Used for the old '#doc' scheme
    OnyxToken *documentation_token_old;

    // Used for the new `///` scheme
    const char *documentation_string;
};
struct AstInjection     {
    AstTyped_base;
    AstTyped* full_loc;
    AstTyped* dest;
    OnyxToken *symbol;
    AstBinding *binding;
};
struct AstMemRes        {
    AstTyped_base;
    AstTyped *initial_value;

    struct Entity *type_entity;

    bh_arr(AstTyped *) tags;

    b32 threadlocal : 1;

    // Set and used in the wasm emission.
    u32 data_id;
    u32 tls_offset;
};
struct AstGlobal        {
    AstTyped_base;

    char* name;

    ForeignReference foreign;
};
struct AstParam {
    // HACK CLEANUP: This does not need to have a local buried inside of it.
    // Convert this to be AstTyped_base and pull the ParamPassType from AstLocal
    // to here.                                        - brendanfh 2020/09/18
    AstLocal *local;
    AstTyped *default_value;

    VarArgKind vararg_kind;
    b32 is_used       : 1;
    b32 use_processed : 1;
    b32 is_baked      : 1;
};
typedef struct OverloadOption OverloadOption;
struct OverloadOption {
    // This is u64 because padding will make it that anyway.
    // Consider: would there be any practical benefit to having the precedence setting
    // be a compile-time known value? as opposed to a hardcoded value?
    u64 order;

    AstTyped* option;
};

struct AstOverloadedFunction {
    AstTyped_base;

    bh_arr(OverloadOption) overloads;

    // CLEANUP: This is unused right now, but should be used to cache
    // the complete set of overloads that can be used by an overloaded
    // function.
    bh_imap            all_overloads;

    AstType *expected_return_node;
    Type    *expected_return_type;

    AstBinding *original_binding_to_node;

    b32 locked : 1;
    b32 only_local_functions : 1;
};

// @CLEANUP: Is this really necessary?
typedef struct InterfaceParam {
    OnyxToken *value_token;
    AstType   *value_type;
    Type      *type;
} InterfaceParam;

typedef struct InterfaceSentinel {
    OnyxToken *name;
    AstType   *type;
} InterfaceSentinel;

typedef struct InterfaceConstraint {
    AstTyped *expr;
    AstType  *expected_type_expr;
    Type     *expected_type;

    char *error_msg;

    b32 invert_condition: 1;
} InterfaceConstraint;

struct AstInterface {
    AstTyped_base;
    char *name;

    bh_arr(InterfaceParam)      params;
    bh_arr(InterfaceSentinel)   sentinels;
    bh_arr(InterfaceConstraint) exprs;

    Scope *scope;

    b32 is_intrinsic: 1;
};

typedef enum ConstraintPhase {
    Constraint_Phase_Waiting_To_Be_Queued = 0,
    Constraint_Phase_Cloning_Expressions  = 1,
    Constraint_Phase_Checking_Expressions = 2,
    Constraint_Phase_Finished             = 3,
} ConstraintPhase;

struct AstConstraint {
    AstNode_base;

    ConstraintPhase        phase;
    ConstraintCheckStatus* report_status;

    union {
        struct {
            AstInterface *              interface;
            bh_arr(AstTyped *)          args;
            Scope*                      scope;
            bh_arr(InterfaceConstraint) exprs;
            u32                         expr_idx;
        };
        AstTyped *const_expr; // only used when flags & Ast_Flag_Constraint_Is_Expression
    };
};


struct AstPackage {
    AstTyped_base;

    // Allocated in the ast arena
    char * package_name;

    // NOTE: Symbol nodes
    bh_arr(OnyxToken *) path;

    Package* package;
};

typedef struct QualifiedImport {
    OnyxToken* symbol_name;
    OnyxToken* as_name;
} QualifiedImport;

struct AstImport {
    AstNode_base;

    AstPackage *imported_package;

    bh_arr(QualifiedImport) only;

    // When true, it means a list of imports
    // was specified and the top-level package
    // should not be imported.
    b32 specified_imports;

    // When true, even if a list of specified
    // imports was given, the package name should
    // also be imported.
    //
    //     use core {package, *}
    //
    b32 import_package_itself;

    // Set to be the symbol that the package will
    // be imported as. If NULL, the last token of
    // the package path is used. The following
    // imports the core package as C.
    //
    //     use core { C :: package }
    //
    OnyxToken *qualified_package_name;
};


//
// Polymorphic procedures
//

typedef enum PolyParamKind {
    // Dont love these names
    PPK_Undefined,
    PPK_Poly_Type,
    PPK_Baked_Value,
} PolyParamKind;

typedef enum PolySolutionKind {
    PSK_Undefined,
    PSK_Type,
    PSK_Value,
} PolySolutionKind;

typedef enum PolyProcLookupMethod {
    PPLM_By_Arguments,
    PPLM_By_Function_Type,
} PolyProcLookupMethod;

struct AstPolyParam {
    PolyParamKind kind;

    // The parameter index where the polymorphic variable occurs.
    u32 idx;

    // The symbol node that represents the polymorphic variable.
    AstNode* poly_sym;

    // The type expression that contains `poly_sym` in it somewhere.
    // Matching polymorphic variables does a parallel tree traversal
    // to find the pairing of the actual type and polymorphic variable
    // symbol.
    AstType* type_expr;

    // Used for baked values. The expected type of the parameter.
    Type* type;

    // Used to store interface specified with $T/Interface.
    AstNode *implicit_interface;
};

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

struct AstSolidifiedFunction {
    AstFunction   *func;
    struct Entity *func_header_entity;
};

struct AstFunction {
    AstTyped_base;

    Scope *scope;

    bh_arr(AstParam) params;
    AstType* return_type;
    bh_arr(AstLocal *) named_return_locals;

    AstBlock *body;

    char* name;
    char* assembly_name;

    // NOTE: This is NULL, unless this function was generated from a polymorphic
    // procedure call. Then it is set to the token of the call node.
    OnyxToken* generated_from;
    Scope*     poly_scope;

    // NOTE: This is NULL, unless this function is used in a "#export" directive.
    // It is undefined which name it will have if there are multiple export directives
    // for this particular function.
    OnyxToken* exported_name;

    OnyxToken* closing_brace;

    union {
        OnyxToken* intrinsic_name;

        ForeignReference foreign;
    };

    struct Entity* entity_header;
    struct Entity* entity_body;

    ConstraintContext constraints;

    bh_arr(AstTyped *) tags;

    AstStrLit *deprecated_warning;

    // Polymorphic procedures use the following fields
    Scope *parent_scope_of_poly_proc;
    bh_arr(AstPolyParam) poly_params;

    bh_arr(AstPolySolution) known_slns;

    Table(AstSolidifiedFunction) concrete_funcs;
    bh_imap active_queries;

    bh_arr(AstNode *) nodes_that_need_entities_after_clone;

    AstBinding *original_binding_to_node;

    AstCaptureBlock *captures;
    Scope *scope_to_lookup_captured_values;

    // NOTE: Only set on when --stack-trace is passed.
    AstLocal *stack_trace_local;

    b32 is_exported        : 1;
    b32 is_foreign         : 1;
    b32 is_foreign_dyncall : 1;
    b32 is_intrinsic       : 1;

    b32 named_return_locals_added : 1;
    b32 ready_for_body_to_be_checked : 1;
};

struct AstCaptureBlock {
    AstNode_base;

    bh_arr(AstCaptureLocal *) captures;

    u32 total_size_in_bytes;
};

struct AstCaptureLocal {
    AstTyped_base;

    AstTyped *captured_value;

    u32 offset;
    b32 by_reference: 1;
};

struct AstPolyQuery {
    AstNode_base;

    AstFunction *proc;
    PolyProcLookupMethod pp_lookup;
    ptr given;
    OnyxToken *error_loc;

    bh_arr(AstPolySolution) slns;

    AstFunction *function_header;

    b32 error_on_fail : 1;     // Whether or not to report errors on failing to match.
};


struct AstDirectiveError {
    AstNode_base;

    OnyxToken* error_msg;
};

struct AstDirectiveAddOverload {
    AstNode_base;

    // NOTE: used by the #add_overload directive. Initially set to a symbol,
    // then resolved to an overloaded function.
    AstNode *overloaded_function;

    // See note in OverloadOption. This could be refactored into an OverloadOption?
    u64 order;
    AstTyped *overload;
};

struct AstDirectiveOperator {
    AstNode_base;

    BinaryOp operator;
    OnyxToken *operator_token;

    u64 order;
    AstTyped *overload;
};

struct AstDirectiveExport {
    AstNode_base;

    OnyxToken* export_name;
    AstTyped* export_name_expr;
    AstTyped* export;
};

struct AstDirectiveDefined {
    AstTyped_base;
    AstTyped *expr;

    b32 is_defined: 1;
};

struct AstDirectiveRemove {
    AstNode_base;
};

struct AstDirectiveFirst {
    AstTyped_base;

    AstIfWhile *while_node;
};

struct AstDirectiveExportName {
    AstTyped_base;
    AstFunction *func;
    AstStrLit   *name;
    b32 created_export_entity : 1;
};

struct AstDirectiveWasmSection {
    AstNode_base;
    AstTyped *section_name;
    AstTyped *section_contents;

    char *name;
    char *contents;
    u32 length;

    b32 from_file : 1;
};


struct AstCallSite {
    AstTyped_base;

    OnyxToken* callsite_token;

    AstStrLit* filename;
    AstNumLit* line;
    AstNumLit* column;

    b32 collapsed : 1;
};

typedef struct CodeBlockBindingSymbol {
    OnyxToken *symbol;
    AstType   *type_node; // This can be NULL if no type was given.
} CodeBlockBindingSymbol;

// Represents a "pastable" block of code.
struct AstCodeBlock {
    AstTyped_base;

    AstNode *code;
    bh_arr(CodeBlockBindingSymbol) binding_symbols;

    b32 is_expression: 1;
};

struct AstDirectiveInsert {
    AstTyped_base;

    AstTyped *code_expr;
    bh_arr(AstTyped *) binding_exprs;

    // Set when using #skip_scope
    AstTyped *skip_scope_index;
};

struct AstDirectiveInit {
    AstNode_base;

    AstTyped *init_proc;
    bh_arr(AstDirectiveInit *) dependencies;
};

struct AstMacro {
    AstTyped_base;

    AstTyped* body;

    AstBinding *original_binding_to_node;
};

struct AstDirectiveLibrary {
    AstNode_base;

    AstTyped *library_symbol; // This should resolve to a string literal
    char *library_name;
};

struct AstForeignBlock {
    AstTyped_base;

    Scope* scope;
    AstTyped *module_name;
    bh_arr(struct Entity *) captured_entities;

    u32 foreign_block_number;

    b32 uses_dyncall : 1;
};

struct AstJsNode {
    AstNode_base;

    u32 order; 
    AstTyped *order_expr;

    AstTyped *code;
    AstTyped *filepath;
};


struct AstCompilerExtension {
    AstNode_base;

    OnyxToken *name;
    bh_arr(AstProceduralMacro *) proc_macros;

    i32 extension_id;
};

struct AstProceduralMacro {
    AstNode_base;

    // name is stored in the `token`

    AstCompilerExtension *extension;
};

struct AstProceduralExpansion {
    AstTyped_base;

    AstTyped *proc_macro;
    OnyxToken *expansion_body;

    u32 expansion_id;
};

// Need to forward declare this for later on.
typedef struct Context Context;


typedef struct EntityJobData {
    enum TypeMatch (*func)(Context *context, void *job_data);
    void *job_data;
} EntityJobData;

typedef enum EntityState {
    Entity_State_Error,

    Entity_State_Parse_Builtin,
    Entity_State_Introduce_Symbols,
    Entity_State_Parse,
    Entity_State_Resolve_Symbols,
    Entity_State_Check_Types,
    Entity_State_Code_Gen,
    Entity_State_Finalized,
    Entity_State_Failed,

    Entity_State_Count,
} EntityState;

extern const char* entity_state_strings[Entity_State_Count];

// NOTE: An Entity represents something will need to be
// processed later down the pipeline.
typedef enum EntityType {
    Entity_Type_Unknown,

    Entity_Type_Error,
    Entity_Type_Load_Path,
    Entity_Type_Load_File,
    Entity_Type_Binding,
    Entity_Type_Use_Package,
    Entity_Type_Import,
    Entity_Type_Static_If,
    Entity_Type_String_Literal,
    Entity_Type_File_Contents,
    Entity_Type_Compiler_Extension,
    Entity_Type_Procedural_Expansion,
    Entity_Type_Enum,
    Entity_Type_Enum_Value,
    Entity_Type_Type_Alias,
    Entity_Type_Memory_Reservation_Type,
    Entity_Type_Use,
    Entity_Type_Interface,
    Entity_Type_Constraint_Check,
    Entity_Type_Polymorphic_Proc,
    Entity_Type_Polymorph_Query,
    Entity_Type_Foreign_Block,
    Entity_Type_Macro,
    Entity_Type_Foreign_Function_Header,
    Entity_Type_Temp_Function_Header,    // Same as a Function_Header, except it disappears after it checks completely.
    Entity_Type_Function_Header,
    Entity_Type_Global_Header,
    Entity_Type_Process_Directive,
    Entity_Type_Struct_Member_Default,
    Entity_Type_Memory_Reservation,
    Entity_Type_Expression,
    Entity_Type_Job,                    // Represents an arbitrary job (function pointer).
    Entity_Type_Global,
    Entity_Type_Overloaded_Function,
    Entity_Type_Function,
    Entity_Type_JS,

    Entity_Type_Count,
} EntityType;

extern const char* entity_type_strings[Entity_Type_Count];

typedef struct Entity {
    u32 id;

    EntityType type;
    EntityState state;

    // TODO: Document this!
    u32 macro_attempts;
    u32 micro_attempts;

    b32 entered_in_queue : 1;

    Package *package;
    Scope *scope;

    // TODO: This is incomplete. Add proper cycle detection and halting.
    // struct Entity *waiting_on;

    union {
        AstDirectiveError     *error;
        AstInclude            *include;
        AstImport             *import;
        AstBinding            *binding;
        AstIf                 *static_if;
        AstFunction           *function;
        AstOverloadedFunction *overloaded_function;
        AstGlobal             *global;
        AstTyped              *expr;
        AstStrLit             *strlit;
        AstFileContents       *file_contents;
        AstType               *type_alias;
        AstEnumType           *enum_type;
        AstEnumValue          *enum_value;
        AstMemRes             *mem_res;
        AstFunction           *poly_proc;
        AstPolyQuery          *poly_query;
        AstForeignBlock       *foreign_block;
        AstMacro              *macro;
        AstInterface          *interface;
        AstConstraint         *constraint;
        AstDirectiveLibrary   *library;
        EntityJobData         *job_data;
        AstJsNode             *js;
        AstCompilerExtension  *compiler_extension;
        AstProceduralExpansion *proc_expansion;
    };
} Entity;

typedef struct EntityHeap {
    bh_arena entity_arena;
    bh_allocator allocator;

    bh_arr(Entity *) entities;
    bh_arr(Entity *) quick_unsorted_entities;
    i32 next_id;

    i32 state_count[Entity_State_Count];
    i32 type_count[Entity_Type_Count];

    i32 all_count[Entity_State_Count][Entity_Type_Count];
} EntityHeap;

void entity_heap_init(bh_allocator a, EntityHeap* entities);
void entity_heap_insert_existing(EntityHeap* entities, Entity* e);
Entity* entity_heap_insert(EntityHeap* entities, Entity e);
Entity* entity_heap_top(EntityHeap* entities);
void entity_heap_change_top(EntityHeap* entities, Entity* new_top);
void entity_heap_remove_top(EntityHeap* entities);
void entity_change_type(EntityHeap* entities, Entity *ent, EntityType new_type);
void entity_change_state(EntityHeap* entities, Entity *ent, EntityState new_state);
void entity_heap_add_job(EntityHeap *entities, enum TypeMatch (*func)(Context *, void *), void *job_data);

// If target_arr is null, the entities will be placed directly in the heap.
void add_entities_for_node(EntityHeap *entities, bh_arr(Entity *)* target_arr, AstNode* node, Scope* scope, Package* package);

void check_entity(Context *context, Entity* ent);
void emit_entity(Context *context, Entity* ent);

struct Package {
    char *name;
    char *unqualified_name;

    Scope *scope;
    Scope *private_scope;

    u32 id;

    // NOTE: id's of the sub-packages
    bh_arr(u32) sub_packages;
    i32 parent_id;

    // NOTE: This tracks all of the 'use package' statements of this package throughout
    // the code base. This is used when a static if clears and new symbols are introduced.
    // 'use package' statements have to be reevaluated to pull in the new symbols.
    bh_arr(Entity *) use_package_entities;

    // NOTE: This tracks all #package_doc statements used for this package.
    bh_arr(OnyxToken *) doc_string_tokens;

    bh_arr(const char *) doc_strings;

    // NOTE: These are entities that are stored in packages marked with `#allow_stale_code`.
    // These entities are flushed to the entity heap when the package has been explicit used
    // somewhere.
    bh_arr(Entity *) buffered_entities;
    b32 is_included_somewhere : 1;
};

typedef enum CompileAction CompileAction;
enum CompileAction {
    ONYX_COMPILE_ACTION_COMPILE,
    ONYX_COMPILE_ACTION_CHECK,
    ONYX_COMPILE_ACTION_RUN,
    ONYX_COMPILE_ACTION_RUN_WASM,
    ONYX_COMPILE_ACTION_WATCH,
    ONYX_COMPILE_ACTION_WATCH_RUN,
    ONYX_COMPILE_ACTION_DOCUMENT,
    ONYX_COMPILE_ACTION_PRINT_HELP,
    ONYX_COMPILE_ACTION_PRINT_VERSION,
    ONYX_COMPILE_ACTION_SELF_UPGRADE,
};


// ROBUSTNESS: The Runtime definitions here must match those in build_opts.onyx!!
typedef enum Runtime Runtime;
enum Runtime {
    Runtime_Unknown = 0,
    Runtime_Onyx    = 1,
    Runtime_Wasi    = 2,
    Runtime_Js      = 3,
    Runtime_Custom  = 4,
};


typedef struct OnyxDocInfo {
    bh_arr(AstBinding *) procedures;
    bh_arr(AstBinding *) structures;
    bh_arr(AstBinding *) enumerations;
    bh_arr(AstBinding *) distinct_types;
    bh_arr(AstBinding *) unions;

    Table(u32) file_ids;
    u32 next_file_id;
} OnyxDocInfo;

typedef enum CheckerMode {
    CM_Dont_Resolve_Symbols    = BH_BIT(1),
    CM_Dont_Check_Case_Bodies  = BH_BIT(2),
    CM_Allow_Init_Expressions  = BH_BIT(3),
} CheckerMode;

typedef struct CheckerData {
    b32 expression_types_must_be_known;
    b32 all_checks_are_final;
    bh_arr(AstIfWhile *) while_node_stack;
    bh_imap __binop_impossible_cache[Binary_Op_Count];
    AstCall __op_maybe_overloaded;
    Entity *current_entity;
    bh_arr(Type **) expected_return_type_stack;
    bh_arr(bh_arr(AstLocal *)) named_return_values_stack;

    u32 current_checking_level;
    CheckerMode mode;

    Scope *current_scope;
    bh_arr(Scope *) scope_stack;

    b32 resolved_a_symbol;
} CheckerData;

typedef struct ClonerData {
    u32 clone_depth;
    b32 dont_copy_structs;

    bh_arr(AstNode *) captured_entities;
} ClonerData;

typedef struct PolymorphData {
    // This flag is used by some of the procedures that try working with polymorphic things,
    // but need to wait until more information is known. Instead of passing a out parameter
    // into each of these procedures, a single global variable is used instead. If the type
    // checker ever gets multi-threaded, this would have to become a threadlocal variable.
    b32 flag_to_yield;

    // This flag is used in the very special case that you are passing a polymorphic procedure
    // to a polymorphic procedure, and you have enough information to instantiate said procedure
    // in order to resolve the type of one of the return values.
    b32 doing_nested_polymorph_lookup;
} PolymorphData;

typedef struct ContextCaches {
    bh_imap implicit_cast_to_bool_cache;
} ContextCaches;

typedef struct DefinedVariable {
    char *key;
    char *value;
} DefinedVariable;


typedef enum ProceduralMacroExpansionKind {
    PMEK_Expression,
    PMEK_Statement,
    PMEK_Top_Level
} ProceduralMacroExpansionKind;

typedef enum CompilerExtensionState {
    COMP_EXT_STATE_SPAWNING,
    COMP_EXT_STATE_INITIATING,
    COMP_EXT_STATE_READY,
    COMP_EXT_STATE_EXPANDING,
    COMP_EXT_STATE_HANDLING_HOOK,
} CompilerExtensionState;

typedef struct CompilerExtension {
    u32 id;

    u64 pid;
    u64 send_file;
    u64 recv_file;

    char *name;

    i32 current_expansion_id;
    CompilerExtensionState state;

    Entity *entity;

    bh_arena arena;

    b32 alive : 1;

    b32 supports_stalled_hook : 1;
} CompilerExtension;

typedef struct CompileOptions CompileOptions;
struct CompileOptions {
    bh_allocator allocator;

    b32 print_static_if_results : 1;
    b32 no_file_contents        : 1;
    b32 no_compiler_extensions  : 1;

    b32 use_post_mvp_features : 1;
    b32 use_multi_threading   : 1;
    b32 generate_foreign_info : 1;
    b32 generate_type_info    : 1;
    b32 generate_method_info  : 1;
    b32 generate_name_section : 1;
    b32 generate_odoc         : 1;
    b32 no_core               : 1;
    b32 no_stale_code         : 1;
    b32 show_all_errors       : 1;

    b32 enable_optional_semicolons : 1;

    b32 generate_symbol_info_file : 1;
    b32 generate_lsp_info_file    : 1;

    b32 running_perf : 1;

    Runtime runtime;

    bh_arr(bh_mapped_folder) mapped_folders;
    const char* symbol_info_file;

    b32 debug_session;
    b32 debug_info_enabled;
    b32 stack_trace_enabled;
};

typedef struct CompilerBasicTypes CompilerBasicTypes;
struct CompilerBasicTypes {
    AstBasicType type_void;
    AstBasicType type_bool;
    AstBasicType type_i8;
    AstBasicType type_u8;
    AstBasicType type_i16;
    AstBasicType type_u16;
    AstBasicType type_i32;
    AstBasicType type_u32;
    AstBasicType type_i64;
    AstBasicType type_u64;
    AstBasicType type_f32;
    AstBasicType type_f64;
    AstBasicType type_rawptr;
    AstBasicType type_type_expr; // :TypeExprHack

    AstBasicType type_int_unsized;
    AstBasicType type_float_unsized;

    AstBasicType type_i8x16;
    AstBasicType type_i16x8;
    AstBasicType type_i32x4;
    AstBasicType type_i64x2;
    AstBasicType type_f32x4;
    AstBasicType type_f64x2;
    AstBasicType type_v128;

    AstBasicType type_auto_return;
};

typedef struct CompilerBuiltins CompilerBuiltins;
struct CompilerBuiltins {
    AstGlobal heap_start;
    AstGlobal stack_top;
    AstGlobal tls_base;
    AstGlobal tls_size;
    AstGlobal closure_base;
    AstGlobal stack_trace;
    AstType  *string_type;
    AstType  *cstring_type;
    AstType  *range_type;
    Type     *range_type_type;
    AstType  *range64_type;
    Type     *range64_type_type;
    AstType  *vararg_type;
    Type     *vararg_type_type;
    AstTyped *context_variable;
    AstType  *allocator_type;
    AstType  *iterator_type;
    AstType  *optional_type;
    AstType  *callsite_type;
    AstType  *any_type;
    AstType  *code_type;
    AstType  *link_options_type;
    AstType  *package_id_type;
    AstType  *stack_trace_type;
    AstType  *slice_type;
    AstType  *array_type;
    AstTyped *type_table_node;
    AstTyped *foreign_blocks_node;
    AstType  *foreign_block_type;
    AstTyped *tagged_procedures_node;
    AstTyped *tagged_globals_node;
    AstFunction *initialize_data_segments;
    AstFunction *run_init_procedures;
    AstFunction *closure_block_allocate;
    bh_arr(AstFunction *) init_procedures;
    AstOverloadedFunction *implicit_bool_cast;
    AstOverloadedFunction *dispose_used_local;

    AstType *for_expansion_flag_type;
    AstOverloadedFunction *for_expansion;
};

typedef struct TypeStore TypeStore;
struct TypeStore {
    bh_imap type_map;

    bh_imap pointer_map;
    bh_imap multi_pointer_map;
    bh_imap array_map;
    bh_imap slice_map;
    bh_imap dynarr_map;
    bh_imap vararg_map;
    Table(u64) func_map;

    Type* basic[Basic_Kind_Count];
    Type* auto_return;
};

typedef struct CompilerStats CompilerStats;
struct CompilerStats {
    u64 lexer_lines_processed;
    u64 lexer_tokens_processed;

    u64 microseconds_per_state[Entity_State_Count];
    u64 microseconds_per_type[Entity_Type_Count];
};

typedef struct SpecialGlobalEntities SpecialGlobalEntities;
struct SpecialGlobalEntities {
    u32 remaining;
    Entity *runtime_info_types_entity;
    Entity *runtime_info_foreign_entity;
    Entity *runtime_info_proc_tags_entity;
    Entity *runtime_info_global_tags_entity;
    Entity *runtime_info_stack_trace_entity;
};

typedef struct CompilerEventField {
    struct CompilerEventField *next;
    char *field;
    u32 type;
    union {
        char *s;
        i32   i;
    };
} CompilerEventField;

typedef struct CompilerEvent {
    struct CompilerEvent *next;

    u32 type;
    CompilerEventField *first_field;
} CompilerEvent;

typedef struct EventSystem {
    bh_arena     event_arena;
    bh_allocator event_alloc;

    i32 event_count;

    CompilerEvent *first;
    CompilerEvent *last;
} EventSystem;

struct Context {
    Table(Package *)      packages;
    EntityHeap            entities;

    Scope *global_scope;

    CompileOptions* options;

    bh_arena                  ast_arena;
    bh_allocator token_alloc, ast_alloc;

    bh_scratch scratch;
    bh_allocator scratch_alloc;

    bh_managed_heap heap;
    bh_allocator gp_alloc; // General purpose allocator 

    bh_arr(bh_file_contents) loaded_files;
    bh_arr(DefinedVariable) defined_variables;

    // NOTE: This is defined in wasm_emit.h
    struct OnyxWasmModule* wasm_module;
    bh_buffer generated_wasm_buffer;
    bh_buffer generated_js_buffer;
    bh_buffer generated_odoc_buffer;
    bh_buffer generated_osym_buffer;

    struct SymbolInfoTable *symbol_info;
    struct OnyxDocInfo     *doc_info;

    bh_arr(CompilerExtension) extensions;
    u32 next_expansion_id;

    CompilerBuiltins builtins;
    CompilerBasicTypes basic_types;
    TypeStore types;

    CheckerData   checker;
    ClonerData    cloner;
    PolymorphData polymorph;
    ContextCaches caches;


    // TODO: Move these
    bh_arr(OverloadOption) operator_overloads[Binary_Op_Count];
    bh_arr(OverloadOption) unary_operator_overloads[Unary_Op_Count];

    // The name is pretty self-descriptive, but this is a node that is returned from things
    // like polymorphic_proc_lookup when it is determined that everything works so far, but
    // the caller must yield in order to finish checking this polymorphic procedure.
    AstTyped node_that_signals_a_yield;
    AstTyped node_that_signals_failure;

    // Currently, this only needs to exist so all the scope's symbol array can be freed later.
    bh_arr(Scope *) scopes;

    EventSystem events;

    OnyxErrors errors;
    b32 errors_enabled;

    u32 next_package_id;
    u32 next_scope_id;
    u32 next_type_id;
    u32 next_entity_id;

    CompilerStats stats;

    // HACK
    SpecialGlobalEntities special_global_entities;

    Entity* watermarked_node;
    u32 highest_watermark;

    u32 cycle_almost_detected : 3;
    b32 cycle_detected : 1;

    b32 builtins_initialized : 1;
    b32 wasm_module_linked : 1;
};

typedef struct BuiltinSymbol {
    char*    package;
    char*    sym;
    u32      offset; // Offset into the context structure where the builtin node lives
} BuiltinSymbol;

extern const BuiltinSymbol builtin_symbols[];

typedef struct IntrinsicMap {
    char*         name;
    OnyxIntrinsic intrinsic;
} IntrinsicMap;

extern const IntrinsicMap builtin_intrinsics[];

void prepare_builtins(Context *context);
void initialize_builtins(Context *context);
void initalize_special_globals(Context *context);
void introduce_build_options(Context *context);


// NOTE: Useful not inlined functions
AstTyped* ast_reduce(Context *context, AstTyped* node);
AstNode* ast_clone(Context *context, void* n);
AstNode* ast_clone_with_captured_entities(Context *context, void* n, bh_arr(AstNode *)* ents);
AstFunction* clone_function_header(Context *context, AstFunction* func);
void clone_function_body(Context *context, AstFunction* dest, AstFunction* source);

void promote_numlit_to_larger(Context *context, AstNumLit* num);
b32 convert_numlit_to_type(Context *context, AstNumLit* num, Type* type, b32 permanent);

typedef enum TypeMatch {
    TYPE_MATCH_SUCCESS,
    TYPE_MATCH_FAILED,
    TYPE_MATCH_YIELD,
    TYPE_MATCH_SPECIAL, // Only used for nest polymorph function lookups
} TypeMatch;

#define unify_node_and_type(ctx, node, type) (unify_node_and_type_((ctx), (node), (type), 1))
TypeMatch unify_node_and_type_(Context *context, AstTyped** pnode, Type* type, b32 permanent);

// resolve_expression_type is a permanent action that modifies
// the node in whatever is necessary to cement a type into it.
Type* resolve_expression_type(Context *context, AstTyped* node);

// query_expression_type does not modify the node at all, but
// does its best to deduce the type of the node without context.
Type* query_expression_type(Context *context, AstTyped *node);

i64   get_expression_integer_value(Context *context, AstTyped* node, b32 *out_is_valid);
char *get_expression_string_value(Context *context, AstTyped* node, b32 *out_is_valid);

b32 cast_is_legal(Context *context, Type* from_, Type* to_, char** err_msg);
char* get_function_name(Context *context, AstFunction* func);
char* get_function_assembly_name(Context *context, AstFunction* func);
char* generate_name_within_scope(Context *context, Scope *scope, OnyxToken* symbol);

TypeMatch implicit_cast_to_bool(Context *context, AstTyped **pnode);

AstNode* strip_aliases(AstNode* node);

AstNumLit*        make_bool_literal(Context *context, b32 b);
AstNumLit*        make_int_literal(Context *context, i64 value);
AstNumLit*        make_float_literal(Context *context, f64 value);
AstRangeLiteral*  make_range_literal(Context *context, AstTyped* low, AstTyped* high);
AstStrLit*        make_string_literal(Context *context, OnyxToken *token);
AstBinaryOp*      make_binary_op(Context *context, BinaryOp operation, AstTyped* left, AstTyped* right);
AstArgument*      make_argument(Context *context, AstTyped* value);
AstFieldAccess*   make_field_access(Context *context, AstTyped* node, char* field);
AstAddressOf*     make_address_of(Context *context, AstTyped* node);
AstLocal*         make_local(Context *context, OnyxToken* token, AstType* type_node);
AstLocal*         make_local_with_type(Context *context, OnyxToken* token, Type* type);
AstNode*          make_symbol(Context *context, OnyxToken* sym);
AstUnaryOp*       make_cast(Context *context, AstTyped* expr, Type* to);
AstZeroValue*     make_zero_value(Context *context, OnyxToken *token, Type* type);
AstStructLiteral* make_optional_literal_some(Context *context, AstTyped *expr, Type* opt_type);
AstStructLiteral* make_union_variant_of_void(Context *context, Type* union_type, OnyxToken* token, UnionVariant* variant);

void arguments_initialize(Context *context, Arguments* args);
b32 fill_in_arguments(Context *context, Arguments* args, AstNode* provider, char** err_msg, b32 insert_zero_values);
void arguments_ensure_length(Context *context, Arguments* args, u32 count);
void arguments_copy(Context *context, Arguments* dest, Arguments* src);
void arguments_clone(Context *context, Arguments* dest, Arguments* src);
void arguments_deep_clone(Context *context, Arguments* dest, Arguments* src);
void arguments_remove_baked(Arguments* args);
void arguments_clear_baked_flags(Arguments* args);
TypeMatch check_arguments_against_type(Context *context, Arguments* args, TypeFunction* func_type, VarArgKind* va_kind,
                                 OnyxToken* location, char* func_name, struct OnyxError* error);
i32 get_argument_buffer_size(Context *, TypeFunction* type, Arguments* args);

// GROSS: Using void* to avoid having to cast everything.
const char* node_get_type_name(Context *context, void* node);

b32 static_if_resolution(Context *context, AstIf* static_if);

void insert_poly_sln_into_scope(Context *context, Scope* scope, AstPolySolution *sln);
TypeMatch find_polymorphic_sln(Context *context, AstPolySolution *out, AstPolyParam *param, AstFunction *func, PolyProcLookupMethod pp_lookup, ptr actual, OnyxError* err_msg);
AstFunction* polymorphic_proc_lookup(Context *context, AstFunction* pp, PolyProcLookupMethod pp_lookup, ptr actual, OnyxToken* tkn);
AstFunction* polymorphic_proc_solidify(Context *context, AstFunction* pp, bh_arr(AstPolySolution) slns, OnyxToken* tkn);
AstNode* polymorphic_proc_try_solidify(Context *context, AstFunction* pp, bh_arr(AstPolySolution) slns, OnyxToken* tkn);
AstFunction* polymorphic_proc_build_only_header(Context *context, AstFunction* pp, PolyProcLookupMethod pp_lookup, ptr actual);
AstFunction* polymorphic_proc_build_only_header_with_slns(Context *context, AstFunction* pp, bh_arr(AstPolySolution) slns, b32 error_if_failed);
b32 potentially_convert_function_to_polyproc(Context *context, AstFunction *func);
AstPolyCallType* convert_call_to_polycall(Context *context, AstCall* call);

void insert_auto_dispose_call(Context *context, AstLocal *local);

AstCall * create_implicit_for_expansion_call(Context *context, AstFor *fornode);

typedef struct OverloadReturnTypeCheck {
    Type *expected_type;
    AstTyped *node;
    OnyxToken *group;
} OverloadReturnTypeCheck;

void add_overload_option(bh_arr(OverloadOption)* poverloads, u64 order, AstTyped* overload);
AstTyped* find_matching_overload_by_arguments(Context *context, bh_arr(OverloadOption) overloads, Arguments* args);
AstTyped* find_matching_overload_by_type(Context *context, bh_arr(OverloadOption) overloads, Type* type);
void report_unable_to_match_overload(Context *context, AstCall* call, bh_arr(OverloadOption) overloads);
void report_incorrect_overload_expected_type(Context *context, Type *given, Type *expected, OnyxToken *overload, OnyxToken *group);
void ensure_overload_returns_correct_type(Context *context, AstTyped *overload, AstOverloadedFunction *group);

void expand_macro(Context *context, AstCall** pcall, AstFunction* template);
AstFunction* macro_resolve_header(Context *context, AstMacro* macro, Arguments* args, OnyxToken* callsite, b32 error_if_failed);

Type* polymorphic_struct_lookup(Context *context, AstPolyStructType* ps_type, bh_arr(AstPolySolution) slns, OnyxFilePos pos, b32 error_if_failed);
Type* polymorphic_union_lookup(Context *context, AstPolyUnionType* pu_type, bh_arr(AstPolySolution) slns, OnyxFilePos pos, b32 error_if_failed);

b32 resolve_intrinsic_interface_constraint(Context *context, AstConstraint *constraint);

void track_declaration_for_symbol_info(Context *context, OnyxFilePos, AstNode *);
void track_documentation_for_symbol_info(Context *context, AstNode *, AstBinding *);
void track_resolution_for_symbol_info(Context *context, AstNode *original, AstNode *resolved);


// Compiler Extensions
TypeMatch compiler_extension_start(Context *context, const char *name, const char *containing_filename, Entity *ent, i32 *out_extension_id);
TypeMatch compiler_extension_expand_macro(
    Context *context,
    int extension_id,
    ProceduralMacroExpansionKind kind,
    const char *macro_name,
    OnyxToken *body,
    Entity *entity,
    AstNode **out_node,
    u32 *out_expansion_id,
    b32 wait_for_response);
TypeMatch compiler_extension_hook_stalled(Context *context, int extension_id);


// Compiler Events
void compiler_events_init(Context *context);
void compiler_events_clear(Context *context);
CompilerEvent *compiler_event_add(Context *context, u32 event_type);
void compiler_event_add_field_str(Context *context, CompilerEvent *event, char *field, char *value);
void compiler_event_add_field_int(Context *context, CompilerEvent *event, char *field, i32 value);


// NOTE: Useful inlined functions
static inline b32 is_lval(AstNode* node) {
    node = strip_aliases(node);

    if    ((node->kind == Ast_Kind_Local)
        || (node->kind == Ast_Kind_Param)
        || (node->kind == Ast_Kind_Global)
        || (node->kind == Ast_Kind_Dereference)
        || (node->kind == Ast_Kind_Subscript)
        || (node->kind == Ast_Kind_Field_Access)
        || (node->kind == Ast_Kind_Memres)
        || (node->kind == Ast_Kind_Capture_Local)
        || (node->kind == Ast_Kind_Constraint_Sentinel)) // Bit of a hack, but this makes constraints like 'T->foo()' work.
        return 1;

    if (node->kind == Ast_Kind_Compound) {
        b32 all_lval = 1;
        bh_arr_each(AstTyped *, expr, ((AstCompound *) node)->exprs) all_lval = all_lval && is_lval((AstNode *) *expr);
        return all_lval;
    }

    return 0;
}

static inline b32 binop_is_assignment(BinaryOp binop) {
    return (binop >= Binary_Op_Assign_Start && binop <= Binary_Op_Assign_End);
}

static inline b32 binop_is_compare(BinaryOp binop) {
    return (binop >= Binary_Op_Equal && binop <= Binary_Op_Greater_Equal);
}

static inline b32 node_is_type(AstNode* node) {
    node = strip_aliases(node);

    return (node->kind > Ast_Kind_Type_Start) && (node->kind < Ast_Kind_Type_End);
}

static inline b32 node_is_auto_cast(AstNode* node) {
    return (node->kind == Ast_Kind_Unary_Op) && (((AstUnaryOp *) node)->operation == Unary_Op_Auto_Cast);
}

static inline b32 node_is_addressable_literal(AstNode* node) {
    return (node->kind == Ast_Kind_Struct_Literal)
        || (node->kind == Ast_Kind_Array_Literal);
}

static inline Type* get_expression_type(Context *context, AstTyped* expr) {
    switch (expr->kind) {
        case Ast_Kind_Block: case Ast_Kind_If: case Ast_Kind_While: return NULL;
        case Ast_Kind_Typeof: return context->types.basic[Basic_Kind_Type_Index];
        default: return expr->type;
    }
}

static inline CallingConvention type_function_get_cc(Type* type) {
    if (type == NULL) return CC_Undefined;
    if (type->kind != Type_Kind_Function) return CC_Undefined;
    if (type_is_compound(type->Function.return_type)) return CC_Return_Stack;
    return CC_Return_Wasm;
}

static inline ParamPassType type_get_param_pass(Type* type) {
    if (type_should_be_passed_like_a_struct(type) && !type_struct_is_just_one_basic_value(type)) {
        if (type_structlike_is_simple(type)) return Param_Pass_By_Multiple_Values;
        else                                 return Param_Pass_By_Implicit_Pointer;
    }
    return Param_Pass_By_Value;
}

static inline AstFunction* get_function_from_node(AstNode* node) {
    if (node->kind == Ast_Kind_Function) return (AstFunction *) node;
    if (node->kind == Ast_Kind_Polymorphic_Proc) return (AstFunction *) node;
    if (node->kind == Ast_Kind_Macro) return get_function_from_node((AstNode*) ((AstMacro *) node)->body);
    return NULL;
}

static inline AstCall *get_call_expr_from_node(AstNode *node) {
    if (node->kind == Ast_Kind_Call) return (AstCall *) node;
    if (node->kind == Ast_Kind_Method_Call) return (AstCall *) ((AstBinaryOp *) node)->right;
    return NULL;
}

static inline void convert_polyproc_to_function(AstFunction *func) {
    if (func->kind != Ast_Kind_Polymorphic_Proc) return;

    func->kind = Ast_Kind_Function;
    func->parent_scope_of_poly_proc = NULL;
    func->poly_params = NULL;
    func->known_slns = NULL;
    func->concrete_funcs = NULL;
    func->active_queries.hashes = NULL;
    func->active_queries.entries = NULL;
    func->poly_scope = NULL;
    func->entity = NULL;
    func->type = NULL;
    func->tags = NULL;
}

static inline void convert_function_to_polyproc(Context *context, AstFunction *func) {
    if (func->kind != Ast_Kind_Function) return;

    func->kind = Ast_Kind_Polymorphic_Proc;
    func->parent_scope_of_poly_proc = func->scope->parent;
    func->scope = NULL;
    if (func->entity) entity_change_type(&context->entities, func->entity, Entity_Type_Polymorphic_Proc);
}

#endif // #ifndef ONYXASTNODES_H
