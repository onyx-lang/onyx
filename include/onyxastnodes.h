#ifndef ONYXASTNODES_H
#define ONYXASTNODES_H

#include "onyxlex.h"

typedef struct AstNode AstNode;
typedef struct AstNodeTyped AstNodeTyped;
typedef struct AstNodeUnaryOp AstNodeUnaryOp;
typedef struct AstNodeBinOp AstNodeBinOp;
typedef struct AstNodeAssign AstNodeAssign;
typedef struct AstNodeNumLit AstNodeNumLit;
typedef struct AstNodeLocal AstNodeLocal;
typedef struct AstNodeScope AstNodeScope;
typedef struct AstNodeReturn AstNodeReturn;
typedef struct AstNodeBlock AstNodeBlock;
typedef struct AstNodeIf AstNodeIf;
typedef struct AstNodeWhile AstNodeWhile;
typedef struct AstNodeFunction AstNodeFunction;
typedef struct AstNodeForeign AstNodeForeign;
typedef struct AstNodeGlobal AstNodeGlobal;
typedef struct AstNodeCall AstNodeCall;
typedef struct AstNodeArgument AstNodeArgument;
typedef struct AstNodeUse AstNodeUse;

typedef enum AstNodeKind {
    AST_NODE_KIND_ERROR,
    AST_NODE_KIND_PROGRAM,
    AST_NODE_KIND_USE,

    AST_NODE_KIND_FUNCTION,
    AST_NODE_KIND_FOREIGN,
    AST_NODE_KIND_BLOCK,
    AST_NODE_KIND_SCOPE,
    AST_NODE_KIND_LOCAL,
    AST_NODE_KIND_GLOBAL,
    AST_NODE_KIND_SYMBOL,

    AST_NODE_KIND_UNARY_OP,
    AST_NODE_KIND_BIN_OP,

    AST_NODE_KIND_TYPE,
    AST_NODE_KIND_LITERAL,
    AST_NODE_KIND_PARAM,
    AST_NODE_KIND_ARGUMENT,
    AST_NODE_KIND_CALL,
    AST_NODE_KIND_ASSIGNMENT,
    AST_NODE_KIND_RETURN,

    AST_NODE_KIND_IF,
    AST_NODE_KIND_WHILE,
    AST_NODE_KIND_BREAK,
    AST_NODE_KIND_CONTINUE,

    AST_NODE_KIND_COUNT
} AstNodeKind;

typedef enum TypeInfoKind {
    TYPE_INFO_KIND_UNKNOWN,
    TYPE_INFO_KIND_VOID,
    TYPE_INFO_KIND_BOOL,

    TYPE_INFO_KIND_UINT32,
    TYPE_INFO_KIND_UINT64,

    TYPE_INFO_KIND_INT32,
    TYPE_INFO_KIND_INT64,

    TYPE_INFO_KIND_FLOAT32,
    TYPE_INFO_KIND_FLOAT64,
    TYPE_INFO_KIND_SOFT_FLOAT, // 64-bits of data but could be treated as 32-bit
} TypeInfoKind;

typedef struct TypeInfo {
    TypeInfoKind kind;
    u32 size; // in bytes
    const char* name;
    u32 is_int : 1;
    u32 is_unsigned : 1;
    u32 is_float : 1;
    u32 is_bool : 1;
    u32 is_known : 1;
} TypeInfo;

extern TypeInfo builtin_types[];

// NOTE: Some of these flags will overlap since there are
// only 32-bits of flags to play with
typedef enum OnyxAstFlags {
    // Top-level flags
    ONYX_AST_FLAG_EXPORTED        = BH_BIT(0),
    ONYX_AST_FLAG_LVAL            = BH_BIT(1),
    ONYX_AST_FLAG_CONST           = BH_BIT(2),
    ONYX_AST_FLAG_COMPTIME        = BH_BIT(3),
} OnyxAstFlags;

typedef enum OnyxUnaryOp {
    ONYX_UNARY_OP_NEGATE,
    ONYX_UNARY_OP_NOT,
    ONYX_UNARY_OP_CAST,
} OnyxUnaryOp;

typedef enum OnyxBinaryOp {
    ONYX_BINARY_OP_ADD           = 0,
    ONYX_BINARY_OP_MINUS         = 1,
    ONYX_BINARY_OP_MULTIPLY      = 2,
    ONYX_BINARY_OP_DIVIDE        = 3,
    ONYX_BINARY_OP_MODULUS       = 4,

    ONYX_BINARY_OP_EQUAL         = 5,
    ONYX_BINARY_OP_NOT_EQUAL     = 6,
    ONYX_BINARY_OP_LESS          = 7,
    ONYX_BINARY_OP_LESS_EQUAL    = 8,
    ONYX_BINARY_OP_GREATER       = 9,
    ONYX_BINARY_OP_GREATER_EQUAL = 10,
} OnyxBinaryOp;

// NOTE: AstNode and AstNodeTyped need to be EXACTLY the same for
// all arguments existing in AstNode. I do this to avoid a nested
// "inheiritance" where you would have to say node.base.base.next
// for example
struct AstNode {
    AstNodeKind kind;
    u32 flags;
    OnyxToken *token;
    AstNode *next;
};

struct AstNodeTyped {
    AstNodeKind kind;
    u32 flags;
    OnyxToken *token;
    AstNode *next;
    TypeInfo *type;
};

struct AstNodeBinOp {
    AstNodeTyped base;

    OnyxBinaryOp operation;

    AstNodeTyped *left, *right;
};

struct AstNodeUnaryOp {
    AstNodeTyped base;

    OnyxUnaryOp operation;

    AstNodeTyped *expr;
};

struct AstNodeAssign {
    AstNode base;

    AstNodeTyped* lval;
    AstNodeTyped* expr;
};

struct AstNodeNumLit {
    AstNodeTyped base;

    union { i32 i; i64 l; f32 f; f64 d; } value;
};

struct AstNodeLocal {
    AstNodeTyped base;

    AstNodeLocal *prev_local;
};

struct AstNodeReturn {
    AstNode base;

    AstNodeTyped* expr;
};

struct AstNodeScope {
    AstNode base;

    AstNodeScope *prev_scope;
    AstNodeLocal *last_local;
};

struct AstNodeBlock {
    AstNode base;

    AstNode *body;
    AstNodeScope *scope;
};

struct AstNodeIf {
    AstNode base;

    AstNodeTyped *cond;
    AstNode *true_block;
    AstNode *false_block;
};

struct AstNodeWhile {
    AstNode base;

    AstNodeTyped *cond;
    AstNodeBlock *body;
};

struct AstNodeFunction {
    AstNodeTyped base;

    AstNodeBlock *body;
    AstNodeLocal *params;
};

struct AstNodeForeign {
    AstNode base;

    OnyxToken *mod_token, *name_token;
    AstNode *import;
};

struct AstNodeGlobal {
    AstNodeTyped base;

    AstNodeTyped *initial_value;
};

struct AstNodeCall {
    AstNodeTyped base;

    AstNode *callee;                // NOTE: Function definition node
    AstNodeArgument *arguments;     // NOTE: Expressions that form the actual param list
                                    // They will be chained down using the "next" property
                                    // unless this becomes used by something else
};

struct AstNodeArgument {
    AstNodeTyped base;

    AstNodeTyped *value;
};

struct AstNodeUse {
    AstNode base;

    OnyxToken *filename;
};

typedef struct OnyxProgram {
    bh_arr(AstNodeUse *) uses;
    bh_arr(AstNodeGlobal *) globals;
    bh_arr(AstNodeFunction *) functions;
    bh_arr(AstNodeForeign *) foreigns;
} OnyxProgram;

#endif // #ifndef ONYXASTNODES_H
