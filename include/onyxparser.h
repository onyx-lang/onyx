#ifndef ONYXPARSER_H
#define ONYXPARSER_H

#include "bh.h"

#include "onyxlex.h"
#include "onyxmsgs.h"

typedef union OnyxAstNode OnyxAstNode;
typedef struct OnyxAstNodeUnaryOp OnyxAstNodeUnaryOp;
typedef struct OnyxAstNodeBinOp OnyxAstNodeBinOp;
typedef struct OnyxAstNodeNumLit OnyxAstNodeNumLit;
typedef struct OnyxAstNodeLocal OnyxAstNodeLocal;
typedef struct OnyxAstNodeScope OnyxAstNodeScope;
typedef struct OnyxAstNodeBlock OnyxAstNodeBlock;
typedef struct OnyxAstNodeIf OnyxAstNodeIf;
typedef struct OnyxAstNodeWhile OnyxAstNodeWhile;
typedef struct OnyxAstNodeParam OnyxAstNodeParam;
typedef struct OnyxAstNodeFuncDef OnyxAstNodeFuncDef;
typedef struct OnyxAstNodeForeign OnyxAstNodeForeign;
typedef struct OnyxAstNodeGlobal OnyxAstNodeGlobal;
typedef struct OnyxAstNodeCall OnyxAstNodeCall;
typedef struct OnyxAstNodeUse OnyxAstNodeUse;
typedef struct OnyxAstNodeFile OnyxAstNodeFile;

typedef struct OnyxParser {
    OnyxTokenizer *tokenizer; // NOTE: not used since all tokens are lexed before parsing starts
    OnyxToken *prev_token;
    OnyxToken *curr_token;

    // NOTE: Identifiers currently is only used to resolve type names
    // at parse time, since these are the only symbols we know.
    bh_table(OnyxAstNode *) identifiers;
    OnyxMessages *msgs;

    bh_allocator allocator;
} OnyxParser;

typedef enum OnyxAstNodeKind {
    ONYX_AST_NODE_KIND_ERROR,
    ONYX_AST_NODE_KIND_PROGRAM,
    ONYX_AST_NODE_KIND_USE,

    ONYX_AST_NODE_KIND_FUNCDEF,
    ONYX_AST_NODE_KIND_FOREIGN,
    ONYX_AST_NODE_KIND_BLOCK,
    ONYX_AST_NODE_KIND_SCOPE,
    ONYX_AST_NODE_KIND_LOCAL,
    ONYX_AST_NODE_KIND_GLOBAL,
    ONYX_AST_NODE_KIND_SYMBOL,

    ONYX_AST_NODE_KIND_UNARY_OP,
    ONYX_AST_NODE_KIND_BIN_OP,

    ONYX_AST_NODE_KIND_TYPE,
    ONYX_AST_NODE_KIND_LITERAL,
    ONYX_AST_NODE_KIND_PARAM,
    ONYX_AST_NODE_KIND_ARGUMENT,
    ONYX_AST_NODE_KIND_CALL,
    ONYX_AST_NODE_KIND_ASSIGNMENT,
    ONYX_AST_NODE_KIND_RETURN,

    ONYX_AST_NODE_KIND_IF,
    ONYX_AST_NODE_KIND_WHILE,
    ONYX_AST_NODE_KIND_BREAK,
    ONYX_AST_NODE_KIND_CONTINUE,

    ONYX_AST_NODE_KIND_COUNT
} OnyxAstNodeKind;

typedef enum OnyxTypeInfoKind {
    ONYX_TYPE_INFO_KIND_UNKNOWN,
    ONYX_TYPE_INFO_KIND_VOID,
    ONYX_TYPE_INFO_KIND_BOOL,

    ONYX_TYPE_INFO_KIND_UINT32,
    ONYX_TYPE_INFO_KIND_UINT64,

    ONYX_TYPE_INFO_KIND_INT32,
    ONYX_TYPE_INFO_KIND_INT64,

    ONYX_TYPE_INFO_KIND_FLOAT32,
    ONYX_TYPE_INFO_KIND_FLOAT64,
    ONYX_TYPE_INFO_KIND_SOFT_FLOAT, // 64-bits of data but could be treated as 32-bit
} OnyxTypeInfoKind;

typedef struct OnyxTypeInfo {
    OnyxTypeInfoKind kind;
    u32 size; // in bytes
    const char* name;
    u32 is_int : 1;
    u32 is_unsigned : 1;
    u32 is_float : 1;
    u32 is_bool : 1;
    u32 is_known : 1;
} OnyxTypeInfo;

extern OnyxTypeInfo builtin_types[];

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

struct OnyxAstNodeBinOp {
    OnyxAstNodeKind kind;
    u32 flags;
    OnyxToken *token;
    OnyxTypeInfo *type;
    OnyxBinaryOp operation;
    OnyxAstNode *next;
    OnyxAstNode *left;
    OnyxAstNode *right;
};

struct OnyxAstNodeUnaryOp {
    OnyxAstNodeKind kind;
    u32 flags;
    OnyxToken *token;
    OnyxTypeInfo *type;
    OnyxUnaryOp operation;
    OnyxAstNode *next;
    OnyxAstNode *left;
};

struct OnyxAstNodeNumLit {
    OnyxAstNodeKind kind;
    u32 flags;
    OnyxToken *token;
    OnyxTypeInfo *type;
    u64 data;
    OnyxAstNode *next;
    union { i32 i; i64 l; f32 f; f64 d; } value;
};

struct OnyxAstNodeLocal {
    OnyxAstNodeKind kind;
    u32 flags;
    OnyxToken *token;
    OnyxTypeInfo *type;
    u64 data;                   // NOTE: Unused
    OnyxAstNode *next;
    OnyxAstNodeLocal *prev_local;
};

struct OnyxAstNodeParam {
    OnyxAstNodeKind kind;
    u32 flags;
    OnyxToken *token;            // NOTE: Symbol name i.e. 'a', 'b'
    OnyxTypeInfo *type;
    u64 data;                   // NOTE: UNUSED
    OnyxAstNodeParam *next;
    OnyxAstNodeLocal *prev_local;
};

struct OnyxAstNodeScope {
    OnyxAstNodeKind kind;
    u32 flags;
    OnyxToken *token;    // NOTE: UNUSED
    OnyxTypeInfo *type; // NOTE: UNUSED
    u64 data;           // NOTE: UNUSED
    OnyxAstNodeScope *prev_scope;
    OnyxAstNodeLocal *last_local;
};

struct OnyxAstNodeBlock {
    OnyxAstNodeKind kind;
    u32 flags;
    OnyxToken *token;
    OnyxTypeInfo *return_type;
    u64 data;                       // NOTE: UNUSED
    OnyxAstNode *next;
    OnyxAstNode *body;
    OnyxAstNodeScope *scope;
};

struct OnyxAstNodeIf {
    OnyxAstNodeKind kind;
    u32 flags;
    OnyxToken *token;    // NOTE: UNUSED
    u64 data;            // NOTE: UNUSED
    OnyxAstNode *false_block;
    OnyxAstNode *next;
    OnyxAstNode *cond;
    OnyxAstNode *true_block;
};

struct OnyxAstNodeWhile {
    OnyxAstNodeKind kind;
    u32 flags;
    OnyxToken *token;   // NOTE: UNUSED
    OnyxTypeInfo *type;
    u64 data;
    OnyxAstNode *next;
    OnyxAstNode *cond;
    OnyxAstNodeBlock *body;
};

struct OnyxAstNodeFuncDef {
    OnyxAstNodeKind kind;
    u32 flags;
    OnyxToken *token; // This will point to the symbol token to identify it
    OnyxTypeInfo *return_type;
    u64 data;
    OnyxAstNode *next;
    OnyxAstNodeBlock *body;
    OnyxAstNodeParam *params;
};

struct OnyxAstNodeForeign {
    OnyxAstNodeKind kind;
    u32 flags;
    OnyxToken *mod_token;
    OnyxTypeInfo *type;
    u64 data;
    OnyxAstNode *next;
    OnyxToken *name_token;
    OnyxAstNode *import;
};

struct OnyxAstNodeGlobal {
    OnyxAstNodeKind kind;
    u32 flags;
    OnyxToken *token;
    OnyxTypeInfo *type;
    u64 data;
    OnyxAstNode* next;
    OnyxAstNode* initial_value;
};

struct OnyxAstNodeCall {
    OnyxAstNodeKind kind;
    u32 flags;
    OnyxToken *token;
    OnyxTypeInfo *type;         // NOTE: The type that the function returns
    u64 data;
    OnyxAstNode *next;
    OnyxAstNode *callee;        // NOTE: Function definition node
    OnyxAstNode *arguments;        // NOTE: Expressions that form the actual param list
                                // They will be chained down using the "next" property
                                // unless this becomes used by something else
};

struct OnyxAstNodeUse {
    OnyxAstNodeKind kind;
    u32 flags;
    OnyxToken *token;
    OnyxTypeInfo *type;
    u64 data;
    OnyxAstNode *next;
    OnyxToken *filename;
};

struct OnyxAstNodeFile {
    OnyxAstNodeKind kind;
    u32 flags;
    OnyxToken *token;           // NOTE: unused
    OnyxTypeInfo *type;         // NOTE: unused
    u64 data;
    OnyxAstNodeFile *next;      // NOTE: next file
    OnyxAstNode *contents;      // NOTE: the first top-level element
};

union OnyxAstNode {

    // Generic node structure for capturing all binary ops and statements
    struct {
        OnyxAstNodeKind kind;
        u32 flags;
        OnyxToken *token;
        OnyxTypeInfo *type;
        u64 data;
        OnyxAstNode *next;
        OnyxAstNode *left;
        OnyxAstNode *right;
    };

    OnyxAstNodeBlock as_block;
    OnyxAstNodeFuncDef as_funcdef;
    OnyxAstNodeParam as_param;
    OnyxAstNodeLocal as_local;
    OnyxAstNodeScope as_scope;
    OnyxAstNodeCall as_call;
    OnyxAstNodeNumLit as_numlit;
    OnyxAstNodeBinOp as_binop;
    OnyxAstNodeUnaryOp as_unaryop;
    OnyxAstNodeForeign as_foreign;
    OnyxAstNodeGlobal as_global;
    OnyxAstNodeIf as_if;
    OnyxAstNodeWhile as_while;
    OnyxAstNodeUse as_use;
    OnyxAstNodeFile as_file;
};

const char* onyx_ast_node_kind_string(OnyxAstNodeKind kind);
void* onyx_ast_node_new(bh_allocator alloc, OnyxAstNodeKind kind);
OnyxParser onyx_parser_create(bh_allocator alloc, OnyxTokenizer *tokenizer, OnyxMessages* msgs);
void onyx_parser_free(OnyxParser* parser);
OnyxAstNodeFile* onyx_parse(OnyxParser *parser);

#endif // #ifndef ONYXPARSER_H
