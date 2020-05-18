#define BH_NO_STRING
#include "bh.h"

#include "onyxlex.h"

typedef struct OnyxParser {
	OnyxTokenizer tokenizer;
	OnyxToken *prev;
	OnyxToken *curr;

	bh_allocator allocator;
} OnyxParser;

typedef enum OnyxAstNodeKind {
	ONYX_PARSE_NODE_KIND_ERROR,
	ONYX_PARSE_NODE_KIND_PROGRAM,

	ONYX_PARSE_NODE_KIND_FUNCDEF,
	ONYX_PARSE_NODE_KIND_BLOCK,
	ONYX_PARSE_NODE_KIND_SCOPE,

	ONYX_PARSE_NODE_KIND_ADD,
	ONYX_PARSE_NODE_KIND_SUB,
	ONYX_PARSE_NODE_KIND_MUL,
	ONYX_PARSE_NODE_KIND_DIVIDE,
	ONYX_PARSE_NODE_KIND_MODULUS,
	ONYX_PARSE_NODE_KIND_NEGATE,

	ONYX_PARSE_NODE_KIND_TYPE,
	ONYX_PARSE_NODE_KIND_LITERAL,
	ONYX_PARSE_NODE_KIND_CAST,
	ONYX_PARSE_NODE_KIND_PARAM,
	ONYX_PARSE_NODE_KIND_CALL,
	ONYX_PARSE_NODE_KIND_RETURN,

	ONYX_PARSE_NODE_KIND_EQUAL,
	ONYX_PARSE_NODE_KIND_NOT_EQUAL,
	ONYX_PARSE_NODE_KIND_GREATER,
	ONYX_PARSE_NODE_KIND_GREATER_EQUAL,
	ONYX_PARSE_NODE_KIND_LESS,
	ONYX_PARSE_NODE_KIND_LESS_EQUAL,
	ONYX_PARSE_NODE_KIND_NOT,

	ONYX_PARSE_NODE_KIND_IF,
	ONYX_PARSE_NODE_KIND_LOOP,

	ONYX_PARSE_NODE_KIND_COUNT
} OnyxAstNodeKind;

typedef enum OnyxTypeInfoKind {
	ONYX_TYPE_INFO_KIND_UNKNOWN,
	ONYX_TYPE_INFO_KIND_VOID,
	ONYX_TYPE_INFO_KIND_BOOL,

	ONYX_TYPE_INFO_KIND_UINT8,
	ONYX_TYPE_INFO_KIND_UINT16,
	ONYX_TYPE_INFO_KIND_UINT32,
	ONYX_TYPE_INFO_KIND_UINT64,

	ONYX_TYPE_INFO_KIND_INT8,
	ONYX_TYPE_INFO_KIND_INT16,
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
} OnyxTypeInfo;

extern OnyxTypeInfo builtin_types[];

typedef union OnyxAstNode OnyxAstNode;
typedef struct OnyxAstNodeBlock OnyxAstNodeBlock;
typedef struct OnyxAstNodeParam OnyxAstNodeParam;
typedef struct OnyxAstNodeFuncDef OnyxAstNodeFuncDef;

typedef enum OnyxAstFlags {
	ONYX_AST_BLOCK_FLAG_HAS_RETURN = BH_BIT(1),
	ONYX_AST_BLOCK_FLAG_TOP_LEVEL  = BH_BIT(2),
	ONYX_AST_BLOCK_FLAG_EXPORTED   = BH_BIT(3),
} OnyxAstFlags;

struct OnyxAstNodeBlock {
	OnyxAstNodeKind kind;
	u32 flags;
	OnyxToken *token;
	OnyxTypeInfo *return_type;
	OnyxAstNode *next;
	OnyxAstNode *unused1;
	OnyxAstNode *unused2;
};

struct OnyxAstNodeParam {
	OnyxAstNodeKind kind;
	u32 flags;
	OnyxToken *token;
	OnyxTypeInfo *type;
	OnyxAstNode *next;
	OnyxAstNode *left;
	OnyxAstNode *right;	
};

struct OnyxAstNodeFuncDef {
	OnyxAstNodeKind kind;
	u32 flags;
	OnyxToken *token; // This will point to the symbol token to identify it
	OnyxTypeInfo *return_type;
	OnyxAstNodeBlock *body;
	OnyxAstNodeParam *params;
	u64 param_count; // Same size as ptr
	u64 unused1;
};

union OnyxAstNode {

	// Generic node structure for capturing all binary ops and statements
	struct {
		OnyxAstNodeKind kind;
		u32 flags;
		OnyxToken *token;
		OnyxTypeInfo* type;
		OnyxAstNode *next;
		OnyxAstNode *left;
		OnyxAstNode *right;
	} as_node;

	OnyxAstNodeBlock as_block;
};

ptr onyx_ast_node_new(bh_allocator alloc, OnyxAstNodeKind kind);
OnyxParser onyx_parser_create(bh_allocator alloc, OnyxTokenizer tokenizer);
OnyxAstNode* onyx_parse(OnyxParser parser);