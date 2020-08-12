#include "onyxastnodes.h"
#include "onyxtypes.h"

AstBasicType basic_type_void   = { { Ast_Kind_Basic_Type, 0, NULL, "void"   }, &basic_types[Basic_Kind_Void]  };
AstBasicType basic_type_bool   = { { Ast_Kind_Basic_Type, 0, NULL, "bool"   }, &basic_types[Basic_Kind_Bool]  };
AstBasicType basic_type_i8     = { { Ast_Kind_Basic_Type, 0, NULL, "i8"     }, &basic_types[Basic_Kind_I8]    };
AstBasicType basic_type_u8     = { { Ast_Kind_Basic_Type, 0, NULL, "u8"     }, &basic_types[Basic_Kind_U8]    };
AstBasicType basic_type_i16    = { { Ast_Kind_Basic_Type, 0, NULL, "i16"    }, &basic_types[Basic_Kind_I16]   };
AstBasicType basic_type_u16    = { { Ast_Kind_Basic_Type, 0, NULL, "u16"    }, &basic_types[Basic_Kind_U16]   };
AstBasicType basic_type_i32    = { { Ast_Kind_Basic_Type, 0, NULL, "i32"    }, &basic_types[Basic_Kind_I32]   };
AstBasicType basic_type_u32    = { { Ast_Kind_Basic_Type, 0, NULL, "u32"    }, &basic_types[Basic_Kind_U32]   };
AstBasicType basic_type_i64    = { { Ast_Kind_Basic_Type, 0, NULL, "i64"    }, &basic_types[Basic_Kind_I64]   };
AstBasicType basic_type_u64    = { { Ast_Kind_Basic_Type, 0, NULL, "u64"    }, &basic_types[Basic_Kind_U64]   };
AstBasicType basic_type_f32    = { { Ast_Kind_Basic_Type, 0, NULL, "f32"    }, &basic_types[Basic_Kind_F32]   };
AstBasicType basic_type_f64    = { { Ast_Kind_Basic_Type, 0, NULL, "f64"    }, &basic_types[Basic_Kind_F64]   };
AstBasicType basic_type_rawptr = { { Ast_Kind_Basic_Type, 0, NULL, "rawptr" }, &basic_types[Basic_Kind_Rawptr] };

static OnyxToken builtin_heap_start_token = { Token_Type_Symbol, 12, "__heap_start ", { 0 } };
static OnyxToken builtin_stack_top_token  = { Token_Type_Symbol, 11, "__stack_top ",  { 0 } };
AstNumLit builtin_heap_start = { Ast_Kind_NumLit, Ast_Flag_Const, &builtin_heap_start_token, NULL, (AstType *) &basic_type_rawptr, NULL, 0 };
AstGlobal builtin_stack_top  = { Ast_Kind_Global, Ast_Flag_Const | Ast_Flag_Global_Stack_Top,  &builtin_stack_top_token,  NULL, (AstType *) &basic_type_rawptr, NULL };

const BuiltinSymbol builtin_symbols[] = {
    { NULL, "void",       (AstNode *) &basic_type_void },
    { NULL, "bool",       (AstNode *) &basic_type_bool },
    { NULL, "i8",         (AstNode *) &basic_type_i8 },
    { NULL, "u8",         (AstNode *) &basic_type_u8 },
    { NULL, "i16",        (AstNode *) &basic_type_i16 },
    { NULL, "u16",        (AstNode *) &basic_type_u16 },
    { NULL, "i32",        (AstNode *) &basic_type_i32 },
    { NULL, "u32",        (AstNode *) &basic_type_u32 },
    { NULL, "i64",        (AstNode *) &basic_type_i64 },
    { NULL, "u64",        (AstNode *) &basic_type_u64 },
    { NULL, "f32",        (AstNode *) &basic_type_f32 },
    { NULL, "f64",        (AstNode *) &basic_type_f64 },
    { NULL, "rawptr",     (AstNode *) &basic_type_rawptr },

    { "builtin", "__heap_start", (AstNode *) &builtin_heap_start },
    { "builtin", "__stack_top",  (AstNode *) &builtin_stack_top },

    { NULL, NULL, NULL },
};

void initialize_builtins() {
}