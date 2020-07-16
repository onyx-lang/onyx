#ifndef ONYXIR_H
#define ONYXIR_H

#include "bh.h"
#include "onyxastnodes.h"
#include "onyxtypes.h"
#include "onyxmsgs.h"

typedef struct IrFunction {
    AstFunction* ast_func;
    Type* type;
    bh_arr(AstLocal *) locals;
    AstLocal* first_param;
    AstNode* body;

    // NOTE: A function can either be either be:
    //      Normal
    //      Intrinsic
    //      Exported
    //      Foreign
    union {
        // NOTE: Set if the function is exported
        char* exported_name;

        // NOTE: Set if the function is a foreign
        struct {
            char* foreign_module;
            char* foreign_name;
        };

        // NOTE: Set if the function is intrinsic
        OnyxIntrinsic intrinsic;
    };

    u32 is_exported  : 1;
    u32 is_foreign   : 1;
    u32 is_intrinsic : 1;

} IrFunction;

typedef struct IrContext {
    // NOTE: Properties used after ir generation is done
    bh_allocator allocator;

    bh_arr(IrFunction *) functions;

    // NOTE: Properties used while ir is generating
    IrFunction* curr_function;

    OnyxMessages* msgs;
} IrContext;


IrContext ir_context_create(bh_allocator allocator);
void ir_context_free(IrContext* context);
void ir_generate(IrContext* context, ParserOutput parse_output);

#endif // #ifndef ONYXIR_H
