#ifndef ONYXMSGS_H
#define ONYXMSGS_H

#include "bh.h"
#include "onyxlex.h"

#include <stdarg.h>

#define ONYX_MSG_BUFFER_SIZE 256

typedef enum MsgType {
    Msg_Type_Literal,
    Msg_Type_Expected_Token,
    Msg_Type_Unexpected_Token,
    Msg_Type_Unknown_Type,
    Msg_Type_Not_Lval,
    Msg_Type_Assign_Const,
    Msg_Type_Unknown_Symbol,
    Msg_Type_Unknown_Directive,

    Msg_Type_Redeclare_Symbol,
    Msg_Type_Binop_Mismatch,
    Msg_Type_Assignment_Mismatch,
    Msg_Type_Global_Type_Mismatch,
    Msg_Type_Expected_Expression,
    Msg_Type_Call_Non_Function,

    Msg_Type_Function_Return_Mismatch,
    Msg_Type_Function_Param_Mismatch,

    Msg_Type_Duplicate_Member,
    Msg_Type_No_Field,
    Msg_Type_Duplicate_Value,
    Msg_Type_Field_No_Value,

    Msg_Type_Multiple_Cases,

    Msg_Type_Unresolved_Type,
    Msg_Type_Unresolved_Symbol,

    Msg_Type_Failed_Gen_Load,
    Msg_Type_Failed_Gen_Store,
    Msg_Type_File_Not_Found,

    Msg_Type_Count,
} MsgType;

typedef struct Message {
    MsgType type;
    OnyxFilePos pos;
    struct Message* next;
    char text[ONYX_MSG_BUFFER_SIZE];
} Message;

typedef struct OnyxMessages {
    bh_allocator allocator;

    // NOTE: Pointer to a table mapping file paths to
    // their file contents. Used for better error messages
    bh_table(bh_file_contents)* file_contents;

    Message* first;
} OnyxMessages;

extern OnyxMessages msgs;

void onyx_message_init(bh_allocator allocator, bh_table(bh_file_contents)* files);
void onyx_message_add(MsgType type, OnyxFilePos pos, ...);
void onyx_message_print();
b32  onyx_message_has_errors();

#endif
