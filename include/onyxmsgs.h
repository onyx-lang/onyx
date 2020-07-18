#ifndef ONYXMSGS_H
#define ONYXMSGS_H

#include "bh.h"
#include "onyxlex.h"

#include <stdarg.h>

#define ONYX_MSG_BUFFER_SIZE 256

typedef enum OnyxMessageType {
    ONYX_MESSAGE_TYPE_LITERAL,
    ONYX_MESSAGE_TYPE_EXPECTED_TOKEN,
    ONYX_MESSAGE_TYPE_UNEXPECTED_TOKEN,
    ONYX_MESSAGE_TYPE_UNKNOWN_TYPE,
    ONYX_MESSAGE_TYPE_NOT_LVAL,
    ONYX_MESSAGE_TYPE_ASSIGN_CONST,
    ONYX_MESSAGE_TYPE_UNKNOWN_SYMBOL,
    ONYX_MESSAGE_TYPE_UNKNOWN_DIRECTIVE,

    ONYX_MESSAGE_TYPE_REDECLARE_SYMBOL,
    ONYX_MESSAGE_TYPE_BINOP_MISMATCH_TYPE,
    ONYX_MESSAGE_TYPE_ASSIGNMENT_TYPE_MISMATCH,
    ONYX_MESSAGE_TYPE_GLOBAL_TYPE_MISMATCH,
    ONYX_MESSAGE_TYPE_EXPECTED_EXPRESSION,
    ONYX_MESSAGE_TYPE_CALL_NON_FUNCTION,

    ONYX_MESSAGE_TYPE_FUNCTION_RETURN_MISMATCH,
    ONYX_MESSAGE_TYPE_FUNCTION_PARAM_TYPE_MISMATCH,

    ONYX_MESSAGE_TYPE_UNRESOLVED_TYPE,
    ONYX_MESSAGE_TYPE_UNRESOLVED_SYMBOL,

    ONYX_MESSAGE_TYPE_COUNT,
} OnyxMessageType;

typedef struct OnyxMessage {
    OnyxMessageType type;
    OnyxFilePos pos;
    struct OnyxMessage* next;
    char text[ONYX_MSG_BUFFER_SIZE];
} OnyxMessage;

typedef struct OnyxMessages {
    bh_allocator allocator;

    // NOTE: Pointer to a table mapping file paths to
    // their file contents. Used for better error messages
    bh_table(bh_file_contents)* file_contents;

    OnyxMessage* first;
} OnyxMessages;

void onyx_message_add(OnyxMessages* msgs, OnyxMessageType type, OnyxFilePos pos, ...);
void onyx_message_print(OnyxMessages* msgs);
b32 onyx_message_has_errors(OnyxMessages* msgs);
void onyx_message_create(bh_allocator allocator, OnyxMessages* msgs, bh_table(bh_file_contents)* files);

#endif
