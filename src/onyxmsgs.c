
#include "onyxmsgs.h"
#include "onyxutils.h"

#define MAX_MSGS 5

static const char* msg_formats[] = {
    "%s",
    "expected token '%s', got '%s'",
    "unexpected token '%s'",
    "unknown type '%s'",
    "expected lval '%b'",
    "attempt to assign to constant '%b'",
    "unknown symbol '%s'",
    "unknown directive '%b'",

    "conflicting declarations of global '%s'",
    "mismatched types for binary operator, '%s', '%s'",
    "mismatched types on assignment, expected '%s', got '%s'",
    "mismatched types for global '%b'; expected '%s', got '%s'",
    "expected expression, got '%s'",
    "attempt to call non-function, '%b'",

    "returning '%s' from function that returns '%s'",
    "function '%b' expected type '%s' in position '%d', got '%s'",

    "unable to resolve type for symbol '%b'",
    "unable to resolve symbol '%b'",
};

void onyx_message_add(OnyxMessages* msgs, OnyxMessageType type, OnyxFilePos pos, ...) {
    OnyxMessage* msg = bh_alloc_item(msgs->allocator, OnyxMessage);
    msg->type = type;
    msg->pos = pos;

    va_list arg_list;
    va_start(arg_list, pos);
    bh_snprintf_va(msg->text, ONYX_MSG_BUFFER_SIZE, msg_formats[type], arg_list);
    va_end(arg_list);

    OnyxMessage** walker = &msgs->first;
    while (*walker && (*walker)->pos.line < pos.line) walker = &(*walker)->next;
    while (*walker && (*walker)->pos.line == pos.line && (*walker)->pos.column < pos.column) walker = &(*walker)->next;

    msg->next = *walker;
    *walker = msg;
}

static void print_detailed_message(OnyxMessage* msg, bh_file_contents* fc) {
    bh_printf("(%s:%l:%l) %s\n", msg->pos.filename, msg->pos.line, msg->pos.column, msg->text);

    i32 linelength = 0;
    char* walker = msg->pos.line_start;
    while (*walker != '\n') linelength++, walker++;

    bh_printf("| %b\n", msg->pos.line_start, linelength);

    char* pointer_str = bh_alloc_array(global_scratch_allocator, char, linelength);
    memset(pointer_str, 0, linelength);
    memset(pointer_str, '~', msg->pos.column - 1);
    pointer_str[msg->pos.column - 1] = '^';

    bh_printf("|~%s\n", pointer_str);
}

void onyx_message_print(OnyxMessages* msgs) {
    OnyxMessage* msg = msgs->first;
    i32 msg_count = MAX_MSGS;

    while (msg && msg_count-- > 0) {
        if (msg->pos.filename) {
            bh_file_contents* fc = &bh_table_get(bh_file_contents, *msgs->file_contents, (char *) msg->pos.filename);

            print_detailed_message(msg, fc);
        } else {
            bh_printf("(%l:%l) %s\n", msg->pos.line, msg->pos.column, msg->text);
        }
        msg = msg->next;
    }
}

b32 onyx_message_has_errors(OnyxMessages* msgs) {
    return msgs->first != NULL;
}

void onyx_message_create(bh_allocator allocator, OnyxMessages* msgs, bh_table(bh_file_contents)* files) {
    msgs->allocator = allocator;
    msgs->first = NULL;
    msgs->file_contents = files;
}
