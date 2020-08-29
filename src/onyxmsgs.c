
#include "onyxmsgs.h"
#include "onyxutils.h"

#define MAX_MSGS 100

OnyxMessages msgs;

static const char* msg_formats[] = {
    "%s",
    "expected token '%s', got '%s'",
    "unexpected token '%s'",
    "unknown type '%s'",
    "cannot assign to '%b'",
    "attempt to assign to constant '%b'",
    "unknown symbol '%s'",
    "unknown directive '%b'",

    "redeclaration of symbol '%s'",
    "mismatched types for binary operator, '%s', '%s'",
    "mismatched types on assignment, expected '%s', got '%s'",
    "mismatched types for global '%b'; expected '%s', got '%s'",
    "expected expression, got '%s'",
    "attempt to call non-function, '%b'",

    "returning '%s' from function that returns '%s'",
    "function '%b' expected type '%s' in position '%d', got '%s'",

    "duplicate declaration of struct member '%s'",
    "field '%s' does not exist on '%s'",
    "duplicate value for struct member '%b'",
    "no value provided for field '%b'",

    "multiple cases for '%l'",

    "unable to resolve type for symbol '%b'",
    "unable to resolve symbol '%b'",

    "failed to generate load instruction for type '%s'",
    "failed to generate store instruction for type '%s'",

    "file not found '%s'",
};

void onyx_message_init(bh_allocator allocator, bh_table(bh_file_contents)* files) {
    msgs.allocator = allocator;
    msgs.first = NULL;
    msgs.file_contents = files;
}

void onyx_message_add(MsgType type, OnyxFilePos pos, ...) {
    Message* msg = bh_alloc_item(msgs.allocator, Message);
    msg->type = type;
    msg->pos = pos;

    va_list arg_list;
    va_start(arg_list, pos);
    bh_snprintf_va(msg->text, ONYX_MSG_BUFFER_SIZE, msg_formats[type], arg_list);
    va_end(arg_list);

    Message** walker = &msgs.first;
    while (*walker && strcmp((*walker)->pos.filename, pos.filename) < 0) walker = &(*walker)->next;
    while (*walker && (*walker)->pos.line < pos.line) walker = &(*walker)->next;
    while (*walker && (*walker)->pos.line == pos.line && (*walker)->pos.column < pos.column) walker = &(*walker)->next;

    msg->next = *walker;
    *walker = msg;
}

static void print_detailed_message(Message* msg, bh_file_contents* fc) {
    bh_printf("(%s:%l,%l) %s\n", msg->pos.filename, msg->pos.line, msg->pos.column, msg->text);

    i32 linelength = 0;
    char* walker = msg->pos.line_start;
    while (*walker != '\n') linelength++, walker++;

    i32 numlen = bh_printf(" %d | ", msg->pos.line);
    bh_printf("%b\n", msg->pos.line_start, linelength);

    char* pointer_str = bh_alloc_array(global_scratch_allocator, char, linelength + numlen);
    memset(pointer_str, ' ', linelength + numlen);
    memset(pointer_str + msg->pos.column + numlen - 1, '~', msg->pos.length - 1);
    pointer_str[msg->pos.column + numlen - 2] = '^';
    pointer_str[msg->pos.column + numlen + msg->pos.length - 1] = 0;

    bh_printf("%s\n", pointer_str);
}

void onyx_message_print() {
    Message* msg = msgs.first;
    i32 msg_count = MAX_MSGS;

    while (msg && msg_count-- > 0) {
        if (msg->pos.filename) {
            bh_file_contents* fc = &bh_table_get(bh_file_contents, *msgs.file_contents, (char *) msg->pos.filename);

            print_detailed_message(msg, fc);
        } else {
            bh_printf("(%l,%l) %s\n", msg->pos.line, msg->pos.column, msg->text);
        }
        msg = msg->next;
    }
}

b32 onyx_message_has_errors() {
    return msgs.first != NULL;
}
