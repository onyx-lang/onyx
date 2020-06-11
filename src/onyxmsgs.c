
#include "onyxmsgs.h"

static const char* msg_formats[] = {
	"expected token '%s', got '%s'",
	"unexpected token '%s'",
	"unknown type '%s'",
	"expected lval '%s'",
	"attempt to assign to constant '%s'",
	"unknown symbol '%s'",
	"redefinition of function '%s'",
	"mismatched types for binary operator, '%s', '%s'",
	"mismatched types on assignment, '%s', '%s'",
	"expected expression, got '%s'",
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

void onyx_message_print(OnyxMessages* msgs) {
	OnyxMessage* msg = msgs->first;

	while (msg) {
		if (msg->pos.filename) {
			bh_printf("(%s:%l:%l) %s\n", msg->pos.filename, msg->pos.line, msg->pos.column, msg->text);
		} else {
			bh_printf("(%l:%l) %s\n", msg->pos.line, msg->pos.column, msg->text);
		}
		msg = msg->next;
	}
}

b32 onyx_message_has_errors(OnyxMessages* msgs) {
	return msgs->first != NULL;
}

void onyx_message_create(bh_allocator allocator, OnyxMessages* msgs) {
	msgs->allocator = allocator;
	msgs->first = NULL;
}
