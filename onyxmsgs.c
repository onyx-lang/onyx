
#include "onyxmsgs.h"

static const char* msg_formats[] = {
	"expected token '%s'",
	"unexpected token '%s'",
	"unknown type '%s'"
};

void onyx_message_add(OnyxMessages* msgs, OnyxMessageType type, OnyxFilePos pos, ...) {
	OnyxMessage* msg = bh_alloc_item(msgs->allocator, OnyxMessage);
	msg->type = type;
	msg->pos = pos;

	va_list arg_list;
	va_start(arg_list, pos);
	vsnprintf(msg->text, ONYX_MSG_BUFFER_SIZE, msg_formats[type], arg_list);
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
			printf("(%s:%ld:%ld) %s\n", msg->pos.filename, msg->pos.line, msg->pos.column, msg->text);
		} else {
			printf("(%ld:%ld) %s\n", msg->pos.line, msg->pos.column, msg->text);
		}
		msg = msg->next;
	}	
}

void onyx_message_create(bh_allocator allocator, OnyxMessages* msgs) {
	msgs->allocator = allocator;
	msgs->first = NULL;
}
