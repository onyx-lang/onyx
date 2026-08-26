#ifndef ONYX_PB_H
#define ONYX_PB_H

#include "bh.h"

#define PB_WIRE_VARINT 0
#define PB_WIRE_LEN    2

void pb_write_varint(bh_buffer *buf, u64 v);
void pb_write_tag(bh_buffer *buf, u32 field, u32 wire);
void pb_write_uint32(bh_buffer *buf, u32 field, u32 v);
void pb_write_uint32_always(bh_buffer *buf, u32 field, u32 v);
void pb_write_uint64(bh_buffer *buf, u32 field, u64 v);
void pb_write_optional_uint64(bh_buffer *buf, u32 field, u64 v);
void pb_write_string(bh_buffer *buf, u32 field, const char *s);
void pb_write_bytes(bh_buffer *buf, u32 field, const void *data, i32 len);

#endif
