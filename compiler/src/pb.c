#include "pb.h"

void pb_write_varint(bh_buffer *buf, u64 v) {
    while (v >= 0x80) {
        bh_buffer_write_byte(buf, (u8)((v & 0x7f) | 0x80));
        v >>= 7;
    }
    bh_buffer_write_byte(buf, (u8)v);
}

void pb_write_tag(bh_buffer *buf, u32 field, u32 wire) {
    pb_write_varint(buf, ((u64)field << 3) | wire);
}

void pb_write_uint32(bh_buffer *buf, u32 field, u32 v) {
    if (v == 0) return;
    pb_write_tag(buf, field, PB_WIRE_VARINT);
    pb_write_varint(buf, v);
}

void pb_write_uint64(bh_buffer *buf, u32 field, u64 v) {
    if (v == 0) return;
    pb_write_tag(buf, field, PB_WIRE_VARINT);
    pb_write_varint(buf, v);
}

void pb_write_optional_uint64(bh_buffer *buf, u32 field, u64 v) {
    pb_write_tag(buf, field, PB_WIRE_VARINT);
    pb_write_varint(buf, v);
}

void pb_write_string(bh_buffer *buf, u32 field, const char *s) {
    if (!s || !*s) return;
    pb_write_bytes(buf, field, s, (i32)strlen(s));
}

void pb_write_bytes(bh_buffer *buf, u32 field, const void *data, i32 len) {
    if (len < 0) len = 0;
    pb_write_tag(buf, field, PB_WIRE_LEN);
    pb_write_varint(buf, (u64)len);
    if (len > 0) bh_buffer_append(buf, data, len);
}
