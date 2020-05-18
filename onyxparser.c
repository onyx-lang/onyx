
#include "onyxparser.h"

struct OnyxTypeInfo builtin_types[] = {
	{ ONYX_TYPE_INFO_KIND_UNKNOWN, 0, "unknown" },
	{ ONYX_TYPE_INFO_KIND_VOID, 0, "void" },

	{ ONYX_TYPE_INFO_KIND_BOOL, 1, "bool", 0, 0, 0, 1 },

	{ ONYX_TYPE_INFO_KIND_UINT8, 1, "u8", 1, 1, 0, 0 },
	{ ONYX_TYPE_INFO_KIND_UINT16, 2, "u16", 1, 1, 0, 0 },
	{ ONYX_TYPE_INFO_KIND_UINT32, 4, "u32", 1, 1, 0, 0 },
	{ ONYX_TYPE_INFO_KIND_UINT64, 8, "u64", 1, 1, 0, 0 },

	{ ONYX_TYPE_INFO_KIND_INT8, 1, "i8", 1, 0, 0, 0 },
	{ ONYX_TYPE_INFO_KIND_INT16, 2, "i16", 1, 0, 0, 0 },
	{ ONYX_TYPE_INFO_KIND_INT32, 4, "i32", 1, 0, 0, 0 },
	{ ONYX_TYPE_INFO_KIND_INT64, 8, "i64", 1, 0, 0, 0 },

	{ ONYX_TYPE_INFO_KIND_FLOAT32, 4, "f32", 0, 0, 1, 0 },
	{ ONYX_TYPE_INFO_KIND_FLOAT64, 8, "f64", 0, 0, 1, 0 },
	{ ONYX_TYPE_INFO_KIND_SOFT_FLOAT, 8, "sf64", 0, 0, 1, 0 },
};