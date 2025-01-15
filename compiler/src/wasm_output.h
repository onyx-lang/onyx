// This file is included in src/onyxwasm.c.
// It is separated because of its fundamentally different goals.

//-------------------------------------------------
// BINARY OUPUT
//-------------------------------------------------

#define WASM_SECTION_ID_CUSTOM 0
#define WASM_SECTION_ID_TYPE 1
#define WASM_SECTION_ID_IMPORT 2
#define WASM_SECTION_ID_FUNCTION 3
#define WASM_SECTION_ID_TABLE 4
#define WASM_SECTION_ID_MEMORY 5
#define WASM_SECTION_ID_GLOBAL 6
#define WASM_SECTION_ID_EXPORT 7
#define WASM_SECTION_ID_START 8
#define WASM_SECTION_ID_ELEMENT 9
#define WASM_SECTION_ID_DATACOUNT 12
#define WASM_SECTION_ID_CODE 10
#define WASM_SECTION_ID_DATA 11

typedef i32 vector_func(void*, bh_buffer*);

static const u8 WASM_MAGIC_STRING[] = { 0x00, 0x61, 0x73, 0x6D };
static const u8 WASM_VERSION[] = { 0x01, 0x00, 0x00, 0x00 };

static inline i32 output_unsigned_integer(u64 i, bh_buffer *buff) {
    i32 leb_len;
    u8* leb = uint_to_uleb128(i, &leb_len);
    bh_buffer_append(buff, leb, leb_len);
    return leb_len;
}

static inline i32 output_custom_section_name(char *name, bh_buffer *buff) {
    u32 len = strlen(name);
    i32 len_len = output_unsigned_integer(len, buff);
    bh_buffer_append(buff, name, len);
    return len_len + len;
}

static void output_instruction(WasmFunc* func, WasmInstruction* instr, b32 debug_enabled, bh_buffer* buff);

static i32 output_vector(void** arr, i32 stride, i32 arrlen, vector_func elem, bh_buffer* vec_buff) {
    i32 len;
    u8* leb = uint_to_uleb128((u64) arrlen, &len);
    bh_buffer_append(vec_buff, leb, len);

    i32 i = 0;
    while (i < arrlen) {
        elem(*arr, vec_buff);
        arr = bh_pointer_add(arr, stride);
        i++;
    }

    return vec_buff->length;
}

static i32 output_name(const char* start, i32 length, bh_buffer* buff) {
    i32 leb_len, prev_len = buff->length;
    u8* leb = uint_to_uleb128((u64) length, &leb_len);
    bh_buffer_append(buff, leb, leb_len);
    bh_buffer_append(buff, start, length);
    return buff->length - prev_len;
}

static i32 output_limits(i32 min, i32 max, b32 shared, bh_buffer* buff) {
    i32 leb_len, prev_len = buff->length;
    u8* leb;

    u8 mem_type = 0x00;
    if (max >= 0) mem_type |= 0x01;
    if (shared)   mem_type |= 0x02;

    bh_buffer_write_byte(buff, mem_type);

    leb = uint_to_uleb128((u64) min, &leb_len);
    bh_buffer_append(buff, leb, leb_len);

    if (max >= 0) {
        leb = uint_to_uleb128((u64) max, &leb_len);
        bh_buffer_append(buff, leb, leb_len);
    }

    return buff->length - prev_len;
}

static i32 output_functype(WasmFuncType* type, bh_buffer* buff) {
    i32 prev_len = buff->length;

    bh_buffer_write_byte(buff, 0x60);

    i32 len;
    u8* leb_buff = uint_to_uleb128(type->param_count, &len);
    bh_buffer_append(buff, leb_buff, len);
    bh_buffer_append(buff, type->param_types, type->param_count);

    if (type->return_type != WASM_TYPE_VOID) {
        bh_buffer_write_byte(buff, 0x01);
        bh_buffer_write_byte(buff, type->return_type);
    } else {
        bh_buffer_write_byte(buff, 0x00);
    }

    return buff->length - prev_len;
}

static i32 output_typesection(OnyxWasmModule* module, bh_buffer* buff) {
    i32 prev_len = buff->length;
    bh_buffer_write_byte(buff, 0x01);

    bh_buffer vec_buff;
    bh_buffer_init(&vec_buff, buff->allocator, 128);

    i32 vec_len = output_vector(
                                (void**) module->types,
                                sizeof(WasmFuncType*),
                                bh_arr_length(module->types),
                                (vector_func *) output_functype,
                                &vec_buff);

    i32 leb_len;
    u8* leb = uint_to_uleb128((u64) vec_len, &leb_len);
    bh_buffer_append(buff, leb, leb_len);

    bh_buffer_concat(buff, vec_buff);
    bh_buffer_free(&vec_buff);

    return buff->length - prev_len;
}

static i32 output_funcsection(OnyxWasmModule* module, bh_buffer* buff) {
    i32 prev_len = buff->length;
    bh_buffer_write_byte(buff, WASM_SECTION_ID_FUNCTION);

    bh_buffer vec_buff;
    bh_buffer_init(&vec_buff, buff->allocator, 128);

    i32 leb_len;
    u8* leb = uint_to_uleb128((u64) (bh_arr_length(module->funcs)), &leb_len);
    bh_buffer_append(&vec_buff, leb, leb_len);

    bh_arr_each(WasmFunc, func, module->funcs) {
        assert(func->code);

        leb = uint_to_uleb128((u64) (func->type_idx), &leb_len);
        bh_buffer_append(&vec_buff, leb, leb_len);
    }

    leb = uint_to_uleb128((u64) (vec_buff.length), &leb_len);
    bh_buffer_append(buff, leb, leb_len);

    bh_buffer_concat(buff, vec_buff);
    bh_buffer_free(&vec_buff);

    return buff->length - prev_len;
}

static i32 output_tablesection(OnyxWasmModule* module, bh_buffer* buff) {
    i32 prev_len = buff->length;
    bh_buffer_write_byte(buff, WASM_SECTION_ID_TABLE);

    bh_buffer vec_buff;
    bh_buffer_init(&vec_buff, buff->allocator, 128);

    i32 leb_len;
    u8* leb = uint_to_uleb128((u64) 1, &leb_len);
    bh_buffer_append(&vec_buff, leb, leb_len);

    // NOTE: funcrefs are the only valid table element type
    bh_buffer_write_byte(&vec_buff, 0x70);
    output_limits(bh_arr_length(module->elems), -1, 0, &vec_buff);

    leb = uint_to_uleb128((u64) (vec_buff.length), &leb_len);
    bh_buffer_append(buff, leb, leb_len);

    bh_buffer_concat(buff, vec_buff);
    bh_buffer_free(&vec_buff);

    return buff->length - prev_len;
}

static i32 output_memorysection(OnyxWasmModule* module, bh_buffer* buff) {
    // :ProperLinking
    if (!module->needs_memory_section) return 0;

    i32 prev_len = buff->length;
    bh_buffer_write_byte(buff, WASM_SECTION_ID_MEMORY);

    bh_buffer vec_buff;
    bh_buffer_init(&vec_buff, buff->allocator, 128);

    i32 leb_len;
    u8* leb = uint_to_uleb128((u64) 1, &leb_len);
    bh_buffer_append(&vec_buff, leb, leb_len);

    output_limits(module->memory_min_size, -1, 0, &vec_buff);

    leb = uint_to_uleb128((u64) (vec_buff.length), &leb_len);
    bh_buffer_append(buff, leb, leb_len);

    bh_buffer_concat(buff, vec_buff);
    bh_buffer_free(&vec_buff);

    return buff->length - prev_len;
}

static i32 output_globalsection(OnyxWasmModule* module, bh_buffer* buff) {
    i32 prev_len = buff->length;
    bh_buffer_write_byte(buff, WASM_SECTION_ID_GLOBAL);

    bh_buffer vec_buff;
    bh_buffer_init(&vec_buff, buff->allocator, 128);

    i32 leb_len;
    u8* leb = uint_to_uleb128((u64) (bh_arr_length(module->globals)), &leb_len);
    bh_buffer_append(&vec_buff, leb, leb_len);

    bh_arr_each(WasmGlobal, global, module->globals) {
        bh_buffer_write_byte(&vec_buff, global->type);
        bh_buffer_write_byte(&vec_buff, 0x01);

        bh_arr_each(WasmInstruction, instr, global->initial_value)
            output_instruction(NULL, instr, module->context->options->debug_info_enabled, &vec_buff);

        // NOTE: Initial value expression terminator
        bh_buffer_write_byte(&vec_buff, (u8) WI_BLOCK_END);
    }

    leb = uint_to_uleb128((u64) (vec_buff.length), &leb_len);
    bh_buffer_append(buff, leb, leb_len);

    bh_buffer_concat(buff, vec_buff);
    bh_buffer_free(&vec_buff);

    return buff->length - prev_len;
}

static i32 output_importsection(OnyxWasmModule* module, bh_buffer* buff) {
    i32 prev_len = buff->length;
    bh_buffer_write_byte(buff, WASM_SECTION_ID_IMPORT);

    bh_buffer vec_buff;
    bh_buffer_init(&vec_buff, buff->allocator, 128);

    i32 leb_len;
    u8* leb = uint_to_uleb128((u64) (bh_arr_length(module->imports)), &leb_len);
    bh_buffer_append(&vec_buff, leb, leb_len);

    bh_arr_each(WasmImport, import, module->imports) {
        output_name(import->mod, strlen(import->mod), &vec_buff);
        output_name(import->name, strlen(import->name), &vec_buff);
        bh_buffer_write_byte(&vec_buff, (u8) import->kind);

        switch (import->kind) {
            case WASM_FOREIGN_FUNCTION:
                leb = uint_to_uleb128((u64) import->idx, &leb_len);
                bh_buffer_append(&vec_buff, leb, leb_len);
                break;

            case WASM_FOREIGN_MEMORY:
                output_limits(import->min, import->max, import->shared, &vec_buff);
                break;

            case WASM_FOREIGN_GLOBAL:
            case WASM_FOREIGN_TABLE: assert("Bad foreign import kind" && 0);
        }
    }

    leb = uint_to_uleb128((u64) (vec_buff.length), &leb_len);
    bh_buffer_append(buff, leb, leb_len);

    bh_buffer_concat(buff, vec_buff);
    bh_buffer_free(&vec_buff);

    return buff->length - prev_len;
}

static i32 output_exportsection(OnyxWasmModule* module, bh_buffer* buff) {
    i32 prev_len = buff->length;
    bh_buffer_write_byte(buff, WASM_SECTION_ID_EXPORT);

    bh_buffer vec_buff;
    bh_buffer_init(&vec_buff, buff->allocator, 128);

    i32 leb_len;
    u8* leb = uint_to_uleb128((u64) (module->export_count), &leb_len);
    bh_buffer_append(&vec_buff, leb, leb_len);

    i32 key_len = 0;
    fori (i, 0, shlen(module->exports)) {
        char *key = module->exports[i].key;
        WasmExport value = module->exports[i].value;

        key_len = strlen(key);
        output_name(key, key_len, &vec_buff);

        bh_buffer_write_byte(&vec_buff, (u8) (value.kind));
        leb = uint_to_uleb128((u64) value.idx, &leb_len);
        bh_buffer_append(&vec_buff, leb, leb_len);
    }

    leb = uint_to_uleb128((u64) (vec_buff.length), &leb_len);
    bh_buffer_append(buff, leb, leb_len);

    bh_buffer_concat(buff, vec_buff);
    bh_buffer_free(&vec_buff);

    return buff->length - prev_len;
}

static i32 output_startsection(OnyxWasmModule* module, bh_buffer* buff) {
    i32 prev_len = buff->length;

    i32 start_idx = -1;
    fori (i, 0, shlen(module->exports)) {
        char *key = module->exports[i].key;
        WasmExport value = module->exports[i].value;

        if (value.kind == WASM_FOREIGN_FUNCTION) {
            if (strncmp("main", key, 5) == 0) {
                start_idx = value.idx;
                break;
            }
        }
    }

    if (start_idx != -1) {
        bh_buffer_write_byte(buff, WASM_SECTION_ID_START);

        i32 start_leb_len, section_leb_len;
        uint_to_uleb128((u64) start_idx, &start_leb_len);
        u8* section_leb = uint_to_uleb128((u64) start_leb_len, &section_leb_len);
        bh_buffer_append(buff, section_leb, section_leb_len);

        u8* start_leb = uint_to_uleb128((u64) start_idx, &start_leb_len);
        bh_buffer_append(buff, start_leb, start_leb_len);
    }

    return buff->length - prev_len;
}

static i32 output_elemsection(OnyxWasmModule* module, bh_buffer* buff) {
    if (bh_arr_length(module->elems) == 0) return 0;

    i32 prev_len = buff->length;

    bh_buffer_write_byte(buff, WASM_SECTION_ID_ELEMENT);

    bh_buffer vec_buff;
    bh_buffer_init(&vec_buff, buff->allocator, 128);

    i32 leb_len;
    u8* leb;

    // NOTE: 0x01 count of elems
    bh_buffer_write_byte(&vec_buff, 0x01);

    // NOTE: 0x00 table index
    bh_buffer_write_byte(&vec_buff, 0x00);

    bh_buffer_write_byte(&vec_buff, WI_I32_CONST);
    bh_buffer_write_byte(&vec_buff, 0x00);
    bh_buffer_write_byte(&vec_buff, WI_BLOCK_END);

    leb = uint_to_uleb128((u64) bh_arr_length(module->elems), &leb_len);
    bh_buffer_append(&vec_buff, leb, leb_len);

    bh_arr_each(i32, elem, module->elems) {
        leb = uint_to_uleb128((u64) *elem, &leb_len);
        bh_buffer_append(&vec_buff, leb, leb_len);
    }

    leb = uint_to_uleb128((u64) (vec_buff.length), &leb_len);
    bh_buffer_append(buff, leb, leb_len);

    bh_buffer_concat(buff, vec_buff);
    bh_buffer_free(&vec_buff);

    return buff->length - prev_len;
}

static i32 output_locals(WasmFunc* func, bh_buffer* buff) {
    i32 prev_len = buff->length;

    // NOTE: Output vector length
    i32 total_locals =
        (i32) (func->locals.allocated[0] != 0) +
        (i32) (func->locals.allocated[1] != 0) +
        (i32) (func->locals.allocated[2] != 0) +
        (i32) (func->locals.allocated[3] != 0) +
        (i32) (func->locals.allocated[4] != 0);

    i32 leb_len;
    u8* leb = uint_to_uleb128((u64) total_locals, &leb_len);
    bh_buffer_append(buff, leb, leb_len);

    if (func->locals.allocated[0] != 0) {
        leb = uint_to_uleb128((u64) func->locals.allocated[0], &leb_len);
        bh_buffer_append(buff, leb, leb_len);
        bh_buffer_write_byte(buff, WASM_TYPE_INT32);
    }
    if (func->locals.allocated[1] != 0) {
        leb = uint_to_uleb128((u64) func->locals.allocated[1], &leb_len);
        bh_buffer_append(buff, leb, leb_len);
        bh_buffer_write_byte(buff, WASM_TYPE_INT64);
    }
    if (func->locals.allocated[2] != 0) {
        leb = uint_to_uleb128((u64) func->locals.allocated[2], &leb_len);
        bh_buffer_append(buff, leb, leb_len);
        bh_buffer_write_byte(buff, WASM_TYPE_FLOAT32);
    }
    if (func->locals.allocated[3] != 0) {
        leb = uint_to_uleb128((u64) func->locals.allocated[3], &leb_len);
        bh_buffer_append(buff, leb, leb_len);
        bh_buffer_write_byte(buff, WASM_TYPE_FLOAT64);
    }
    if (func->locals.allocated[4] != 0) {
        leb = uint_to_uleb128((u64) func->locals.allocated[4], &leb_len);
        bh_buffer_append(buff, leb, leb_len);
        bh_buffer_write_byte(buff, WASM_TYPE_VAR128);
    }

    return buff->length - prev_len;
}

static void output_instruction(WasmFunc* func, WasmInstruction* instr, b32 debug_enabled, bh_buffer* buff) {
    i32 leb_len;
    u8* leb;

    if (instr->type == WI_NOP && !debug_enabled) return;

    if (instr->type & SIMD_INSTR_MASK) {
        bh_buffer_write_byte(buff, 0xFD);
        leb = uint_to_uleb128((u64) (instr->type &~ SIMD_INSTR_MASK), &leb_len);
        bh_buffer_append(buff, leb, leb_len);

    } else if (instr->type & EXT_INSTR_MASK) {
        bh_buffer_write_byte(buff, 0xFC);
        leb = uint_to_uleb128((u64) (instr->type &~ EXT_INSTR_MASK), &leb_len);
        bh_buffer_append(buff, leb, leb_len);

    } else if (instr->type & ATOMIC_INSTR_MASK) {
        bh_buffer_write_byte(buff, 0xFE);
        leb = uint_to_uleb128((u64) (instr->type &~ ATOMIC_INSTR_MASK), &leb_len);
        bh_buffer_append(buff, leb, leb_len);

        if (instr->type == WI_ATOMIC_FENCE) {
            bh_buffer_write_byte(buff, 0x00);

        } else {
            leb = uint_to_uleb128((u64) instr->data.i1, &leb_len);
            bh_buffer_append(buff, leb, leb_len);

            leb = uint_to_uleb128((u64) instr->data.i2, &leb_len);
            bh_buffer_append(buff, leb, leb_len);
        }

    } else {
        bh_buffer_write_byte(buff, (u8) instr->type);
    }

    switch (instr->type) {
        case WI_LOCAL_GET:
        case WI_LOCAL_SET:
        case WI_LOCAL_TEE: {
            u64 actual_idx = local_lookup_idx(&func->locals, instr->data.l);
            leb = uint_to_uleb128(actual_idx, &leb_len);
            bh_buffer_append(buff, leb, leb_len);
            break;
        }

        case WI_GLOBAL_GET:
        case WI_GLOBAL_SET:
        case WI_CALL:
        case WI_BLOCK_START:
        case WI_LOOP_START:
        case WI_JUMP:
        case WI_COND_JUMP:
        case WI_IF_START:
        case WI_MEMORY_SIZE:
        case WI_MEMORY_GROW:
        case WI_MEMORY_FILL:
            leb = uint_to_uleb128((u64) instr->data.i1, &leb_len);
            bh_buffer_append(buff, leb, leb_len);
            break;

        case WI_MEMORY_INIT:
        case WI_MEMORY_COPY:
            leb = uint_to_uleb128((u64) instr->data.i1, &leb_len);
            bh_buffer_append(buff, leb, leb_len);

            leb = uint_to_uleb128((u64) instr->data.i2, &leb_len);
            bh_buffer_append(buff, leb, leb_len);
            break;

        case WI_JUMP_TABLE: {
            BranchTable* bt = (BranchTable *) instr->data.p;

            leb = uint_to_uleb128((u64) bt->count, &leb_len);
            bh_buffer_append(buff, leb, leb_len);

            fori (i, 0, bt->count) {
                leb = uint_to_uleb128((u64) bt->cases[i], &leb_len);
                bh_buffer_append(buff, leb, leb_len);
            }

            leb = uint_to_uleb128((u64) bt->default_case, &leb_len);
            bh_buffer_append(buff, leb, leb_len);
            break;
        }


        case WI_CALL_INDIRECT:
        case WI_I32_STORE: case WI_I32_STORE_8: case WI_I32_STORE_16:
        case WI_I64_STORE: case WI_I64_STORE_8: case WI_I64_STORE_16: case WI_I64_STORE_32:
        case WI_F32_STORE: case WI_F64_STORE:
        case WI_V128_STORE:
        case WI_I32_LOAD:
        case WI_I32_LOAD_8_S: case WI_I32_LOAD_8_U:
        case WI_I32_LOAD_16_S: case WI_I32_LOAD_16_U:
        case WI_I64_LOAD:
        case WI_I64_LOAD_8_S: case WI_I64_LOAD_8_U:
        case WI_I64_LOAD_16_S: case WI_I64_LOAD_16_U:
        case WI_I64_LOAD_32_S: case WI_I64_LOAD_32_U:
        case WI_F32_LOAD: case WI_F64_LOAD:
        case WI_V128_LOAD:
            leb = uint_to_uleb128((u64) instr->data.i1, &leb_len);
            bh_buffer_append(buff, leb, leb_len);
            leb = uint_to_uleb128((u64) instr->data.i2, &leb_len);
            bh_buffer_append(buff, leb, leb_len);
            break;

        case WI_I32_CONST:
            leb = int_to_leb128((i64) instr->data.i1, &leb_len);
            bh_buffer_append(buff, leb, leb_len);
            break;
        case WI_I64_CONST:
            leb = int_to_leb128((i64) instr->data.l, &leb_len);
            bh_buffer_append(buff, leb, leb_len);
            break;
        case WI_F32_CONST:
            leb = float_to_ieee754(instr->data.f, 0);
            bh_buffer_append(buff, leb, 4);
            break;
        case WI_F64_CONST:
            leb = double_to_ieee754(instr->data.d, 0);
            bh_buffer_append(buff, leb, 8);
            break;

        case WI_V128_CONST:
        case WI_I8X16_SHUFFLE:
            fori (i, 0, 16) bh_buffer_write_byte(buff, ((u8*) instr->data.p)[i]);
            break;

        case WI_I8X16_EXTRACT_LANE_S: case WI_I8X16_EXTRACT_LANE_U: case WI_I8X16_REPLACE_LANE:
        case WI_I16X8_EXTRACT_LANE_S: case WI_I16X8_EXTRACT_LANE_U: case WI_I16X8_REPLACE_LANE:
        case WI_I32X4_EXTRACT_LANE: case WI_I32X4_REPLACE_LANE:
        case WI_I64X2_EXTRACT_LANE: case WI_I64X2_REPLACE_LANE:
        case WI_F32X4_EXTRACT_LANE: case WI_F32X4_REPLACE_LANE:
        case WI_F64X2_EXTRACT_LANE: case WI_F64X2_REPLACE_LANE:
            bh_buffer_write_byte(buff, (u8) instr->data.i1);
            break;

        default: break;
    }
}

static i32 output_code(WasmFunc* func, b32 debug_enabled, bh_buffer* buff) {

    bh_buffer code_buff;
    bh_buffer_init(&code_buff, buff->allocator, 128);

    // Output locals
    output_locals(func, &code_buff);

    assert(func->code);

    // Output code
    bh_arr_each(WasmInstruction, instr, func->code) output_instruction(func, instr, debug_enabled, &code_buff);

    i32 leb_len;
    u8* leb = uint_to_uleb128((u64) code_buff.length, &leb_len);
    bh_buffer_append(buff, leb, leb_len);

    bh_buffer_concat(buff, code_buff);
    bh_buffer_free(&code_buff);

    return 0;
}

static i32 output_codesection(OnyxWasmModule* module, bh_buffer* buff) {
    i32 prev_len = buff->length;

    bh_buffer_write_byte(buff, WASM_SECTION_ID_CODE);

    bh_buffer vec_buff;
    bh_buffer_init(&vec_buff, buff->allocator, 128);

    i32 leb_len;
    u8* leb = uint_to_uleb128((u64) bh_arr_length(module->funcs), &leb_len);
    bh_buffer_append(&vec_buff, leb, leb_len);

    bh_arr_each(WasmFunc, func, module->funcs) {
        assert(func->code);
        output_code(func, module->context->options->debug_info_enabled, &vec_buff);
    }

    leb = uint_to_uleb128((u64) (vec_buff.length), &leb_len);
    bh_buffer_append(buff, leb, leb_len);

    bh_buffer_concat(buff, vec_buff);
    bh_buffer_free(&vec_buff);

    return buff->length - prev_len;
}

static i32 output_datacountsection(OnyxWasmModule* module, bh_buffer* buff) {
    if (!module->context->options->use_post_mvp_features) return 0;

    i32 prev_len = buff->length;

    bh_buffer_write_byte(buff, WASM_SECTION_ID_DATACOUNT);

    bh_buffer vec_buff;
    bh_buffer_init(&vec_buff, buff->allocator, 128);

    i32 leb_len;
    u8* leb = uint_to_uleb128((u64) bh_arr_length(module->data), &leb_len);
    bh_buffer_append(&vec_buff, leb, leb_len);

    leb = uint_to_uleb128((u64) (vec_buff.length), &leb_len);
    bh_buffer_append(buff, leb, leb_len);

    bh_buffer_concat(buff, vec_buff);
    bh_buffer_free(&vec_buff);

    return buff->length - prev_len;
}

static i32 output_datasection(OnyxWasmModule* module, bh_buffer* buff) {
    i32 prev_len = buff->length;

    bh_buffer_write_byte(buff, WASM_SECTION_ID_DATA);

    bh_buffer vec_buff;
    bh_buffer_init(&vec_buff, buff->allocator, 128);

    i32 leb_len;
    u8* leb = uint_to_uleb128((u64) bh_arr_length(module->data), &leb_len);
    bh_buffer_append(&vec_buff, leb, leb_len);

    bh_arr_each(WasmDatum, datum, module->data) {
        i32 memory_flags = 0x00;
        // :ProperLinking
        if (module->context->options->use_multi_threading) memory_flags |= 0x01;

        bh_buffer_write_byte(&vec_buff, memory_flags);

        // :ProperLinking
        if (!module->context->options->use_multi_threading) {
            bh_buffer_write_byte(&vec_buff, WI_I32_CONST);
            leb = int_to_leb128((i64) datum->offset_, &leb_len);
            bh_buffer_append(&vec_buff, leb, leb_len);
            bh_buffer_write_byte(&vec_buff, WI_BLOCK_END);
        }

        if (datum->data != NULL) {
            leb = uint_to_uleb128((u64) datum->length, &leb_len);
            bh_buffer_append(&vec_buff, leb, leb_len);
            fori (i, 0, datum->length) bh_buffer_write_byte(&vec_buff, ((u8 *) datum->data)[i]);
        } else {
            leb = uint_to_uleb128(0, &leb_len);
            bh_buffer_append(&vec_buff, leb, leb_len);
        }
    }

    leb = uint_to_uleb128((u64) (vec_buff.length), &leb_len);
    bh_buffer_append(buff, leb, leb_len);

    bh_buffer_concat(buff, vec_buff);
    bh_buffer_free(&vec_buff);

    return buff->length - prev_len;
}

static i32 output_onyx_libraries_section(OnyxWasmModule* module, bh_buffer* buff) {
    if (bh_arr_length(module->libraries) == 0) return 0;
    i32 prev_len = buff->length;

    bh_buffer_write_byte(buff, WASM_SECTION_ID_CUSTOM);

    bh_buffer libs_buff;
    bh_buffer_init(&libs_buff, buff->allocator, 128);

    output_custom_section_name("_onyx_libs", &libs_buff);

    output_unsigned_integer(bh_arr_length(module->library_paths), &libs_buff);

    bh_arr_each(char *, lib, module->library_paths) {
        assert(*lib != NULL);

        u32 lib_len = strlen(*lib);
        output_unsigned_integer(lib_len, &libs_buff);
        bh_buffer_append(&libs_buff, *lib, lib_len);
    }

    output_unsigned_integer(bh_arr_length(module->libraries), &libs_buff);

    bh_arr_each(char *, lib, module->libraries) {
        assert(*lib != NULL);

        u32 lib_len = strlen(*lib);
        output_unsigned_integer(lib_len, &libs_buff);
        bh_buffer_append(&libs_buff, *lib, lib_len);
    }

    output_unsigned_integer(libs_buff.length, buff);

    bh_buffer_concat(buff, libs_buff);
    bh_buffer_free(&libs_buff);

    return buff->length - prev_len;
}

#ifdef ENABLE_DEBUG_INFO
static i32 output_ovm_debug_sections(OnyxWasmModule* module, bh_buffer* buff) {
    if (!module->debug_context || !module->context->options->debug_info_enabled) return 0;

    DebugContext *ctx = module->debug_context;

    bh_buffer section_buff;
    bh_buffer_init(&section_buff, buff->allocator, 128);

    {
        // ovm_debug_files section
        bh_buffer_clear(&section_buff);
        bh_buffer_write_byte(buff, WASM_SECTION_ID_CUSTOM);

        output_custom_section_name("ovm_debug_files", &section_buff);

        i32 file_count = shlenu(ctx->file_info);
        output_unsigned_integer(file_count, &section_buff);

        fori (i, 0, file_count) {
            Table(DebugFileInfo) entry = (void *) &ctx->file_info[i];
            output_unsigned_integer(entry->value.file_id, &section_buff);
            output_unsigned_integer(entry->value.line_count, &section_buff);
            output_name(entry->key, strlen(entry->key), &section_buff);
        }

        output_unsigned_integer(section_buff.length, buff);

        bh_buffer_concat(buff, section_buff);
    }

    {
        // ovm_debug_funcs section
        bh_buffer_clear(&section_buff);
        bh_buffer_write_byte(buff, WASM_SECTION_ID_CUSTOM);

        output_custom_section_name("ovm_debug_funcs", &section_buff);

        i32 func_count = bh_arr_length(ctx->funcs);
        output_unsigned_integer(func_count, &section_buff);

        fori (i, 0, func_count) {
            DebugFuncContext *func = &ctx->funcs[i];
            output_unsigned_integer(func->func_index, &section_buff);
            output_unsigned_integer(func->file_id, &section_buff);
            output_unsigned_integer(func->line, &section_buff);
            output_name(func->name, func->name_length, &section_buff);
            output_unsigned_integer(1, &section_buff);
            output_unsigned_integer(func->op_offset, &section_buff);

            LocalAllocator *locals = &module->funcs[i].locals;
            if (func->stack_ptr_idx > 0) {
                u32 local_idx = local_lookup_idx(locals, func->stack_ptr_idx);
                output_unsigned_integer(local_idx, &section_buff);
            } else {
                output_unsigned_integer(0, &section_buff);
            }

            output_unsigned_integer(0, &section_buff);
        }

        output_unsigned_integer(section_buff.length, buff);

        bh_buffer_concat(buff, section_buff);
    }

    {
        // ovm_debug_syms section

        // First, apply patches for register locations
        bh_arr_each(DebugSymPatch, patch, ctx->sym_patches) {
            // CLEANUP: This is (kind of) incorrect, as there is nothing guarenteeing
            // that the symbol with id a will be a position a, other than the way
            // that this has been implemented right now.
            assert(ctx->sym_info[patch->sym_id].location_type == DSL_REGISTER);

            LocalAllocator *locals = &module->funcs[patch->func_idx].locals;
            ctx->sym_info[patch->sym_id].location_num = local_lookup_idx(locals, patch->local_idx);
        }

        bh_buffer_clear(&section_buff);
        bh_buffer_write_byte(buff, WASM_SECTION_ID_CUSTOM);

        output_custom_section_name("ovm_debug_syms", &section_buff);

        i32 sym_count = bh_arr_length(ctx->sym_info);
        output_unsigned_integer(sym_count, &section_buff);

        fori (i, 0, sym_count) {
            DebugSymInfo *sym = &ctx->sym_info[i];
            output_unsigned_integer(sym->sym_id, &section_buff);
            if (sym->name) {
                output_name(sym->name, strlen(sym->name), &section_buff);
            } else {
                output_unsigned_integer(0, &section_buff);
            }
            output_unsigned_integer(sym->location_type, &section_buff);
            output_unsigned_integer(sym->location_num, &section_buff);
            output_unsigned_integer(sym->type, &section_buff);
        }

        output_unsigned_integer(section_buff.length, buff);

        bh_buffer_concat(buff, section_buff);
    }

    {
        // ovm_debug_types section
        bh_buffer_clear(&section_buff);
        bh_buffer_write_byte(buff, WASM_SECTION_ID_CUSTOM);

        output_custom_section_name("ovm_debug_types", &section_buff);

        i32 type_count = bh_arr_length(module->context->types.type_map.entries);
        output_unsigned_integer(type_count, &section_buff);

        bh_arr_each(bh__imap_entry, entry, module->context->types.type_map.entries) {
            u32 id     = entry->key;
            Type *type = (Type *) entry->value;
            const char *name = type_get_name(module->context, type);

            output_unsigned_integer(id, &section_buff);
            output_name(name, strlen(name), &section_buff);
            output_unsigned_integer(type_size_of(type), &section_buff);

            if (type->kind == Type_Kind_Basic) {
                // Type indicies are special because they are encoded
                // as a "distinct" unsigned 32-bit integer, which is
                // effectively how they are used in the code anyway.
                //
                // This is probably a change that will be made throughout
                // the entire compiler, but for now they will remain as
                // a special type.
                if (type->Basic.kind == Basic_Kind_Type_Index) {
                    output_unsigned_integer(5, &section_buff);
                    output_unsigned_integer(2, &section_buff);
                    output_unsigned_integer(module->context->types.basic[Basic_Kind_U32]->id, &section_buff);
                    continue;
                }

                if (type->Basic.kind == Basic_Kind_Rawptr) {
                    // rawptr -> ^void
                    output_unsigned_integer(2, &section_buff);
                    output_unsigned_integer(1, &section_buff);
                    output_unsigned_integer(module->context->types.basic[Basic_Kind_Void]->id, &section_buff);
                    continue;
                }

                if (type->Basic.kind == Basic_Kind_U8) {
                    output_unsigned_integer(1, &section_buff);
                    output_unsigned_integer(5, &section_buff);
                    continue;
                }

                output_unsigned_integer(1, &section_buff);
                if      (type->Basic.kind == Basic_Kind_Void) output_unsigned_integer(0, &section_buff);
                else if (type_is_bool(type))                  output_unsigned_integer(4, &section_buff);
                else if (type_is_integer(type)) {
                    if (type->Basic.flags & Basic_Flag_Unsigned) output_unsigned_integer(2, &section_buff);
                    else                                         output_unsigned_integer(1, &section_buff);
                }
                else if (type->Basic.flags & Basic_Flag_Float)   output_unsigned_integer(3, &section_buff);
                else if (type_is_simd(type))                     output_unsigned_integer(6, &section_buff);
                else {
                    output_unsigned_integer(0, &section_buff);
                }

                continue;
            }

            if (type->kind == Type_Kind_Pointer) {
                output_unsigned_integer(2, &section_buff);
                output_unsigned_integer(1, &section_buff);
                output_unsigned_integer(type->Pointer.elem->id, &section_buff);
                continue; 
            }

            // In the debug info, multi-pointers are just pointers.
            if (type->kind == Type_Kind_MultiPointer) {
                output_unsigned_integer(2, &section_buff);
                output_unsigned_integer(1, &section_buff);
                output_unsigned_integer(type->Pointer.elem->id, &section_buff);
                continue; 
            }

            if (type->kind == Type_Kind_Enum) {
                output_unsigned_integer(8, &section_buff);
                output_unsigned_integer(type->Enum.backing->id, &section_buff);

                AstEnumType *e_type = (AstEnumType *) type->ast_type;
                assert(e_type->kind == Ast_Kind_Enum_Type);

                output_unsigned_integer(bh_arr_length(e_type->values), &section_buff);
                bh_arr_each(AstEnumValue *, pev, e_type->values) {
                    AstEnumValue *ev = *pev;

                    output_unsigned_integer(get_expression_integer_value(module->context, ev->value, NULL), &section_buff);
                    output_name(ev->token->text, ev->token->length, &section_buff);
                }
                continue;
            }

            if (type->kind == Type_Kind_Array) {
                output_unsigned_integer(4, &section_buff);
                output_unsigned_integer(type->Array.count, &section_buff);
                output_unsigned_integer(type->Array.elem->id, &section_buff);
                continue;
            }

            if (type->kind == Type_Kind_Slice) {
                output_unsigned_integer(7, &section_buff);
                output_unsigned_integer(type->Slice.elem->id, &section_buff);
                continue;
            }

            // This would be nice if this would work...
            // But it breaks when passing dynamic arrays as parameters.
            // if (type->kind == Type_Kind_DynArray) {
            //     output_unsigned_integer(7, &section_buff);
            //     output_unsigned_integer(type->DynArray.elem->id, &section_buff);
            //     continue;
            // }

            if (type->kind == Type_Kind_Union) {
                output_unsigned_integer(9, &section_buff);
                output_unsigned_integer(type->Union.alignment, &section_buff);

                i32 var_count = bh_arr_length(type->Union.variants_ordered);
                output_unsigned_integer(var_count, &section_buff);

                fori (i, 0, var_count) {
                    char *name = type->Union.variants_ordered[i]->name;
                    output_name(name, strlen(name), &section_buff);
                    
                    output_unsigned_integer(type->Union.variants_ordered[i]->type->id, &section_buff);
                }

                continue;
            }

            if (type->kind == Type_Kind_Function) {
                output_unsigned_integer(6, &section_buff);
                output_unsigned_integer(type->Function.param_count, &section_buff);

                fori (i, 0, (i32) type->Function.param_count) {
                    output_unsigned_integer(type->Function.params[i]->id, &section_buff);
                }

                output_unsigned_integer(type->Function.return_type->id, &section_buff);
                continue;
            }

            if (type_is_structlike_strict(type)) {
                output_unsigned_integer(3, &section_buff);

                output_unsigned_integer(type_structlike_is_simple(type), &section_buff);

                i32 mem_count = type_structlike_mem_count(type);
                output_unsigned_integer(mem_count, &section_buff);

                fori (i, 0, mem_count) {
                    StructMember smem;
                    type_lookup_member_by_idx(module->context, type, i, &smem);

                    output_unsigned_integer(smem.offset, &section_buff);
                    output_unsigned_integer(smem.type->id, &section_buff);
                    output_name(smem.name, strlen(smem.name), &section_buff);
                }

                continue;
            }

            if (type->kind == Type_Kind_Distinct) {
                output_unsigned_integer(5, &section_buff);
                output_unsigned_integer(2, &section_buff);
                output_unsigned_integer(type->Distinct.base_type->id, &section_buff);
                continue;
            }

            // No debug information will be given about the poly struct
            // or compound types.
            // Outside of runtime type information, they provide no useful
            // debugging information (I don't think at least...).
            if (type->kind == Type_Kind_PolyStruct ||
                type->kind == Type_Kind_PolyUnion ||
                type->kind == Type_Kind_Compound) {
                output_unsigned_integer(1, &section_buff);
                output_unsigned_integer(0, &section_buff);
                continue;
            }

            assert("Unhandled type in debug info builder" && 0);
        }

        output_unsigned_integer(section_buff.length, buff);

        bh_buffer_concat(buff, section_buff);
    }

    {
        // ovm_debug_ops section
        bh_buffer_clear(&section_buff);
        bh_buffer_write_byte(buff, WASM_SECTION_ID_CUSTOM);

        output_custom_section_name("ovm_debug_ops", &section_buff);
        bh_buffer_concat(&section_buff, ctx->op_buffer);

        output_unsigned_integer(section_buff.length, buff);

        bh_buffer_concat(buff, section_buff);
    }

    bh_buffer_free(&section_buff);
    return 0;
}
#endif

static i32 output_name_section(OnyxWasmModule* module, bh_buffer* buff) {
    i32 prev_len = buff->length;

    bh_buffer_write_byte(buff, WASM_SECTION_ID_CUSTOM);

    bh_buffer name_buff;
    bh_buffer_init(&name_buff, buff->allocator, 128);

    output_custom_section_name("name", &name_buff);

    output_unsigned_integer(1, &name_buff); // 1 for function names

    bh_buffer func_name_buff;
    bh_buffer_init(&func_name_buff, buff->allocator, 128);

    output_unsigned_integer(bh_arr_length(module->funcs), &func_name_buff);
    bh_arr_each(WasmFunc, func, module->funcs) {
        if (func->name == NULL) continue;

        u64 func_idx = func - module->funcs;
        func_idx += module->next_foreign_func_idx;

        output_unsigned_integer(func_idx, &func_name_buff);

        output_name(func->name, strlen(func->name), &func_name_buff);
    }

    output_unsigned_integer(func_name_buff.length, &name_buff);
    bh_buffer_concat(&name_buff, func_name_buff);
    bh_buffer_free(&func_name_buff);

    output_unsigned_integer(name_buff.length, buff);
    bh_buffer_concat(buff, name_buff);
    bh_buffer_free(&name_buff);

    return buff->length - prev_len;
}

#define VERSION__(m,i,p) #m "." #i "." #p
#define VERSION_(m,i,p) VERSION__(m,i,p)
#define VERSION VERSION_(VERSION_MAJOR, VERSION_MINOR, VERSION_PATCH)

static i32 output_producer_section(OnyxWasmModule* module, bh_buffer *buff) {
    i32 prev_len = buff->length;

    bh_buffer_write_byte(buff, WASM_SECTION_ID_CUSTOM);

    bh_buffer prod_buff;
    bh_buffer_init(&prod_buff, buff->allocator, 128);

    output_custom_section_name("producers", &prod_buff);

    output_unsigned_integer(2, &prod_buff);

    output_name("language", 8, &prod_buff);
    output_unsigned_integer(1, &prod_buff);
    output_name("onyx", 4, &prod_buff);
    output_name(VERSION, strlen(VERSION), &prod_buff);

    output_name("processed-by", 12, &prod_buff);
    output_unsigned_integer(1, &prod_buff);
    output_name("onyx", 4, &prod_buff);
    output_name(VERSION, strlen(VERSION), &prod_buff);

    output_unsigned_integer(prod_buff.length, buff);
    bh_buffer_concat(buff, prod_buff);
    bh_buffer_free(&prod_buff);

    return buff->length - prev_len;
}

i32 output_custom_section(OnyxWasmModule *module, bh_buffer *buff, WasmCustomSection *section) {
    i32 prev_len = buff->length;

    bh_buffer_write_byte(buff, WASM_SECTION_ID_CUSTOM);

    bh_buffer inner_buff;
    bh_buffer_init(&inner_buff, buff->allocator, 128);

    output_custom_section_name(section->name, &inner_buff);

    bh_buffer_append(&inner_buff, section->contents, section->len);

    output_unsigned_integer(inner_buff.length, buff);
    bh_buffer_concat(buff, inner_buff);
    bh_buffer_free(&inner_buff);

    return buff->length - prev_len;
}

void onyx_wasm_module_write_to_buffer(OnyxWasmModule* module, bh_buffer* buffer) {
    bh_buffer_init(buffer, module->context->gp_alloc, 128);
    bh_buffer_append(buffer, WASM_MAGIC_STRING, 4);
    bh_buffer_append(buffer, WASM_VERSION, 4);

#ifdef ENABLE_DEBUG_INFO
    if (module->context->options->debug_info_enabled) {
        output_ovm_debug_sections(module, buffer);
    }
#endif
    output_typesection(module, buffer);
    output_importsection(module, buffer);
    output_funcsection(module, buffer);
    output_tablesection(module, buffer);
    output_memorysection(module, buffer);
    output_globalsection(module, buffer);
    output_exportsection(module, buffer);
    output_startsection(module, buffer);
    output_elemsection(module, buffer);
    output_datacountsection(module, buffer);
    output_codesection(module, buffer);
    output_datasection(module, buffer);
    output_onyx_libraries_section(module, buffer);

    if (module->context->options->generate_name_section) {
        output_name_section(module, buffer);
    }

    output_producer_section(module, buffer);

    fori (i, 0, shlen(module->custom_sections)) {
        output_custom_section(module, buffer, &module->custom_sections[i].value);
    }

    // TODO: Consider if this should always be included?
    // It can amount to a lot of extra data.
    // output_onyx_func_offset_section(module, buffer);
}




//
// JS File
//

static i32 compare_js_partials(const void *p1, const void *p2) {
    return ((JsPartial *) p2)->order - ((JsPartial *) p1)->order;
}

void onyx_wasm_module_write_js_partials_to_buffer(OnyxWasmModule* module, bh_buffer* buffer) {
    bh_buffer_init(buffer, module->context->gp_alloc, 128);

    qsort(module->js_partials,
        bh_arr_length(module->js_partials),
        sizeof(JsPartial),
        compare_js_partials);

    bh_arr_each(JsPartial, partial, module->js_partials) {
        bh_buffer_write_string(buffer, partial->code);
    }
}