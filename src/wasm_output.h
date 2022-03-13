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

static const u8 ONYX_MAGIC_STRING[] = "ONYX";
static const u8 WASM_MAGIC_STRING[] = { 0x00, 0x61, 0x73, 0x6D };
static const u8 WASM_VERSION[] = { 0x01, 0x00, 0x00, 0x00 };

static void output_instruction(WasmFunc* func, WasmInstruction* instr, bh_buffer* buff);

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
    if (bh_arr_length(module->elems) == 0) return 0;

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
    if (context.options->use_multi_threading) return 0;

    i32 prev_len = buff->length;
    bh_buffer_write_byte(buff, WASM_SECTION_ID_MEMORY);

    bh_buffer vec_buff;
    bh_buffer_init(&vec_buff, buff->allocator, 128);

    i32 leb_len;
    u8* leb = uint_to_uleb128((u64) 1, &leb_len);
    bh_buffer_append(&vec_buff, leb, leb_len);

    // FIXME: This needs to be dynamically chosen depending on the size of
    // the data section and stack size pre-requeseted.
    // :WasmMemory
    output_limits(1024, -1, 0, &vec_buff);

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
            output_instruction(NULL, instr, &vec_buff);

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

            case WASM_FOREIGN_TABLE: assert(0);
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

static void output_instruction(WasmFunc* func, WasmInstruction* instr, bh_buffer* buff) {
    i32 leb_len;
    u8* leb;

    if (instr->type == WI_NOP) return;
    if (instr->type == WI_UNREACHABLE) assert(("EMITTING UNREACHABLE!!", 0));

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

static i32 output_code(WasmFunc* func, bh_buffer* buff) {

    bh_buffer code_buff;
    bh_buffer_init(&code_buff, buff->allocator, 128);

    // Output locals
    output_locals(func, &code_buff);

    assert(func->code);

    // Output code
    bh_arr_each(WasmInstruction, instr, func->code) output_instruction(func, instr, &code_buff);

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

    // DEBUG_HERE;

    bh_arr_each(WasmFunc, func, module->funcs) output_code(func, &vec_buff);

    leb = uint_to_uleb128((u64) (vec_buff.length), &leb_len);
    bh_buffer_append(buff, leb, leb_len);

    bh_buffer_concat(buff, vec_buff);
    bh_buffer_free(&vec_buff);

    return buff->length - prev_len;
}

static i32 output_datacountsection(OnyxWasmModule* module, bh_buffer* buff) {
    if (!context.options->use_post_mvp_features) return 0;

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
        assert(datum->data != NULL);

        i32 memory_flags = 0x00;
        if (context.options->use_multi_threading) memory_flags |= 0x01;

        bh_buffer_write_byte(&vec_buff, memory_flags);

        if (!context.options->use_multi_threading) {
            bh_buffer_write_byte(&vec_buff, WI_I32_CONST);
            leb = int_to_leb128((i64) datum->offset, &leb_len);
            bh_buffer_append(&vec_buff, leb, leb_len);
            bh_buffer_write_byte(&vec_buff, WI_BLOCK_END);
        }

        leb = uint_to_uleb128((u64) datum->length, &leb_len);
        bh_buffer_append(&vec_buff, leb, leb_len);
        fori (i, 0, datum->length) bh_buffer_write_byte(&vec_buff, ((u8 *) datum->data)[i]);
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

    const char *custom_name = "_onyx_libs";
    i32 leb_len;
    u8* leb = uint_to_uleb128(strlen(custom_name), &leb_len);
    bh_buffer_append(&libs_buff, leb, leb_len);
    bh_buffer_append(&libs_buff, custom_name, strlen(custom_name));

    leb = uint_to_uleb128((u64) bh_arr_length(module->library_paths), &leb_len);
    bh_buffer_append(&libs_buff, leb, leb_len);

    bh_arr_each(char *, lib, module->library_paths) {
        assert(*lib != NULL);

        u32 lib_len = strlen(*lib);
        leb = uint_to_uleb128((u64) lib_len, &leb_len);
        bh_buffer_append(&libs_buff, leb, leb_len);
        bh_buffer_append(&libs_buff, *lib, lib_len);
    }

    leb = uint_to_uleb128((u64) bh_arr_length(module->libraries), &leb_len);
    bh_buffer_append(&libs_buff, leb, leb_len);

    bh_arr_each(char *, lib, module->libraries) {
        assert(*lib != NULL);

        u32 lib_len = strlen(*lib);
        leb = uint_to_uleb128((u64) lib_len, &leb_len);
        bh_buffer_append(&libs_buff, leb, leb_len);
        bh_buffer_append(&libs_buff, *lib, lib_len);
    }

    leb = uint_to_uleb128((u64) (libs_buff.length), &leb_len);
    bh_buffer_append(buff, leb, leb_len);

    bh_buffer_concat(buff, libs_buff);
    bh_buffer_free(&libs_buff);

    return buff->length - prev_len;
}

static i32 output_onyx_func_offset_section(OnyxWasmModule* module, bh_buffer* buff) {
    i32 prev_len = buff->length;

    bh_buffer_write_byte(buff, WASM_SECTION_ID_CUSTOM);

    bh_buffer section_buff;
    bh_buffer_init(&section_buff, buff->allocator, 128);

    const char *custom_name = "_onyx_func_offsets";
    i32 leb_len;
    u8* leb = uint_to_uleb128(strlen(custom_name), &leb_len);
    bh_buffer_append(&section_buff, leb, leb_len);
    bh_buffer_append(&section_buff, custom_name, strlen(custom_name));

    i32 func_count = bh_arr_length(module->funcs) + module->foreign_function_count;

    bh_buffer name_buff;
    bh_buffer_init(&name_buff, buff->allocator, 1024);
    u32 str_cursor = func_count * 4;
    fori (i, 0, func_count) {
        bh_buffer_write_u32(&section_buff, str_cursor);

        if (i < module->foreign_function_count) {
            bh_buffer_append(&name_buff, "<imported function>", 20);
            str_cursor += 20;
        } else {
            WasmFunc *func = &module->funcs[i - module->foreign_function_count];
            assert(func->location);
            char *str = bh_bprintf("%s:%d,%d\0", func->location->pos.filename, func->location->pos.line, func->location->pos.column);
            i32 len = strlen(str);
            bh_buffer_append(&name_buff, str, len + 1);
            str_cursor += len + 1;
        }
    }

    bh_buffer_concat(&section_buff, name_buff);

    leb = uint_to_uleb128((u64) (section_buff.length), &leb_len);
    bh_buffer_append(buff, leb, leb_len);

    bh_buffer_concat(buff, section_buff);
    bh_buffer_free(&section_buff);

    return buff->length - prev_len;
}

void onyx_wasm_module_write_to_buffer(OnyxWasmModule* module, bh_buffer* buffer) {
    bh_buffer_init(buffer, global_heap_allocator, 128);
    if (context.options->runtime == Runtime_Onyx) {
        bh_buffer_append(buffer, ONYX_MAGIC_STRING, 4);
    } else {
        bh_buffer_append(buffer, WASM_MAGIC_STRING, 4);
    }
    bh_buffer_append(buffer, WASM_VERSION, 4);

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
    output_onyx_func_offset_section(module, buffer);
}

void onyx_wasm_module_write_to_file(OnyxWasmModule* module, bh_file file) {
    bh_buffer master_buffer;
    onyx_wasm_module_write_to_buffer(module, &master_buffer);

    bh_file_write(&file, master_buffer.data, master_buffer.length);
}
