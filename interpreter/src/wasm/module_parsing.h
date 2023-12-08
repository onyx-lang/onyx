// vim: ft=c:

//
// This file is not to be compile like normal.
// It is instead included in wasm/module.c
//
// Currently, this file has a lot of code that directly manipulates
// the code builder object. I would like to move this into the API
// for the code builder itself, to make it more portable and easy
// to read.

typedef struct build_context build_context;
struct build_context {
    wasm_byte_vec_t binary;
    unsigned int    offset;

    wasm_module_t *module;
    ovm_program_t *program;
    ovm_store_t   *store;

    int func_table_arr_idx;
    int next_external_func_idx;

    debug_info_builder_t debug_builder;

    // This will be set/reset for every code (function) entry.
    ovm_code_builder_t builder;
};

#define PEEK_BYTE(ctx)    ((ctx)->binary.data[(ctx)->offset])
#define CONSUME_BYTE(ctx) ((ctx)->binary.data[(ctx)->offset++])

enum wasm_section_numbers_t {
    WASM_CUSTOM_SECTION = 0,
    WASM_TYPE_SECTION   = 1,
    WASM_IMPORT_SECTION = 2,
    WASM_FUNC_SECTION   = 3,
    WASM_TABLE_SECTION  = 4,
    WASM_MEMORY_SECTION = 5,
    WASM_GLOBAL_SECTION = 6,
    WASM_EXPORT_SECTION = 7,
    WASM_START_SECTION  = 8,
    WASM_ELEM_SECTION   = 9,
    WASM_CODE_SECTION   = 10,
    WASM_DATA_SECTION   = 11,
    WASM_DATAC_SECTION  = 12,
};

static inline wasm_valkind_t parse_valtype(build_context *ctx) {
    switch (CONSUME_BYTE(ctx)) {
        case 0x7f: return WASM_I32;
        case 0x7e: return WASM_I64;
        case 0x7d: return WASM_F32;
        case 0x7c: return WASM_F64;
        case 0x7b: assert(0 && "SIMD values are not currently supported");
        case 0x70: return WASM_FUNCREF;
        case 0x6F: return WASM_ANYREF;
        default:   assert(0 && "Invalid valtype.");
    }
}

static void parse_custom_section(build_context *ctx) {
    unsigned int section_size = uleb128_to_uint((u8 *)ctx->binary.data, (i32 *)&ctx->offset);
    unsigned int end_of_section = ctx->offset + section_size;

    struct wasm_custom_section_t cs;

    char name[256];
    unsigned int name_len = uleb128_to_uint((u8 *)ctx->binary.data, (i32 *)&ctx->offset);
    if (name_len < sizeof(name) - 1) {
        strncpy(name, &((char *) ctx->binary.data)[ctx->offset], name_len);
        name[name_len] = '\0';

        ctx->offset += name_len;
        cs.size = end_of_section - ctx->offset;

        unsigned int data_size = end_of_section - ctx->offset;
        cs.data = bh_alloc_array(ctx->store->heap_allocator, char, data_size);
        memcpy(cs.data, &((char *) ctx->binary.data)[ctx->offset], data_size);

        shput(ctx->module->custom_sections, name, cs);

        if (!strcmp(name, "ovm_debug_files")) {
            debug_info_import_file_info(ctx->debug_builder.info, (u8 *)cs.data, cs.size);
        }

        if (!strcmp(name, "ovm_debug_funcs")) {
            debug_info_import_func_info(ctx->debug_builder.info, (u8 *)cs.data, cs.size);
        }

        if (!strcmp(name, "ovm_debug_ops")) {
            debug_info_builder_prepare(&ctx->debug_builder, (u8 *)cs.data);
        }

        if (!strcmp(name, "ovm_debug_syms")) {
            debug_info_import_sym_info(ctx->debug_builder.info, (u8 *)cs.data, cs.size);
        }

        if (!strcmp(name, "ovm_debug_types")) {
            debug_info_import_type_info(ctx->debug_builder.info, (u8 *)cs.data, cs.size);
        }
    }

    ctx->offset = end_of_section;
}

static void parse_type_section(build_context *ctx) {
    unsigned int section_size = uleb128_to_uint((u8 *)ctx->binary.data, (i32 *)&ctx->offset);
    unsigned int type_count   = uleb128_to_uint((u8 *)ctx->binary.data, (i32 *)&ctx->offset);

    wasm_functype_vec_new_uninitialized(&ctx->module->type_section, type_count);

    fori (i, 0, (int) type_count) {
        assert(CONSUME_BYTE(ctx) == 0x60); // @ReportError

        unsigned int param_count = uleb128_to_uint((u8 *)ctx->binary.data, (i32 *)&ctx->offset);
        wasm_valtype_vec_t param_types;
        wasm_valtype_vec_new_uninitialized(&param_types, param_count);
        fori (p, 0, (int) param_count) {
            param_types.data[p] = wasm_valtype_new(parse_valtype(ctx));
        }

        unsigned int result_count = uleb128_to_uint((u8 *)ctx->binary.data, (i32 *)&ctx->offset);
        wasm_valtype_vec_t result_types;
        wasm_valtype_vec_new_uninitialized(&result_types, result_count);
        fori (p, 0, (int) result_count) {
            result_types.data[p] = wasm_valtype_new(parse_valtype(ctx));
        }

        wasm_functype_t *functype = wasm_functype_new(&param_types, &result_types);
        ctx->module->type_section.data[i] = functype;
    }
}

static wasm_limits_t parse_limits(build_context *ctx) {
    bool maximum_present = CONSUME_BYTE(ctx) & 0x01;

    wasm_limits_t limits;
    limits.min = uleb128_to_uint((u8 *)ctx->binary.data, (i32 *)&ctx->offset);
    if (maximum_present) {
        limits.max = uleb128_to_uint((u8 *)ctx->binary.data, (i32 *)&ctx->offset);
    } else {
        limits.max = wasm_limits_max_default;
    }

    return limits;
}

static wasm_tabletype_t *parse_tabletype(build_context *ctx) {
    assert(CONSUME_BYTE(ctx) == 0x70); // @ReportError

    wasm_limits_t limits = parse_limits(ctx);
    wasm_tabletype_t *tt = wasm_tabletype_new(wasm_valtype_new(WASM_FUNCREF), &limits);
    return tt;
}

static wasm_memorytype_t *parse_memorytype(build_context *ctx) {
    wasm_limits_t limits = parse_limits(ctx);
    wasm_memorytype_t *mt = wasm_memorytype_new(&limits);
    return mt;
}

static wasm_globaltype_t *parse_globaltype(build_context *ctx) {
    wasm_valtype_t *valtype = wasm_valtype_new(parse_valtype(ctx));
    bool mutable = CONSUME_BYTE(ctx) == 0x01;

    wasm_globaltype_t *gt = wasm_globaltype_new(valtype, mutable);
    return gt;
}

static void parse_import_section(build_context *ctx) {
    unsigned int section_size = uleb128_to_uint((u8 *)ctx->binary.data, (i32 *)&ctx->offset);
    unsigned int import_count = uleb128_to_uint((u8 *)ctx->binary.data, (i32 *)&ctx->offset);

    wasm_importtype_vec_new_uninitialized(&ctx->module->imports, import_count);

    fori (i, 0, (int) import_count) {
        unsigned int mod_name_size = uleb128_to_uint((u8 *)ctx->binary.data, (i32 *)&ctx->offset);
        wasm_byte_vec_t module_name;
        wasm_byte_vec_new_uninitialized(&module_name, mod_name_size);
        fori (n, 0, mod_name_size) module_name.data[n] = CONSUME_BYTE(ctx);

        unsigned int import_name_size = uleb128_to_uint((u8 *)ctx->binary.data, (i32 *)&ctx->offset);
        wasm_byte_vec_t import_name;
        wasm_byte_vec_new_uninitialized(&import_name, import_name_size);
        fori (n, 0, import_name_size) import_name.data[n] = CONSUME_BYTE(ctx);

        wasm_externtype_t *import_type = NULL;
        switch (CONSUME_BYTE(ctx)) {
            case 0x00: {
                unsigned int type_idx = uleb128_to_uint((u8 *)ctx->binary.data, (i32 *)&ctx->offset);
                import_type = wasm_functype_as_externtype(ctx->module->type_section.data[type_idx]);
                break;
            }

            case 0x01: import_type = wasm_tabletype_as_externtype(parse_tabletype(ctx)); break;
            case 0x02: import_type = wasm_memorytype_as_externtype(parse_memorytype(ctx)); break;
            case 0x03: import_type = wasm_globaltype_as_externtype(parse_globaltype(ctx)); break;
        }

        wasm_importtype_t *import = wasm_importtype_new(&module_name, &import_name, import_type);
        ctx->module->imports.data[i] = import;

        if (import_type->kind == WASM_EXTERN_FUNC) {
            char *external_func_name = bh_aprintf(ctx->program->store->arena_allocator, "%b.%b",
                module_name.data, module_name.size,
                import_name.data, import_name.size);

            int external_func_idx = ctx->next_external_func_idx++;
            import->external_func_idx = external_func_idx;

            ovm_program_register_external_func(ctx->program, external_func_name, import_type->func.params.size, external_func_idx);
        }
    }
}

static void parse_func_section(build_context *ctx) {
    unsigned int section_size = uleb128_to_uint((u8 *)ctx->binary.data, (i32 *)&ctx->offset);
    unsigned int func_count = uleb128_to_uint((u8 *)ctx->binary.data, (i32 *)&ctx->offset);

    wasm_functype_vec_new_uninitialized(&ctx->module->functypes, func_count);

    fori (i, 0, (int) func_count) {
        unsigned int index = uleb128_to_uint((u8 *)ctx->binary.data, (i32 *)&ctx->offset);
        ctx->module->functypes.data[i] = ctx->module->type_section.data[index];
    }
}

static void parse_table_section(build_context *ctx) {
    unsigned int section_size = uleb128_to_uint((u8 *)ctx->binary.data, (i32 *)&ctx->offset);
    unsigned int table_count = uleb128_to_uint((u8 *)ctx->binary.data, (i32 *)&ctx->offset);

    wasm_tabletype_vec_new_uninitialized(&ctx->module->tabletypes, table_count);

    fori (i, 0, (int) table_count) {
        ctx->module->tabletypes.data[i] = parse_tabletype(ctx);
    }
}

static void parse_memory_section(build_context *ctx) {
    unsigned int section_size = uleb128_to_uint((u8 *)ctx->binary.data, (i32 *)&ctx->offset);
    unsigned int memory_count = uleb128_to_uint((u8 *)ctx->binary.data, (i32 *)&ctx->offset);

    wasm_memorytype_vec_new_uninitialized(&ctx->module->memorytypes, memory_count);

    fori (i, 0, (int) memory_count) {
        ctx->module->memorytypes.data[i] = parse_memorytype(ctx);
    }
}

static void parse_global_section(build_context *ctx) {
    unsigned int section_size = uleb128_to_uint((u8 *)ctx->binary.data, (i32 *)&ctx->offset);
    unsigned int global_count = uleb128_to_uint((u8 *)ctx->binary.data, (i32 *)&ctx->offset);

    wasm_globaltype_vec_new_uninitialized(&ctx->module->globaltypes, global_count);

    fori (i, 0, (int) global_count) {
        wasm_globaltype_t *gt = parse_globaltype(ctx);

        switch (CONSUME_BYTE(ctx)) {
            case 0x41: {
                gt->type.global.initial_value.kind = WASM_I32;
                gt->type.global.initial_value.of.i32 = (i32) uleb128_to_uint((u8 *)ctx->binary.data, (i32 *)&ctx->offset);
                break;
            }

            case 0x42: {
                gt->type.global.initial_value.kind = WASM_I64;
                gt->type.global.initial_value.of.i64 = (i64) uleb128_to_uint((u8 *)ctx->binary.data, (i32 *)&ctx->offset);
                break;
            }

            case 0x43: {
                gt->type.global.initial_value.kind = WASM_F32;
                gt->type.global.initial_value.of.f32 = *(f32 *) &ctx->binary.data[ctx->offset]; // HACK: This assumes IEEE-754 floats
                ctx->offset += 4;
                break;
            }

            case 0x44: {
                gt->type.global.initial_value.kind = WASM_F64;
                gt->type.global.initial_value.of.f64 = *(f64 *) &ctx->binary.data[ctx->offset]; // HACK: This assumes IEEE-754 floats
                ctx->offset += 8;
                break;
            }
        }

        assert(CONSUME_BYTE(ctx) == 0x0b);

        ctx->module->globaltypes.data[i] = gt;
    }
}

static void parse_export_section(build_context *ctx) {
    unsigned int section_size = uleb128_to_uint((u8 *)ctx->binary.data, (i32 *)&ctx->offset);
    unsigned int export_count = uleb128_to_uint((u8 *)ctx->binary.data, (i32 *)&ctx->offset);

    wasm_exporttype_vec_new_uninitialized(&ctx->module->exports, export_count);

    fori (i, 0, (int) export_count) {
        unsigned int export_name_size = uleb128_to_uint((u8 *)ctx->binary.data, (i32 *)&ctx->offset);
        wasm_byte_vec_t export_name;
        wasm_byte_vec_new_uninitialized(&export_name, export_name_size);
        fori (n, 0, export_name_size) export_name.data[n] = CONSUME_BYTE(ctx);

        unsigned int type = CONSUME_BYTE(ctx);
        unsigned int idx = uleb128_to_uint((u8 *)ctx->binary.data, (i32 *)&ctx->offset);

        wasm_externtype_t *export_type = NULL;

        switch (type) {
            case 0x00: export_type = wasm_functype_as_externtype(wasm_module_index_functype(ctx->module, idx)); break;
            case 0x01: export_type = wasm_tabletype_as_externtype(wasm_module_index_tabletype(ctx->module, idx)); break;
            case 0x02: export_type = wasm_memorytype_as_externtype(wasm_module_index_memorytype(ctx->module, idx)); break;
            case 0x03: export_type = wasm_globaltype_as_externtype(wasm_module_index_globaltype(ctx->module, idx)); break;
            default: assert(0);
        }

        assert(export_type);

        wasm_exporttype_t *export = wasm_exporttype_new(&export_name, export_type);
        export->index = idx;
        ctx->module->exports.data[i] = export;
    }
}

static void parse_start_section(build_context *ctx) {
    unsigned int section_size = uleb128_to_uint((u8 *)ctx->binary.data, (i32 *)&ctx->offset);
    ctx->module->start_func_idx = uleb128_to_uint((u8 *)ctx->binary.data, (i32 *)&ctx->offset);
}

static void parse_elem_section(build_context *ctx) {
    unsigned int section_size = uleb128_to_uint((u8 *)ctx->binary.data, (i32 *)&ctx->offset);
    unsigned int elem_count = uleb128_to_uint((u8 *)ctx->binary.data, (i32 *)&ctx->offset);

    // This is going to be a mess...
    // I am only going to handle the case of a single, active, offset-0,
    // element entry. This is all that Onyx uses and will probably ever
    // use.
    assert(elem_count == 1);
    assert(CONSUME_BYTE(ctx) == 0x00);
    assert(CONSUME_BYTE(ctx) == 0x41);
    assert(CONSUME_BYTE(ctx) == 0x00);
    assert(CONSUME_BYTE(ctx) == 0x0B);

    unsigned int entry_count = uleb128_to_uint((u8 *)ctx->binary.data, (i32 *)&ctx->offset);
    ctx->module->elem_count = entry_count;
    ctx->module->elem_entries = malloc(sizeof(unsigned int) * entry_count);

    fori (i, 0, (int) entry_count) {
        ctx->module->elem_entries[i] = uleb128_to_uint((u8 *)ctx->binary.data, (i32 *)&ctx->offset);
    }

    ctx->func_table_arr_idx = ovm_program_register_static_ints(ctx->program, entry_count, (int *)ctx->module->elem_entries);

    assert(ctx->module->tabletypes.size == 1);
    ctx->module->tabletypes.data[0]->type.table.static_arr = ctx->func_table_arr_idx;
}

static void parse_data_section(build_context *ctx) {
    unsigned int section_size = uleb128_to_uint((u8 *)ctx->binary.data, (i32 *)&ctx->offset);
    unsigned int data_count = uleb128_to_uint((u8 *)ctx->binary.data, (i32 *)&ctx->offset);

    if (ctx->module->data_count_present) {
        assert(ctx->module->data_count == data_count);
    } else {
        ctx->module->data_count = data_count;
    }

    ctx->module->data_entries = malloc(sizeof(struct wasm_data_t) * data_count);

    fori (i, 0, (int) data_count) {
        struct wasm_data_t data_entry;
        data_entry.data = NULL;
        data_entry.offset = 0;
        data_entry.length = 0;
        data_entry.passive = true;

        char data_type = CONSUME_BYTE(ctx);
        if (data_type == 0x00) {
            assert(CONSUME_BYTE(ctx) == 0x41);
            data_entry.offset = uleb128_to_uint((u8 *)ctx->binary.data, (i32 *)&ctx->offset);
            data_entry.passive = false;
            assert(CONSUME_BYTE(ctx) == 0x0B);
        }

        data_entry.length = uleb128_to_uint((u8 *)ctx->binary.data, (i32 *)&ctx->offset);
        data_entry.data = bh_pointer_add(ctx->binary.data, ctx->offset);
        ctx->offset += data_entry.length;

        ctx->module->data_entries[i] = data_entry;
    }
}

static void parse_data_count_section(build_context *ctx) {
    unsigned int section_size = uleb128_to_uint((u8 *)ctx->binary.data, (i32 *)&ctx->offset);
    unsigned int data_count = uleb128_to_uint((u8 *)ctx->binary.data, (i32 *)&ctx->offset);

    ctx->module->data_count_present = true;
    ctx->module->data_count = data_count;
}



//
// Instruction building
//

static void parse_expression(build_context *ctx);

static void parse_fc_instruction(build_context *ctx) {
    int instr_num = uleb128_to_uint((u8 *)ctx->binary.data, (i32 *)&ctx->offset);

    switch (instr_num) {
        case 0: ovm_code_builder_add_unop (&ctx->builder, OVM_TYPED_INSTR(OVMI_CVT_F32, OVM_TYPE_I32)); break;
        case 1: ovm_code_builder_add_unop (&ctx->builder, OVM_TYPED_INSTR(OVMI_CVT_F32, OVM_TYPE_I32)); break;
        case 2: ovm_code_builder_add_unop (&ctx->builder, OVM_TYPED_INSTR(OVMI_CVT_F64, OVM_TYPE_I32)); break;
        case 3: ovm_code_builder_add_unop (&ctx->builder, OVM_TYPED_INSTR(OVMI_CVT_F64, OVM_TYPE_I32)); break;
        case 4: ovm_code_builder_add_unop (&ctx->builder, OVM_TYPED_INSTR(OVMI_CVT_F32, OVM_TYPE_I64)); break;
        case 5: ovm_code_builder_add_unop (&ctx->builder, OVM_TYPED_INSTR(OVMI_CVT_F32, OVM_TYPE_I64)); break;
        case 6: ovm_code_builder_add_unop (&ctx->builder, OVM_TYPED_INSTR(OVMI_CVT_F64, OVM_TYPE_I64)); break;
        case 7: ovm_code_builder_add_unop (&ctx->builder, OVM_TYPED_INSTR(OVMI_CVT_F64, OVM_TYPE_I64)); break;

        case 8: {
            int dataidx = uleb128_to_uint((u8 *)ctx->binary.data, (i32 *)&ctx->offset);
            assert(CONSUME_BYTE(ctx) == 0x00);

            ovm_code_builder_add_imm(&ctx->builder, OVM_TYPE_I32, &dataidx);
            ovm_code_builder_add_call(&ctx->builder, ctx->module->memory_init_idx, 4, false);
            break;
        }

        case 10: {
            assert(CONSUME_BYTE(ctx) == 0x00);
            assert(CONSUME_BYTE(ctx) == 0x00);

            ovm_code_builder_add_memory_copy(&ctx->builder);
            break;
        }

        case 11: {
            assert(CONSUME_BYTE(ctx) == 0x00);

            ovm_code_builder_add_memory_fill(&ctx->builder);
            break;
        }

        default: assert(0 && "UNHANDLED FC INSTRUCTION");
    }
}

static void parse_fe_instruction(build_context *ctx) {
    int instr_num = uleb128_to_uint((u8 *)ctx->binary.data, (i32 *)&ctx->offset);

    switch (instr_num) {

#define LOAD_CASE(num, type) \
        case num : { \
            int alignment = uleb128_to_uint((u8 *)ctx->binary.data, (i32 *)&ctx->offset); \
            int offset    = uleb128_to_uint((u8 *)ctx->binary.data, (i32 *)&ctx->offset); \
            ovm_code_builder_add_atomic_load(&ctx->builder, type, offset); \
            break; \
        }

        LOAD_CASE(0x10, OVM_TYPE_I32)
        LOAD_CASE(0x11, OVM_TYPE_I64)
        LOAD_CASE(0x12, OVM_TYPE_I8)
        LOAD_CASE(0x13, OVM_TYPE_I16)
        LOAD_CASE(0x14, OVM_TYPE_I8)
        LOAD_CASE(0x15, OVM_TYPE_I16)
        LOAD_CASE(0x16, OVM_TYPE_I32)

#undef LOAD_CASE

#define STORE_CASE(num, type) \
        case num : { \
            int alignment = uleb128_to_uint((u8 *)ctx->binary.data, (i32 *)&ctx->offset); \
            int offset    = uleb128_to_uint((u8 *)ctx->binary.data, (i32 *)&ctx->offset); \
            ovm_code_builder_add_atomic_store(&ctx->builder, type, offset); \
            break; \
        }

        STORE_CASE(0x17, OVM_TYPE_I32)
        STORE_CASE(0x18, OVM_TYPE_I64)
        STORE_CASE(0x19, OVM_TYPE_I8)
        STORE_CASE(0x1A, OVM_TYPE_I16)
        STORE_CASE(0x1B, OVM_TYPE_I8)
        STORE_CASE(0x1C, OVM_TYPE_I16)
        STORE_CASE(0x1D, OVM_TYPE_I32)

#undef STORE_CASE

#define CMPXCHG_CASE(num, type) \
        case num : { \
            int alignment = uleb128_to_uint((u8 *)ctx->binary.data, (i32 *)&ctx->offset); \
            int offset    = uleb128_to_uint((u8 *)ctx->binary.data, (i32 *)&ctx->offset); \
            ovm_code_builder_add_cmpxchg(&ctx->builder, type, offset); \
            break; \
        }

        CMPXCHG_CASE(0x48, OVM_TYPE_I32)
        CMPXCHG_CASE(0x49, OVM_TYPE_I64)
        CMPXCHG_CASE(0x4A, OVM_TYPE_I8)
        CMPXCHG_CASE(0x4B, OVM_TYPE_I16)
        CMPXCHG_CASE(0x4C, OVM_TYPE_I8)
        CMPXCHG_CASE(0x4D, OVM_TYPE_I16)
        CMPXCHG_CASE(0x4E, OVM_TYPE_I32)

#undef CMPXCHG_CASE

        default: assert(0 && "UNHANDLED ATOMIC INSTRUCTION... SORRY :/");
    }
}

static void parse_instruction(build_context *ctx) {
    debug_info_builder_step(&ctx->debug_builder);

    unsigned char instr_byte = CONSUME_BYTE(ctx);
    switch (instr_byte) {
        case 0x00:
            ovm_code_builder_add_break(&ctx->builder);
            break;

        case 0x01:
            ovm_code_builder_add_nop(&ctx->builder);
            break;

        case 0x02: {
            // Currently, only "void" block types are valid.
            assert(CONSUME_BYTE(ctx) == 0x40);
            ovm_code_builder_push_label_target(&ctx->builder, label_kind_block);
            break;
        }

        case 0x03: {
            // Currently, only "void" block types are valid.
            assert(CONSUME_BYTE(ctx) == 0x40);
            ovm_code_builder_push_label_target(&ctx->builder, label_kind_loop);
            break;
        }

        case 0x04: {
            // Currently, only "void" block types are valid.
            assert(CONSUME_BYTE(ctx) == 0x40);
            int if_target = ovm_code_builder_push_label_target(&ctx->builder, label_kind_if);

            //
            // This uses the pattern of "branch if zero" to skip a section of
            // code if the condition was not true.
            ovm_code_builder_add_cond_branch(&ctx->builder, if_target, false, true);
            break;
        }

        case 0x05: {
            label_target_t if_target = ovm_code_builder_wasm_target_idx(&ctx->builder, 0);
            ovm_code_builder_add_branch(&ctx->builder, if_target.idx);
            ovm_code_builder_patch_else(&ctx->builder, if_target);
            break;
        }

        case 0x0B: {
            ovm_code_builder_pop_label_target(&ctx->builder);
            break;
        }

        case 0x0C: {
            int label_idx = uleb128_to_uint((u8 *)ctx->binary.data, (i32 *)&ctx->offset);

            label_target_t target = ovm_code_builder_wasm_target_idx(&ctx->builder, label_idx);
            ovm_code_builder_add_branch(&ctx->builder, target.idx);
            break;
        }

        case 0x0D: {
            int label_idx = uleb128_to_uint((u8 *)ctx->binary.data, (i32 *)&ctx->offset);

            label_target_t target = ovm_code_builder_wasm_target_idx(&ctx->builder, label_idx);
            ovm_code_builder_add_cond_branch(&ctx->builder, target.idx, true, false);
            break;
        }

        case 0x0E: {
            // Branch tables are the most complicated thing ever :/

            int entry_count = uleb128_to_uint((u8 *)ctx->binary.data, (i32 *)&ctx->offset);
            int *entries = bh_alloc_array(bh_heap_allocator(), int, entry_count);

            fori (i, 0, entry_count) {
                int label_idx = uleb128_to_uint((u8 *)ctx->binary.data, (i32 *)&ctx->offset);
                label_target_t target = ovm_code_builder_wasm_target_idx(&ctx->builder, label_idx);
                entries[i] = target.idx;
            }

            int default_entry_idx = uleb128_to_uint((u8 *)ctx->binary.data, (i32 *)&ctx->offset);
            label_target_t target = ovm_code_builder_wasm_target_idx(&ctx->builder, default_entry_idx);
            int default_entry = target.idx;

            ovm_code_builder_add_branch_table(&ctx->builder, entry_count, entries, default_entry);
            break;
        }

        case 0x0F: {
            ovm_code_builder_add_return(&ctx->builder);
            break;
        }

        case 0x10: {
            int func_idx = uleb128_to_uint((u8 *)ctx->binary.data, (i32 *)&ctx->offset);

            wasm_functype_t *functype = wasm_module_index_functype(ctx->module, func_idx);
            int param_count = functype->type.func.params.size;

            ovm_code_builder_add_call(&ctx->builder, func_idx, param_count, functype->type.func.results.size != 0);
            break;
        }

        case 0x11: {
            int type_idx = uleb128_to_uint((u8 *)ctx->binary.data, (i32 *)&ctx->offset);
            int table_idx = uleb128_to_uint((u8 *)ctx->binary.data, (i32 *)&ctx->offset);
            assert(table_idx == 0);

            wasm_functype_t *functype = ctx->module->type_section.data[type_idx];
            int param_count = functype->type.func.params.size;
            ovm_code_builder_add_indirect_call(&ctx->builder, param_count, functype->type.func.results.size != 0);
            break;
        }

        case 0x1A: {
            ovm_code_builder_drop_value(&ctx->builder);
            break;
        }

        case 0x1B: assert(0);

        case 0x20: {
            int local_idx = uleb128_to_uint((u8 *)ctx->binary.data, (i32 *)&ctx->offset);
            ovm_code_builder_add_local_get(&ctx->builder, local_idx);
            break;
        }

        case 0x21: {
            int local_idx = uleb128_to_uint((u8 *)ctx->binary.data, (i32 *)&ctx->offset);
            ovm_code_builder_add_local_set(&ctx->builder, local_idx);
            break;
        }

        case 0x22: {
            int local_idx = uleb128_to_uint((u8 *)ctx->binary.data, (i32 *)&ctx->offset);
            ovm_code_builder_add_local_tee(&ctx->builder, local_idx);
            break;
        }

        case 0x23: {
            int global_idx = uleb128_to_uint((u8 *)ctx->binary.data, (i32 *)&ctx->offset);
            ovm_code_builder_add_register_get(&ctx->builder, global_idx);
            break;
        }

        case 0x24: {
            int global_idx = uleb128_to_uint((u8 *)ctx->binary.data, (i32 *)&ctx->offset);
            ovm_code_builder_add_register_set(&ctx->builder, global_idx);
            break;
        }

#define LOAD_CASE(num, type, convert, convert_op, convert_type) \
        case num : { \
            int alignment = uleb128_to_uint((u8 *)ctx->binary.data, (i32 *)&ctx->offset); \
            int offset    = uleb128_to_uint((u8 *)ctx->binary.data, (i32 *)&ctx->offset); \
            ovm_code_builder_add_load(&ctx->builder, type, offset); \
            if (convert) ovm_code_builder_add_unop(&ctx->builder, OVM_TYPED_INSTR(convert_op, convert_type)); \
            break; \
        }

        LOAD_CASE(0x28, OVM_TYPE_I32, false, 0, 0)
        LOAD_CASE(0x29, OVM_TYPE_I64, false, 0, 0)
        LOAD_CASE(0x2A, OVM_TYPE_F32, false, 0, 0)
        LOAD_CASE(0x2B, OVM_TYPE_F64, false, 0, 0)

        LOAD_CASE(0x2C, OVM_TYPE_I8,  true, OVMI_CVT_I8_S,  OVM_TYPE_I32)
        LOAD_CASE(0x2D, OVM_TYPE_I8,  true, OVMI_CVT_I8,    OVM_TYPE_I32)
        LOAD_CASE(0x2E, OVM_TYPE_I16, true, OVMI_CVT_I16_S, OVM_TYPE_I32)
        LOAD_CASE(0x2F, OVM_TYPE_I16, true, OVMI_CVT_I16,   OVM_TYPE_I32)
        LOAD_CASE(0x30, OVM_TYPE_I8,  true, OVMI_CVT_I8_S,  OVM_TYPE_I64)
        LOAD_CASE(0x31, OVM_TYPE_I8,  true, OVMI_CVT_I8,    OVM_TYPE_I64)
        LOAD_CASE(0x32, OVM_TYPE_I16, true, OVMI_CVT_I16_S, OVM_TYPE_I64)
        LOAD_CASE(0x33, OVM_TYPE_I16, true, OVMI_CVT_I16,   OVM_TYPE_I64)
        LOAD_CASE(0x34, OVM_TYPE_I32, true, OVMI_CVT_I32_S, OVM_TYPE_I64)
        LOAD_CASE(0x35, OVM_TYPE_I32, true, OVMI_CVT_I32,   OVM_TYPE_I64)

#undef LOAD_CASE

#define STORE_CASE(num, type) \
        case num : { \
            int alignment = uleb128_to_uint((u8 *)ctx->binary.data, (i32 *)&ctx->offset); \
            int offset    = uleb128_to_uint((u8 *)ctx->binary.data, (i32 *)&ctx->offset); \
            ovm_code_builder_add_store(&ctx->builder, type, offset); \
            break; \
        }

        STORE_CASE(0x36, OVM_TYPE_I32);
        STORE_CASE(0x37, OVM_TYPE_I64);
        STORE_CASE(0x38, OVM_TYPE_F32);
        STORE_CASE(0x39, OVM_TYPE_F64);

        STORE_CASE(0x3A, OVM_TYPE_I8);
        STORE_CASE(0x3B, OVM_TYPE_I16);
        STORE_CASE(0x3C, OVM_TYPE_I8);
        STORE_CASE(0x3D, OVM_TYPE_I16);
        STORE_CASE(0x3E, OVM_TYPE_I32);

#undef STORE_CASE

        case 0x3F: {
            assert(CONSUME_BYTE(ctx) == 0x00);
            ovm_code_builder_add_memory_size(&ctx->builder);
            break;
        }

        case 0x40: {
            assert(CONSUME_BYTE(ctx) == 0x00);
            ovm_code_builder_add_memory_grow(&ctx->builder);
            break;
        }

        case 0x41: {
            i64 value = leb128_to_int((u8 *)ctx->binary.data, (i32 *)&ctx->offset);

            // NOTE: This assumes a little-endian CPU as the address is assumes
            // to be the least significant byte.
            ovm_code_builder_add_imm(&ctx->builder, OVM_TYPE_I32, &value);
            break;
        }

        case 0x42: {
            i64 value = leb128_to_int((u8 *)ctx->binary.data, (i32 *)&ctx->offset);
            ovm_code_builder_add_imm(&ctx->builder, OVM_TYPE_I64, &value);
            break;
        }

        case 0x43: {
            float value = * (f32 *) &ctx->binary.data[ctx->offset];
            ctx->offset += 4;
            ovm_code_builder_add_imm(&ctx->builder, OVM_TYPE_F32, &value);
            break;
        }

        case 0x44: {
            double value = * (f64 *) &ctx->binary.data[ctx->offset];
            ctx->offset += 8;
            ovm_code_builder_add_imm(&ctx->builder, OVM_TYPE_F64, &value);
            break;
        }

        case 0x45: {
            unsigned int zero = 0;
            ovm_code_builder_add_imm(&ctx->builder, OVM_TYPE_I32, &zero);
            ovm_code_builder_add_binop(&ctx->builder, OVM_TYPED_INSTR(OVMI_EQ, OVM_TYPE_I32));
            break;
        }
        case 0x46: ovm_code_builder_add_binop(&ctx->builder, OVM_TYPED_INSTR(OVMI_EQ, OVM_TYPE_I32)); break;
        case 0x47: ovm_code_builder_add_binop(&ctx->builder, OVM_TYPED_INSTR(OVMI_NE, OVM_TYPE_I32)); break;
        case 0x48: ovm_code_builder_add_binop(&ctx->builder, OVM_TYPED_INSTR(OVMI_LT_S, OVM_TYPE_I32)); break;
        case 0x49: ovm_code_builder_add_binop(&ctx->builder, OVM_TYPED_INSTR(OVMI_LT, OVM_TYPE_I32)); break;
        case 0x4A: ovm_code_builder_add_binop(&ctx->builder, OVM_TYPED_INSTR(OVMI_GT_S, OVM_TYPE_I32)); break;
        case 0x4B: ovm_code_builder_add_binop(&ctx->builder, OVM_TYPED_INSTR(OVMI_GT, OVM_TYPE_I32)); break;
        case 0x4C: ovm_code_builder_add_binop(&ctx->builder, OVM_TYPED_INSTR(OVMI_LE_S, OVM_TYPE_I32)); break;
        case 0x4D: ovm_code_builder_add_binop(&ctx->builder, OVM_TYPED_INSTR(OVMI_LE, OVM_TYPE_I32)); break;
        case 0x4E: ovm_code_builder_add_binop(&ctx->builder, OVM_TYPED_INSTR(OVMI_GE_S, OVM_TYPE_I32)); break;
        case 0x4F: ovm_code_builder_add_binop(&ctx->builder, OVM_TYPED_INSTR(OVMI_GE, OVM_TYPE_I32)); break;

        case 0x50: {
            unsigned long long zero = 0;
            ovm_code_builder_add_imm(&ctx->builder, OVM_TYPE_I64, &zero);
            ovm_code_builder_add_binop(&ctx->builder, OVM_TYPED_INSTR(OVMI_EQ, OVM_TYPE_I64));
            break;
        }
        case 0x51: ovm_code_builder_add_binop(&ctx->builder, OVM_TYPED_INSTR(OVMI_EQ, OVM_TYPE_I64)); break;
        case 0x52: ovm_code_builder_add_binop(&ctx->builder, OVM_TYPED_INSTR(OVMI_NE, OVM_TYPE_I64)); break;
        case 0x53: ovm_code_builder_add_binop(&ctx->builder, OVM_TYPED_INSTR(OVMI_LT_S, OVM_TYPE_I64)); break;
        case 0x54: ovm_code_builder_add_binop(&ctx->builder, OVM_TYPED_INSTR(OVMI_LT, OVM_TYPE_I64)); break;
        case 0x55: ovm_code_builder_add_binop(&ctx->builder, OVM_TYPED_INSTR(OVMI_GT_S, OVM_TYPE_I64)); break;
        case 0x56: ovm_code_builder_add_binop(&ctx->builder, OVM_TYPED_INSTR(OVMI_GT, OVM_TYPE_I64)); break;
        case 0x57: ovm_code_builder_add_binop(&ctx->builder, OVM_TYPED_INSTR(OVMI_LE_S, OVM_TYPE_I64)); break;
        case 0x58: ovm_code_builder_add_binop(&ctx->builder, OVM_TYPED_INSTR(OVMI_LE, OVM_TYPE_I64)); break;
        case 0x59: ovm_code_builder_add_binop(&ctx->builder, OVM_TYPED_INSTR(OVMI_GE_S, OVM_TYPE_I64)); break;
        case 0x5A: ovm_code_builder_add_binop(&ctx->builder, OVM_TYPED_INSTR(OVMI_GE, OVM_TYPE_I64)); break;

        case 0x5B: ovm_code_builder_add_binop(&ctx->builder, OVM_TYPED_INSTR(OVMI_EQ, OVM_TYPE_F32)); break;
        case 0x5C: ovm_code_builder_add_binop(&ctx->builder, OVM_TYPED_INSTR(OVMI_NE, OVM_TYPE_F32)); break;
        case 0x5D: ovm_code_builder_add_binop(&ctx->builder, OVM_TYPED_INSTR(OVMI_LT, OVM_TYPE_F32)); break;
        case 0x5E: ovm_code_builder_add_binop(&ctx->builder, OVM_TYPED_INSTR(OVMI_GT, OVM_TYPE_F32)); break;
        case 0x5F: ovm_code_builder_add_binop(&ctx->builder, OVM_TYPED_INSTR(OVMI_LE, OVM_TYPE_F32)); break;
        case 0x60: ovm_code_builder_add_binop(&ctx->builder, OVM_TYPED_INSTR(OVMI_GE, OVM_TYPE_F32)); break;

        case 0x61: ovm_code_builder_add_binop(&ctx->builder, OVM_TYPED_INSTR(OVMI_EQ, OVM_TYPE_F64)); break;
        case 0x62: ovm_code_builder_add_binop(&ctx->builder, OVM_TYPED_INSTR(OVMI_NE, OVM_TYPE_F64)); break;
        case 0x63: ovm_code_builder_add_binop(&ctx->builder, OVM_TYPED_INSTR(OVMI_LT, OVM_TYPE_F64)); break;
        case 0x64: ovm_code_builder_add_binop(&ctx->builder, OVM_TYPED_INSTR(OVMI_GT, OVM_TYPE_F64)); break;
        case 0x65: ovm_code_builder_add_binop(&ctx->builder, OVM_TYPED_INSTR(OVMI_LE, OVM_TYPE_F64)); break;
        case 0x66: ovm_code_builder_add_binop(&ctx->builder, OVM_TYPED_INSTR(OVMI_GE, OVM_TYPE_F64)); break;

        case 0x67: ovm_code_builder_add_unop (&ctx->builder, OVM_TYPED_INSTR(OVMI_CLZ, OVM_TYPE_I32)); break;
        case 0x68: ovm_code_builder_add_unop (&ctx->builder, OVM_TYPED_INSTR(OVMI_CTZ, OVM_TYPE_I32)); break;
        case 0x69: ovm_code_builder_add_unop (&ctx->builder, OVM_TYPED_INSTR(OVMI_POPCNT, OVM_TYPE_I32)); break;
        case 0x6A: ovm_code_builder_add_binop(&ctx->builder, OVM_TYPED_INSTR(OVMI_ADD, OVM_TYPE_I32)); break;
        case 0x6B: ovm_code_builder_add_binop(&ctx->builder, OVM_TYPED_INSTR(OVMI_SUB, OVM_TYPE_I32)); break;
        case 0x6C: ovm_code_builder_add_binop(&ctx->builder, OVM_TYPED_INSTR(OVMI_MUL, OVM_TYPE_I32)); break;
        case 0x6D: ovm_code_builder_add_binop(&ctx->builder, OVM_TYPED_INSTR(OVMI_DIV_S, OVM_TYPE_I32)); break;
        case 0x6E: ovm_code_builder_add_binop(&ctx->builder, OVM_TYPED_INSTR(OVMI_DIV, OVM_TYPE_I32)); break;
        case 0x6F: ovm_code_builder_add_binop(&ctx->builder, OVM_TYPED_INSTR(OVMI_REM_S, OVM_TYPE_I32)); break;
        case 0x70: ovm_code_builder_add_binop(&ctx->builder, OVM_TYPED_INSTR(OVMI_REM, OVM_TYPE_I32)); break;
        case 0x71: ovm_code_builder_add_binop(&ctx->builder, OVM_TYPED_INSTR(OVMI_AND, OVM_TYPE_I32)); break;
        case 0x72: ovm_code_builder_add_binop(&ctx->builder, OVM_TYPED_INSTR(OVMI_OR, OVM_TYPE_I32)); break;
        case 0x73: ovm_code_builder_add_binop(&ctx->builder, OVM_TYPED_INSTR(OVMI_XOR, OVM_TYPE_I32)); break;
        case 0x74: ovm_code_builder_add_binop(&ctx->builder, OVM_TYPED_INSTR(OVMI_SHL, OVM_TYPE_I32)); break;
        case 0x75: ovm_code_builder_add_binop(&ctx->builder, OVM_TYPED_INSTR(OVMI_SAR, OVM_TYPE_I32)); break;
        case 0x76: ovm_code_builder_add_binop(&ctx->builder, OVM_TYPED_INSTR(OVMI_SHR, OVM_TYPE_I32)); break;
        case 0x77: ovm_code_builder_add_binop(&ctx->builder, OVM_TYPED_INSTR(OVMI_ROTL, OVM_TYPE_I32)); break;
        case 0x78: ovm_code_builder_add_binop(&ctx->builder, OVM_TYPED_INSTR(OVMI_ROTR, OVM_TYPE_I32)); break;

        case 0x79: ovm_code_builder_add_unop (&ctx->builder, OVM_TYPED_INSTR(OVMI_CLZ, OVM_TYPE_I64)); break;
        case 0x7A: ovm_code_builder_add_unop (&ctx->builder, OVM_TYPED_INSTR(OVMI_CTZ, OVM_TYPE_I64)); break;
        case 0x7B: ovm_code_builder_add_unop (&ctx->builder, OVM_TYPED_INSTR(OVMI_POPCNT, OVM_TYPE_I64)); break;
        case 0x7C: ovm_code_builder_add_binop(&ctx->builder, OVM_TYPED_INSTR(OVMI_ADD, OVM_TYPE_I64)); break;
        case 0x7D: ovm_code_builder_add_binop(&ctx->builder, OVM_TYPED_INSTR(OVMI_SUB, OVM_TYPE_I64)); break;
        case 0x7E: ovm_code_builder_add_binop(&ctx->builder, OVM_TYPED_INSTR(OVMI_MUL, OVM_TYPE_I64)); break;
        case 0x7F: ovm_code_builder_add_binop(&ctx->builder, OVM_TYPED_INSTR(OVMI_DIV_S, OVM_TYPE_I64)); break;
        case 0x80: ovm_code_builder_add_binop(&ctx->builder, OVM_TYPED_INSTR(OVMI_DIV, OVM_TYPE_I64)); break;
        case 0x81: ovm_code_builder_add_binop(&ctx->builder, OVM_TYPED_INSTR(OVMI_REM_S, OVM_TYPE_I64)); break;
        case 0x82: ovm_code_builder_add_binop(&ctx->builder, OVM_TYPED_INSTR(OVMI_REM, OVM_TYPE_I64)); break;
        case 0x83: ovm_code_builder_add_binop(&ctx->builder, OVM_TYPED_INSTR(OVMI_AND, OVM_TYPE_I64)); break;
        case 0x84: ovm_code_builder_add_binop(&ctx->builder, OVM_TYPED_INSTR(OVMI_OR, OVM_TYPE_I64)); break;
        case 0x85: ovm_code_builder_add_binop(&ctx->builder, OVM_TYPED_INSTR(OVMI_XOR, OVM_TYPE_I64)); break;
        case 0x86: ovm_code_builder_add_binop(&ctx->builder, OVM_TYPED_INSTR(OVMI_SHL, OVM_TYPE_I64)); break;
        case 0x87: ovm_code_builder_add_binop(&ctx->builder, OVM_TYPED_INSTR(OVMI_SAR, OVM_TYPE_I64)); break;
        case 0x88: ovm_code_builder_add_binop(&ctx->builder, OVM_TYPED_INSTR(OVMI_SHR, OVM_TYPE_I64)); break;
        case 0x89: ovm_code_builder_add_binop(&ctx->builder, OVM_TYPED_INSTR(OVMI_ROTL, OVM_TYPE_I64)); break;
        case 0x8A: ovm_code_builder_add_binop(&ctx->builder, OVM_TYPED_INSTR(OVMI_ROTR, OVM_TYPE_I64)); break;

        case 0x8B: ovm_code_builder_add_unop (&ctx->builder, OVM_TYPED_INSTR(OVMI_ABS, OVM_TYPE_F32)); break;
        case 0x8C: ovm_code_builder_add_unop (&ctx->builder, OVM_TYPED_INSTR(OVMI_NEG, OVM_TYPE_F32)); break;
        case 0x8D: ovm_code_builder_add_unop (&ctx->builder, OVM_TYPED_INSTR(OVMI_CEIL, OVM_TYPE_F32)); break;
        case 0x8E: ovm_code_builder_add_unop (&ctx->builder, OVM_TYPED_INSTR(OVMI_FLOOR, OVM_TYPE_F32)); break;
        case 0x8F: ovm_code_builder_add_unop (&ctx->builder, OVM_TYPED_INSTR(OVMI_TRUNC, OVM_TYPE_F32)); break;
        case 0x90: ovm_code_builder_add_unop (&ctx->builder, OVM_TYPED_INSTR(OVMI_NEAREST, OVM_TYPE_F32)); break;
        case 0x91: ovm_code_builder_add_unop (&ctx->builder, OVM_TYPED_INSTR(OVMI_SQRT, OVM_TYPE_F32)); break;
        case 0x92: ovm_code_builder_add_binop(&ctx->builder, OVM_TYPED_INSTR(OVMI_ADD, OVM_TYPE_F32)); break;
        case 0x93: ovm_code_builder_add_binop(&ctx->builder, OVM_TYPED_INSTR(OVMI_SUB, OVM_TYPE_F32)); break;
        case 0x94: ovm_code_builder_add_binop(&ctx->builder, OVM_TYPED_INSTR(OVMI_MUL, OVM_TYPE_F32)); break;
        case 0x95: ovm_code_builder_add_binop(&ctx->builder, OVM_TYPED_INSTR(OVMI_DIV, OVM_TYPE_F32)); break;
        case 0x96: ovm_code_builder_add_binop(&ctx->builder, OVM_TYPED_INSTR(OVMI_MIN, OVM_TYPE_F32)); break;
        case 0x97: ovm_code_builder_add_binop(&ctx->builder, OVM_TYPED_INSTR(OVMI_MAX, OVM_TYPE_F32)); break;
        case 0x98: ovm_code_builder_add_binop(&ctx->builder, OVM_TYPED_INSTR(OVMI_COPYSIGN, OVM_TYPE_F32)); break;

        case 0x99: ovm_code_builder_add_unop (&ctx->builder, OVM_TYPED_INSTR(OVMI_ABS, OVM_TYPE_F64)); break;
        case 0x9A: ovm_code_builder_add_unop (&ctx->builder, OVM_TYPED_INSTR(OVMI_NEG, OVM_TYPE_F64)); break;
        case 0x9B: ovm_code_builder_add_unop (&ctx->builder, OVM_TYPED_INSTR(OVMI_CEIL, OVM_TYPE_F64)); break;
        case 0x9C: ovm_code_builder_add_unop (&ctx->builder, OVM_TYPED_INSTR(OVMI_FLOOR, OVM_TYPE_F64)); break;
        case 0x9D: ovm_code_builder_add_unop (&ctx->builder, OVM_TYPED_INSTR(OVMI_TRUNC, OVM_TYPE_F64)); break;
        case 0x9E: ovm_code_builder_add_unop (&ctx->builder, OVM_TYPED_INSTR(OVMI_NEAREST, OVM_TYPE_F64)); break;
        case 0x9F: ovm_code_builder_add_unop (&ctx->builder, OVM_TYPED_INSTR(OVMI_SQRT, OVM_TYPE_F64)); break;
        case 0xA0: ovm_code_builder_add_binop(&ctx->builder, OVM_TYPED_INSTR(OVMI_ADD, OVM_TYPE_F64)); break;
        case 0xA1: ovm_code_builder_add_binop(&ctx->builder, OVM_TYPED_INSTR(OVMI_SUB, OVM_TYPE_F64)); break;
        case 0xA2: ovm_code_builder_add_binop(&ctx->builder, OVM_TYPED_INSTR(OVMI_MUL, OVM_TYPE_F64)); break;
        case 0xA3: ovm_code_builder_add_binop(&ctx->builder, OVM_TYPED_INSTR(OVMI_DIV, OVM_TYPE_F64)); break;
        case 0xA4: ovm_code_builder_add_binop(&ctx->builder, OVM_TYPED_INSTR(OVMI_MIN, OVM_TYPE_F64)); break;
        case 0xA5: ovm_code_builder_add_binop(&ctx->builder, OVM_TYPED_INSTR(OVMI_MAX, OVM_TYPE_F64)); break;
        case 0xA6: ovm_code_builder_add_binop(&ctx->builder, OVM_TYPED_INSTR(OVMI_COPYSIGN, OVM_TYPE_F64)); break;

        case 0xA7: ovm_code_builder_add_unop (&ctx->builder, OVM_TYPED_INSTR(OVMI_CVT_I64,   OVM_TYPE_I32)); break;
        case 0xA8: ovm_code_builder_add_unop (&ctx->builder, OVM_TYPED_INSTR(OVMI_CVT_F32_S, OVM_TYPE_I32)); break;
        case 0xA9: ovm_code_builder_add_unop (&ctx->builder, OVM_TYPED_INSTR(OVMI_CVT_F32,   OVM_TYPE_I32)); break;
        case 0xAA: ovm_code_builder_add_unop (&ctx->builder, OVM_TYPED_INSTR(OVMI_CVT_F64_S, OVM_TYPE_I32)); break;
        case 0xAB: ovm_code_builder_add_unop (&ctx->builder, OVM_TYPED_INSTR(OVMI_CVT_F64,   OVM_TYPE_I32)); break;
        case 0xAC: ovm_code_builder_add_unop (&ctx->builder, OVM_TYPED_INSTR(OVMI_CVT_I32_S, OVM_TYPE_I64)); break;
        case 0xAD: ovm_code_builder_add_unop (&ctx->builder, OVM_TYPED_INSTR(OVMI_CVT_I32,   OVM_TYPE_I64)); break;
        case 0xAE: ovm_code_builder_add_unop (&ctx->builder, OVM_TYPED_INSTR(OVMI_CVT_F32_S, OVM_TYPE_I64)); break;
        case 0xAF: ovm_code_builder_add_unop (&ctx->builder, OVM_TYPED_INSTR(OVMI_CVT_F32,   OVM_TYPE_I64)); break;
        case 0xB0: ovm_code_builder_add_unop (&ctx->builder, OVM_TYPED_INSTR(OVMI_CVT_F64_S, OVM_TYPE_I64)); break;
        case 0xB1: ovm_code_builder_add_unop (&ctx->builder, OVM_TYPED_INSTR(OVMI_CVT_F64,   OVM_TYPE_I64)); break;
        case 0xB2: ovm_code_builder_add_unop (&ctx->builder, OVM_TYPED_INSTR(OVMI_CVT_I32_S, OVM_TYPE_F32)); break;
        case 0xB3: ovm_code_builder_add_unop (&ctx->builder, OVM_TYPED_INSTR(OVMI_CVT_I32,   OVM_TYPE_F32)); break;
        case 0xB4: ovm_code_builder_add_unop (&ctx->builder, OVM_TYPED_INSTR(OVMI_CVT_I64_S, OVM_TYPE_F32)); break;
        case 0xB5: ovm_code_builder_add_unop (&ctx->builder, OVM_TYPED_INSTR(OVMI_CVT_I64,   OVM_TYPE_F32)); break;
        case 0xB6: ovm_code_builder_add_unop (&ctx->builder, OVM_TYPED_INSTR(OVMI_CVT_F64,   OVM_TYPE_F32)); break;
        case 0xB7: ovm_code_builder_add_unop (&ctx->builder, OVM_TYPED_INSTR(OVMI_CVT_I32_S, OVM_TYPE_F64)); break;
        case 0xB8: ovm_code_builder_add_unop (&ctx->builder, OVM_TYPED_INSTR(OVMI_CVT_I32,   OVM_TYPE_F64)); break;
        case 0xB9: ovm_code_builder_add_unop (&ctx->builder, OVM_TYPED_INSTR(OVMI_CVT_I64_S, OVM_TYPE_F64)); break;
        case 0xBA: ovm_code_builder_add_unop (&ctx->builder, OVM_TYPED_INSTR(OVMI_CVT_I64,   OVM_TYPE_F64)); break;
        case 0xBB: ovm_code_builder_add_unop (&ctx->builder, OVM_TYPED_INSTR(OVMI_CVT_F32,   OVM_TYPE_F64)); break;
        case 0xBC: ovm_code_builder_add_unop (&ctx->builder, OVM_TYPED_INSTR(OVMI_TRANSMUTE_F32, OVM_TYPE_I32)); break;
        case 0xBD: ovm_code_builder_add_unop (&ctx->builder, OVM_TYPED_INSTR(OVMI_TRANSMUTE_F64, OVM_TYPE_I64)); break;
        case 0xBE: ovm_code_builder_add_unop (&ctx->builder, OVM_TYPED_INSTR(OVMI_TRANSMUTE_I32, OVM_TYPE_F32)); break;
        case 0xBF: ovm_code_builder_add_unop (&ctx->builder, OVM_TYPED_INSTR(OVMI_TRANSMUTE_I64, OVM_TYPE_F64)); break;
        case 0xC0: ovm_code_builder_add_unop (&ctx->builder, OVM_TYPED_INSTR(OVMI_CVT_I8_S, OVM_TYPE_I32)); break;
        case 0xC1: ovm_code_builder_add_unop (&ctx->builder, OVM_TYPED_INSTR(OVMI_CVT_I16_S, OVM_TYPE_I32)); break;
        case 0xC2: ovm_code_builder_add_unop (&ctx->builder, OVM_TYPED_INSTR(OVMI_CVT_I8_S, OVM_TYPE_I64)); break;
        case 0xC3: ovm_code_builder_add_unop (&ctx->builder, OVM_TYPED_INSTR(OVMI_CVT_I16_S, OVM_TYPE_I64)); break;
        case 0xC4: ovm_code_builder_add_unop (&ctx->builder, OVM_TYPED_INSTR(OVMI_CVT_I32_S, OVM_TYPE_I64)); break;

        case 0xFC: parse_fc_instruction(ctx); break;
        case 0xFE: parse_fe_instruction(ctx); break;

        default: assert(0 && "UNHANDLED INSTRUCTION");
    }
}

static void parse_expression(build_context *ctx) {
    while (bh_arr_length(ctx->builder.label_stack) > 0) {
        parse_instruction(ctx);
    }
}

static void parse_code_section(build_context *ctx) {
    unsigned int section_size = uleb128_to_uint((u8 *)ctx->binary.data, (i32 *)&ctx->offset);
    unsigned int code_count = uleb128_to_uint((u8 *)ctx->binary.data, (i32 *)&ctx->offset);
    assert(ctx->module->functypes.size == code_count);

    ctx->module->memory_init_external_idx = ctx->next_external_func_idx++;

    // HACK HACK HACK THIS IS SUCH A BAD WAY OF DOING THIS
    ctx->module->memory_init_idx = bh_arr_length(ctx->program->funcs) + code_count;

    fori (i, 0, (int) code_count) {
        unsigned int code_size = uleb128_to_uint((u8 *)ctx->binary.data, (i32 *)&ctx->offset);
        unsigned int local_sections_count = uleb128_to_uint((u8 *)ctx->binary.data, (i32 *)&ctx->offset);

        unsigned int total_locals = 0;
        fori (j, 0, (int) local_sections_count) {
            unsigned int local_count = uleb128_to_uint((u8 *)ctx->binary.data, (i32 *)&ctx->offset);
            wasm_valkind_t valtype = parse_valtype(ctx);

            total_locals += local_count;
        }

        // Set up a lot of stuff...

        i32 func_idx = bh_arr_length(ctx->program->funcs);
        i32 param_count  = ctx->module->functypes.data[i]->type.func.params.size;
        i32 result_count = ctx->module->functypes.data[i]->type.func.results.size;

        debug_info_builder_begin_func(&ctx->debug_builder, func_idx);

        ctx->builder = ovm_code_builder_new(ctx->program, &ctx->debug_builder, param_count, result_count, total_locals);
        ctx->builder.func_table_arr_idx = ctx->func_table_arr_idx;

        ovm_code_builder_push_label_target(&ctx->builder, label_kind_func);
        parse_expression(ctx);
        ovm_code_builder_add_return(&ctx->builder);

        char *func_name = bh_aprintf(bh_heap_allocator(), "wasm_loaded_%d", func_idx);
        ovm_program_register_func(ctx->program, func_name, ctx->builder.start_instr, ctx->builder.param_count, ctx->builder.highest_value_number + 1);

        ovm_code_builder_free(&ctx->builder);
        debug_info_builder_end_func(&ctx->debug_builder);
    }

    ovm_program_register_external_func(ctx->program, "__internal_wasm_memory_init", 4, ctx->module->memory_init_external_idx);
}


static void parse_section(build_context *ctx) {
    char section_number = CONSUME_BYTE(ctx);

    switch (section_number) {
        case WASM_CUSTOM_SECTION: parse_custom_section(ctx);     break;
        case WASM_TYPE_SECTION:   parse_type_section(ctx);       break;
        case WASM_IMPORT_SECTION: parse_import_section(ctx);     break;
        case WASM_FUNC_SECTION:   parse_func_section(ctx);       break;
        case WASM_TABLE_SECTION:  parse_table_section(ctx);      break;
        case WASM_MEMORY_SECTION: parse_memory_section(ctx);     break;
        case WASM_GLOBAL_SECTION: parse_global_section(ctx);     break;
        case WASM_EXPORT_SECTION: parse_export_section(ctx);     break;
        case WASM_START_SECTION:  parse_start_section(ctx);      break;
        case WASM_ELEM_SECTION:   parse_elem_section(ctx);       break;
        case WASM_CODE_SECTION:   parse_code_section(ctx);       break;
        case WASM_DATA_SECTION:   parse_data_section(ctx);       break;
        case WASM_DATAC_SECTION:  parse_data_count_section(ctx); break;
        default: assert(0 && "bad section number"); break;
    }
}
