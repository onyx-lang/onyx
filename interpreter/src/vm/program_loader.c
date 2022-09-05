#include "vm.h"

//
// I'm very lazy and silly, so this code make the drastic assumption that the
// endianness of the machine that the file was built on and the machine the
// VM is running on are the SAME. If this is not the case, the integers very
// well could be f-ed up. Might be worth switching all integers to use ntohl,
// so at least that part is consistent...               -brendanfh 06/13/2022
//

//
// I wish this didn't take the engine as a parameter... but currently the memory
// is stored on the engine. So unless the data section elements can be aggregated
// into an array to be applied later, this is best I got...
//
// ALSO, I wish this didn't have to take a state... but because native_funcs are stored
// on the state, this is also the best I got...
bool ovm_program_load_from_file(ovm_program_t *program, ovm_engine_t *engine, char *filename) {
    bh_file file;
    bh_file_error error = bh_file_open(&file, filename);
    if (error != BH_FILE_ERROR_NONE) {
        fprintf(stderr, "Failed to open '%s' for reading.\n", filename);
        return false;
    }

    char magic[4], version[4];
    bh_file_read(&file, magic, 4);
    bh_file_read(&file, version, 4);

    if (strncmp(magic, "OVMI", 4) || strncmp(version, "\x00\x00\x00\x01", 4)) {
        fprintf(stderr, "Mismatched version/magic number in '%s'.\n", filename);
        return false;
    }

    //
    // Code section
    // Just copy in the bytes directly.
    // What's validation anyway?
    //
    i32 entry_count;
    bh_file_read(&file, &entry_count, sizeof(i32));
    bh_arr_insert_end(program->code, entry_count);
    bh_file_read(&file, program->code, entry_count * sizeof(ovm_instr_t));

    //
    // Data section
    //
    bh_file_read(&file, &entry_count, sizeof(i32));
    fori (i, 0, entry_count) {
        i32 offset, size;
        bh_file_read(&file, &offset, sizeof(i32));
        bh_file_read(&file, &size, sizeof(i32));

        assert(engine);
        assert(engine->memory);
        bh_file_read(&file, ((u8 *) engine->memory) + offset, size);
    }

    //
    // Native link section
    //
    i32 next_external_func_idx = 0;
    bh_file_read(&file, &entry_count, sizeof(i32));
    fori (i, 0, entry_count) {
        i32 param_count, name_len;
        bh_file_read(&file, &param_count, sizeof(i32));
        bh_file_read(&file, &name_len, sizeof(i32));

        char *name_buf = bh_alloc_array(program->store->arena_allocator, char, name_len);
        bh_file_read(&file, name_buf, name_len);

        ovm_program_register_external_func(program, name_buf, param_count, next_external_func_idx++);
    }

    //
    // Func section
    //
    bh_file_read(&file, &entry_count, sizeof(i32));
    fori (i, 0, entry_count) {
        i32 start_instr, param_count, value_number_count;
        bh_file_read(&file, &start_instr, sizeof(i32));
        bh_file_read(&file, &param_count, sizeof(i32));
        bh_file_read(&file, &value_number_count, sizeof(i32));

        ovm_program_register_func(program, "LOADED", start_instr, param_count, value_number_count);
    }

    //
    // Register section
    //
    bh_file_read(&file, &entry_count, sizeof(i32));
    fori (i, 0, entry_count) {
        i32 param_count, name_len;
        bh_file_read(&file, &param_count, sizeof(i32));
        bh_file_read(&file, &name_len, sizeof(i32));

        char *name_buf = bh_alloc_array(program->store->arena_allocator, char, name_len);
        bh_file_read(&file, name_buf, name_len);

        // For now, these are just ignored...
    }
    program->register_count = entry_count;

    return true;
}

void ovm_state_link_external_funcs(ovm_program_t *program, ovm_state_t *state, ovm_linkable_func_t *funcs) {
    bh_arr_each(ovm_func_t, f, program->funcs) {
        if (f->kind == OVM_FUNC_INTERNAL) continue;
        
        ovm_linkable_func_t *func = funcs;
        while (func->name) {
            if (!strcmp(f->name, func->name) && f->param_count == func->param_count) {
                ovm_state_register_external_func(state, f->external_func_idx, func->func.native_func, func->func.userdata);
                break;
            }

            func++;
        }

        if (!state->external_funcs[f->external_func_idx].native_func) {
            fprintf(stderr, "Failed to link to native function '%s'.\n", f->name);
            exit(1);
        }
    }
}
