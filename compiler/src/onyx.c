
    if (context->options->verbose_output > 0) {
        bh_printf("Mapped folders:\n");
        bh_arr_each(bh_mapped_folder, p, context->options->mapped_folders) {
            bh_printf("\t%s: %s\n", p->name, p->folder);
        }
        bh_printf("\n");
    }


    if (context->options->verbose_output == 2)
        bh_printf("Processing source file:    %s (%d bytes)\n", file.filename, fc.length);

    static char verbose_output_buffer[512];
    if (context->options->verbose_output == 3) {
        if (ent->expr && ent->expr->token)
            snprintf(verbose_output_buffer, 511,
                    "%20s | %24s (%d, %d) | %5d | %s:%i:%i \n",
                   entity_state_strings[ent->state],
                   entity_type_strings[ent->type],
                   (u32) ent->macro_attempts,
                   (u32) ent->micro_attempts,
                   ent->id,
                   ent->expr->token->pos.filename,
                   ent->expr->token->pos.line,
                   ent->expr->token->pos.column);

        else if (ent->expr)
            snprintf(verbose_output_buffer, 511,
                    "%20s | %24s (%d, %d) \n",
                   entity_state_strings[ent->state],
                   entity_type_strings[ent->type],
                   (u32) ent->macro_attempts,
                   (u32) ent->micro_attempts);
    }

    b32 changed = ent->state != before_state;
    if (context->options->verbose_output == 3) {
        if (changed) printf("SUCCESS to %20s | %s", entity_state_strings[ent->state], verbose_output_buffer);
        else         printf("YIELD   to %20s | %s", entity_state_strings[ent->state], verbose_output_buffer);
    }

    return changed;

// Just having fun with some visual output - brendanfh 2020/12/14
#if defined(_BH_LINUX) || defined(_BH_DARWIN)
static void output_dummy_progress_bar(Context *context) {
    EntityHeap* eh = &context->entities;
    if (bh_arr_length(eh->entities) == 0) return;

    static const char* state_colors[] = {
        "\e[91m", "\e[93m", "\e[94m", "\e[93m",
        "\e[97m", "\e[95m", "\e[96m", "\e[92m", "\e[91m",
    };

    printf("\e[2;1H");

    for (i32 i = 0; i < Entity_State_Count - 1; i++) {
        if (i % 2 == 0) printf("\n");
        printf("%s %25s \xe2\x96\x88 ", state_colors[i], entity_state_strings[i]);
    }

    printf("\n\n");

    for (i32 i = 0; i < Entity_Type_Count; i++) {
        if      (eh->type_count[i] == 0)           printf("\e[90m");
        else if ((i32) eh->entities[0]->type == i) printf("\e[92m");
        else                                       printf("\e[97m");

        printf("%25s (%4d) | ", entity_type_strings[i], eh->type_count[i]);

        printf("\e[0K");
        for (i32 j = 0; j < Entity_State_Count; j++) {
            if (eh->all_count[j][i] == 0) continue;

            printf("%s", state_colors[j]);

            i32 count = (eh->all_count[j][i] >> 5) + 1;
            for (i32 c = 0; c < count * 2; c++) printf("\xe2\x96\x88");

            printf("\e[0m");
        }
        printf("\n");
    }
}
#endif


    if (context->options->fun_output)
        printf("\e[2J");

#if defined(_BH_LINUX) || defined(_BH_DARWIN)
        if (context->options->fun_output) {
            output_dummy_progress_bar(context);

            if (ent->expr->token) {
                OnyxFilePos pos = ent->expr->token->pos;
                printf("\e[0K%s on %s in %s:%d:%d\n", entity_state_strings[ent->state], entity_type_strings[ent->type], pos.filename, pos.line, pos.column);
            }

            // Slowing things down for the effect
            usleep(1000);
        }
#endif

        /*
        struct timespec spec;
        clock_gettime(CLOCK_REALTIME, &spec);
        u64 nano_time = spec.tv_nsec + 1000000000 * (spec.tv_sec % 100);
        printf("%lu %d %d %d %d %d %d %d\n",
                nano_time,
                bh_arr_length(context->entities.entities),
                context->entities.state_count[Entity_State_Introduce_Symbols],
                context->entities.state_count[Entity_State_Parse],
                context->entities.state_count[Entity_State_Resolve_Symbols],
                context->entities.state_count[Entity_State_Check_Types],
                context->entities.state_count[Entity_State_Code_Gen],
                context->entities.state_count[Entity_State_Finalized]);
        */

    u64 duration = bh_time_duration(start_time);

    if (context->options->verbose_output > 0) {
        printf("Type table size: %d bytes\n", context->wasm_module->type_info_size);
    }

    if (context->options->running_perf) {
        fori (i, 0, Entity_State_Count) {
            printf("| %27s | %10llu us |\n", entity_state_strings[i], context->stats.microseconds_per_state[i]);
        }
        printf("\n");
        fori (i, 0, Entity_Type_Count) {
            printf("| %27s | %10llu us |\n", entity_type_strings[i], context->stats.microseconds_per_type[i]);
        }
        printf("\n");
    }

