typedef struct onyx_error_details_t {
    onyx_error_t rank;
    const char *message;
    const char *filename;
    int   line;
    int   column;
    int   length; 
    char *line_text;
} onyx_error_details_t;

static void print_error_text(const char *text, b32 color) {
    if (!color) {
        bh_printf("%s", text);
        return;
    }

    const char *ch = text;
    b32 in_color = 0;

    while (*ch != '\0') {
        if (*ch == '\'') {
            in_color = !in_color;
            if (in_color) bh_printf("\033[92m");
            else          bh_printf("\033[0m");
        } else {
            bh_printf("%c", *ch);
        }

        ch++;
    }
}

static void print_underline(onyx_error_details_t *err, i32 len, i32 first_non_whitespace, b32 colored_printing) {
    len = bh_min(len, 1024);
    char* pointer_str = alloca(sizeof(char) * len);
    memset(pointer_str, ' ', len);
    
    int c = err->column - 1;
    int l = err->length;

    memcpy(pointer_str, err->line_text, first_non_whitespace);
    memset(pointer_str + c + 1, '~', l - 1);
    pointer_str[c] = '^';
    pointer_str[c + l] = 0;

    if (colored_printing) bh_printf("\033[91m");
    bh_printf("%s\n", pointer_str);
    if (colored_printing) bh_printf("\033[0m\n");
}

static void print_detailed_message_v1(onyx_error_details_t *err, b32 colored_printing) {
    bh_printf("(%s:%l,%l) %s\n", err->filename, err->line, err->column, err->message);

    i32 linelength = 0;
    i32 first_char = 0;
    char* walker = err->line_text;
    while (*walker == ' ' || *walker == '\t') first_char++, linelength++, walker++;
    while (*walker && *walker != '\n') linelength++, walker++;

    if (colored_printing) bh_printf("\033[90m");
    i32 numlen = bh_printf(" %d | ", err->line);
    if (colored_printing) bh_printf("\033[94m");
    bh_printf("%b\n", err->line_text, linelength);
    
    fori (i, 0, numlen - 1) bh_printf(" ");
    print_underline(err, linelength, first_char, colored_printing);
}

static void print_detailed_message_v2(onyx_error_details_t* err, b32 colored_printing) {
    if (colored_printing) {
        switch (err->rank) {
            case ONYX_ERROR_WARNING:
                bh_printf("\033[93mwarning\033[0m: ");
                print_error_text(err->message, colored_printing);
                bh_printf("\n\033[90m     at: %s:%l,%l\033[0m\n", err->filename, err->line, err->column);
                break;

            default:
                bh_printf("\033[91merror\033[0m: ");
                print_error_text(err->message, colored_printing);
                bh_printf("\n\033[90m   at: %s:%l,%l\033[0m\n", err->filename, err->line, err->column);
                break;
        }
    } else {
        switch (err->rank) {
            case ONYX_ERROR_WARNING:
                bh_printf("warning: ");
                print_error_text(err->message, colored_printing);
                bh_printf("\n     at: %s:%l,%l\n", err->filename, err->line, err->column);
                break;

            default:
                bh_printf("error: ");
                print_error_text(err->message, colored_printing);
                bh_printf("\n   at: %s:%l,%l\n", err->filename, err->line, err->column);
                break;
        }
    }

    i32 linelength = 0;
    i32 first_char = 0;
    char* walker = err->line_text;
    while (*walker == ' ' || *walker == '\t') first_char++, linelength++, walker++;
    while (*walker && *walker != '\n') linelength++, walker++;

    char numbuf[32] = {0};
    i32 numlen = bh_snprintf(numbuf, 31, " %d | ", err->line);

    if (colored_printing) bh_printf("\033[90m");
    fori (i, 0, numlen - 3) bh_printf(" ");
    bh_printf("|\n%s", numbuf);
    if (colored_printing) bh_printf("\033[94m");

    bh_printf("%b\n", err->line_text, linelength);

    if (colored_printing) bh_printf("\033[90m");
    fori (i, 0, numlen - 3) bh_printf(" ");
    bh_printf("| ");
    if (colored_printing) bh_printf("\033[94m");

    print_underline(err, linelength, first_char, colored_printing);
}

static void print_detailed_message_json(onyx_error_details_t* err, b32 colored_printing) {
    bh_printf(
        "{\"rank\":%d,\"file\":\"%s\",\"line\":%d,\"column\":%d,\"length\":%d,\"msg\":\"%s\"}",
        err->rank,
        err->filename,
        err->line,
        err->column,
        err->length,
        err->message
    );
}

static void print_detailed_message(onyx_error_details_t* err, b32 colored_printing, char *error_format) {
    if (!err->filename) {
        // This makes the assumption that if a file is not specified for an error,
        // the error must have come from the command line.
        
        if (colored_printing) {
            bh_printf("\033[91merror\033[0m: ");
            bh_printf("%s\n", err->message);
            bh_printf("\033[90m   at: command line argument\033[0m\n");
        } else {
            bh_printf("error: ");
            bh_printf("%s\n", err->message);
            bh_printf("   at: command line argument\n");
        }

        return;
    }

    if (!strcmp(error_format, "v2")) {
        print_detailed_message_v2(err, colored_printing);
    }
    else if (!strcmp(error_format, "v1")) {
        print_detailed_message_v1(err, colored_printing);
    }
    else if (!strcmp(error_format, "json")) {
        print_detailed_message_json(err, colored_printing);
    }
    else {
        bh_printf("Unknown error format: '%s'.\n", error_format);
    }
}

void onyx_errors_print(onyx_context_t *ctx, char *error_format, b32 colored_printing, b32 show_all_errors) {
    b32 error_format_json = !strcmp(error_format, "json");
    if (error_format_json) bh_printf("[");

    onyx_error_t last_rank = onyx_error_rank(ctx, 0);
    fori (i, 0, onyx_error_count(ctx)) {
        onyx_error_details_t err;
        err.rank      = onyx_error_rank(ctx, i);
        err.message   = onyx_error_message(ctx, i);
        err.filename  = onyx_error_filename(ctx, i);
        err.line      = onyx_error_line(ctx, i);
        err.column    = onyx_error_column(ctx, i);
        err.length    = onyx_error_length(ctx, i);

        char line_text[256];
        onyx_error_line_text(ctx, i, line_text, 255);
        err.line_text = line_text;

        if (!show_all_errors && last_rank != err.rank) break;
        if (error_format_json && i != 0) bh_printf(",");

        print_detailed_message(&err, colored_printing, error_format);

        last_rank = err.rank;
    }

    if (error_format_json) bh_printf("]");
}

b32 onyx_errors_present(onyx_context_t *ctx) {
    fori (i, 0, onyx_error_count(ctx)) {
        onyx_error_details_t err;
        if (onyx_error_rank(ctx, i) >= ONYX_ERROR_WAITING) {
            return 1;
        }
    }

    return 0;
}

