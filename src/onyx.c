// #define BH_DEBUG
#define BH_DEFINE
#include "bh.h"

#include "onyxlex.h"
#include "onyxmsgs.h"
#include "onyxparser.h"
#include "onyxsempass.h"
#include "onyxutils.h"
#include "onyxwasm.h"

#define VERSION "0.1"

static const char* docstring = "Onyx compiler version " VERSION "\n"
    "\n"
    "The standard compiler for the Onyx programming language.\n"
    "\n"
    " $ onyx [-o <target file>] [--help] <input files>\n"
    "\n"
    "   -o <target_file>        Specify the target file\n"
    "   --help                  Print this help message\n";

typedef enum CompileAction {
    ONYX_COMPILE_ACTION_COMPILE,
    ONYX_COMPILE_ACTION_CHECK_ERRORS,
    ONYX_COMPILE_ACTION_PRINT_HELP,
} CompileAction;

typedef struct OnyxCompileOptions {
    bh_allocator allocator;
    CompileAction action;

    bh_arr(const char *) files;
    const char* target_file;
} OnyxCompileOptions;

typedef enum CompilerProgress {
    ONYX_COMPILER_PROGRESS_FAILED_READ,
    ONYX_COMPILER_PROGRESS_FAILED_PARSE,
    ONYX_COMPILER_PROGRESS_FAILED_SEMPASS,
    ONYX_COMPILER_PROGRESS_FAILED_BINARY_GEN,
    ONYX_COMPILER_PROGRESS_FAILED_OUTPUT,
    ONYX_COMPILER_PROGRESS_SUCCESS
} CompilerProgress;

typedef struct CompilerState {
    bh_arena ast_arena, msg_arena, sp_arena;
    bh_allocator token_alloc, ast_alloc, msg_alloc, sp_alloc;
    bh_table(bh_file_contents) loaded_files;

    OnyxMessages msgs;
    OnyxWasmModule wasm_mod;
} CompilerState;

static OnyxCompileOptions compile_opts_parse(bh_allocator alloc, int argc, char *argv[]) {
    OnyxCompileOptions options = {
        .allocator = alloc,
        .action = ONYX_COMPILE_ACTION_PRINT_HELP,

        .files = NULL,
        .target_file = "out.wasm",
    };

    bh_arr_new(alloc, options.files, 1);

    fori(i, 1, argc - 1) {
        if (!strcmp(argv[i], "--help")) {
            options.action = ONYX_COMPILE_ACTION_PRINT_HELP;
            break;
        }
        else if (!strcmp(argv[i], "-o")) {
            options.action = ONYX_COMPILE_ACTION_COMPILE;
            options.target_file = argv[++i];
        }
        else {
            options.action = ONYX_COMPILE_ACTION_COMPILE;
            bh_arr_push(options.files, argv[i]);
        }
    }

    return options;
}

void compile_opts_free(OnyxCompileOptions* opts) {
    bh_arr_free(opts->files);
}

OnyxAstNodeFile* parse_source_file(bh_file_contents* file_contents, CompilerState* compiler_state) {
    // NOTE: Maybe don't want to recreate the tokenizer and parser for every file
	OnyxTokenizer tokenizer = onyx_tokenizer_create(compiler_state->token_alloc, file_contents);
    bh_printf("Lexing  %s\n", file_contents->filename);
	onyx_lex_tokens(&tokenizer);

    bh_printf("Parsing %s\n", file_contents->filename);
	OnyxParser parser = onyx_parser_create(compiler_state->ast_alloc, &tokenizer, &compiler_state->msgs);
	return onyx_parse(&parser);
}

i32 onyx_compile(OnyxCompileOptions* opts, CompilerState* compiler_state) {

	bh_arena_init(&compiler_state->msg_arena, opts->allocator, 4096);
	compiler_state->msg_alloc = bh_arena_allocator(&compiler_state->msg_arena);

    onyx_message_create(compiler_state->msg_alloc, &compiler_state->msgs);

    compiler_state->token_alloc = opts->allocator;

	// NOTE: Create the arena where AST nodes will exist
	// Prevents nodes from being scattered across memory due to fragmentation
	bh_arena_init(&compiler_state->ast_arena, opts->allocator, 16 * 1024 * 1024); // 16MB
	compiler_state->ast_alloc = bh_arena_allocator(&compiler_state->ast_arena);

    bh_arena_init(&compiler_state->sp_arena, opts->allocator, 16 * 1024);
    compiler_state->sp_alloc = bh_arena_allocator(&compiler_state->sp_arena);

    bh_table_init(opts->allocator, compiler_state->loaded_files, 7);

    bh_arr_each(const char *, filename, opts->files) {
        bh_file file;

        bh_file_error err = bh_file_open(&file, *filename);
        if (err != BH_FILE_ERROR_NONE) {
            bh_printf_err("Failed to open file %s\n", *filename);
            return ONYX_COMPILER_PROGRESS_FAILED_READ;
        }

        bh_printf("Reading %s\n", file.filename);
        bh_file_contents fc = bh_file_read_contents(compiler_state->token_alloc, &file);
        bh_file_close(&file);

        bh_table_put(bh_file_contents, compiler_state->loaded_files, (char *) filename, fc);
    }

    OnyxAstNodeFile* root_file = NULL;
    OnyxAstNodeFile* prev_file = NULL;
    bh_table_each_start(bh_file_contents, compiler_state->loaded_files);
        OnyxAstNodeFile* file_node = parse_source_file(&value, compiler_state);

        if (!root_file) {
            root_file = file_node;
        }

        if (prev_file) {
            prev_file->next = file_node;
        }

        prev_file = file_node;
    bh_table_each_end;

    if (onyx_message_has_errors(&compiler_state->msgs)) {
        return ONYX_COMPILER_PROGRESS_FAILED_PARSE;
    }

    bh_printf("Checking semantics and types\n");
    OnyxSemPassState sp_state = onyx_sempass_create( compiler_state->sp_alloc, compiler_state->ast_alloc, &compiler_state->msgs);
    onyx_sempass(&sp_state, root_file);

    if (onyx_message_has_errors(&compiler_state->msgs)) {
        return ONYX_COMPILER_PROGRESS_FAILED_SEMPASS;
    }

    bh_printf("Creating WASM code\n");
    compiler_state->wasm_mod = onyx_wasm_module_create(opts->allocator);
    onyx_wasm_module_compile(&compiler_state->wasm_mod, root_file);

    if (onyx_message_has_errors(&compiler_state->msgs)) {
        return ONYX_COMPILER_PROGRESS_FAILED_BINARY_GEN;
    }

    bh_file output_file;
    if (bh_file_create(&output_file, opts->target_file) != BH_FILE_ERROR_NONE) {
        return ONYX_COMPILER_PROGRESS_FAILED_OUTPUT;
    }

    bh_printf("Writing WASM to %s\n", output_file.filename);
    onyx_wasm_module_write_to_file(&compiler_state->wasm_mod, output_file);

    return ONYX_COMPILER_PROGRESS_SUCCESS;
}

void compiler_state_free(CompilerState* cs) {
    // NOTE: There is a memory leak here because the token's aren't freed

    bh_arena_free(&cs->ast_arena);
    bh_arena_free(&cs->msg_arena);
    bh_arena_free(&cs->sp_arena);
    bh_table_free(cs->loaded_files);
    onyx_wasm_module_free(&cs->wasm_mod);
}

int main(int argc, char *argv[]) {

	bh_allocator alloc = bh_heap_allocator();

    bh_scratch_init(&global_scratch, alloc, 16 * 1024); // NOTE: 16 KB
    global_scratch_allocator = bh_scratch_allocator(&global_scratch);

    OnyxCompileOptions compile_opts = compile_opts_parse(alloc, argc, argv);
    CompilerState compile_state = {
        .wasm_mod = { 0 }
    };

    CompilerProgress compiler_progress = ONYX_COMPILER_PROGRESS_FAILED_READ;

    switch (compile_opts.action) {
        case ONYX_COMPILE_ACTION_PRINT_HELP:
            // NOTE: This could probably be made better
            bh_printf(docstring);
            return 1;

        case ONYX_COMPILE_ACTION_COMPILE:
            compiler_progress = onyx_compile(&compile_opts, &compile_state);

            break;

        default: break;
    }

    switch (compiler_progress) {
        case ONYX_COMPILER_PROGRESS_FAILED_READ:
            // NOTE: Do nothing since it was already printed above
            break;

        case ONYX_COMPILER_PROGRESS_FAILED_PARSE:
        case ONYX_COMPILER_PROGRESS_FAILED_SEMPASS:
        case ONYX_COMPILER_PROGRESS_FAILED_BINARY_GEN:
            onyx_message_print(&compile_state.msgs);
            break;

        case ONYX_COMPILER_PROGRESS_FAILED_OUTPUT:
            bh_printf_err("Failed to open file for writing: '%s'\n", compile_opts.target_file);
            break;

        case ONYX_COMPILER_PROGRESS_SUCCESS:
            bh_printf("Successfully compiled to '%s'\n", compile_opts.target_file);
            break;
    }

    compiler_state_free(&compile_state);

	return compiler_progress == ONYX_COMPILER_PROGRESS_SUCCESS;
}

// NOTE: Old bits of code that may be useful again at some point.
#if 0
	bh_printf("There are %d tokens (Allocated space for %d tokens)\n", bh_arr_length(token_arr), bh_arr_capacity(token_arr));

	bh_arr_each(OnyxToken, it, token_arr) {
		onyx_token_null_toggle(*it);
		bh_printf("%s (%s:%l:%l)\n", onyx_get_token_type_name(it->type), it->pos.filename, it->pos.line, it->pos.column);
		onyx_token_null_toggle(*it);
	}
#endif

#if 0
	// NOTE: Ensure type table made correctly

	bh_printf("Type map:\n");
	bh_hash_each_start(i32, wasm_mod.type_map);
		bh_printf("%s -> %d\n", key, value);
	bh_hash_each_end;

	bh_printf("Type list:\n");
	WasmFuncType** func_type = wasm_mod.functypes;
	while (!bh_arr_end(wasm_mod.functypes, func_type)) {
		for (int p = 0; p < (*func_type)->param_count; p++) {
			bh_printf("%c ", (*func_type)->param_types[p]);
		}
		bh_printf("-> ");
		bh_printf("%c\n", (*func_type)->return_type);

		func_type++;
	}
#endif

#if 0
	// NOTE: Ensure the export table was built correctly

	bh_printf("Function types:\n");
	bh_arr_each(WasmFunc, func_it, wasm_mod.funcs) {
		bh_printf("%d\n", func_it->type_idx);
	}

	bh_printf("Exports:\n");
	bh_hash_each_start(WasmExport, wasm_mod.exports);
		bh_printf("%s: %d %d\n", key, value.kind, value.idx);
	bh_hash_each_end;
#endif
