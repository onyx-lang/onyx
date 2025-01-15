#define BH_NO_TABLE
#include "bh.h"

#include "onyx.h"
#include <stdio.h>
#include <stdlib.h>

#if defined(_BH_LINUX) || defined(_BH_DARWIN)
    #define C_NORM    "\e[0m"
    #define C_BOLD    "\e[1m"
    #define C_RED     "\e[91m"
    #define C_YELLOW  "\e[93m"
    #define C_GREY    "\e[90m"
    #define C_GREEN   "\e[33m"
    #define C_BLUE    "\e[34m"
    #define C_LBLUE   "\e[94m"
#else 
    #define C_NORM
    #define C_BOLD
    #define C_RED
    #define C_YELLOW
    #define C_GREY
    #define C_GREEN
    #define C_BLUE
    #define C_LBLUE
#endif

#define DOCSTRING_HEADER \
    "\n" \
    "The toolchain for the " C_BLUE C_BOLD "Onyx" C_NORM " programming language, created by Brendan Hansen.\n" \
    "Learn more at " C_BLUE "https://onyxlang.io" C_NORM ".\n" \
    "\n"

static const char* top_level_docstring = DOCSTRING_HEADER
    C_BOLD "Usage: " C_BLUE "onyx" C_LBLUE " <command> " C_NORM C_YELLOW "[..flags] " C_GREEN "[..args]\n" C_NORM
    "\n"
    C_BOLD "Commands:\n" C_NORM
    C_LBLUE "    help             " C_NORM "Shows this help message\n"
    C_LBLUE "    version          " C_NORM "Prints version information\n"
    "\n"
    C_LBLUE "    build " C_GREY "files      " C_NORM "Compiles an Onyx program into an executable " C_GREY "(onyx b)" C_NORM "\n"
    C_LBLUE "    check " C_GREY "files      " C_NORM "Checks syntax and types of a program\n"
#if defined(_BH_LINUX) || defined(_BH_DARWIN)
    C_LBLUE "    watch            " C_NORM "Continuously rebuilds a program on file changes\n"
#endif
    "\n"
#if defined(_BH_LINUX) || defined(_BH_DARWIN)
    C_LBLUE "    self-upgrade     " C_NORM "Upgrade your toolchain\n"
#endif
    "\n";

static const char *run_commands_docstring = 
    C_LBLUE "    run " C_GREY "files        " C_NORM "Compiles and runs an Onyx program " C_GREY "(onyx r)" C_NORM "\n"
#if (defined(_BH_LINUX) || defined(_BH_DARWIN))
    C_LBLUE "    run-watch        " C_NORM "Continuously rebuilds and runs a program on file changes " C_GREY "(onyx rw)" C_NORM "\n"
#endif
    "\n"
    C_LBLUE "    package " C_GREY "cmd      " C_NORM "Package manager " C_GREY "(onyx pkg cmd)" C_NORM "\n"
    C_LBLUE "    new              " C_NORM "Create a new project from a template\n"
    C_LBLUE "    init             " C_NORM "Initialize a project in the current directory\n"
    C_LBLUE "    add " C_GREY "package      " C_NORM "Add a package to dependency list " C_GREY "(onyx a)" C_NORM "\n"
    C_LBLUE "    remove " C_GREY "package   " C_NORM "Remove a package from dependency list " C_GREY "(onyx rm)" C_NORM "\n"
    C_LBLUE "    sync             " C_NORM "Synchronize installed packages\n"
    "\n";

static const char *build_docstring = DOCSTRING_HEADER
    C_BOLD "Usage: " C_BLUE "onyx" C_LBLUE " %s " C_NORM C_YELLOW "[..flags] " C_GREEN "files " C_NORM "%s" "\n"
    "\n"
    C_BOLD "Flags:\n" C_NORM
    C_LBLUE "    -o, --output " C_GREY "target_file    " C_NORM "Specify the target file " C_GREY "(default: out.wasm)\n"
    C_LBLUE "    -r, --runtime " C_GREY "runtime       " C_NORM "Specifies the runtime " C_GREY "(onyx, wasi, js, custom)\n"
    C_LBLUE "    --map-dir " C_GREY "name:folder       " C_NORM "Adds a mapped directory\n"
    "\n"
    C_LBLUE "    --debug                     " C_NORM "Output a debugable build\n"
    C_LBLUE "    --feature " C_GREY "feature           " C_NORM "Enable an experimental language feature\n"
    C_LBLUE "    --multi-threaded            " C_NORM "Enables multi-threading for this compilation\n"
    C_LBLUE "    --stack-trace               " C_NORM "Enable dynamic stack trace\n"
    C_LBLUE "    --wasm-mvp                  " C_NORM "Use only WebAssembly MVP features\n"
    "\n"
    C_LBLUE "    --no-core                   " C_NORM "Disable automatically including \"core/module\"\n"
    C_LBLUE "    --no-type-info              " C_NORM "Disables generating type information\n"
    C_LBLUE "    --generate-method-info      " C_NORM "Populate method information in type information structures\n"
    C_LBLUE "    --generate-foreign-info     " C_NORM "Generate information for foreign blocks\n"
    C_LBLUE "    --generate-name-section     " C_NORM "Generate the 'name' custom section for better debugging\n"
    C_LBLUE "    --no-stale-code             " C_NORM "Disables use of " C_YELLOW "#allow_stale_code" C_NORM " directive\n"
    "\n"
    C_LBLUE "    --doc                       " C_NORM "Generate a .odoc file, Onyx's documentation format used by " C_YELLOW "onyx-doc-gen\n"
    C_LBLUE "    --lspinfo " C_GREY "target_file       " C_NORM "Generate an LSP information file\n"
    "\n"
    C_LBLUE "    -V, --verbose               " C_NORM "Verbose output\n"
    C_LBLUE "    --no-colors                 " C_NORM "Disables colors in the error message\n"
    C_LBLUE "    --error-format " C_GREY "(v1|v2)      " C_NORM "Changes the output error format\n"
    C_LBLUE "    --show-all-errors           " C_NORM "Print all errors\n"
    C_LBLUE "    --print-function-mappings   " C_NORM "Prints a mapping from WASM function index to source location\n"
    C_LBLUE "    --print-static-if-results   " C_NORM "Prints the conditional result of every " C_YELLOW "#if" C_NORM " statement\n"
    "\n"
    C_LBLUE "    --no-file-contents          " C_NORM "Disables " C_YELLOW "#file_contents" C_NORM " for security\n"
    C_LBLUE "    --no-compiler-extensions    " C_NORM "Disables " C_YELLOW "#compiler_extension" C_NORM " for security\n"
    "\n";

static const char *self_upgrade_docstring = DOCSTRING_HEADER
    C_BOLD "Usage: " C_BLUE "onyx" C_LBLUE " self-upgrade " C_GREEN "[version]\n" C_NORM
    "\n"
    C_BOLD "Arguments:\n" C_NORM
    C_GREEN "    version      " C_NORM "Specify which version to install. Defaults to latest\n";


static b32 is_flag(char *s) {
    if (!s) return 0;
    return s[0] == '-';
}


typedef enum CompileAction CompileAction;
enum CompileAction {
    ONYX_COMPILE_ACTION_COMPILE,
    ONYX_COMPILE_ACTION_CHECK,
    ONYX_COMPILE_ACTION_RUN,
    ONYX_COMPILE_ACTION_RUN_WASM,
    ONYX_COMPILE_ACTION_WATCH,
    ONYX_COMPILE_ACTION_WATCH_RUN,
    ONYX_COMPILE_ACTION_DOCUMENT,
    ONYX_COMPILE_ACTION_PACKAGE,
    ONYX_COMPILE_ACTION_PRINT_HELP,
    ONYX_COMPILE_ACTION_PRINT_VERSION,
    ONYX_COMPILE_ACTION_SELF_UPGRADE,
};

typedef struct CLIArgs {
    CompileAction action;

    u32 verbose_output          : 2;
    b32 fun_output              : 1;
    b32 print_function_mappings : 1;
    b32 print_static_if_results : 1;
    b32 debug_session           : 1;
    b32 no_colors               : 1;
    b32 show_all_errors         : 1;

    i32    passthrough_argument_count;
    char** passthrough_argument_data;

    const char* target_file;
    const char* symbol_info_file;
    const char* help_subcommand;

    char *error_format;
    char *debug_socket;
    char *core_installation;
    char *upgrade_version;
} CLIArgs;

#include "./error_printing.h"

static void print_subcommand_help(const char *subcommand);

static int32_t cli_args_init(CLIArgs *cli_args) {
    memset(cli_args, 0, sizeof(* cli_args));

    cli_args->target_file = "out.wasm";
    cli_args->error_format = "v2";

    #if defined(_BH_LINUX) || defined(_BH_DARWIN)
    cli_args->no_colors = 0;
    cli_args->core_installation = getenv("ONYX_PATH");

    if (getenv("ONYX_ERROR_FORMAT")) {
        cli_args->error_format = getenv("ONYX_ERROR_FORMAT");
    }
    #endif

    #ifdef _BH_WINDOWS
    cli_args->no_colors = 1;

    bh_allocator alloc = bh_heap_allocator();
    char *tmp_core_installation = bh_alloc_array(alloc, u8, 512);
    char *tmp_error_format      = bh_alloc_array(alloc, u8, 512);

    if (GetEnvironmentVariableA("ONYX_PATH", tmp_core_installation, 512) > 0) {
        cli_args->core_installation = tmp_core_installation;
    }
    if (GetEnvironmentVariableA("ONYX_ERROR_FORMAT", tmp_error_format, 512) > 0) {
        cli_args->error_format = tmp_error_format;
    }
    #endif

    if (cli_args->core_installation == NULL) {
        bh_printf(C_RED "error" C_NORM ": ONYX_PATH environment variable is not set. Please set this to the location of your Onyx installation.\n");
        return 0;
    }
    
    return 1;
}

static int32_t cli_determine_action(CLIArgs *cli_args, int *first_sub_arg, int argc, char *argv[]) {
    if (argc == 1 || is_flag(argv[1])) {
        cli_args->action = ONYX_COMPILE_ACTION_PRINT_HELP;
        return 0;
    }

    bh_allocator allocator = bh_heap_allocator();
    cli_args->help_subcommand = argc > 1 ? argv[1] : NULL;

    if (!strcmp(argv[1], "help")) {
        cli_args->action = ONYX_COMPILE_ACTION_PRINT_HELP;
        cli_args->help_subcommand = argc > 2 ? argv[2] : NULL;
        *first_sub_arg = 1;
        return 1;
    }

    if (!strcmp(argv[1], "version")) {
        cli_args->action = ONYX_COMPILE_ACTION_PRINT_VERSION;
        *first_sub_arg = 1;
        return 1;
    }

    if (!strcmp(argv[1], "compile") || !strcmp(argv[1], "build") || !strcmp(argv[1], "b")) {
        cli_args->action = ONYX_COMPILE_ACTION_COMPILE;
        *first_sub_arg = 2;
        return 1;
    }

    if (!strcmp(argv[1], "check")) {
        cli_args->action = ONYX_COMPILE_ACTION_CHECK;
        *first_sub_arg = 2;
        return 1;
    }

    if (!strcmp(argv[1], "pkg") || !strcmp(argv[1], "package")) {
        // Maybe we should consider caching the package WASM file so it doesn't need to be recompiled
        // every time? Compilation is very fast, but it would be even snappier if the whole package
        // manager didn't need to compile every time.

        cli_args->action = ONYX_COMPILE_ACTION_PACKAGE;
        cli_args->passthrough_argument_count = argc - 2;
        cli_args->passthrough_argument_data  = &argv[2];
        *first_sub_arg = argc;
        return 1;
    }

    if (!strcmp(argv[1], "add") || !strcmp(argv[1], "a")) {
        argv[1] = "add";

        cli_args->action = ONYX_COMPILE_ACTION_PACKAGE;
        cli_args->passthrough_argument_count = argc - 1;
        cli_args->passthrough_argument_data  = &argv[1];
        *first_sub_arg = argc;
        return 1;
    }

    if (!strcmp(argv[1], "remove") || !strcmp(argv[1], "rm")) {
        argv[1] = "remove";

        cli_args->action = ONYX_COMPILE_ACTION_PACKAGE;
        cli_args->passthrough_argument_count = argc - 1;
        cli_args->passthrough_argument_data  = &argv[1];
        *first_sub_arg = argc;
        return 1;
    }

    if (!strcmp(argv[1], "sync") || !strcmp(argv[1], "init") || !strcmp(argv[1], "new")) {
        cli_args->action = ONYX_COMPILE_ACTION_PACKAGE;
        cli_args->passthrough_argument_count = argc - 1;
        cli_args->passthrough_argument_data  = &argv[1];
        *first_sub_arg = argc;
        return 1;
    }

    int32_t has_runtime = strcmp(onyx_version_runtime(), "none") != 0;

    if (has_runtime) {
        if (!strcmp(argv[1], "run") || !strcmp(argv[1], "r")) {
            cli_args->action = ONYX_COMPILE_ACTION_RUN;
            *first_sub_arg = 2;
            return 1;
        }
    }

    #if defined(_BH_LINUX) || defined(_BH_DARWIN)
    if (!strcmp(argv[1], "watch")) {
        cli_args->action = ONYX_COMPILE_ACTION_WATCH;
        *first_sub_arg = 2;
        return 1;
    }
    #endif

    #if (defined(_BH_LINUX) || defined(_BH_DARWIN))
    if (has_runtime) {
        if (!strcmp(argv[1], "run-watch") || !strcmp(argv[1], "rw")) {
            cli_args->action = ONYX_COMPILE_ACTION_WATCH_RUN;
            *first_sub_arg = 2;
            return 1;
        }
    }
    #endif

    #if defined(_BH_LINUX) || defined(_BH_DARWIN)
    if (!strcmp(argv[1], "self-upgrade")) {
        cli_args->action = ONYX_COMPILE_ACTION_SELF_UPGRADE;
        *first_sub_arg = 2;
        return 1;
    }
    #endif


    // First try `./.onyx` for the executable.
    char *script_filename = bh_aprintf(allocator, "./.onyx/%s.wasm", argv[1]);

    // If that doesn't exist, then try the core installation.
    if (!bh_file_exists(script_filename)) {
        script_filename = bh_aprintf(allocator, "%s/tools/%s.wasm", cli_args->core_installation, argv[1]);
    }

    if (bh_file_exists(script_filename)) {
        cli_args->action = ONYX_COMPILE_ACTION_RUN_WASM;
        cli_args->target_file = script_filename;

        cli_args->passthrough_argument_count = argc - 2;
        cli_args->passthrough_argument_data  = &argv[2];
        return 1;
    }

    return 0;
}

static int32_t cli_parse_compilation_options(CLIArgs *cli_args, onyx_context_t *ctx, int arg_parse_start, int argc, char **argv) {
    b32 using_onyx_runtime = 1;

    fori(i, arg_parse_start, argc) {
        arg_parse_start = i;

        if (!is_flag(argv[i])) {
            // On the first non-flag argument, break to add the files.
            break;
        }

        if (!strcmp(argv[i], "-o") || !strcmp(argv[i], "--output")) {
            cli_args->target_file = argv[++i]; // :InCli
        }
        else if (!strcmp(argv[i], "--verbose") || !strcmp(argv[i], "-V")) {
            cli_args->verbose_output = 1; // :InCli
        }
        else if (!strcmp(argv[i], "--help")) {
            print_subcommand_help(argv[1]);
            return 0;
        }
        else if (!strcmp(argv[i], "-VV")) {
            cli_args->verbose_output = 2; // :InCli
        }
        else if (!strcmp(argv[i], "-VVV")) {
            cli_args->verbose_output = 3; // :InCli
        }
        else if (!strcmp(argv[i], "--print-function-mappings")) {
            cli_args->print_function_mappings = 1; // :InCli
        }
        else if (!strcmp(argv[i], "--print-static-if-results")) {
            cli_args->print_static_if_results = 1; // :InCli
        }
        else if (!strcmp(argv[i], "--no-colors")) {
            cli_args->no_colors = 1; // :InCli
        }
        else if (!strcmp(argv[i], "--no-file-contents")) {
            onyx_set_option_int(ctx, ONYX_OPTION_DISABLE_FILE_CONTENTS, 1);
        }
        else if (!strcmp(argv[i], "--no-compiler-extensions")) {
            onyx_set_option_int(ctx, ONYX_OPTION_DISABLE_EXTENSIONS, 1);
        }
        else if (!strcmp(argv[i], "--wasm-mvp")) {
            onyx_set_option_int(ctx, ONYX_OPTION_POST_MVP_FEATURES, 0);
        }
        else if (!strcmp(argv[i], "--multi-threaded")) {
            onyx_set_option_int(ctx, ONYX_OPTION_MULTI_THREADING, 1);
        }
        else if (!strcmp(argv[i], "--generate-foreign-info")) {
            onyx_set_option_int(ctx, ONYX_OPTION_GENERATE_FOREIGN_INFO, 1);
        }
        else if (!strcmp(argv[i], "--generate-method-info")) {
            onyx_set_option_int(ctx, ONYX_OPTION_GENERATE_METHOD_INFO, 1);
        }
        else if (!strcmp(argv[i], "--generate-name-section")) {
            onyx_set_option_int(ctx, ONYX_OPTION_GENERATE_NAME_SECTION, 1);
        }
        else if (!strcmp(argv[i], "--no-type-info")) {
            onyx_set_option_int(ctx, ONYX_OPTION_GENERATE_TYPE_INFO, 1);
        }
        else if (!strcmp(argv[i], "--no-core")) {
            onyx_set_option_int(ctx, ONYX_OPTION_DISABLE_CORE, 1);
        }
        else if (!strcmp(argv[i], "--no-stale-code")) {
            onyx_set_option_int(ctx, ONYX_OPTION_DISABLE_STALE_CODE, 1);
        }
        else if (!strcmp(argv[i], "--show-all-errors")) {
            cli_args->show_all_errors = 1; // :InCli
        }
        else if (!strcmp(argv[i], "--error-format")) {
            cli_args->error_format = argv[++i]; // :InCli
        }
        else if (!strcmp(argv[i], "--feature")) {
            char *next_arg = argv[++i];
            if (!strcmp(next_arg, "optional-semicolons")) {
                onyx_set_option_int(ctx, ONYX_OPTION_OPTIONAL_SEMICOLONS, 1);
            }
        }
        else if (!strcmp(argv[i], "--map-dir")) {
            char *arg = argv[++i];
            int len = strnlen(arg, 256);

            char *name = arg;
            char *folder = NULL;
            fori (i, 0, len) if (arg[i] == ':') {
                arg[i] = '\0';
                folder = &arg[i + 1];
            }

            onyx_add_mapped_dir(ctx, name, -1, folder, -1);
        }
        else if (!strncmp(argv[i], "-D", 2)) {
            i32 len = strlen(argv[i]);
            i32 j=2;
            while (argv[i][j] != '=' && j < len) j++;

            char    *key = argv[i] + 2;
            int32_t  key_len = j - 2;
            char    *value = argv[i] + j + 1;
            int32_t  value_len = len - j - 1;

            if (value_len <= 0) {
                value = "true";
                value_len = 4;
            }

            onyx_add_defined_var(ctx, key, key_len, value, value_len);
        }
        else if (!strcmp(argv[i], "-r") || !strcmp(argv[i], "--runtime")) {
            using_onyx_runtime = 0;
            i += 1;
            if (!strcmp(argv[i], "onyx")) using_onyx_runtime = 1;
            else if (!strcmp(argv[i], "wasi"))   onyx_set_option_int(ctx, ONYX_OPTION_PLATFORM, ONYX_PLATFORM_WASI);
            else if (!strcmp(argv[i], "js"))     onyx_set_option_int(ctx, ONYX_OPTION_PLATFORM, ONYX_PLATFORM_JS);
            else if (!strcmp(argv[i], "custom")) onyx_set_option_int(ctx, ONYX_OPTION_PLATFORM, ONYX_PLATFORM_CUSTOM);
            else {
                bh_printf(C_YELLOW "warning" C_NORM ": '%s' is not a valid runtime. Defaulting to 'onyx'.\n", argv[i]);
                using_onyx_runtime = 1;
            }
        }
        else if (!strcmp(argv[i], "--doc")) {
            onyx_set_option_int(ctx, ONYX_OPTION_GENERATE_DOC_INFO, 1);
        }
        else if (!strcmp(argv[i], "--syminfo")) {
            onyx_set_option_int(ctx, ONYX_OPTION_GENERATE_SYMBOL_INFO, 1);
            cli_args->symbol_info_file = argv[++i];
        }
        else if (!strcmp(argv[i], "--lspinfo")) {
            onyx_set_option_int(ctx, ONYX_OPTION_GENERATE_SYMBOL_INFO, 1);
            onyx_set_option_int(ctx, ONYX_OPTION_GENERATE_LSP_INFO, 1);
            cli_args->symbol_info_file = argv[++i];
        }
        else if (!strcmp(argv[i], "--debug")) {
            cli_args->debug_session = 1;
            onyx_set_option_int(ctx, ONYX_OPTION_GENERATE_DEBUG_INFO, 1);
            onyx_set_option_int(ctx, ONYX_OPTION_GENERATE_STACK_TRACE, 1);
            onyx_set_option_int(ctx, ONYX_OPTION_GENERATE_NAME_SECTION, 1);
        }
        else if (!strcmp(argv[i], "--debug-socket")) {
            cli_args->debug_session = 1;
            cli_args->debug_socket = argv[++i]; // :InCli
        }
        else if (!strcmp(argv[i], "--debug-info")) {
            onyx_set_option_int(ctx, ONYX_OPTION_GENERATE_DEBUG_INFO, 1);
            onyx_set_option_int(ctx, ONYX_OPTION_GENERATE_STACK_TRACE, 1);
            onyx_set_option_int(ctx, ONYX_OPTION_GENERATE_NAME_SECTION, 1);
        }
        else if (!strcmp(argv[i], "--stack-trace")) {
            onyx_set_option_int(ctx, ONYX_OPTION_GENERATE_STACK_TRACE, 1);
            onyx_set_option_int(ctx, ONYX_OPTION_GENERATE_NAME_SECTION, 1);
        }
        else if (!strcmp(argv[i], "--perf")) {
            onyx_set_option_int(ctx, ONYX_OPTION_COLLECT_PERF, 1);
        }
#if defined(_BH_LINUX) || defined(_BH_DARWIN)
        // NOTE: Fun output is only enabled for Linux because Windows command line
        // is not ANSI compatible and for a silly feature, I don't want to learn
        // how to properly do arbitrary graphics in it.
        else if (!strcmp(argv[i], "--fun") || !strcmp(argv[i], "-F")) {
            cli_args->fun_output = 1; // :InCli
        }
#endif
        else {
            bh_printf(C_RED "error" C_NORM ": Unknown flag '%s'.\n", argv[i]);
            print_subcommand_help(argv[1]);
            return 0;
        }
    }

    int32_t at_least_one_file = 0;
    
    fori (i, arg_parse_start, argc) {
        if (is_flag(argv[i])) {
            if (!strcmp(argv[i], "--")) {
                cli_args->passthrough_argument_count = argc - i - 1;
                cli_args->passthrough_argument_data  = &argv[i + 1];
                break;
            }

            // FLAG AFTER SOURCE FILES
            bh_printf(C_RED  "error" C_NORM ": Flag provided after start of source files.\n");
            bh_printf(C_GREY " hint: Try moving the flag '%s' to be earlier in your command.\n" C_NORM, argv[i]);
            return 0;
        }

        if (bh_str_ends_with(argv[i], ".wasm") && cli_args->action == ONYX_COMPILE_ACTION_RUN) {
            if (at_least_one_file) {
                bh_printf(C_RED "error" C_NORM ": Expected only one '.wasm', or multiple '.onyx' files to be given.\n");
                return 0;
            }
            cli_args->action = ONYX_COMPILE_ACTION_RUN_WASM;
            cli_args->target_file = argv[i];

            cli_args->passthrough_argument_count = argc - i - 1;
            cli_args->passthrough_argument_data  = &argv[i + 1];

            at_least_one_file = 1;
            break;
        }

        at_least_one_file = 1;
        onyx_include_file(ctx, argv[i], -1);
    }

    if (!at_least_one_file) {
        bh_printf(C_RED "error" C_NORM ": No files were provided.\n");
        print_subcommand_help(argv[1]);
        return 0;
    }

    if (using_onyx_runtime) {
        onyx_set_option_int(ctx, ONYX_OPTION_MULTI_THREADING, 1);
    }

    return 1;
}

static void print_subcommand_help(const char *subcommand) {
    if (!strcmp(subcommand, "build") || !strcmp(subcommand, "b")
        || !strcmp(subcommand, "check") || !strcmp(subcommand, "watch")) {
        bh_printf(build_docstring, subcommand, "");
        return;
    }

    if (!strcmp(subcommand, "run") || !strcmp(subcommand, "r")
        || !strcmp(subcommand, "run-watch") || !strcmp(subcommand, "rw")) {
        bh_printf(build_docstring, subcommand, "[-- program args]");
        bh_printf(
            C_LBLUE "    --debug-socket " C_GREY "addr         " C_NORM "Specifies the address or port used for the debug server.\n"
        );
        return;
    }

    if (!strcmp(subcommand, "self-upgrade")) {
        bh_printf(self_upgrade_docstring);
        return;
    }

    bh_printf(C_RED  "error" C_NORM ": Unknown command: '%s'\n", subcommand);
    bh_printf(C_GREY " hint: Run 'onyx --help' for valid commands.\n");
    exit(1);
}

static char *get_description_for_subcommand(char *path) {
    bh_file_contents contents = bh_file_read_contents(bh_heap_allocator(), path);

    if (contents.data == NULL) return NULL;

    u8 *d = contents.data;
    char *out = NULL;

    i32 cursor = 8; // Skip magic bytes and version.
    while (cursor < contents.length) {
        char kind = d[cursor++];
        int section_length = uleb128_to_uint(d, &cursor);

        // If not a custom section, skip it.
        if (kind != 0) {
            cursor += section_length;
            continue;
        }

        int previous_cursor = cursor;
        int name_length = uleb128_to_uint(d, &cursor);

        if (strncmp("onyx-command-description", (const char *) &d[cursor], name_length)) {
            cursor = previous_cursor + section_length;
            continue;
        }

        cursor += name_length;
        int description_length = section_length - (cursor - previous_cursor);
        out = bh_alloc_array(bh_heap_allocator(), char, description_length + 1);
        memcpy(out, &d[cursor], description_length);
        out[description_length] = 0;

        break;
    }

    bh_file_contents_free(&contents);
    return out;
}

static void print_commands_in_directory(char *dir) {
    bh_dir d = bh_dir_open(dir);

    if (!d) return;

    bh_dirent ent;
    while (bh_dir_read(d, &ent)) {
        if (bh_str_ends_with(ent.name, ".wasm")) {
            ent.name[ent.name_length - 5] = 0; // Remove the .wasm from the name
            bh_printf(C_LBLUE "    %s", ent.name);

            fori (i, 0, 21 - ent.name_length) bh_printf(" ");

            char *description = get_description_for_subcommand(
                bh_aprintf(bh_heap_allocator(), "%s/%s.wasm", dir, ent.name)
            );

            if (!description) {
                bh_printf(C_GREY " Description not provided\n");
            } else {
                bh_printf(C_NORM " %s\n", description);
            }
        }
    }

    bh_dir_close(d);
}

static void print_top_level_docs(CLIArgs *cli_args) {
    bh_printf(top_level_docstring);

    int32_t has_runtime = strcmp(onyx_version_runtime(), "none") != 0;
    if (has_runtime) {
        bh_printf(run_commands_docstring);
    }

    bh_printf(C_BOLD "Global custom commands:\n" C_NORM);
    print_commands_in_directory(
        bh_aprintf(bh_heap_allocator(), "%s/tools", cli_args->core_installation)
    );

    bh_printf(C_NORM C_BOLD "\nLocal custom commands:\n" C_NORM);
    print_commands_in_directory("./.onyx");
}

static int32_t output_file_to_disk(CLIArgs *cli_args, onyx_context_t *ctx, const char *filename, onyx_output_type_t type) {
    int64_t output_length = onyx_output_length(ctx, type);
    if (output_length > 0) {
        void *output = malloc(output_length);
        assert(output);
        memset(output, 0, output_length);

        onyx_output_write(ctx, type, output);

        bh_file out_file;
        if (bh_file_create(&out_file, filename) != BH_FILE_ERROR_NONE) {
            bh_printf(C_RED "error" C_NORM ": Failed to open file for writing '%s'\n", filename);
            return 0;
        }

        bh_file_write(&out_file, output, output_length);
        bh_file_close(&out_file);

        free(output);
    }

    return 1;
}

static int32_t output_files_to_disk(CLIArgs *cli_args, onyx_context_t *ctx, const char *filename) {
    if (!output_file_to_disk(cli_args, ctx, filename, ONYX_OUTPUT_TYPE_WASM)) return 0;
    if (!output_file_to_disk(cli_args, ctx, bh_bprintf("%s.js", filename), ONYX_OUTPUT_TYPE_JS)) return 0;
    if (!output_file_to_disk(cli_args, ctx, bh_bprintf("%s.odoc", filename), ONYX_OUTPUT_TYPE_ODOC)) return 0;

    if (cli_args->symbol_info_file) {
        if (!output_file_to_disk(cli_args, ctx, cli_args->symbol_info_file, ONYX_OUTPUT_TYPE_OSYM)) return 0;
    }

    return 1;
}

#if defined(_BH_LINUX) || defined(_BH_DARWIN)
#include <sys/wait.h>

static void perform_self_upgrade(CLIArgs *cli_args, char *version) {
    int curl_pid;
    bh_file upgrade_script;

    char file_path[512];
    bh_snprintf(file_path, 511, "%s/upgrade.sh", cli_args->core_installation);

    switch (curl_pid = fork()) {
        case -1: exit(1);
        case 0:
            if (bh_file_create(&upgrade_script, file_path)) {
                exit(1);
            }

            dup2(upgrade_script.fd, STDOUT_FILENO);
            bh_file_close(&upgrade_script);

            execlp("curl", "curl", "https://get.onyxlang.io", "-sSfL", NULL);
            exit(1);
            break;
    }

    int status;
    waitpid(curl_pid, &status, 0);

    if (status == 0) {
        execlp("sh", "sh", file_path, version, onyx_version_runtime(), NULL);
    }

    printf("error: Failed to download upgrade script.\n");
    printf(" hint: Ensure you have an active internet connection and 'curl' installed.\n");
}
#endif

#if defined(_BH_LINUX) || defined(_BH_DARWIN)
#include <signal.h>
#include <sys/wait.h>

static bh_file_watch watches;
static i32 watch_run_pid = -1;

static void onyx_watch_stop(int sig) {
    bh_file_watch_stop(&watches);
}

static void onyx_watch_run_executable(const char *target) {
    watch_run_pid = fork();
    switch (watch_run_pid) {
        case -1: bh_printf("error: fork() failed\n"); break;
        case 0:
            setpgid(0, getpid());
            close(STDIN_FILENO);
            open("/dev/null", O_RDONLY);
            execlp("onyx", "onyx", "run", target, NULL);
            exit(1);
            break;
        default:
            break;
    }
}

static void onyx_watch(CLIArgs *cli_args, int arg_parse_start, int argc, char **argv) {
    signal(SIGINT, onyx_watch_stop);

    b32 run_the_program = cli_args->action == ONYX_COMPILE_ACTION_WATCH_RUN;

    while (1) {
        bh_printf("\e[2J\e[?25l\n");
        bh_printf("\e[3;1H");

        onyx_context_t *ctx = onyx_context_create();
        onyx_add_mapped_dir(ctx, "core", -1, bh_bprintf("%s/core", cli_args->core_installation), -1);
        onyx_set_option_int(ctx, ONYX_OPTION_PLATFORM, ONYX_PLATFORM_ONYX);

        if (!cli_parse_compilation_options(cli_args, ctx, arg_parse_start, argc, argv)) {
            return;
        }

        onyx_options_ready(ctx);
        while (onyx_pump(ctx) == ONYX_PUMP_CONTINUE) {
            // doing the compilation
        }

        i32 error_count = onyx_error_count(ctx);
        if (error_count == 0) {
            output_files_to_disk(cli_args, ctx, cli_args->target_file);

            bh_printf("\e[92mNo errors!\n");
        } else {
            onyx_errors_print(ctx, cli_args->error_format, !cli_args->no_colors, cli_args->show_all_errors);
        }

        char time_buf[128] = {0};
        time_t now = time(NULL);
        strftime(time_buf, 128, "%X", localtime(&now));
        bh_printf("\e[1;1H\e[30;105m Onyx %d.%d.%d%s \e[30;104m Built %s \e[0m", 
            onyx_version_major(),
            onyx_version_minor(),
            onyx_version_patch(),
            onyx_version_suffix(),
            time_buf
        );

        if (error_count == 0) {
            bh_printf("\e[30;102m Errors 0 \e[0m");
        } else {
            bh_printf("\e[30;101m Error%s %d \e[0m", bh_num_plural(error_count), error_count);
        }

        if (run_the_program && error_count == 0) {
            bh_printf("\n\n\nRunning your program...\n");
            onyx_watch_run_executable(cli_args->target_file);
        }

        watches = bh_file_watch_new();

        fori (i, 0, onyx_stat(ctx, ONYX_STAT_FILE_COUNT)) {
            bh_file_watch_add(&watches, onyx_stat_filepath(ctx, i));
        }

        onyx_context_free(ctx);

        b32 wait_successful = bh_file_watch_wait(&watches);

        if (run_the_program && watch_run_pid > 0) {
            int status;
            killpg(watch_run_pid, SIGTERM);
            waitpid(watch_run_pid, &status, 0);
            watch_run_pid = -1;
        }

        bh_file_watch_free(&watches);

        if (!wait_successful) {
            break;
        }
    }

    bh_printf("\e[2J\e[1;1H\e[?25h\n");
}
#endif

int main(int argc, char *argv[]) {
    CLIArgs cli_args;
    if (!cli_args_init(&cli_args)) {
        return 1;
    }

    int32_t arg_parse_start = 0;
    int32_t action_was_determined = cli_determine_action(&cli_args, &arg_parse_start, argc, argv);

    if (!action_was_determined || cli_args.action == ONYX_COMPILE_ACTION_PRINT_HELP) {
        if (cli_args.help_subcommand) {
            print_subcommand_help(cli_args.help_subcommand);
        } else {
            print_top_level_docs(&cli_args);
        }

        return !action_was_determined;
    }

    if (cli_args.action == ONYX_COMPILE_ACTION_PRINT_VERSION) { 
        bh_printf("Onyx tool-chain version %d.%d.%d%s\n",
            onyx_version_major(),
            onyx_version_minor(),
            onyx_version_patch(),
            onyx_version_suffix()
        );
        bh_printf("Runtime: %s\n", onyx_version_runtime());
        bh_printf("Built:   %s\n", onyx_version_build_time());

        return 0;
    }

    #if defined(_BH_LINUX) || defined(_BH_DARWIN)
    if (cli_args.action == ONYX_COMPILE_ACTION_SELF_UPGRADE) {
        if (arg_parse_start < argc && !is_flag(argv[arg_parse_start])) {
            perform_self_upgrade(&cli_args, argv[arg_parse_start]);
        }

        if (arg_parse_start < argc && !strcmp(argv[arg_parse_start], "--help")) {
            print_subcommand_help(argv[1]);
        }

        return 1;
    }

    if (cli_args.action == ONYX_COMPILE_ACTION_WATCH || cli_args.action == ONYX_COMPILE_ACTION_WATCH_RUN) {
        onyx_watch(&cli_args, arg_parse_start, argc, argv);
        return 0;
    }
    #endif

    onyx_context_t *ctx = onyx_context_create();
    onyx_add_mapped_dir(ctx, "core", -1, bh_bprintf("%s/core", cli_args.core_installation), -1);
    onyx_set_option_int(ctx, ONYX_OPTION_PLATFORM, ONYX_PLATFORM_ONYX);

    if (cli_args.action == ONYX_COMPILE_ACTION_PACKAGE) {
        onyx_set_option_int(ctx, ONYX_OPTION_GENERATE_METHOD_INFO, 1);
        onyx_set_option_int(ctx, ONYX_OPTION_MULTI_THREADING, 1);
        onyx_include_file(ctx, bh_aprintf(bh_heap_allocator(), "%s/tools/onyx-pkg.onyx", cli_args.core_installation), -1);

    } else {
        if (!cli_parse_compilation_options(&cli_args, ctx, arg_parse_start, argc, argv)) {
            return 1;
        }
    }

    if (cli_args.action == ONYX_COMPILE_ACTION_RUN_WASM) {
        bh_file_contents wasm_content = bh_file_read_contents(bh_heap_allocator(), cli_args.target_file);
        if (wasm_content.length <= 0) {
            bh_printf(C_RED "error" C_NORM ": Failed to read '%s'\n", cli_args.target_file);
            return 1;
        }

        if (cli_args.debug_session) {
            onyx_run_wasm_with_debug(wasm_content.data, wasm_content.length, cli_args.passthrough_argument_count, cli_args.passthrough_argument_data, cli_args.debug_socket);
        } else {
            onyx_run_wasm(wasm_content.data, wasm_content.length, cli_args.passthrough_argument_count, cli_args.passthrough_argument_data);
        }

        return 0;
    }

    u64 start_time = bh_time_curr();

    onyx_options_ready(ctx);
    while (onyx_pump(ctx) == ONYX_PUMP_CONTINUE) {
        fori (i, 0, onyx_event_count(ctx)) {
            switch (onyx_event_type(ctx, i)) {
            case ONYX_EVENT_LOG:
                if (cli_args.verbose_output > 0) {
                    bh_printf("%s %s\n",
                        cli_args.no_colors ? "INFO " : "\x1b[94mINFO\x1b[0m ",
                        onyx_event_field_str(ctx, i, "message")
                    );
                }
                break;

            case ONYX_EVENT_ALL_TYPES_CHECKED:
                break;

            case ONYX_EVENT_PHASE_START:
                break;

            case ONYX_EVENT_SYMBOL_DEFINED:
                // bh_printf("DEFINED SYMBOL AT %s:%d,%d\n",
                //     onyx_event_field_str(ctx, i, "filename"),
                //     onyx_event_field_int(ctx, i, "line"),
                //     onyx_event_field_int(ctx, i, "column")
                // );
                break;

            case ONYX_EVENT_UNKNOWN:
                break;
            }
        }
    }

    u64 duration = bh_time_duration(start_time);

    onyx_errors_print(ctx, cli_args.error_format, !cli_args.no_colors, cli_args.show_all_errors);
    if (onyx_errors_present(ctx)) {
        return 1;
    }

    if (cli_args.verbose_output > 0) {
        int tokens = onyx_stat(ctx, ONYX_STAT_TOKEN_COUNT);
        int lines  = onyx_stat(ctx, ONYX_STAT_LINE_COUNT);

        float tokens_per_sec = (1000.0f * tokens) / duration;
        float lines_per_sec = (1000.0f * lines) / duration;

        printf("\nStatistics:\n");
        printf("    Time taken: %lf ms\n", (double) duration);
        printf("    Processed %d lines (%f lines/second).\n", lines, lines_per_sec);
        printf("    Processed %d tokens (%f tokens/second).\n", tokens, tokens_per_sec);
        printf("\n");
    }
  
    switch (cli_args.action) {
        case ONYX_COMPILE_ACTION_RUN:
        case ONYX_COMPILE_ACTION_PACKAGE: {
            int64_t output_length = onyx_output_length(ctx, ONYX_OUTPUT_TYPE_WASM);
            void *output = malloc(output_length);
            onyx_output_write(ctx, ONYX_OUTPUT_TYPE_WASM, output);
            onyx_context_free(ctx);

            if (cli_args.debug_session) {
                onyx_run_wasm_with_debug(output, output_length, cli_args.passthrough_argument_count, cli_args.passthrough_argument_data, cli_args.debug_socket);
            } else {
                onyx_run_wasm(output, output_length, cli_args.passthrough_argument_count, cli_args.passthrough_argument_data);
            }

            free(output);
            return 0;
        }

        case ONYX_COMPILE_ACTION_CHECK: {
            if (cli_args.symbol_info_file) {
                if (!output_file_to_disk(&cli_args, ctx, cli_args.symbol_info_file, ONYX_OUTPUT_TYPE_OSYM)) {
                    return 1;
                }
            }
            break;
        }

        case ONYX_COMPILE_ACTION_COMPILE: {
            if (!output_files_to_disk(&cli_args, ctx, cli_args.target_file)) {
                return 1;
            }

            break;
        }

        default:
            break;
    }
  
    onyx_context_free(ctx);
    return 0;
}
