#define STRINGIFY_(x) #x
#define STRINGIFY(x) STRINGIFY_(x)

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


#define VERSION_STRING "Onyx toolchain version " VERSION "\n" \
    "Runtime: " STRINGIFY(ONYX_RUNTIME_LIBRARY_MAPPED) "\n" \
    "Built:   " __TIMESTAMP__ "\n"

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
#ifdef ONYX_RUNTIME_LIBRARY
    C_LBLUE "    run " C_GREY "files        " C_NORM "Compiles and runs an Onyx program " C_GREY "(onyx r)" C_NORM "\n"
#endif
    C_LBLUE "    check " C_GREY "files      " C_NORM "Checks syntax and types of a program\n"
#if defined(_BH_LINUX) || defined(_BH_DARWIN)
    C_LBLUE "    watch            " C_NORM "Continuously rebuilds a program on file changes\n"
#endif
#ifdef ONYX_RUNTIME_LIBRARY
    "\n"
    C_LBLUE "    package " C_GREY "cmd      " C_NORM "Package manager " C_GREY "(onyx pkg cmd)" C_NORM "\n"
    C_LBLUE "    new              " C_NORM "Create a new project from a template\n"
    C_LBLUE "    init             " C_NORM "Initialize a project in the current directory\n"
    C_LBLUE "    add " C_GREY "package      " C_NORM "Add a package to dependency list " C_GREY "(onyx a)" C_NORM "\n"
    C_LBLUE "    remove " C_GREY "package   " C_NORM "Remove a package from dependency list " C_GREY "(onyx rm)" C_NORM "\n"
    C_LBLUE "    sync             " C_NORM "Synchronize installed packages\n"
#endif
    "\n"
#if defined(_BH_LINUX) || defined(_BH_DARWIN)
    C_LBLUE "    self-upgrade     " C_NORM "Upgrade your toolchain\n"
#endif
    "\n";

static const char *build_docstring = DOCSTRING_HEADER
    C_BOLD "Usage: " C_BLUE "onyx" C_LBLUE " %s " C_NORM C_YELLOW "[..flags] " C_GREEN "files " C_NORM "%s" "\n"
    "\n"
    C_BOLD "Flags:\n" C_NORM
    C_LBLUE "    -o, --output " C_GREY "target_file    " C_NORM "Specify the target file " C_GREY "(default: out.wasm)\n"
    C_LBLUE "    -r, --runtime " C_GREY "runtime       " C_NORM "Specifies the runtime " C_GREY "(onyx, wasi, js, custom)\n"
    C_LBLUE "    -I, --include " C_GREY "dir           " C_NORM "Include a directory in the search path\n"
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
    C_LBLUE "    --doc " C_GREY "doc_file              " C_NORM "Generate a .odoc file, Onyx's documentation format used by " C_YELLOW "onyx-doc-gen\n"
    C_LBLUE "    --tag                       " C_NORM "Generate a C-Tag file\n"
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

static void cli_determine_action(CompileOptions *options, int *first_sub_arg, int argc, char *argv[]) {
    if (is_flag(argv[1])) {
        options->action = ONYX_COMPILE_ACTION_PRINT_HELP;
        return;
    }

    options->help_subcommand = argc > 1 ? argv[1] : NULL;

    if (!strcmp(argv[1], "help")) {
        options->action = ONYX_COMPILE_ACTION_PRINT_HELP;
        options->help_subcommand = argc > 2 ? argv[2] : NULL;
        *first_sub_arg = 1;
        return;
    }

    if (!strcmp(argv[1], "version")) {
        options->action = ONYX_COMPILE_ACTION_PRINT_VERSION;
        *first_sub_arg = 1;
        return;
    }

    if (!strcmp(argv[1], "compile") || !strcmp(argv[1], "build") || !strcmp(argv[1], "b")) {
        options->action = ONYX_COMPILE_ACTION_COMPILE;
        *first_sub_arg = 2;
        return;
    }

    if (!strcmp(argv[1], "check")) {
        options->action = ONYX_COMPILE_ACTION_CHECK;
        *first_sub_arg = 2;
        return;
    }

    if (!strcmp(argv[1], "pkg") || !strcmp(argv[1], "package")) {
        // Maybe we should consider caching the package WASM file so it doesn't need to be recompiled
        // every time? Compilation is very fast, but it would be even snappier if the whole package
        // manager didn't need to compile every time.

        options->action = ONYX_COMPILE_ACTION_RUN;
        options->passthrough_argument_count = argc - 2;
        options->passthrough_argument_data  = &argv[2];
        options->generate_method_info = 1; // The package manager needs this to be enabled.
        *first_sub_arg = argc;

        bh_arr_push(options->files, bh_aprintf(options->allocator, "%s/tools/onyx-pkg.onyx", options->core_installation));
        return;
    }

    if (!strcmp(argv[1], "add") || !strcmp(argv[1], "a")) {
        argv[1] = "add";

        options->action = ONYX_COMPILE_ACTION_RUN;
        options->passthrough_argument_count = argc - 1;
        options->passthrough_argument_data  = &argv[1];
        options->generate_method_info = 1; // The package manager needs this to be enabled.
        *first_sub_arg = argc;

        bh_arr_push(options->files, bh_aprintf(options->allocator, "%s/tools/onyx-pkg.onyx", options->core_installation));
        return;
    }

    if (!strcmp(argv[1], "remove") || !strcmp(argv[1], "rm")) {
        argv[1] = "remove";

        options->action = ONYX_COMPILE_ACTION_RUN;
        options->passthrough_argument_count = argc - 1;
        options->passthrough_argument_data  = &argv[1];
        options->generate_method_info = 1; // The package manager needs this to be enabled.
        *first_sub_arg = argc;

        bh_arr_push(options->files, bh_aprintf(options->allocator, "%s/tools/onyx-pkg.onyx", options->core_installation));
        return;
    }

    if (!strcmp(argv[1], "sync") || !strcmp(argv[1], "init") || !strcmp(argv[1], "new")) {
        options->action = ONYX_COMPILE_ACTION_RUN;
        options->passthrough_argument_count = argc - 1;
        options->passthrough_argument_data  = &argv[1];
        options->generate_method_info = 1; // The package manager needs this to be enabled.
        *first_sub_arg = argc;

        bh_arr_push(options->files, bh_aprintf(options->allocator, "%s/tools/onyx-pkg.onyx", options->core_installation));
        return;
    }

    #ifdef ONYX_RUNTIME_LIBRARY
    if (!strcmp(argv[1], "run") || !strcmp(argv[1], "r")) {
        options->action = ONYX_COMPILE_ACTION_RUN;
        *first_sub_arg = 2;
        return;
    }
    #endif

    #if defined(_BH_LINUX) || defined(_BH_DARWIN)
    if (!strcmp(argv[1], "watch")) {
        options->action = ONYX_COMPILE_ACTION_WATCH;
        *first_sub_arg = 2;
        return;
    }
    #endif

    #if defined(_BH_LINUX) || defined(_BH_DARWIN)
    if (!strcmp(argv[1], "self-upgrade")) {
        options->action = ONYX_COMPILE_ACTION_SELF_UPGRADE;
        *first_sub_arg = 2;
        return;
    }
    #endif


    char *script_filename = bh_aprintf(options->allocator, "%s/tools/%s.wasm", options->core_installation, argv[1]);
    if (bh_file_exists(script_filename)) {
        options->action = ONYX_COMPILE_ACTION_RUN_WASM;
        options->target_file = script_filename;

        options->passthrough_argument_count = argc - 2;
        options->passthrough_argument_data  = &argv[2];
        return;
    }

    bh_printf(C_RED  "error" C_NORM ": Unknown command: '%s'\n", argv[1]);
    bh_printf(C_GREY " hint: Run 'onyx --help' for valid commands.\n");
    exit(1);
}

static void cli_parse_compilation_options(CompileOptions *options, int arg_parse_start, int argc, char **argv) {
    fori(i, arg_parse_start, argc) {
        arg_parse_start = i;

        if (!is_flag(argv[i])) {
            // On the first non-flag argument, break to add the files.
            break;
        }

        if (!strcmp(argv[i], "-o") || !strcmp(argv[i], "--output")) {
            options->target_file = argv[++i];
        }
        else if (!strcmp(argv[i], "--verbose") || !strcmp(argv[i], "-V")) {
            options->verbose_output = 1;
        }
        else if (!strcmp(argv[i], "--help")) {
            options->action = ONYX_COMPILE_ACTION_PRINT_HELP;
            options->help_subcommand = argv[1];
            return;
        }
        else if (!strcmp(argv[i], "-VV")) {
            options->verbose_output = 2;
        }
        else if (!strcmp(argv[i], "-VVV")) {
            options->verbose_output = 3;
        }
        else if (!strcmp(argv[i], "--print-function-mappings")) {
            options->print_function_mappings = 1;
        }
        else if (!strcmp(argv[i], "--print-static-if-results")) {
            options->print_static_if_results = 1;
        }
        else if (!strcmp(argv[i], "--no-colors")) {
            options->no_colors = 1;
        }
        else if (!strcmp(argv[i], "--no-file-contents")) {
            options->no_file_contents = 1;
        }
        else if (!strcmp(argv[i], "--no-compiler-extensions")) {
            options->no_compiler_extensions = 1;
        }
        else if (!strcmp(argv[i], "--wasm-mvp")) {
            options->use_post_mvp_features = 0;
        }
        else if (!strcmp(argv[i], "--multi-threaded")) {
            options->use_multi_threading = 1;
        }
        else if (!strcmp(argv[i], "--generate-foreign-info")) {
            options->generate_foreign_info = 1;
        }
        else if (!strcmp(argv[i], "--generate-method-info")) {
            options->generate_method_info = 1;
        }
        else if (!strcmp(argv[i], "--generate-name-section")) {
            options->generate_name_section = 1;
        }
        else if (!strcmp(argv[i], "--no-type-info")) {
            options->generate_type_info = 0;
        }
        else if (!strcmp(argv[i], "--no-core")) {
            options->no_core = 1;
        }
        else if (!strcmp(argv[i], "--no-stale-code")) {
            options->no_stale_code = 1;
        }
        else if (!strcmp(argv[i], "--show-all-errors")) {
            options->show_all_errors = 1;
        }
        else if (!strcmp(argv[i], "--error-format")) {
            options->error_format = argv[++i];
        }
        else if (!strcmp(argv[i], "--feature")) {
            char *next_arg = argv[++i];
            if (!strcmp(next_arg, "optional-semicolons")) {
                options->enable_optional_semicolons = 1;
            }
        }
        else if (!strcmp(argv[i], "-I") || !strcmp(argv[i], "--include")) {
            bh_arr_push(options->included_folders, argv[++i]);
        }
        else if (!strncmp(argv[i], "-D", 2)) {
            i32 len = strlen(argv[i]);
            i32 j=2;
            while (argv[i][j] != '=' && j < len) j++;

            if (j < len) argv[i][j] = '\0';

            DefinedVariable dv;
            dv.key   = argv[i] + 2;
            dv.value = argv[i] + j + 1;
            bh_arr_push(options->defined_variables, dv);
        }
        else if (!strcmp(argv[i], "-r") || !strcmp(argv[i], "--runtime")) {
            i += 1;
            if      (!strcmp(argv[i], "onyx"))   options->runtime = Runtime_Onyx;
            else if (!strcmp(argv[i], "wasi"))   options->runtime = Runtime_Wasi;
            else if (!strcmp(argv[i], "js"))     options->runtime = Runtime_Js;
            else if (!strcmp(argv[i], "custom")) options->runtime = Runtime_Custom;
            else {
                bh_printf(C_YELLOW "warning" C_NORM ": '%s' is not a valid runtime. Defaulting to 'onyx'.\n", argv[i]);
                options->runtime = Runtime_Onyx;
            }
        }
        else if (!strcmp(argv[i], "--doc")) {
            options->documentation_file = argv[++i];
        }
        else if (!strcmp(argv[i], "--tag")) {
            options->generate_tag_file = 1;
        }
        else if (!strcmp(argv[i], "--syminfo")) {
            options->generate_symbol_info_file = 1;
            options->symbol_info_file = argv[++i];
        }
        else if (!strcmp(argv[i], "--lspinfo")) {
            options->generate_symbol_info_file = 1;
            options->generate_lsp_info_file = 1;
            options->symbol_info_file = argv[++i];
        }
        else if (!strcmp(argv[i], "--debug")) {
            options->debug_session = 1;
            options->debug_info_enabled = 1;
            options->stack_trace_enabled = 1;
            options->generate_name_section = 1;
        }
        else if (!strcmp(argv[i], "--debug-socket")) {
            options->debug_socket = argv[++i];
        }
        else if (!strcmp(argv[i], "--debug-info")) {
            options->debug_info_enabled = 1;
            options->stack_trace_enabled = 1;
            options->generate_name_section = 1;
        }
        else if (!strcmp(argv[i], "--stack-trace")) {
            options->stack_trace_enabled = 1;
            options->generate_name_section = 1;
        }
        else if (!strcmp(argv[i], "--perf")) {
            options->running_perf = 1;
        }
#if defined(_BH_LINUX) || defined(_BH_DARWIN)
        // NOTE: Fun output is only enabled for Linux because Windows command line
        // is not ANSI compatible and for a silly feature, I don't want to learn
        // how to properly do arbitrary graphics in it.
        else if (!strcmp(argv[i], "--fun") || !strcmp(argv[i], "-F")) {
            options->fun_output = 1;
        }
#endif
        else {
            bh_printf(C_RED "error" C_NORM ": Unknown flag '%s'.\n", argv[i]);
            options->action = ONYX_COMPILE_ACTION_PRINT_HELP;
            return;
        }
    }
    
    fori (i, arg_parse_start, argc) {
        if (is_flag(argv[i])) {
            if (!strcmp(argv[i], "--")) {
                options->passthrough_argument_count = argc - i - 1;
                options->passthrough_argument_data  = &argv[i + 1];
                return;
            }

            // FLAG AFTER SOURCE FILES
            bh_printf(C_RED  "error" C_NORM ": Flag provided after start of source files.\n");
            bh_printf(C_GREY " hint: Try moving the flag '%s' to be earlier in your command.\n" C_NORM, argv[i]);
            exit(1);
        }

        if (bh_str_ends_with(argv[i], ".wasm") && options->action == ONYX_COMPILE_ACTION_RUN) {
            if (bh_arr_length(options->files) > 0) {
                bh_printf(C_RED "error" C_NORM ": Expected only one '.wasm', or multiple '.onyx' files to be given.\n");
                exit(1);
            }

            options->action = ONYX_COMPILE_ACTION_RUN_WASM;
            options->target_file = argv[i];

            options->passthrough_argument_count = argc - i - 1;
            options->passthrough_argument_data  = &argv[i + 1];
            return;
        }

        bh_arr_push(options->files, argv[i]);
    }
}

static CompileOptions compile_opts_parse(bh_allocator alloc, int argc, char *argv[]) {
    CompileOptions options = {
        .allocator = alloc,
        .action = ONYX_COMPILE_ACTION_PRINT_HELP,

        .verbose_output          = 0,
        .fun_output              = 0,
        .print_function_mappings = 0,
        .no_file_contents        = 0,

        .use_post_mvp_features   = 1,
        .use_multi_threading     = 0,
        .generate_foreign_info   = 0,
        .generate_type_info      = 1,
        .generate_method_info    = 0,
        .no_core                 = 0,
        .no_stale_code           = 0,
        .show_all_errors         = 0,

        .enable_optional_semicolons = 1,

        .runtime = Runtime_Onyx,

        .files = NULL,
        .target_file = "out.wasm",

        .documentation_file = NULL,
        .symbol_info_file   = NULL,
        .help_subcommand    = NULL,

        .defined_variables = NULL,

        .debug_info_enabled = 0,

        .passthrough_argument_count = 0,
        .passthrough_argument_data  = NULL,

        .generate_tag_file = 0,
        .generate_symbol_info_file = 0,
        .generate_lsp_info_file = 0,

        .running_perf = 0,

        .error_format = "v1",

        .upgrade_version = "",
    };

    bh_arr_new(alloc, options.files, 2);
    bh_arr_new(alloc, options.included_folders, 2);
    bh_arr_new(alloc, options.defined_variables, 2);

    #if defined(_BH_LINUX) || defined(_BH_DARWIN)
    options.core_installation = getenv("ONYX_PATH");

    if (getenv("ONYX_ERROR_FORMAT")) {
        options.error_format = getenv("ONYX_ERROR_FORMAT");
    }
    #endif

    #ifdef _BH_WINDOWS
    char *tmp_core_installation = bh_alloc_array(alloc, u8, 512);
    char *tmp_error_format      = bh_alloc_array(alloc, u8, 512);

    if (GetEnvironmentVariableA("ONYX_PATH", tmp_core_installation, 512) > 0) {
        options.core_installation = tmp_core_installation;
    }
    if (GetEnvironmentVariableA("ONYX_ERROR_FORMAT", tmp_error_format, 512) > 0) {
        options.error_format = tmp_error_format;
    }
    #endif

    if (options.core_installation == NULL) {
        bh_printf(C_RED "error" C_NORM ": ONYX_PATH environment variable is not set. Please set this to the location of your Onyx installation.\n");
        exit(1);
    }

    // NOTE: Add the current folder
    bh_arr_push(options.included_folders, options.core_installation);
    bh_arr_push(options.included_folders, ".");

    if (argc == 1) return options;

    int arg_parse_start = 1;
    cli_determine_action(&options, &arg_parse_start, argc, argv);

    switch (options.action) {
        case ONYX_COMPILE_ACTION_CHECK:
        case ONYX_COMPILE_ACTION_RUN:
        case ONYX_COMPILE_ACTION_WATCH:
        case ONYX_COMPILE_ACTION_COMPILE:
            cli_parse_compilation_options(&options, arg_parse_start, argc, argv);
            break;

        case ONYX_COMPILE_ACTION_SELF_UPGRADE:
            if (arg_parse_start < argc && !is_flag(argv[arg_parse_start])) {
                options.upgrade_version = argv[arg_parse_start];
            }
            if (arg_parse_start < argc && !strcmp(argv[arg_parse_start], "--help")) {
                options.action = ONYX_COMPILE_ACTION_PRINT_HELP;
                options.help_subcommand = argv[1];
            }
            break;

        case ONYX_COMPILE_ACTION_RUN_WASM:
        case ONYX_COMPILE_ACTION_PRINT_HELP:
        case ONYX_COMPILE_ACTION_PRINT_VERSION:
        case ONYX_COMPILE_ACTION_DOCUMENT:
            break;
    }

    // NOTE: Always enable multi-threading for the Onyx runtime.
    // Maybe this code should be moved?
    if (options.runtime == Runtime_Onyx) {
        options.use_multi_threading = 1;
    }

    switch (options.action) {
        case ONYX_COMPILE_ACTION_CHECK:
        case ONYX_COMPILE_ACTION_RUN:
        case ONYX_COMPILE_ACTION_WATCH:
        case ONYX_COMPILE_ACTION_COMPILE:
            if (bh_arr_length(options.files) == 0) {
                bh_printf(C_RED "error" C_NORM ": No files were provided.\n");
                options.action = ONYX_COMPILE_ACTION_PRINT_HELP;
            }
            break;

        default:
            break;
    }

    return options;
}

static void compile_opts_free(CompileOptions* opts) {
    bh_arr_free(opts->files);
    bh_arr_free(opts->included_folders);
}

static void print_subcommand_help(const char *subcommand) {
    if (!strcmp(subcommand, "build") || !strcmp(subcommand, "b")
        || !strcmp(subcommand, "check") || !strcmp(subcommand, "watch")) {
        bh_printf(build_docstring, subcommand, "");
        return;
    }

    if (!strcmp(subcommand, "run") || !strcmp(subcommand, "r")) {
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

