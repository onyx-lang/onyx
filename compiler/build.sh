#!/bin/sh

# Expand space-separated flag groups into separate arguments for $ONYX_CC.
invoke_cc() {
    _ifs=$IFS
    IFS=' '
    _joined="$*"
    IFS=$_ifs
    set --
    while [ -n "$_joined" ]; do
        case "$_joined" in
            ' '*) _joined=${_joined# } ;;
            *' '*)
                set -- "$@" "${_joined%% *}"
                _joined=${_joined#* }
                ;;
            *)
                set -- "$@" "$_joined"
                break
                ;;
        esac
    done
    unset _ifs _joined
    "$ONYX_CC" "$@"
}

C_FILES="library_main astnodes builtins checker clone doc entities errors lex parser types utils wasm_emit wasm_obj wasm_obj_write extensions pb compilation_data "
LIBS="-lpthread -ldl -lm"
INCLUDES="-I./include -I../shared/include -I../shared/include/dyncall"

WARNINGS='-Wimplicit -Wmisleading-indentation -Wparentheses -Wsequence-point -Wreturn-type -Wshift-negative-value -Wunused-but-set-parameter -Wunused-but-set-variable -Wunused-function -Wunused-label -Wsign-compare -Wstrict-overflow -Wtrigraphs -Waddress'

if [ "$1" = "debug" ]; then
    FLAGS="$WARNINGS -g3"
else
    FLAGS="$WARNINGS -O3"
fi

FLAGS="$FLAGS -DENABLE_DEBUG_INFO -fvisibility=hidden"

if [ ! -z ${ONYX_TARGET+x} ]; then
    FLAGS="$FLAGS --target=$ONYX_TARGET"
fi

if [ ! -z ${ONYX_RUNTIME_LIBRARY+x} ]; then
    FLAGS="$FLAGS -DONYX_RUNTIME_LIBRARY=$ONYX_RUNTIME_LIBRARY"
    C_FILES="${C_FILES}wasm_runtime "

    case "${ONYX_RUNTIME_LIBRARY}" in
        ovmwasm)
            LIBS="../shared/lib/$ONYX_ARCH/lib/libovmwasm.a $LIBS"
            FLAGS="$FLAGS -DUSE_OVM_DEBUGGER"
            ;;

        wasmer)
            # `wasmer config --libdir` only works when the CLI can run. Cross
            # builds and older runners set WASMER_LIB_PATH to a downloaded archive.
            wasmer_lib=""
            if command -v wasmer >/dev/null 2>&1; then
                wasmer_lib="$(wasmer config --libdir 2>/dev/null || true)"
            fi
            if [ -z "$wasmer_lib" ]; then
                wasmer_lib="${WASMER_LIB_PATH:-}"
            fi
            if [ -z "$wasmer_lib" ] || [ ! -f "$wasmer_lib/libwasmer.a" ]; then
                echo "error: libwasmer.a not found (looked in '${wasmer_lib:-<unset>}')"
                echo "Install Wasmer or set WASMER_LIB_PATH to the directory containing libwasmer.a."
                exit 1
            fi
            LIBS="$wasmer_lib/libwasmer.a $LIBS"
            ;;

        *)
            echo "Unknown WebAssembly runtime '$ONYX_RUNTIME_LIBRARY'. Aborting.";
            exit 1
            ;;
    esac
fi

case "$ONYX_ARCH" in
    *darwin*)
        LIBS="$LIBS -lffi -framework CoreFoundation -framework SystemConfiguration -framework Security"
        LIBRARY_BUILD_ARGS="-install_name @rpath/libonyx.dylib"
        AUTOCOMPILER_BUILD_ARGS="-rpath @loader_path/"
        ;;

    *linux*)
        LIBRARY_BUILD_ARGS=""
        AUTOCOMPILER_BUILD_ARGS="-Wl,-rpath,\$ORIGIN"
        ;;
esac

if [ "$ONYX_USE_DYNCALL" = "1" ] && [ "$ONYX_RUNTIME_LIBRARY" = "ovmwasm" ]; then
    LIBS="$LIBS ../shared/lib/$ONYX_ARCH/lib/libdyncall_s.a ../shared/lib/$ONYX_ARCH/lib/libdyncallback_s.a"
    FLAGS="$FLAGS -DUSE_DYNCALL"
fi

case "$(uname)" in
    Linux)  suffix='so' ;;
    *BSD)   suffix='so' ;;
    Darwin) suffix='dylib' ;;
    *)      suffix='dll' ;;
esac

echo "Compiling libonyx.$suffix"
OBJECTS=""
for c_file in $(echo "$C_FILES" | sed 's/ /\n/g;s/\([a-zA-Z_0-9]*\)\n/src\/\1.c\n/g;s/\n/ /g'); do
    obj="$(basename "$c_file").o"
    invoke_cc "$FLAGS" "$INCLUDES" -fPIC -o "$obj" -c "$c_file"
    OBJECTS="$OBJECTS $obj"
done

# Keep cli/main.c's DWARF in a real object file. Passing the .c file on the
# link line compiles it to a temporary object that is gone before dsymutil runs.
invoke_cc "$FLAGS" "$INCLUDES" -fPIC -o cli_main.o -c cli/main.c

echo "Compiling onyx executable"
invoke_cc "$INCLUDES" "$FLAGS" "$OBJECTS" cli_main.o -o onyx "$LIBS"

FLAGS=""
if [ ! -z ${ONYX_TARGET+x} ]; then
    FLAGS="$FLAGS --target=$ONYX_TARGET"
fi
invoke_cc -shared -fPIC "$FLAGS" -o "libonyx.$suffix" "$OBJECTS" "$LIBS" "$LIBRARY_BUILD_ARGS"

# It does seem possible to allow for the CLI to directly
# use the shared library, but it was causing some weird
# segfaults in misc places, so I thought it best to keep
# it how it was for how.
#
# $ONYX_CC $INCLUDES $FLAGS \
#   cli/main.c \
#   -o onyx \
#   -L. -lonyx -rpath @loader_path/../lib
#   $LIBS

echo "Compiling onyx auto-compiler"
invoke_cc -shared -fPIC "$FLAGS" "$INCLUDES" \
    -Wno-int-conversion \
    -o "onyx_autocompiler.$suffix" \
    src/autocompiler_binding.c \
    -L. -lonyx \
    "$LIBS" \
    "$AUTOCOMPILER_BUILD_ARGS"


# I don't think an archive file is needed to ship right now,
# but it would in theory be pretty easy to get that to ship
# using the line below
#
# ar rcs libonyx.a *.o

# On macOS, -g leaves DWARF in the .o files and only a debug map in the Mach-O.
# LLDB can show source only from a dSYM (or those objects). Link the dSYM while
# the objects still exist; it has to sit next to whichever onyx binary is launched.
if [ "$1" = "debug" ] && [ "$(uname)" = "Darwin" ]; then
    echo "Generating onyx.dSYM"
    DSYMUTIL="dsymutil"
    if [ -x /usr/bin/dsymutil ]; then
        DSYMUTIL="/usr/bin/dsymutil"
    fi
    $DSYMUTIL onyx || exit 1
elif [ "$(uname)" = "Darwin" ]; then
    rm -rf onyx.dSYM
fi

rm -f ./*.o
