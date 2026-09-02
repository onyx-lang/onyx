#!/bin/sh

# set -e

DIST_DIR="./dist"

compile_all() {
    if [ "$ONYX_RUNTIME_LIBRARY" = "ovmwasm" ]; then
        cd interpreter
        ./build.sh $1
        cd ..
    fi

    cd compiler
    ./build.sh $1
    cd ..

    if [ ! -z ${ONYX_RUNTIME_LIBRARY+x} ]; then
        cd runtime
        ./build.sh $1
        cd ..
    fi

    if [ "$ONYX_RUNTIME_LIBRARY" = "ovmwasm" ]; then
        build_native_archive
    fi
}

build_native_archive() {
    DEST="shared/lib/$ONYX_ARCH/lib/libonyx_native.a"
    TMP="./build_native_tmp"
    rm -rf "$TMP"
    mkdir -p "$TMP" "$(dirname "$DEST")"

    echo "Compiling libonyx_native.a"

    OVMWASM="shared/lib/$ONYX_ARCH/lib/libovmwasm.a"
    (cd "$TMP" && ar x "../$OVMWASM")

    NATIVE_FLAGS="-O2 -fPIC -w"
    if [ ! -z ${ONYX_TARGET+x} ]; then
        NATIVE_FLAGS="$NATIVE_FLAGS --target=$ONYX_TARGET"
    fi

    $ONYX_CC $NATIVE_FLAGS -c interpreter/src/jit/native_main.c \
        -Iinterpreter/include -Ishared/include \
        -o "$TMP/native_main.o"

    WASM_RT_FLAGS="-DONYX_RUNTIME_LIBRARY=ovmwasm -DUSE_OVM_DEBUGGER -DENABLE_DEBUG_INFO"
    if [ "$ONYX_USE_DYNCALL" = "1" ]; then
        WASM_RT_FLAGS="$WASM_RT_FLAGS -DUSE_DYNCALL"
    fi

    $ONYX_CC $NATIVE_FLAGS -c compiler/src/wasm_runtime.c \
        -Icompiler/include -Ishared/include -Ishared/include/dyncall \
        $WASM_RT_FLAGS \
        -o "$TMP/wasm_runtime.o"

    if [ -f runtime/libonyx_runtime.a ]; then
        (cd "$TMP" && ar x "../runtime/libonyx_runtime.a")
    fi

    if [ "$ONYX_USE_DYNCALL" = "1" ]; then
        DYN="shared/lib/$ONYX_ARCH/lib/libdyncall_s.a"
        DYNB="shared/lib/$ONYX_ARCH/lib/libdyncallback_s.a"
        [ -f "$DYN" ] && (cd "$TMP" && ar x "../$DYN")
        [ -f "$DYNB" ] && (cd "$TMP" && ar x "../$DYNB")
    fi

    rm -f "$DEST"
    ar crs "$DEST" "$TMP"/*.o
    rm -rf "$TMP"
}

host_lib_suffix() {
    case "$(uname)" in
        Linux)  echo 'so' ;;
        *BSD)   echo 'so' ;;
        Darwin) echo 'dylib' ;;
        *)      echo 'dll' ;;
    esac
}

copy_required() {
    src="$1"
    dest="$2"
    if [ ! -f "$src" ]; then
        echo "error: missing '$src' (run './build.sh compile' first)"
        exit 1
    fi
    cp "$src" "$dest"
}

# Ad-hoc sign so dlopen is not SIGKILL'd by AFMI on macOS.
sign_tree() {
    root="$1"
    [ "$(uname)" = 'Darwin' ] || return 0
    [ -f "$root/bin/onyx" ] && codesign -s - -f "$root/bin/onyx"
    [ -f "$root/lib/libonyx.dylib" ] && codesign -s - -f "$root/lib/libonyx.dylib"
    [ -f "$root/lib/onyx_autocompiler.dylib" ] && codesign -s - -f "$root/lib/onyx_autocompiler.dylib"
    [ -f "$root/lib/onyx_runtime.dylib" ] && codesign -s - -f "$root/lib/onyx_runtime.dylib"
}

package_all() {
    : "${ONYX_ARCH:=$(uname | tr '[:upper:]' '[:lower:]')_$(uname -m)}"

    rm -rf "$DIST_DIR"
    mkdir -p "$DIST_DIR"
    mkdir -p "$DIST_DIR/lib"
    mkdir -p "$DIST_DIR/include"

    echo "Installing on '$(uname -a)'"
    echo "Installing core libs"
    [ -d "$DIST_DIR/core" ] && rm -r "$DIST_DIR/core"
    cp -r ./core "$DIST_DIR/core"

    suffix="$(host_lib_suffix)"

    echo "Installing core tools"
    mkdir -p "$DIST_DIR/bin"
    copy_required compiler/onyx "$DIST_DIR/bin/"
    copy_required compiler/libonyx.$suffix "$DIST_DIR/lib/"
    copy_required compiler/onyx_autocompiler.$suffix "$DIST_DIR/lib/"
    copy_required "shared/include/onyx.h" "$DIST_DIR/include/onyx.h"

    mkdir -p "$DIST_DIR/tools"
    mkdir -p "$DIST_DIR/tools/pkg_templates"
    copy_required ./scripts/onyx-pkg.onyx "$DIST_DIR/tools"
    copy_required ./scripts/default.json "$DIST_DIR/tools/pkg_templates"
    copy_required ./scripts/lsp.wasm "$DIST_DIR/tools"

    echo "Installing runtime library"
    if [ -f "runtime/onyx_runtime.$suffix" ]; then
        cp "runtime/onyx_runtime.$suffix" "$DIST_DIR/lib/"
    elif [ ! -z ${ONYX_RUNTIME_LIBRARY+x} ]; then
        echo "error: missing 'runtime/onyx_runtime.$suffix' (run './build.sh compile' first)"
        exit 1
    fi

    NATIVE_LIB="shared/lib/$ONYX_ARCH/lib/libonyx_native.a"
    if [ ! -f "$NATIVE_LIB" ] && [ "$ONYX_RUNTIME_LIBRARY" = "ovmwasm" ]; then
        build_native_archive
    fi
    if [ -f "$NATIVE_LIB" ]; then
        cp "$NATIVE_LIB" "$DIST_DIR/lib/"
    elif [ "$ONYX_RUNTIME_LIBRARY" = "ovmwasm" ]; then
        echo "error: missing '$NATIVE_LIB' (run './build.sh compile' first)"
        exit 1
    fi

    if [ -f "shared/include/onyx_library.h" ]; then
        cp "shared/include/onyx_library.h" "$DIST_DIR/include/onyx_library.h"
        cp "shared/include/wasm.h" "$DIST_DIR/include/wasm.h"
    fi

    cp -r "examples" "$DIST_DIR/"

    mkdir -p "$DIST_DIR/misc"
    cp misc/onyx-linux.sublime-build "$DIST_DIR/misc"
    cp misc/onyx-windows.sublime-build "$DIST_DIR/misc"
    cp misc/onyx-mode.el "$DIST_DIR/misc"
    cp misc/onyx.sublime-syntax "$DIST_DIR/misc"
    cp misc/onyx.tmPreferences "$DIST_DIR/misc"
    cp misc/vscode/onyxlang-0.1.9.vsix "$DIST_DIR/misc"
    cp core/onyx/toolchain/compiler_events.proto "$DIST_DIR/misc"

    cp LICENSE "$DIST_DIR/LICENSE"

    sign_tree "$DIST_DIR"
}

compress_all() {
    package_all

    tar -C "$DIST_DIR" -zcvf onyx.tar.gz bin core examples include lib misc tools LICENSE

    mv onyx.tar.gz dist/
}

install_all() {
    [ -z ${ONYX_INSTALL_DIR+x} ] && echo "Please set ONYX_INSTALL_DIR to install Onyx." && exit 1

    package_all

    echo "Installing to $ONYX_INSTALL_DIR"
    mkdir -p "$ONYX_INSTALL_DIR"
    cp -r "$DIST_DIR/." "$ONYX_INSTALL_DIR"

    # Re-sign after copy; some Darwin copies drop the ad-hoc signature.
    sign_tree "$ONYX_INSTALL_DIR"
}

for arg in $@; do
    case "$arg" in
        compile) compile_all ;;
        debug) compile_all debug ;;
        package) package_all ;;
        compress) compress_all ;;
        install) install_all ;;
        clean)
            rm -f compiler/onyx 2>/dev/null
            rm -f compiler/libonyx.* 2>/dev/null
            rm -f compiler/onyx_autocompiler.* 2>/dev/null
            rm -f runtime/onyx_runtime.so 2>/dev/null
            ;;
        build_autocompiler)
            onyx run --generate-foreign-info scripts/autocompiler_gen.onyx
            ;;
    esac
done

# Otherwise the prompt ends on the same line
printf "\n"
