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
}

package_all() {
    rm -rf "$DIST_DIR"
    mkdir -p "$DIST_DIR"
    mkdir -p "$DIST_DIR/lib"
    mkdir -p "$DIST_DIR/include"

    echo "Installing on '$(uname -a)'"
    echo "Installing core libs"
    [ -d "$DIST_DIR/core" ] && rm -r "$DIST_DIR/core"
    cp -r ./core "$DIST_DIR/core"

    case "$(uname)" in
        Linux)  suffix='so' ;;
        *BSD)   suffix='so' ;;
        Darwin) suffix='dylib' ;;
        *)      suffix='dll' ;;
    esac

    echo "Installing core tools"
    mkdir -p "$DIST_DIR/bin"
    cp compiler/onyx "$DIST_DIR/bin/"
    cp compiler/libonyx.$suffix "$DIST_DIR/lib/"
    cp "shared/include/onyx.h" "$DIST_DIR/include/onyx.h"

    mkdir -p "$DIST_DIR/tools"
    mkdir -p "$DIST_DIR/tools/pkg_templates"
    cp ./scripts/onyx-pkg.onyx "$DIST_DIR/tools"
    cp ./scripts/default.json "$DIST_DIR/tools/pkg_templates"
    cp ./scripts/lsp.wasm "$DIST_DIR/tools"

    if [ ! -z ${ONYX_RUNTIME_LIBRARY+x} ]; then
        echo "Installing runtime library"

        [ -f runtime/onyx_runtime.$suffix ] && cp runtime/onyx_runtime.$suffix "$DIST_DIR/lib/"
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

    cp LICENSE "$DIST_DIR/LICENSE"
}

compress_all() {
    package_all

    # Sign the binaries on MacOS
    [ "$(uname)" = 'Darwin' ] && \
        codesign -s - "$DIST_DIR/bin/onyx" && \
        codesign -s - "$DIST_DIR/lib/libonyx.dylib" && \
        [ -f "$DIST_DIR/lib/onyx_runtime.dylib" ] && \
            codesign -s - "$DIST_DIR/lib/onyx_runtime.dylib"

    tar -C "$DIST_DIR" -zcvf onyx.tar.gz bin core examples include lib misc tools LICENSE

    mv onyx.tar.gz dist/
}

install_all() {
    [ -z ${ONYX_INSTALL_DIR+x} ] && echo "Please set ONYX_INSTALL_DIR to install Onyx." && exit 1

    package_all

    echo "Installing to $ONYX_INSTALL_DIR"
    mkdir -p "$ONYX_INSTALL_DIR"
    cp -r "$DIST_DIR/." "$ONYX_INSTALL_DIR"

    # Sign the binaries on MacOS
    [ "$(uname)" = 'Darwin' ] && \
        codesign -s - "$ONYX_INSTALL_DIR/bin/onyx" && \
        [ -f "$ONYX_INSTALL_DIR/lib/onyx_runtime.dylib" ] && \
            codesign -s - "$ONYX_INSTALL_DIR/lib/onyx_runtime.dylib"
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
            rm -f runtime/onyx_runtime.so 2>/dev/null
            ;;
    esac
done

# Otherwise the prompt ends on the same line
printf "\n"
