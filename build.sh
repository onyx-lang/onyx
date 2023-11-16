#!/bin/sh

DIST_DIR="./dist"
ONYX_INSTALL_DIR="$HOME/.onyx"

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
    mkdir -p "$DIST_DIR"

    echo "Installing on '$(uname -a)'"
    echo "Installing core libs"
    [ -d "$DIST_DIR/core" ] && rm -r "$DIST_DIR/core"
    cp -r ./core "$DIST_DIR/core"

    echo "Installing core tools"
    mkdir -p "$DIST_DIR/bin"
    cp compiler/onyx "$DIST_DIR/bin/"

    mkdir -p "$DIST_DIR/tools"
    mkdir -p "$DIST_DIR/tools/pkg_templates"
    cp ./scripts/onyx-pkg.onyx "$DIST_DIR/tools"
    cp ./scripts/default.json "$DIST_DIR/tools/pkg_templates"

    echo "Installing runtime library"
    mkdir -p "$DIST_DIR/lib"
    mkdir -p "$DIST_DIR/include"

    [ -f runtime/onyx_runtime.so ] && cp runtime/onyx_runtime.so "$DIST_DIR/lib/"
    cp "shared/include/onyx_library.h" "$DIST_DIR/include/onyx_library.h"
    cp "shared/include/wasm.h" "$DIST_DIR/include/wasm.h"

    cp -r "tests" "$DIST_DIR/"
    cp -r "examples" "$DIST_DIR/"

    mkdir -p "$DIST_DIR/misc"
    cp misc/onyx-linux.sublime-build "$DIST_DIR/misc"
    cp misc/onyx-windows.sublime-build "$DIST_DIR/misc"
    cp misc/onyx-mode.el "$DIST_DIR/misc"
    cp misc/onyx.sublime-syntax "$DIST_DIR/misc"
    cp misc/vscode/onyx-0.1.8.vsix "$DIST_DIR/misc"
}

install_all() {
    package_all

    echo "Installing to $ONYX_INSTALL_DIR"
    mkdir -p "$ONYX_INSTALL_DIR"
    cp -r "$DIST_DIR/." "$ONYX_INSTALL_DIR"
}

for arg in $@; do
    case "$arg" in
        compile) compile_all ;;
        package) package_all ;;
        install) install_all ;;
        clean)
            rm -f compiler/onyx 2>/dev/null
            rm -f runtime/onyx_runtime.so 2>/dev/null
            ;;
    esac
done

# Otherwise the prompt ends on the same line
printf "\n"
