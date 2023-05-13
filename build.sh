#!/bin/sh

. ./settings.sh

echo "Installing on '$(uname -a)'"

echo "Installing core libs"
[ -d "$CORE_DIR/core" ] && sudo rm -r "$CORE_DIR/core"
sudo mkdir -p "$CORE_DIR"
sudo cp -r ./core/ "$CORE_DIR"


sudo mkdir -p "$CORE_DIR/tools"
sudo mkdir -p "$CORE_DIR/tools/pkg_templates"
sudo cp ./scripts/onyx-pkg.onyx "$CORE_DIR/tools"
sudo cp ./scripts/default.json "$CORE_DIR/tools/pkg_templates"

# This is a development feature to allow for quickly reinstalling core libraries
# without have to recompile the entire compiler
[ "$1" = "core" ] && exit 0

if [ "$RUNTIME_LIBRARY" = "ovmwasm" ]; then
    cd interpreter
    ./build.sh $1
    cd ..
fi

if [ ! -f "$CORE_DIR/lib/lib$RUNTIME_LIBRARY.so" ] || true; then
    echo "Copying lib$RUNTIME_LIBRARY to $CORE_DIR/lib (first install)"

    sudo mkdir -p "$CORE_DIR/lib"
    sudo mkdir -p "$CORE_DIR/include"

    sudo cp "$WASMER_LIBRARY_DIR/lib$RUNTIME_LIBRARY.so" "$CORE_DIR/lib/lib$RUNTIME_LIBRARY.so"

    sudo cp "shared/include/onyx_library.h" "$CORE_DIR/include/onyx_library.h"
    sudo cp "$WASMER_INCLUDE_DIR/wasm.h" "$CORE_DIR/include/wasm.h"
fi

cd compiler
./build.sh $1
cd ..

cd runtime
./build.sh $1
cd ..



# Otherwise the prompt ends on the same line
printf "\n"
