#!/bin/sh

. ./settings.sh

echo "Installing core libs"
sudo mkdir -p "$CORE_DIR"
sudo cp -r ./core/ "$CORE_DIR"

if [ ! -f "$CORE_DIR/lib/lib$RUNTIME_LIBRARY.so" ] || true; then
    echo "Copying lib$RUNTIME_LIBRARY to $CORE_DIR/lib (first install)"

    sudo mkdir -p "$CORE_DIR/lib"
    sudo mkdir -p "$CORE_DIR/include"

    sudo cp "$WASMER_LIBRARY_DIR/lib$RUNTIME_LIBRARY.so" "$CORE_DIR/lib/lib$RUNTIME_LIBRARY.so"

    sudo cp "shared/include/onyx_library.h" "$CORE_DIR/include/onyx_library.h"
    sudo cp "$WASMER_INCLUDE_DIR/wasm.h" "$CORE_DIR/include/wasm.h"
fi

# This is a development feature to allow for quickly reinstalling core libraries
# without have to recompile the entire compiler
[ "$1" = "core" ] && exit 0

sudo cp ./bin/onyx-pkg "$BIN_DIR/onyx-pkg"
sudo mkdir -p "$CORE_DIR/tools"
sudo cp ./scripts/onyx-pkg.onyx "$CORE_DIR/tools"

cd compiler
./build.sh $1
cd ..

cd runtime
./build.sh $1
cd ..

# Otherwise the prompt ends on the same line
printf "\n"
