#!/bin/sh

C_FILES="onyx onyxbuiltins onyxchecker onyxclone onyxdoc onyxentities onyxerrors onyxlex onyxparser onyxsempass onyxsymres onyxtypes onyxutils onyxwasm"
TARGET='./bin/onyx'
CC='gcc'
FLAGS='-O3 -I./include'
BUILD_DIR='./build'

mkdir -p "$BUILD_DIR"

for file in $C_FILES ; do
    echo "Compiling $file.c"
    $CC -o build/$file.o $FLAGS -c src/$file.c
done

ALL_FILES="$(for file in $C_FILES ; do printf "$BUILD_DIR/%s.o " $file ; done)"
echo "Linking $TARGET"
$CC -o $TARGET $FLAGS $ALL_FILES


CORE_DIR='/usr/share/onyx'
sudo mkdir -p "$CORE_DIR"
echo "Installing core libs"
sudo cp -r ./core/ "$CORE_DIR"


# Otherwise the prompt ends on the same line
printf "\n"