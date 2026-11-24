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

case "$(uname)" in
    Linux)  suffix='so' ;;
    *BSD)   suffix='so' ;;
    Darwin) suffix='dylib' ;;
    *)      suffix='dll' ;;
esac

FLAGS=""
LINK_FLAGS=""
if [ "$(uname)" = "Darwin" ]; then
    LINK_FLAGS="$LINK_FLAGS -framework Security"
fi
if [ ! -z ${ONYX_TARGET+x} ]; then
    FLAGS="$FLAGS --target=$ONYX_TARGET"
fi


echo "Compiling onyx_runtime.$suffix"
invoke_cc -shared -fpic -w -O2 \
    -o "onyx_runtime.$suffix" \
    "$FLAGS" "$LINK_FLAGS" \
    -Wno-incompatible-pointer-types \
    -I ../shared/include -I ../compiler/include \
    ./onyx_runtime.c \
    -lpthread

echo "Compiling libonyx_runtime.a"
invoke_cc -c -fpic -w -O2 \
    "$FLAGS" \
    -DONYX_RUNTIME_NO_BH \
    -Wno-incompatible-pointer-types \
    -I ../shared/include -I ../compiler/include \
    -o onyx_runtime.o \
    ./onyx_runtime.c
ar crs libonyx_runtime.a onyx_runtime.o
rm -f onyx_runtime.o
