
# The compiler to use. Only GCC and TCC have been tested.
export ONYX_CC='gcc'

# The architecture of your system. If your not sure, leave this alone.
export ONYX_ARCH="$(uname -m)"

export ONYX_RUNTIME_LIBRARY="ovmwasm"
# export ONYX_RUNTIME_LIBRARY=":libwasmer.a"

export ONYX_INCLUDE_DIR="$(pwd)/shared/include"
export ONYX_LIBRARY_DIR="$(pwd)/shared/lib/linux_$ONYX_ARCH/lib"
# export ONYX_LIBRARY_DIR="$(wasmer config --libdir)"

# Enable Dynamic call
export ONYX_USE_DYNCALL=1
