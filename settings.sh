
# The compiler to use. Only GCC and TCC have been tested.
export ONYX_CC='gcc'

# The architecture of your system. If your not sure, leave this alone.
export ONYX_ARCH="$(uname | tr '[:upper:]' '[:lower:]')_$(uname -m)"

export ONYX_RUNTIME_LIBRARY="ovmwasm"
# export ONYX_RUNTIME_LIBRARY="wasmer"

# Enable Dynamic call
export ONYX_USE_DYNCALL=1
