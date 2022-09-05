
# The base path for the installation.
# This is typcially /usr or /usr/local.
INSTALL_DIR="/usr"

# Where the core libraries for Onyx will go. 
CORE_DIR="$INSTALL_DIR/share/onyx"

# Where the onyx executable will be placed.
BIN_DIR="$INSTALL_DIR/bin"

# The compiler to use. Only GCC and TCC have been tested.
CC='gcc'

# The architecture of your system. If your not sure, leave this alone.
ARCH="$(uname -m)"

RUNTIME_LIBRARY="ovmwasm"
# RUNTIME_LIBRARY="wasmer"

# Where the Wasmer library files can be found.
# They are bundled with the project, but if a different version is available, these can be changed.
WASMER_INCLUDE_DIR="$(pwd)/shared/include"
WASMER_LIBRARY_DIR="$(pwd)/shared/lib/linux_$ARCH/lib"
