#!/bin/sh
# To be run from the project root

onyx run modules/glfw3/build --generate-foreign-info
gcc -O2 -shared -fPIC ./modules/glfw3/onyx_glfw3.c -I include -I lib/common/include -lglfw -o modules/glfw3/onyx_glfw3.so