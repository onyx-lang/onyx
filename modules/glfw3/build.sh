#!/bin/sh
# To be run from the project root

gcc -O2 -shared -fPIC ./modules/glfw3/onyx_glfw3.c -I include -I lib/linux_x86_64/include -lglfw -o modules/glfw3/onyx_glfw3.so