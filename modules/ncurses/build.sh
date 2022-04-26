#!/bin/sh

onyx run scripts/c_binding.onyx modules/ncurses/module.onyx > modules/ncurses/ncurses.c
gcc -shared -fPIC -I include -I lib/common/include -O2 modules/ncurses/ncurses.c -o modules/ncurses/onyx_ncurses.so -lncurses
