#!/bin/sh

onyx run modules/ncurses/build.onyx --generate-foreign-info
gcc -shared -fPIC -I include -I lib/common/include -O2 modules/ncurses/ncurses.c -o modules/ncurses/onyx_ncurses.so -lncurses
