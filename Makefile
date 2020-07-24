OBJ_FILES=\
	build/onyxlex.o \
	build/onyxparser.o \
	build/onyxtypes.o \
	build/onyxsempass.o \
	build/onyxsymres.o \
	build/onyxchecker.o \
	build/onyxmsgs.o \
	build/onyxutils.o \
	build/onyxwasm.o \
	build/onyx.o

CC=gcc
INCLUDES=-I./include
LIBS=
FLAGS=-g
TARGET=./onyx

build/%.o: src/%.c include/bh.h
	$(CC) $(FLAGS) -c $< -o $@ $(INCLUDES)

$(TARGET): $(OBJ_FILES)
	$(CC) $(FLAGS) $(OBJ_FILES) -o $@ $(LIBS)

install: $(TARGET)
	cp $(TARGET) /usr/bin/

install_syntax: misc/onyx.vim misc/onyx.sublime-syntax
	cp ./misc/onyx.vim /usr/share/vim/vim82/syntax/onyx.vim
	cp ./misc/onyx.sublime-syntax /home/brendan/.config/sublime-text-3/Packages/User/onyx.sublime-syntax

clean:
	rm -f $(OBJ_FILES) 2>&1 >/dev/null

all: onyx
