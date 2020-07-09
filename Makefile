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

clean:
	rm -f $(OBJ_FILES) 2>&1 >/dev/null

all: onyx
