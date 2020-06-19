OBJ_FILES=\
	build/onyxlex.o \
	build/onyxparser.o \
	build/onyxmsgs.o \
	build/onyxutils.o \
	build/onyxwasm.o \
	build/onyx.o

CC=gcc
INCLUDES=-I./include
LIBS=
FLAGS=-g

build/%.o: src/%.c include/bh.h
	$(CC) $(FLAGS) -c $< -o $@ $(INCLUDES)

onyx: $(OBJ_FILES)
	$(CC) $(FLAGS) $(OBJ_FILES) -o $@ $(LIBS)

clean:
	rm -f $(OBJ_FILES) 2>&1 >/dev/null

all: onyx
