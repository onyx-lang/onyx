OBJ_FILES=\
	src/onyxlex.o \
	src/onyxparser.o \
	src/onyxmsgs.o \
	src/onyxutils.o \
	src/onyxwasm.o \
	src/onyx.o

CC=gcc
INCLUDES=-I./include
LIBS=
FLAGS=-g

%.o: %.c include/bh.h
	$(CC) $(FLAGS) -c $< -o $@ $(INCLUDES)

onyx: $(OBJ_FILES)
	$(CC) $(FLAGS) $? -o $@ $(LIBS)

clean:
	rm $(OBJ_FILES) 2>&1 >/dev/null

all: onyx clean
