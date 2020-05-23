OBJ_FILES=\
	onyxlex.o \
	onyxparser.o \
	onyxmsgs.o \
	onyxutils.o \
	onyx.o

CC=gcc
INCLUDES=
LIBS=
FLAGS=-g

%.o: %.c bh.h
	$(CC) $(FLAGS) -c $< -o $@ $(INCLUDES)

onyx: $(OBJ_FILES)
	$(CC) $(FLAGS) $? -o $@ $(LIBS)

clean:
	rm $(OBJ_FILES) 2>&1 >/dev/null

all: onyx clean
