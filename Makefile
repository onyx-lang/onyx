OBJ_FILES=\
	onyx.o

CC=gcc
INCLUDES=
LIBS=
FLAGS=-g

%.o: %.c
	$(CC) $(FLAGS) -c $< -o $@ $(INCLUDES)

onyx: $(OBJ_FILES)
	$(CC) $(FLAGS) $< -o $@ $(LIBS)

all: onyx clean
