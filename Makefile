RELEASE=1
TIME=0

OBJ_FILES=\
	build/onyxlex.o \
	build/onyxparser.o \
	build/onyxclone.o \
	build/onyxtypes.o \
	build/onyxbuiltins.o \
	build/onyxentities.o \
	build/onyxsempass.o \
	build/onyxsymres.o \
	build/onyxchecker.o \
	build/onyxerrors.o \
	build/onyxutils.o \
	build/onyxwasm.o \
	build/onyxdoc.o \
	build/onyx.o

ifeq (, $(shell which tcc))
	CC=gcc
else
ifeq ($(RELEASE), 0)
	CC=gcc
else
	CC=tcc
endif
endif

ifeq ($(TIME), 1)
	TIMEFLAG=-DREPORT_TIMES=1
endif

INCLUDES=-I./include
LIBS=
TARGET=./onyx

# These aren't working yet
INSTALL_FOLDER=/usr/share/onyx
DEFINES=-DCORE_INSTALLATION=$(INSTALL_FOLDER)

ifeq ($(RELEASE), 1)
	FLAGS=-O3
else
	FLAGS=-g3
endif

build/%.o: src/%.c include/bh.h
	$(CC) $(TIMEFLAG) $(FLAGS) -c $< -o $@ $(INCLUDES)

$(TARGET): $(OBJ_FILES)
	$(CC) $(TIMEFLAG) $(FLAGS) $(OBJ_FILES) -o $@ $(LIBS)

install: $(TARGET) core/*
	cp $(TARGET) /usr/bin/
	cp -r core/ $(INSTALL_FOLDER)/

install_syntax: misc/onyx.vim misc/onyx.sublime-syntax
	cp ./misc/onyx_compiler.vim /usr/share/vim/vim82/compiler/onyx.vim
	cp ./misc/onyx.vim /usr/share/vim/vim82/syntax/onyx.vim
	cp ./misc/onyx.sublime-syntax /home/brendan/.config/sublime-text-3/Packages/User/onyx.sublime-syntax
	cp ./misc/onyx.sublime-build /home/brendan/.config/sublime-text-3/Packages/User/Onyx.sublime-build

clean:
	rm -f $(OBJ_FILES) 2>&1 >/dev/null

all: onyx
