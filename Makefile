ERLANG_PATH:=$(shell erl -eval 'io:format("~s~n", [lists:concat([code:root_dir(), "/erts-", erlang:system_info(version), "/include"])])' -s init stop -noshell)
CFLAGS=-g -O3 -fPIC -Isundown/src -Isundown/html
ERLANG_FLAGS=-I$(ERLANG_PATH)
CC=gcc
BUILD_DIR=_build

ifeq ($(shell uname),Darwin)
	OPTIONS=-dynamiclib -undefined dynamic_lookup
endif

ifeq ($(OS),Windows_NT)
	SUNDOWN_MAKE=nmake -f Makefile.win
else
	SUNDOWN_MAKE=make
endif

all: ex_doc

ex_doc:
	mix compile

test:
	mix test

sundown/src:
	git submodule update --init

sundown/libsundown.so: sundown/src
	cd sundown && $(SUNDOWN_MAKE)

SUNDOWN_OBJS=\
	sundown/html/html.o \
	sundown/html/html_smartypants.o \
	sundown/html/houdini_html_e.o \
	sundown/html/houdini_href_e.o \
	sundown/src/buffer.o \
	sundown/src/autolink.o \
	sundown/src/stack.o \
	sundown/src/markdown.o

NIF_SRC=\
	src/markdown_nif.c

clean:
	cd sundown && make clean
	rm -f priv/markdown.so
	rm -rf $(BUILD_DIR)
	rm -rf test/tmp
	@ echo

priv/markdown.so: sundown/libsundown.so ${NIF_SRC}
	$(CC) $(CFLAGS) $(ERLANG_FLAGS) -shared $(OPTIONS) -o $@ $(SUNDOWN_OBJS) $(NIF_SRC)

.PHONY: clean test

