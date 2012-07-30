ERLANG_PATH:=$(shell erl -eval 'io:format("~s~n", [lists:concat([code:root_dir(), "/erts-", erlang:system_info(version), "/include"])])' -s init stop -noshell)
CFLAGS=-g -O3 -fPIC
LDFLAGS=-Isundown/src -Isundown/html
ERLANG_FLAGS=-I$(ERLANG_PATH)
CC=gcc
EBIN_DIR=ebin

ifeq ($(shell uname),Darwin)
	OPTIONS=-dynamiclib -undefined dynamic_lookup
endif

SUNDOWN_SRC=\
	    sundown/src/buffer.o\
	    sundown/src/markdown.o\
	    sundown/src/stack.o\
	    sundown/src/autolink.o\
	    sundown/html/html.o\
	    sundown/html/html_smartypants.o\
	    sundown/html/houdini_html_e.o\
	    sundown/html/houdini_href_e.o

NIF_SRC=\
	src/markdown_nif.o

.PHONY: clean

clean:
	rm -f sundown/src/*.o sundown/html/*.o src/*.o
	rm -f share/markdown.so
	rm -rf $(EBIN_DIR)
	rm -rf test/tmp
	@ echo

share/markdown.so: $(SUNDOWN_SRC) $(NIF_SRC)
	$(CC) $(CFLAGS) -shared $(OPTIONS) -o $@ $(SUNDOWN_SRC) $(NIF_SRC)

%.o: %.c
	$(CC) $(CFLAGS) $(LDFLAGS) $(ERLANG_FLAGS) -c -o $@ $^