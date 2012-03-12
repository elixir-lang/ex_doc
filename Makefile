CFLAGS=-g -O3 -fPIC
LDFLAGS=-Isundown/src -Isundown/html
ERLANG_FLAGS=-I/usr/local/Cellar/erlang/R15B/lib/erlang/erts-5.9/include
CC=gcc
EBIN_DIR=ebin

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

.PHONY: setup test clean

compile: ebin

setup: markdown.so

ebin: lib/*.ex lib/*/*.ex lib/*/*/*.ex
	@ rm -f ebin/::*.beam
	@ echo Compiling ...
	@ mkdir -p $(EBIN_DIR)
	@ touch $(EBIN_DIR)
	elixirc lib/*/*/*.ex lib/*/*.ex lib/*.ex -o ebin
	@ echo

compile_test:
	@ rm -f test/tmp/*.beam
	@ elixirc --docs test/fixtures/*.ex -o test/tmp

test: markdown.so compile compile_test
	@ echo Running tests ...
	time elixir -pa test/tmp -pa ebin "test/**/*_test.exs"
	@ echo

clean:
	rm -f sundown/src/*.o sundown/html/*.o src/*.o
	rm share/markdown.so
	rm -rf $(EBIN_DIR)
	@ echo

markdown.so: $(SUNDOWN_SRC) $(NIF_SRC)
	$(CC) $(CFLAGS) $(ERLANG_FLAGS) -dynamiclib -undefined dynamic_lookup -o share/$@ $(SUNDOWN_SRC) $(NIF_SRC)

%.o: %.c
	$(CC) $(CFLAGS) $(LDFLAGS) $(ERLANG_FLAGS) -c -o $@ $^
