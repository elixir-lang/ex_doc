EBIN_DIR=ebin

.PHONY: setup test clean

compile: ebin

ebin: lib/*.ex lib/*/*.ex lib/*/*/*.ex
	@ rm -f ebin/::*.beam
	@ echo Compiling ...
	@ mkdir -p $(EBIN_DIR)
	@ touch $(EBIN_DIR)
	elixirc lib/*/*/*.ex lib/*/*.ex lib/*.ex -o ebin
	@ echo

test: compile
	@ echo Running tests ...
	time elixir -pa ebin "test/**/*_test.exs"
	@ echo

clean:
	rm -rf $(EBIN_DIR)
	@ echo
