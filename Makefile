BASE_DIR  = $(shell pwd)
REBAR    ?= $(BASE_DIR)/rebar

top: compile escriptize test

compile:
	$(REBAR) compile
	
escriptize:
	$(REBAR) escriptize

test:
	./ergly tests/hello.erl | diff -u tests/hello.erl -
	./ergly tests/hello2.erl | diff -u tests/hello2.erl -
	./ergly tests/macros.erl | diff -u tests/macros.erl -
