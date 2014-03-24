BASE_DIR  = $(shell pwd)
REBAR    ?= $(BASE_DIR)/rebar

top:
	$(REBAR) compile escriptize
