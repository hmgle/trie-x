PHONY: test test_go test_erlang test_elixir test_zig

test:
	$(MAKE) -C erlang/trie run
	$(MAKE) -C go/trie test
	$(MAKE) -C elixir/trie test
	$(MAKE) -C zig/ test
	$(MAKE) -C python/trie test

test_go:
	$(MAKE) -C go/trie test

test_erlang:
	$(MAKE) -C erlang/trie run

test_elixir:
	$(MAKE) -C elixir/trie test

test_zig:
	$(MAKE) -C zig/ test

test_python:
	$(MAKE) -C python/trie test
