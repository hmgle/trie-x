test::
	$(MAKE) -C erlang/trie run
	$(MAKE) -C go/trie test
	$(MAKE) -C elixir/trie test

test_go::
	$(MAKE) -C go/trie test

test_erlang::
	$(MAKE) -C erlang/trie run

test_elixir::
	$(MAKE) -C elixir/trie test
