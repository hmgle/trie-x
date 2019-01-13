TARGET = _build/default/bin/trie_demo

all:: $(TARGET)

_build/default/bin/trie_demo: src/trie.erl src/trie_demo.erl src/trie_demo.app.src
	rebar3 escriptize

run:: $(TARGET)
	_build/default/bin/trie_demo ../../data/google-10000-english.txt ../../data/bad-words-en.txt ../../data/keyword-pool.txt

clean::
	-rm -rf _build

