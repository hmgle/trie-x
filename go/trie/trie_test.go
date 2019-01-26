package trie

import (
	"testing"
)

func TestTrie(t *testing.T) {
	trie := New()
	trie.Insert("abc", 1)
	trie.Insert("bcx", 2)
	trie.Insert("汉字", 3)
	trie.Insert("汉服", 4)

	val, err := trie.Lookup("bcx")
	t.Logf("val('bcx'): %v, err: %v\n", val, err)
	if err != nil {
		t.Fail()
	}
	val, err = trie.Lookup("ab")
	t.Logf("val('ab'): %v, err: %v\n", val, err)
	if err == nil {
		t.Fail()
	}
	val, err = trie.Lookup("汉服")
	t.Logf("val('汉服'): %v, err: %v\n", val, err)
	if err != nil {
		t.Fail()
	}
}
