package trie

import "fmt"

type Trie struct {
	ChildRen map[rune]*Trie
	Val      int
}

// New return an empty trie.
func New() *Trie { return &Trie{ChildRen: make(map[rune]*Trie)} }

// Insert a word with val to the trie.
func (t *Trie) Insert(word string, val int) {
	if word == "" {
		t.Val = val
		return
	}
	chars := []rune(word)
	if child, ok := t.ChildRen[chars[0]]; ok {
		child.Insert(string(chars[1:]), val)
	} else {
		t.ChildRen[chars[0]] = New()
		t.ChildRen[chars[0]].Insert(string(chars[1:]), val)
	}
}

// Lookup a word's val from the trie.
func (t *Trie) Lookup(word string) (val int, err error) {
	if word == "" {
		if t.Val != 0 {
			return t.Val, nil
		}
		return t.Val, fmt.Errorf("null")
	}
	chars := []rune(word)
	if child, ok := t.ChildRen[chars[0]]; ok {
		return child.Lookup(string(chars[1:]))
	}
	return 0, fmt.Errorf("undefined")
}

func (t *Trie) Traversal() map[string]int {
	ret := make(map[string]int)
	return traversal(t, "", ret)
}

func traversal(t *Trie, prefix string, tmpRet map[string]int) map[string]int {
	if t == nil {
		return tmpRet
	}
	if t.Val != 0 {
		tmpRet[prefix] = t.Val
	}
	for k, v := range t.ChildRen {
		tmpRet = traversal(v, prefix+string(k), tmpRet)
	}
	return tmpRet
}
