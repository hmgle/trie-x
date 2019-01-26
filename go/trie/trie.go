package trie

import "fmt"

type Trie struct {
	ChildRen map[rune]*Trie
	Val      int
}

func New() *Trie { return &Trie{} }

func (t *Trie) Insert(word string, val int) {
	if word == "" {
		t.Val = val
		return
	}
	chars := []rune(word)
	if child, ok := t.ChildRen[chars[0]]; ok {
		child.Insert(string(chars[1:]), val)
	} else {
		newChild := New()
		newChild.Insert(string(chars[1:]), val)
		t.ChildRen[chars[0]] = newChild
	}
}

func (t *Trie) Lookup(word string) (val int, err error) {
	if word == "" {
		return t.Val, fmt.Errorf("null")
	}
	chars := []rune(word)
	if child, ok := t.ChildRen[chars[0]]; ok {
		return child.Lookup(string(chars[1:]))
	}
	return 0, fmt.Errorf("undefined")
}
