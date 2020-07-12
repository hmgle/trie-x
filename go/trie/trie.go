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

func (t *Trie) Traversal(limit ...int) map[string]int {
	ret := make(map[string]int)
	return traversal(t, "", ret, limit...)
}

func traversal(t *Trie, prefix string, tmpRet map[string]int, limit ...int) map[string]int {
	if t == nil {
		return tmpRet
	}
	if len(limit) > 0 && len(tmpRet) >= limit[0] {
		return tmpRet
	}
	if t.Val != 0 {
		tmpRet[prefix] = t.Val
	}
	for k, v := range t.ChildRen {
		tmpRet = traversal(v, prefix+string(k), tmpRet, limit...)
	}
	return tmpRet
}

func (t *Trie) Expand(prefix string, limit ...int) map[string]int {
	return t.expand(prefix, []rune(prefix), limit...)
}

func (t *Trie) expand(originPrefix string, prefix []rune, limit ...int) map[string]int {
	if len(prefix) == 0 {
		tmpRet := make(map[string]int)
		ret := traversal(t, "", tmpRet, limit...)
		retFix := make(map[string]int)
		for k, v := range ret {
			retFix[originPrefix+k] = v
		}
		return retFix
	}
	if child, ok := t.ChildRen[prefix[0]]; ok {
		return child.expand(originPrefix, prefix[1:], limit...)
	}
	return nil
}
