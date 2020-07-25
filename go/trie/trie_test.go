package trie

import (
	"bufio"
	"os"
	"testing"
)

func buildTrieFromPath(path string) (tr *Trie, err error) {
	f, err := os.Open(path)
	if err != nil {
		return nil, err
	}
	defer f.Close()
	tr = New()
	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		tr.Insert(scanner.Text(), 1)
	}
	if err := scanner.Err(); err != nil {
		return tr, err
	}
	return
}

func TestTrie(t *testing.T) {
	enWordTrie, _ := buildTrieFromPath("../../data/google-10000-english.txt")
	badWordTrie, _ := buildTrieFromPath("../../data/bad-words-en.txt")
	senWordTrie, _ := buildTrieFromPath("../../data/keyword-pool.txt")

	expandTr := enWordTrie.Expand("qu", 10)
	t.Logf("words of begin with `qu`, limit 10: \n")
	for k := range expandTr {
		t.Logf("%s\n", k)
	}
	content := "近期发现为数不少的网络评论员及各大媒体网站删帖部门的工作人员"
	hits := senWordTrie.ScanContent(content)
	t.Logf("\n*sensitive words*: %s\n", content)
	for _, hit := range hits {
		t.Logf("%+v\n", hit)
	}

	contentEn := "What does lemon party mean? In a brief filed ahead of oral arguments, the state argued that the law does not ban 18- to 20-year-old women from erotic dancing, which is protected under the First Amendment."
	hits = badWordTrie.ScanContent(contentEn)
	t.Logf("\n*sensitive words*: %s\n", contentEn)
	for _, hit := range hits {
		t.Logf("%+v\n", hit)
	}
}
