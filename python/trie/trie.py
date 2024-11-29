from typing import Dict, List, Generator, Optional, Tuple
from itertools import islice
import os


class HitMeta:
    def __init__(self, word: str, val: int, offset: int) -> None:
        self.word = word
        self.val = val
        self.offset = offset

    def __repr__(self) -> str:
        return f"HitMeta(word='{self.word}', val={self.val}, offset={self.offset})"


class Trie:
    def __init__(self) -> None:
        self.children: Dict[str, "Trie"] = {}
        self.value: int = 0

    def insert(self, word: str, value: int) -> None:
        """Insert a word with a specific value into the Trie."""
        if not word:
            self.value = value
            return
        if word[0] not in self.children:
            self.children[word[0]] = Trie()
        self.children[word[0]].insert(word[1:], value)

    def lookup(self, word: str) -> Tuple[int, Optional[str]]:
        """Lookup a word's value in the Trie."""
        if not word:
            return self.value, "null" if self.value == 0 else None

        if word[0] in self.children:
            return self.children[word[0]].lookup(word[1:])
        return 0, "undefined"

    def traversal(self) -> Generator[Tuple[str, int], None, None]:
        """Traverse the Trie and yield all key-value pairs."""

        def _traverse(
            node: Trie, prefix: str
        ) -> Generator[Tuple[str, int], None, None]:
            if node.value != 0:
                yield prefix, node.value
            for char, child in node.children.items():
                yield from _traverse(child, prefix + char)

        yield from _traverse(self, "")

    def expand(
        self,
        prefix: str,
    ) -> Generator[Tuple[str, int], None, None]:
        """
        Recursive generator method to yield key-value pairs starting with the given prefix.
        """

        def find_node(node: Trie, prefix: str) -> Optional[Trie]:
            if not prefix:
                return node
            if prefix[0] in node.children:
                return find_node(node.children[prefix[0]], prefix[1:])
            return None

        node = find_node(self, prefix)
        if node:
            yield from ((prefix + key, value) for key, value in node.traversal())

    def scan_content(self, content: str) -> List[HitMeta]:
        """
        Scans the given content and finds all matching words with their values and positions.
        """
        if not content:
            return []

        hits: List[HitMeta] = []
        self._scan_content(content[:1], content[1:], 0, hits, 0)
        return hits

    def _scan_content(
        self,
        k: str,
        remain: str,
        offset_remain: int,
        ret: List[HitMeta],
        offset: int,
    ) -> None:
        val, err = self.lookup(k)
        if not remain:
            if err is None:  # Valid word found
                ret.append(HitMeta(word=k, val=val, offset=offset))
            return
        if len(remain) > offset_remain:
            if err == "undefined":
                self._scan_content(remain[:1], remain[1:], 0, ret, offset + 1)
            elif err == "null":
                self._scan_content(
                    k + remain[offset_remain], remain, offset_remain + 1, ret, offset
                )
            else:
                ret.append(HitMeta(word=k, val=val, offset=offset))
                self._scan_content(
                    k + remain[offset_remain], remain, offset_remain + 1, ret, offset
                )
        else:
            if err is not None:
                self._scan_content(remain[:1], remain[1:], 0, ret, offset + 1)
            else:
                ret.append(HitMeta(word=k, val=val, offset=offset))
                self._scan_content(remain[:1], remain[1:], 0, ret, offset + 1)

    @staticmethod
    def build_trie_from_path(path: str) -> Optional["Trie"]:
        if not os.path.exists(path):
            raise FileNotFoundError(f"The file at path '{path}' does not exist.")

        trie = Trie()
        try:
            with open(path, "r", encoding="utf-8") as file:
                for line in file:
                    word = line.strip()
                    if word:
                        trie.insert(word, 1)
        except Exception as e:
            raise IOError(f"An error occurred while reading the file: {e}")
        return trie


if __name__ == "__main__":
    enword_trie = Trie.build_trie_from_path("../../data/google-10000-english.txt")
    if enword_trie is None:
        print("Error: Trie not built.")
    else:
        print("words of begin with `qu`, limit 10:")
        for word, _ in islice(enword_trie.expand("qu"), 10):
            print(word)

    senword_trie = Trie.build_trie_from_path("../../data/keyword-pool.txt")
    if senword_trie is None:
        print("Error: Trie not built.")
    else:
        content = "近期发现为数不少的网络评论员及各大媒体网站删帖部门的工作人员"
        hits = senword_trie.scan_content(content)
        print("\nSensitive Words:", content)
        for hit in hits:
            print(hit)

    badword_trie = Trie.build_trie_from_path("../../data/bad-words-en.txt")
    if badword_trie is None:
        print("Error: Trie not built.")
    else:
        content_en = "What does lemon party mean? In a brief filed ahead of oral arguments, the state argued that the law does not ban 18- to 20-year-old women from erotic dancing, which is protected under the First Amendment."
        hits = badword_trie.scan_content(content_en)
        print("\nSensitive Words:", content_en)
        for hit in hits:
            print(hit)
