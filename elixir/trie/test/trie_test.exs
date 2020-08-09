defmodule TrieTest do
  use ExUnit.Case
  doctest Trie

  defp build_trie_from_filename(filename) do
    case :file.open(filename, [:read]) do
      {:ok, file} ->
        file2trie(file, Trie.new())
      {:error, reason} ->
        :io.format("open ~p error: ~p~n", [filename, reason])
        :erlang.halt(1)
    end
  end

  defp file2trie(file, t) do
    case :file.read_line(file) do
      {:ok, line} ->
        l2 = :string.trim(:unicode.characters_to_list(:erlang.iolist_to_binary(line)))
        t2 = Trie.insert(t, l2, 1)
        file2trie(file, t2)
      _ ->
        t
    end
  end

  defp print(result) do
    :lists.foreach(fn item ->
      {{sen_str, val}, pos} = item
      :io.format("\t'~ts', val: ~p, position: ~p~n", [sen_str, val, pos])
    end, result)
  end

  test "new trie" do
    assert Trie.new() == %Trie{children: %{}, data: nil}
  end

  test "main" do
    :io.setopts([{:encoding, :unicode}])

    en_word_trie = build_trie_from_filename("../../data/google-10000-english.txt")
    bad_word_trie = build_trie_from_filename("../../data/bad-words-en.txt")
    sen_word_trie = build_trie_from_filename("../../data/keyword-pool.txt")

    :io.format("value of `tree`: ~p~n", [Trie.lookup(en_word_trie, 'tree')])

    expand_tr = Trie.expand(en_word_trie, 'qu', 10)
    :io.format("words of begin with `qu`, limit 10: ~p~n", [expand_tr])

    content = '近期发现为数不少的网络评论员及各大媒体网站删帖部门的工作人员'
    sen_ret = Trie.scan_content(content, sen_word_trie)
    :io.format("~ts~n*sensitive words*:~n", [content])
    print(sen_ret)

    content_en = 'What does lemon party mean? In a brief filed ahead of oral arguments, the state argued that the law does not ban 18- to 20-year-old women from erotic dancing, which is protected under the First Amendment.'
    sen_en_ret = Trie.scan_content(content_en, bad_word_trie)
    :io.format("~ts~n*sensitive words*:~n", [content_en])
    print(sen_en_ret)
    assert Trie.new() == %Trie{children: %{}, data: nil}
  end
end
