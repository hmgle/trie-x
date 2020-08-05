defmodule TrieTest do
  use ExUnit.Case
  doctest Trie

  test "new trie" do
    assert Trie.new() == %Trie{children: %{}, data: nil}
  end
end
