defmodule Trie do
  @moduledoc """
  Documentation for `Trie`.
  """

  @doc """
  Hello world.

  ## Examples

      iex> Trie.hello()
      :world

  """
  defstruct children: %{}, data: nil

  def new do
    %Trie{}
  end

  def insert(trie, [], data) do
    Map.put(trie, :data, data)
  end
  def insert(trie, [k1 | kpai], data) do
    child = trie[:children]
    case Map.has_key?(child, k1) do
      true -> Map.put(trie, k1, insert(child[k1], kpai, data))
      false -> Map.put(trie, k1, insert(new(), kpai, data))
    end
  end

  def lookup(trie, []) do
    trie[:data]
  end
  def lookup(trie, [k1 | kpai]) do
    child = trie[:children]
    case Map.has_key?(child, k1) do
      true -> lookup(child[k1], kpai)
      false -> :undefined
    end
  end
end
