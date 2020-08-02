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

  def traversal(trie) do
    traversal(trie, '', '')
  end
  defp traversal(trie, path, kvs) do
    data = trie[:data]
    case map_size(trie[:children]) do
      0 -> case data do
        nil -> kvs
        _ -> kvs ++ [{path, data}]
      end
      _ -> case data do
        nil -> kvs2 = kvs
        _ -> kvs2 = [{path, data} | kvs]
      end
        children = trie[:children]
        ## TODO
    end
  end
end
