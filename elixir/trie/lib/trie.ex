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
        List.foldl(Map.keys(children), kvs2, fn child, ret ->
          ret ++ traversal(children[child], path ++ [child], '')
        end)
    end
  end

  def traversal_limit(trie, max_cnt) do
  end
  defp traversal_limit(_, _, kvs, max_cnt, cnt) when cnt >= max_cnt do
    {kvs, cnt}
  end
  defp traversal_limit(trie, path, kvs, max_cnt, cnt) do
    data = trie[data]
    case map_size(trie[children]) do
      0 ->
        case data do
          nil -> {kvs, cnt}
          _ -> {kvs ++ [{path, data}], cnt + 1}
        end
      _ ->
        case data do
          nil -> cnt2 = cnt; kvs2 = kvs
          _ -> cnt2 = cnt + 1; kvs2 = [{path, data} | kvs]
        end
        children = trie[children]
        List.foldl(Map.keys(children), {kvs2, cnt2}, fn child, ret ->
          next = traversal_limit(children[child], path ++ [child], '', max_cnt, :erlang.element(2, ret))
          {:erlang.element(1, ret) ++ :erlang.element(1, next), :erlang.element(2, next)}
        end)
    end
  end
end
