defmodule Trie do
  @moduledoc """
  Documentation for `Trie`.
  """

  @doc """
  Create the `Trie` data structure.

  ## Examples

      iex> Trie.new()
      %Trie{children: %{}, data: nil}

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
      0 ->
        case data do
          nil -> kvs
          _ -> kvs ++ [{path, data}]
        end
      _ ->
        children = trie[:children]
        case data do
          nil ->
            List.foldl(Map.keys(children), kvs, fn child, ret ->
              ret ++ traversal(children[child], path ++ [child], '')
            end)
          _ ->
            List.foldl(Map.keys(children), [{path, data} | kvs], fn child, ret ->
              ret ++ traversal(children[child], path ++ [child], '')
            end)
        end
    end
  end

  def traversal_limit(trie, max_cnt) do
    traversal_limit(trie, '', '', max_cnt, 0)
  end
  defp traversal_limit(_, _, kvs, max_cnt, cnt) when cnt >= max_cnt do
    {kvs, cnt}
  end
  defp traversal_limit(trie, path, kvs, max_cnt, cnt) do
    data = trie[:data]
    case map_size(trie[:children]) do
      0 ->
        case data do
          nil -> {kvs, cnt}
          _ -> {kvs ++ [{path, data}], cnt + 1}
        end
      _ ->
        children = trie[:children]
        case data do
          nil ->
            cnt2 = cnt; kvs2 = kvs
            List.foldl(Map.keys(children), {kvs2, cnt2}, fn child, ret ->
              next = traversal_limit(children[child], path ++ [child], '', max_cnt, :erlang.element(2, ret))
              {:erlang.element(1, ret) ++ :erlang.element(1, next), :erlang.element(2, next)}
            end)
          _ ->
            cnt2 = cnt + 1; kvs2 = [{path, data} | kvs]
            List.foldl(Map.keys(children), {kvs2, cnt2}, fn child, ret ->
              next = traversal_limit(children[child], path ++ [child], '', max_cnt, :erlang.element(2, ret))
              {:erlang.element(1, ret) ++ :erlang.element(1, next), :erlang.element(2, next)}
            end)
        end
    end
  end

  def expand(trie, prefix, n) do
    expand(trie, prefix, n, prefix)
  end

  defp add_prefix(ret, prefix) do
    case ret do
      :undefined ->
        :undefined
      {kvs, cnt} ->
        kvs2 = Enum.map(kvs, fn {k, v} -> {prefix ++ k, v} end)
        {kvs2, cnt}
      _ ->
        :undefined
    end
  end

  defp expand(trie, '', n, prefix) do
    add_prefix(traversal_limit(trie, n), prefix)
  end
  defp expand(trie, [k1 | kpai], n, prefix) do
    child = trie[:children]
    case Map.has_key?(child, k1) do
      true -> expand(child[k1], kpai, n, prefix)
      false -> :undefined
    end
  end

  def scan_content(content, trie) do
    case content do
      [] -> ''
      [H | T] -> Enum.reverse(scan_content([H], T, 0, trie, [], 0))
    end
  end
  defp scan_content(k, '', _, trie, ret, offset) do
    case lookup(trie, k) do
      :undefined -> ret
      nil -> ret
      data -> [{{k, data}, offset} | ret]
    end
  end
  defp scan_content(k, remain, offset_remain, trie, ret, offset) when length(remain) > offset_remain do
    case lookup(trie, k) do
      :undefined ->
        [remain_h | remain_t] = remain
        scan_content([remain_h], remain_t, 0, trie, ret, offset + 1)
      nil ->
        scan_content(k ++ [Enum.at(remain, offset_remain)], remain, offset_remain + 1, trie, ret, offset)
      data ->
        ret2 = [{{k, data}, offset} | ret]
        scan_content(k ++ [Enum.at(remain, offset_remain)], remain, offset_remain + 1, trie, ret2, offset)
    end
  end
  defp scan_content(k, remain, _offset_remain, trie, ret, offset) do
    [remain_h | remain_t] = remain
    case lookup(trie, k) do
      :undefined ->
        scan_content([remain_h], remain_t, 0, trie, ret, offset + 1)
      nil ->
        scan_content([remain_h], remain_t, 0, trie, ret, offset + 1)
      data ->
        ret2 = [{{k, data}, offset} | ret]
        scan_content([remain_h], remain_t, 0, trie, ret2, offset + 1)
    end
  end
end
