-module(trie).

-export([new/0, insert/3, lookup/2, traversal/1]).

new() ->
    #{children => #{}, data => nil}.

insert(Trie, [], Data) ->
    Trie#{data => Data};

insert(Trie, [K1 | Kpai], Data) ->
    Child = maps:get(children, Trie),
    case maps:find(K1, Child) of
        {ok, Value} ->
            Trie#{children => maps:put(K1, insert(Value, Kpai, Data), Child)};
        error ->
            Trie#{children => maps:put(K1, insert(new(), Kpai, Data), Child)}
    end.


lookup(Trie, []) ->
    maps:get(data, Trie);

lookup(Trie, [K1 | Kpai]) ->
    Child = maps:get(children, Trie),
    case maps:find(K1, Child) of
        {ok, Value} ->
            lookup(Value, Kpai);
        error ->
            undefined
    end.


traversal(Trie) ->
    traversal(Trie, "", "").

traversal(Trie, Path, Kvs) ->
    case map_size(maps:get(children, Trie)) of
        0 ->
            Data = maps:get(data, Trie),
            case Data of
                nil -> Kvs;
                % _ -> [{Path, Data} | Kvs]
                _ -> Kvs ++ [{Path, Data}]
            end;
        _ ->
            case maps:get(data, Trie) of
                nil -> Kvs2 = Kvs;
                % Data -> Kvs2 = [{Path, Data} | Kvs]
                Data -> Kvs2 = Kvs ++ [{Path, Data}]
            end,
            Children = maps:get(children, Trie),
            Childlist = maps:keys(Children),
            lists:flatten([traversal(maps:get(Child, Children), Path ++ [Child], Kvs2) || Child <- Childlist])
    end.
