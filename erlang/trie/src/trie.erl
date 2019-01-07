-module(trie).

%% API exports
-export([new/0, insert/3, lookup/2, traversal/1, traversal_limit/2, expand/3]).

-define(TRIE, ?MODULE).

-record(?TRIE, {children,
                data:: term()}).

-type tree() :: #?TRIE{}.

%%====================================================================
%% API functions
%%====================================================================

-spec new() -> tree().
new() ->
    #{children => #{}, data => nil}.

-spec insert(tree(), list(), term()) -> tree().
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

-spec lookup(tree(), list()) -> term().
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

-spec traversal(tree()) -> list().
traversal(Trie) ->
    traversal(Trie, "", "").


-spec traversal_limit(tree(), number()) -> list().
traversal_limit(Trie, MaxCnt) ->
    traversal_limit(Trie, "", "", MaxCnt, 0).

-spec expand(tree(), list(), number()) -> list().
expand(Trie, [], N) ->
    traversal_limit(Trie, N);
expand(Trie, [K1 | Kpai], N) ->
    Child = maps:get(children, Trie),
    case maps:find(K1, Child) of
        {ok, Value} ->
            expand(Value, Kpai, N);
        error ->
            undefined
    end.


%%====================================================================
%% Internal functions
%%====================================================================

traversal(Trie, Path, Kvs) ->
    Data = maps:get(data, Trie),
    case map_size(maps:get(children, Trie)) of
        0 ->
            case Data of
                nil -> Kvs;
                _ -> Kvs ++ [{Path, Data}]
            end;
        _ ->
            case Data of
                nil -> Kvs2 = Kvs;
                _ ->
                    Kvs2 = [{Path, Data} | Kvs]
            end,
            Children = maps:get(children, Trie),
            lists:foldl(fun(Child, Ret) ->
                                Ret ++ traversal(maps:get(Child, Children), Path ++ [Child], "")
                        end, Kvs2, maps:keys(Children))
    end.


traversal_limit(_, _, Kvs, MaxCnt, Cnt) when Cnt >= MaxCnt ->
    {Kvs, Cnt};
traversal_limit(Trie, Path, Kvs, MaxCnt, Cnt) ->
    Data = maps:get(data, Trie),
    case map_size(maps:get(children, Trie)) of
        0 ->
            case Data of
                nil -> {Kvs, Cnt};
                _ ->
                    {Kvs ++ [{Path, Data}], Cnt + 1}
            end;
        _ ->
            case Data of
                nil ->
                    Cnt2 = Cnt,
                    Kvs2 = Kvs;
                _ ->
                    Cnt2 = Cnt + 1,
                    Kvs2 = [{Path, Data} | Kvs]
            end,
            Children = maps:get(children, Trie),
            lists:foldl(fun(Child, Ret) ->
                                Next = traversal_limit(maps:get(Child, Children), Path ++ [Child], "", MaxCnt, erlang:element(2, Ret)),
                                {erlang:element(1, Ret) ++ erlang:element(1, Next), erlang:element(2, Next)}
                        end, {Kvs2, Cnt2}, maps:keys(Children))
    end.
