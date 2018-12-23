-module(trie).

-export([new/0, insert/3, lookup/2]).

new() ->
    #{children => #{}, data => nil}.

insert(Trie, [], Data) ->
    Trie#{data => Data};

insert(Trie, [K1 | Kpai], Data) ->
    Child = maps:get(children, Trie),
    case maps:find(K1, Child) of
        {ok, Value} ->
            Trie#{children => insert(Value, Kpai, Data)};
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
