-module(trie_demo).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(_) ->
    T = trie:insert(trie:new(), "abc", 1),
    T2 = trie:insert(T, "acd", 2),
    io:format("~p~n", [trie:lookup(T2, "acd")]),
    io:format("~p~n", [trie:traversal(T2)]),
    io:format("~p~n", [trie:expand(T2, "a", 1)]),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================
