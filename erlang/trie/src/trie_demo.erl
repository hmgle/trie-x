-module(trie_demo).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Filename) ->
    case file:open(Filename, [read]) of
        {ok, File} ->
            T = file2trie(File, trie:new()),
            Prefix_tr = trie:expand(T, "tree", 20),
            io:format("tr...: ~p~n", [Prefix_tr]);
        {error, Reason} ->
            io:format("open ~p error: ~p~n", [Filename, Reason])
    end,
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

file2trie(File, T) ->
    case file:read_line(File) of
        {ok, Line} ->
            T2 = trie:insert(T, string:trim(Line), 1),
            file2trie(File, T2);
        _Eof ->
            T
    end.

