-module(trie_demo).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main([EnglishWordsPath, BadWordsPath, SensitiveWordsPath]) ->
    EnWordTrie = build_trie_from_filename(EnglishWordsPath),
    BadWordTrie = build_trie_from_filename(BadWordsPath),
    SenWordTrie = build_trie_from_filename(SensitiveWordsPath),

    io:format("value of `tree`: ~p~n", [trie:lookup(EnWordTrie, "tree")]),

    ExpandTr = trie:expand(EnWordTrie, "qu", 10),
    io:format("words of begin with `qu`, limit 10: ~p~n", [ExpandTr]),

    Content = "近期发现为数不少的网络评论员及各大媒体网站删帖部门的工作人员",
    SenRet = trie:scan_content(Content, SenWordTrie),
    io:format("sensitive words: ~w~n", [SenRet]),

    erlang:halt(0);
main(_) ->
    io:format("usage: trie_demo EnglishWordsPath BadWordsPath SensitiveWordsPath\n"),
    erlang:halt(1).

%%====================================================================
%% Internal functions
%%====================================================================

build_trie_from_filename(Filename) ->
    case file:open(Filename, [read]) of
        {ok, File} ->
            file2trie(File, trie:new());
        {error, Reason} ->
            io:format("open ~p error: ~p~n", [Filename, Reason]),
            erlang:halt(1)
    end.

file2trie(File, T) ->
    case file:read_line(File) of
        {ok, Line} ->
            L2 = unicode:characters_to_list(iolist_to_binary(string:trim(Line))),
            T2 = trie:insert(T, L2, 1),
            file2trie(File, T2);
        _Eof ->
            T
    end.

