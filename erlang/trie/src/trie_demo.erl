-module(trie_demo).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main([EnglishWordsPath, BadWordsPath, SensitiveWordsPath]) ->
    io:setopts([{encoding, unicode}]),
    EnWordTrie = build_trie_from_filename(EnglishWordsPath),
    BadWordTrie = build_trie_from_filename(BadWordsPath),
    SenWordTrie = build_trie_from_filename(SensitiveWordsPath),

    io:format("value of `tree`: ~p~n", [trie:lookup(EnWordTrie, "tree")]),

    ExpandTr = trie:expand(EnWordTrie, "qu", 10),
    io:format("words of begin with `qu`, limit 10: ~p~n", [ExpandTr]),

    Content = "近期发现为数不少的网络评论员及各大媒体网站删帖部门的工作人员",
    SenRet = trie:scan_content(Content, SenWordTrie),
    io:format("~ts~n*sensitive words*:~n", [Content]),
    print(SenRet),

    ContentEn = "What does lemon party mean? In a brief filed ahead of oral arguments, the state argued that the law does not ban 18- to 20-year-old women from erotic dancing, which is protected under the First Amendment.",
    SenEnRet = trie:scan_content(ContentEn, BadWordTrie),
    io:format("~ts~n*sensitive words*:~n", [ContentEn]),
    print(SenEnRet),

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
            L2 = string:trim(unicode:characters_to_list(iolist_to_binary(Line))),
            T2 = trie:insert(T, L2, 1),
            file2trie(File, T2);
        _Eof ->
            T
    end.

print(Result) ->
    lists:foreach(fun(Item) ->
                          {{SenStr, Val}, Pos} = Item,
                          io:format("\t'~ts', val: ~p, position: ~p~n",
                                    [SenStr, Val, Pos])
                  end, Result).
