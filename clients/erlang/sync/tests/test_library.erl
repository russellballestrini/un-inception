#!/usr/bin/env escript
%% -*- erlang -*-
%%
%% Unit Tests for Un Erlang SDK Library Functions
%%
%% Run with: escript test_library.erl
%% No credentials required - tests pure library functions only.

-mode(compile).

main([]) ->
    io:format("\n\033[34m=== Un Erlang SDK Library Tests ===\033[0m\n\n"),

    Tests = [
        {"version", fun test_version/0},
        {"detect_language", fun test_detect_language/0},
        {"hmac_sign", fun test_hmac_sign/0},
        {"hmac_sign_deterministic", fun test_hmac_sign_deterministic/0},
        {"hmac_sign_different_secrets", fun test_hmac_sign_different_secrets/0}
    ],

    Results = run_tests(Tests, []),
    {Passed, Failed} = count_results(Results, 0, 0),
    Total = length(Results),

    io:format("\n\033[34mResults: ~p/~p passed\033[0m\n", [Passed, Total]),
    case Failed > 0 of
        true ->
            io:format("\033[31m~p test(s) failed\033[0m\n", [Failed]),
            halt(1);
        false ->
            io:format("\033[32mAll tests passed!\033[0m\n")
    end.

run_tests([], Acc) ->
    lists:reverse(Acc);
run_tests([{Name, TestFn} | Rest], Acc) ->
    Result = try
        TestFn(),
        io:format("\033[32mPASS\033[0m: ~s\n", [Name]),
        pass
    catch
        _:Error ->
            io:format("\033[31mFAIL\033[0m: ~s - ~p\n", [Name, Error]),
            fail
    end,
    run_tests(Rest, [Result | Acc]).

count_results([], Passed, Failed) ->
    {Passed, Failed};
count_results([pass | Rest], Passed, Failed) ->
    count_results(Rest, Passed + 1, Failed);
count_results([fail | Rest], Passed, Failed) ->
    count_results(Rest, Passed, Failed + 1).

%% ============================================================================
%% Unit Tests
%% ============================================================================

test_version() ->
    Version = un:version(),
    true = is_list(Version),
    %% Should be semver format X.Y.Z
    [_Major, _Minor, _Patch] = string:tokens(Version, "."),
    ok.

test_detect_language() ->
    %% Test common extensions
    {ok, "python"} = un:ext_to_lang(".py"),
    {ok, "javascript"} = un:ext_to_lang(".js"),
    {ok, "go"} = un:ext_to_lang(".go"),
    {ok, "rust"} = un:ext_to_lang(".rs"),
    {ok, "c"} = un:ext_to_lang(".c"),
    {ok, "cpp"} = un:ext_to_lang(".cpp"),
    {ok, "java"} = un:ext_to_lang(".java"),
    {ok, "ruby"} = un:ext_to_lang(".rb"),
    {ok, "bash"} = un:ext_to_lang(".sh"),
    {ok, "lua"} = un:ext_to_lang(".lua"),
    {ok, "perl"} = un:ext_to_lang(".pl"),
    {ok, "php"} = un:ext_to_lang(".php"),
    {ok, "haskell"} = un:ext_to_lang(".hs"),
    {ok, "ocaml"} = un:ext_to_lang(".ml"),
    {ok, "elixir"} = un:ext_to_lang(".ex"),
    {ok, "erlang"} = un:ext_to_lang(".erl"),

    %% Test unknown extensions
    {error, _} = un:ext_to_lang(".unknown"),
    {error, _} = un:ext_to_lang(""),
    ok.

test_hmac_sign() ->
    Signature = un:hmac_sign("my_secret", "test message"),
    true = is_list(Signature),
    64 = length(Signature),
    %% Should be lowercase hex
    true = lists:all(fun(C) -> (C >= $0 andalso C =< $9) orelse (C >= $a andalso C =< $f) end, Signature),
    ok.

test_hmac_sign_deterministic() ->
    Sig1 = un:hmac_sign("test_secret", "same message"),
    Sig2 = un:hmac_sign("test_secret", "same message"),
    Sig1 = Sig2,  %% Pattern match ensures equality
    ok.

test_hmac_sign_different_secrets() ->
    Sig1 = un:hmac_sign("secret1", "test message"),
    Sig2 = un:hmac_sign("secret2", "test message"),
    true = Sig1 =/= Sig2,  %% Different secrets should produce different signatures
    ok.
