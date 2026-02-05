#!/usr/bin/env escript
%% -*- erlang -*-
%%
%% Functional Tests for Un Erlang SDK
%%
%% Run with: escript test_functional.erl
%% Requires: UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY environment variables
%%
%% These tests make real API calls to api.unsandbox.com

-mode(compile).

main([]) ->
    io:format("\n\033[34m=== Un Erlang SDK Functional Tests ===\033[0m\n\n"),

    %% Check for credentials
    case {os:getenv("UNSANDBOX_PUBLIC_KEY"), os:getenv("UNSANDBOX_SECRET_KEY")} of
        {false, _} ->
            io:format("\033[33mSKIP: UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY not set\033[0m\n"),
            halt(0);
        {_, false} ->
            io:format("\033[33mSKIP: UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY not set\033[0m\n"),
            halt(0);
        _ ->
            ok
    end,

    Tests = [
        {"health_check", fun test_health_check/0},
        {"validate_keys", fun test_validate_keys/0},
        {"execute_python", fun test_execute_python/0},
        {"execute_with_error", fun test_execute_with_error/0},
        {"session_list", fun test_session_list/0},
        {"service_list", fun test_service_list/0},
        {"snapshot_list", fun test_snapshot_list/0},
        {"image_list", fun test_image_list/0},
        {"get_languages", fun test_get_languages/0}
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
            io:format("\033[32mAll functional tests passed!\033[0m\n")
    end.

run_tests([], Acc) ->
    lists:reverse(Acc);
run_tests([{Name, TestFn} | Rest], Acc) ->
    io:format("  Running ~s... ", [Name]),
    Result = try
        TestFn(),
        io:format("\033[32mPASS\033[0m\n"),
        pass
    catch
        _:Error ->
            io:format("\033[31mFAIL\033[0m\n"),
            io:format("    ~p\n", [Error]),
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
%% Functional Tests
%% ============================================================================

test_health_check() ->
    Result = un:health_check(),
    true = is_boolean(Result),
    ok.

test_validate_keys() ->
    KeyInfo = un:validate_keys(),
    true = is_map(KeyInfo),
    true = maps:is_key(valid, KeyInfo),
    true = is_boolean(maps:get(valid, KeyInfo)),
    ok.

test_execute_python() ->
    Result = un:execute("python", "print(6 * 7)"),
    true = is_map(Result),
    true = maps:is_key(success, Result),
    true = maps:is_key(stdout, Result),
    true = maps:is_key(exit_code, Result),

    %% Check output
    true = maps:get(success, Result),
    Stdout = maps:get(stdout, Result),
    true = string:find(Stdout, "42") =/= nomatch,
    0 = maps:get(exit_code, Result),
    ok.

test_execute_with_error() ->
    Result = un:execute("python", "import sys; sys.exit(1)"),
    true = is_map(Result),
    false = maps:get(success, Result),
    1 = maps:get(exit_code, Result),
    ok.

test_session_list() ->
    Response = un:session_list(),
    true = is_list(Response),
    %% Response should be valid JSON (starts with [ or {)
    Trimmed = string:trim(Response),
    true = (string:prefix(Trimmed, "[") =/= nomatch) orelse (string:prefix(Trimmed, "{") =/= nomatch),
    ok.

test_service_list() ->
    Response = un:service_list(),
    true = is_list(Response),
    Trimmed = string:trim(Response),
    true = (string:prefix(Trimmed, "[") =/= nomatch) orelse (string:prefix(Trimmed, "{") =/= nomatch),
    ok.

test_snapshot_list() ->
    Response = un:snapshot_list(),
    true = is_list(Response),
    Trimmed = string:trim(Response),
    true = (string:prefix(Trimmed, "[") =/= nomatch) orelse (string:prefix(Trimmed, "{") =/= nomatch),
    ok.

test_image_list() ->
    Response = un:image_list(),
    true = is_list(Response),
    Trimmed = string:trim(Response),
    true = (string:prefix(Trimmed, "[") =/= nomatch) orelse (string:prefix(Trimmed, "{") =/= nomatch),
    ok.

test_get_languages() ->
    Languages = un:get_languages(),
    true = is_list(Languages),
    true = length(Languages) > 0,
    true = lists:member("python", Languages),
    true = lists:member("javascript", Languages),
    ok.
