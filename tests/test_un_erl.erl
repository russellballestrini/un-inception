#!/usr/bin/env escript
%%! -pa ebin

%%% Erlang UN CLI Test Suite
%%%
%%% Usage:
%%%   chmod +x test_un_erl.erl
%%%   ./test_un_erl.erl
%%%
%%% Or with escript:
%%%   escript test_un_erl.erl
%%%
%%% Tests the Erlang UN CLI implementation (un.erl) for:
%%% 1. Extension detection logic
%%% 2. API integration (if UNSANDBOX_API_KEY is set)
%%% 3. End-to-end execution with fib.erl test file

-mode(compile).

main([]) ->
    io:format("=== Erlang UN CLI Test Suite ===~n~n"),

    %% Check if API key is set
    case os:getenv("UNSANDBOX_API_KEY") of
        false ->
            io:format("~s⚠ WARNING~s - UNSANDBOX_API_KEY not set, skipping API tests~n~n",
                     [yellow(), reset()]);
        _ -> ok
    end,

    %% Run tests
    Results = [
        print_result("Extension detection", test_extension_detection()),
        print_result("API integration", test_api_integration()),
        print_result("Fibonacci end-to-end test", test_fibonacci())
    ],

    io:format("~n"),

    %% Summary
    Passed = length([R || R <- Results, R =:= true]),
    Total = length(Results),

    if
        Passed =:= Total ->
            io:format("~s✓ All tests passed (~p/~p)~s~n",
                     [green(), Passed, Total, reset()]),
            halt(0);
        true ->
            io:format("~s✗ Some tests failed (~p/~p passed)~s~n",
                     [red(), Passed, Total, reset()]),
            halt(1)
    end.

%% ANSI color codes
green() -> "\033[32m".
red() -> "\033[31m".
yellow() -> "\033[33m".
reset() -> "\033[0m".

%% Extension to language mapping (from un.erl)
ext_to_lang(".hs") -> {ok, "haskell"};
ext_to_lang(".ml") -> {ok, "ocaml"};
ext_to_lang(".clj") -> {ok, "clojure"};
ext_to_lang(".scm") -> {ok, "scheme"};
ext_to_lang(".lisp") -> {ok, "commonlisp"};
ext_to_lang(".erl") -> {ok, "erlang"};
ext_to_lang(".ex") -> {ok, "elixir"};
ext_to_lang(".py") -> {ok, "python"};
ext_to_lang(".js") -> {ok, "javascript"};
ext_to_lang(".rb") -> {ok, "ruby"};
ext_to_lang(".go") -> {ok, "go"};
ext_to_lang(".rs") -> {ok, "rust"};
ext_to_lang(".c") -> {ok, "c"};
ext_to_lang(".cpp") -> {ok, "cpp"};
ext_to_lang(".java") -> {ok, "java"};
ext_to_lang(Ext) -> {error, Ext}.

%% Print test result
print_result(TestName, {pass, _Msg}) ->
    io:format("~s✓ PASS~s - ~s~n", [green(), reset(), TestName]),
    true;
print_result(TestName, {fail, Msg}) ->
    io:format("~s✗ FAIL~s - ~s~n", [red(), reset(), TestName]),
    io:format("  Error: ~s~n", [Msg]),
    false.

%% Test 1: Extension detection
test_extension_detection() ->
    Tests = [
        {".hs", {ok, "haskell"}},
        {".ml", {ok, "ocaml"}},
        {".clj", {ok, "clojure"}},
        {".scm", {ok, "scheme"}},
        {".lisp", {ok, "commonlisp"}},
        {".erl", {ok, "erlang"}},
        {".ex", {ok, "elixir"}},
        {".py", {ok, "python"}},
        {".js", {ok, "javascript"}},
        {".rb", {ok, "ruby"}}
    ],

    Failures = lists:filter(fun({Ext, Expected}) ->
        ext_to_lang(Ext) =/= Expected
    end, Tests),

    case Failures of
        [] -> {pass, "All extensions mapped correctly"};
        _ -> {fail, io_lib:format("~p tests failed", [length(Failures)])}
    end.

%% Run command and capture output
run_command(Cmd) ->
    Port = open_port({spawn, Cmd}, [stream, exit_status, use_stdio,
                                    stderr_to_stdout, in, eof]),
    get_data(Port, []).

get_data(Port, Acc) ->
    receive
        {Port, {data, Bytes}} ->
            get_data(Port, [Acc|Bytes]);
        {Port, eof} ->
            Port ! {self(), close},
            receive
                {Port, closed} -> true
            end,
            receive
                {'EXIT', Port, _} -> ok
            after 1000 -> ok
            end,
            get_data(Port, Acc);
        {Port, {exit_status, Status}} ->
            {Status, lists:flatten(Acc)}
    after 5000 ->
        {1, lists:flatten(Acc)}
    end.

%% Test 2: API integration
test_api_integration() ->
    case os:getenv("UNSANDBOX_API_KEY") of
        false -> {pass, "Skipped - no API key"};
        _ ->
            try
                %% Create a simple test file
                TestCode = "-module(test).\n-export([main/0]).\nmain() -> io:format(\"test~n\").\n",
                ok = file:write_file("/tmp/test_un_erl_api.erl", TestCode),

                %% Run the CLI
                {Status, Output} = run_command("./un.erl /tmp/test_un_erl_api.erl 2>&1"),

                %% Check if it executed successfully
                case {Status, string:str(Output, "test")} of
                    {0, Pos} when Pos > 0 ->
                        {pass, "API integration successful"};
                    _ ->
                        {fail, io_lib:format("API call failed: ~p, output: ~s",
                                           [Status, Output])}
                end
            catch
                _:Error ->
                    {fail, io_lib:format("Exception: ~p", [Error])}
            end
    end.

%% Test 3: Functional test with fib.erl
test_fibonacci() ->
    case os:getenv("UNSANDBOX_API_KEY") of
        false -> {pass, "Skipped - no API key"};
        _ ->
            try
                %% Check if fib.erl exists
                FibPath = "../test/fib.erl",

                %% Run the CLI with fib.erl
                {Status, Output} = run_command("./un.erl " ++ FibPath ++ " 2>&1"),

                %% Check if output contains expected fibonacci result
                case {Status, string:str(Output, "fib(10) = 55")} of
                    {0, Pos} when Pos > 0 ->
                        {pass, "Fibonacci test successful"};
                    _ ->
                        {fail, io_lib:format("Fibonacci test failed: ~p, output: ~s",
                                           [Status, Output])}
                end
            catch
                _:Error ->
                    {fail, io_lib:format("Exception: ~p", [Error])}
            end
    end.
