%% PUBLIC DOMAIN - NO LICENSE, NO WARRANTY
%%
%% This is free public domain software for the public good of a permacomputer hosted
%% at permacomputer.com - an always-on computer by the people, for the people. One
%% which is durable, easy to repair, and distributed like tap water for machine
%% learning intelligence.
%%
%% The permacomputer is community-owned infrastructure optimized around four values:
%%
%%   TRUTH    - Source code must be open source & freely distributed
%%   FREEDOM  - Voluntary participation without corporate control
%%   HARMONY  - Systems operating with minimal waste that self-renew
%%   LOVE     - Individual rights protected while fostering cooperation
%%
%% This software contributes to that vision by enabling code execution across 42+
%% programming languages through a unified interface, accessible to all. Code is
%% seeds to sprout on any abandoned technology.
%%
%% Learn more: https://www.permacomputer.com
%%
%% Anyone is free to copy, modify, publish, use, compile, sell, or distribute this
%% software, either in source code form or as a compiled binary, for any purpose,
%% commercial or non-commercial, and by any means.
%%
%% NO WARRANTY. THE SOFTWARE IS PROVIDED "AS IS" WITHOUT WARRANTY OF ANY KIND.
%%
%% That said, our permacomputer's digital membrane stratum continuously runs unit,
%% integration, and functional tests on all of it's own software - with our
%% permacomputer monitoring itself, repairing itself, with minimal human in the
%% loop guidance. Our agents do their best.
%%
%% Copyright 2025 TimeHexOn & foxhop & russell@unturf
%% https://www.timehexon.com
%% https://www.foxhop.net
%% https://www.unturf.com/software


#!/usr/bin/env escript

%%% Erlang UN CLI - Unsandbox CLI Client
%%%
%%% Full-featured CLI matching un.py capabilities
%%% Uses curl for HTTP (no external dependencies)

main([]) ->
    io:format("Usage: un.erl [options] <source_file>~n"),
    io:format("       un.erl session [options]~n"),
    io:format("       un.erl service [options]~n"),
    halt(1);

main(["session" | Rest]) ->
    session_command(Rest);

main(["service" | Rest]) ->
    service_command(Rest);

main(Args) ->
    execute_command(Args).

%% Execute command
execute_command(Args) ->
    {File, _Opts} = parse_exec_args(Args, #{file => undefined}),
    case File of
        undefined ->
            io:format("Error: No source file specified~n"),
            halt(1);
        _ ->
            ApiKey = get_api_key(),
            Ext = filename:extension(File),
            case ext_to_lang(Ext) of
                {ok, Language} ->
                    case file:read_file(File) of
                        {ok, CodeBin} ->
                            Code = binary_to_list(CodeBin),
                            Json = build_json(Language, Code),
                            TmpFile = write_temp_file(Json),
                            Response = curl_post(ApiKey, "/execute", TmpFile),
                            file:delete(TmpFile),
                            io:format("~s~n", [Response]);
                        {error, Reason} ->
                            io:format("Error reading file: ~p~n", [Reason]),
                            halt(1)
                    end;
                {error, Ext} ->
                    io:format("Error: Unknown extension: ~s~n", [Ext]),
                    halt(1)
            end
    end.

%% Session command
session_command(["--list" | _]) ->
    ApiKey = get_api_key(),
    Response = curl_get(ApiKey, "/sessions"),
    io:format("~s~n", [Response]);

session_command(["--kill", SessionId | _]) ->
    ApiKey = get_api_key(),
    _ = curl_delete(ApiKey, "/sessions/" ++ SessionId),
    io:format("\033[32mSession terminated: ~s\033[0m~n", [SessionId]);

session_command(Args) ->
    ApiKey = get_api_key(),
    Shell = get_shell_opt(Args, "bash"),
    Json = "{\"shell\":\"" ++ Shell ++ "\"}",
    TmpFile = write_temp_file(Json),
    Response = curl_post(ApiKey, "/sessions", TmpFile),
    file:delete(TmpFile),
    io:format("\033[33mSession created (WebSocket required)\033[0m~n"),
    io:format("~s~n", [Response]).

%% Service command
service_command(["--list" | _]) ->
    ApiKey = get_api_key(),
    Response = curl_get(ApiKey, "/services"),
    io:format("~s~n", [Response]);

service_command(["--info", ServiceId | _]) ->
    ApiKey = get_api_key(),
    Response = curl_get(ApiKey, "/services/" ++ ServiceId),
    io:format("~s~n", [Response]);

service_command(["--logs", ServiceId | _]) ->
    ApiKey = get_api_key(),
    Response = curl_get(ApiKey, "/services/" ++ ServiceId ++ "/logs"),
    io:format("~s~n", [Response]);

service_command(["--sleep", ServiceId | _]) ->
    ApiKey = get_api_key(),
    TmpFile = write_temp_file("{}"),
    _ = curl_post(ApiKey, "/services/" ++ ServiceId ++ "/sleep", TmpFile),
    file:delete(TmpFile),
    io:format("\033[32mService sleeping: ~s\033[0m~n", [ServiceId]);

service_command(["--wake", ServiceId | _]) ->
    ApiKey = get_api_key(),
    TmpFile = write_temp_file("{}"),
    _ = curl_post(ApiKey, "/services/" ++ ServiceId ++ "/wake", TmpFile),
    file:delete(TmpFile),
    io:format("\033[32mService waking: ~s\033[0m~n", [ServiceId]);

service_command(["--destroy", ServiceId | _]) ->
    ApiKey = get_api_key(),
    _ = curl_delete(ApiKey, "/services/" ++ ServiceId),
    io:format("\033[32mService destroyed: ~s\033[0m~n", [ServiceId]);

service_command(Args) ->
    case get_service_name(Args) of
        undefined ->
            io:format("Error: --name required to create service~n"),
            halt(1);
        Name ->
            ApiKey = get_api_key(),
            Ports = get_service_ports(Args),
            Bootstrap = get_service_bootstrap(Args),
            PortsJson = case Ports of
                undefined -> "";
                P -> ",\"ports\":[" ++ P ++ "]"
            end,
            BootstrapJson = case Bootstrap of
                undefined -> "";
                B -> ",\"bootstrap\":\"" ++ escape_json(B) ++ "\""
            end,
            Json = "{\"name\":\"" ++ Name ++ "\"" ++ PortsJson ++ BootstrapJson ++ "}",
            TmpFile = write_temp_file(Json),
            Response = curl_post(ApiKey, "/services", TmpFile),
            file:delete(TmpFile),
            io:format("\033[32mService created\033[0m~n"),
            io:format("~s~n", [Response])
    end.

%% Helpers
get_api_key() ->
    case os:getenv("UNSANDBOX_API_KEY") of
        false ->
            io:format("Error: UNSANDBOX_API_KEY not set~n"),
            halt(1);
        Key -> Key
    end.

ext_to_lang(".hs") -> {ok, "haskell"};
ext_to_lang(".ml") -> {ok, "ocaml"};
ext_to_lang(".clj") -> {ok, "clojure"};
ext_to_lang(".scm") -> {ok, "scheme"};
ext_to_lang(".lisp") -> {ok, "commonlisp"};
ext_to_lang(".erl") -> {ok, "erlang"};
ext_to_lang(".ex") -> {ok, "elixir"};
ext_to_lang(".exs") -> {ok, "elixir"};
ext_to_lang(".py") -> {ok, "python"};
ext_to_lang(".js") -> {ok, "javascript"};
ext_to_lang(".ts") -> {ok, "typescript"};
ext_to_lang(".rb") -> {ok, "ruby"};
ext_to_lang(".go") -> {ok, "go"};
ext_to_lang(".rs") -> {ok, "rust"};
ext_to_lang(".c") -> {ok, "c"};
ext_to_lang(".cpp") -> {ok, "cpp"};
ext_to_lang(".cc") -> {ok, "cpp"};
ext_to_lang(".java") -> {ok, "java"};
ext_to_lang(".kt") -> {ok, "kotlin"};
ext_to_lang(".cs") -> {ok, "csharp"};
ext_to_lang(".fs") -> {ok, "fsharp"};
ext_to_lang(".jl") -> {ok, "julia"};
ext_to_lang(".r") -> {ok, "r"};
ext_to_lang(".cr") -> {ok, "crystal"};
ext_to_lang(".d") -> {ok, "d"};
ext_to_lang(".nim") -> {ok, "nim"};
ext_to_lang(".zig") -> {ok, "zig"};
ext_to_lang(".v") -> {ok, "v"};
ext_to_lang(".dart") -> {ok, "dart"};
ext_to_lang(".sh") -> {ok, "bash"};
ext_to_lang(".pl") -> {ok, "perl"};
ext_to_lang(".lua") -> {ok, "lua"};
ext_to_lang(".php") -> {ok, "php"};
ext_to_lang(Ext) -> {error, Ext}.

escape_json(Str) ->
    escape_json(Str, []).

escape_json([], Acc) ->
    lists:reverse(Acc);
escape_json([$\\ | Rest], Acc) ->
    escape_json(Rest, [$\\, $\\ | Acc]);
escape_json([$\" | Rest], Acc) ->
    escape_json(Rest, [$\", $\\ | Acc]);
escape_json([$\n | Rest], Acc) ->
    escape_json(Rest, [$n, $\\ | Acc]);
escape_json([$\r | Rest], Acc) ->
    escape_json(Rest, [$r, $\\ | Acc]);
escape_json([$\t | Rest], Acc) ->
    escape_json(Rest, [$t, $\\ | Acc]);
escape_json([C | Rest], Acc) ->
    escape_json(Rest, [C | Acc]).

build_json(Language, Code) ->
    "{\"language\":\"" ++ Language ++ "\",\"code\":\"" ++ escape_json(Code) ++ "\"}".

write_temp_file(Data) ->
    TmpFile = "/tmp/un_erl_" ++ integer_to_list(rand:uniform(999999)) ++ ".json",
    file:write_file(TmpFile, Data),
    TmpFile.

curl_post(ApiKey, Endpoint, TmpFile) ->
    Cmd = "curl -s -X POST https://api.unsandbox.com" ++ Endpoint ++
          " -H 'Content-Type: application/json'" ++
          " -H 'Authorization: Bearer " ++ ApiKey ++ "'" ++
          " -d @" ++ TmpFile,
    os:cmd(Cmd).

curl_get(ApiKey, Endpoint) ->
    Cmd = "curl -s https://api.unsandbox.com" ++ Endpoint ++
          " -H 'Authorization: Bearer " ++ ApiKey ++ "'",
    os:cmd(Cmd).

curl_delete(ApiKey, Endpoint) ->
    Cmd = "curl -s -X DELETE https://api.unsandbox.com" ++ Endpoint ++
          " -H 'Authorization: Bearer " ++ ApiKey ++ "'",
    os:cmd(Cmd).

%% Argument parsing
parse_exec_args([], Opts) ->
    {maps:get(file, Opts), Opts};
parse_exec_args([Arg | Rest], Opts) ->
    case Arg of
        "-" ++ _ -> parse_exec_args(Rest, Opts);
        _ -> {Arg, Opts}
    end.

get_shell_opt([], Default) -> Default;
get_shell_opt(["--shell", Shell | _], _) -> Shell;
get_shell_opt(["-s", Shell | _], _) -> Shell;
get_shell_opt([_ | Rest], Default) -> get_shell_opt(Rest, Default).

get_service_name([]) -> undefined;
get_service_name(["--name", Name | _]) -> Name;
get_service_name([_ | Rest]) -> get_service_name(Rest).

get_service_ports([]) -> undefined;
get_service_ports(["--ports", Ports | _]) -> Ports;
get_service_ports([_ | Rest]) -> get_service_ports(Rest).

get_service_bootstrap([]) -> undefined;
get_service_bootstrap(["--bootstrap", Bootstrap | _]) -> Bootstrap;
get_service_bootstrap([_ | Rest]) -> get_service_bootstrap(Rest).
