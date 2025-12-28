%% PUBLIC DOMAIN - NO LICENSE, NO WARRANTY
%%
%% This is free public domain software for the public good of a permacomputer hosted
%% at permacomputer.com - an always-on computer by the people, for the people. One
%% which is durable, easy to repair, and distributed like tap water for machine
%% learning intelligence.
%%
%% The permacomputer is community-owned infrastructure optimized around four values:
%%
%%   TRUTH    - First principles, math & science, open source code freely distributed
%%   FREEDOM  - Voluntary partnerships, freedom from tyranny & corporate control
%%   HARMONY  - Minimal waste, self-renewing systems with diverse thriving connections
%%   LOVE     - Be yourself without hurting others, cooperation through natural law
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
    io:format("       un.erl key [options]~n"),
    halt(1);

main(["session" | Rest]) ->
    session_command(Rest);

main(["service" | Rest]) ->
    service_command(Rest);

main(["key" | Rest]) ->
    key_command(Rest);

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

service_command(["--freeze", ServiceId | _]) ->
    ApiKey = get_api_key(),
    TmpFile = write_temp_file("{}"),
    _ = curl_post(ApiKey, "/services/" ++ ServiceId ++ "/sleep", TmpFile),
    file:delete(TmpFile),
    io:format("\033[32mService sleeping: ~s\033[0m~n", [ServiceId]);

service_command(["--unfreeze", ServiceId | _]) ->
    ApiKey = get_api_key(),
    TmpFile = write_temp_file("{}"),
    _ = curl_post(ApiKey, "/services/" ++ ServiceId ++ "/wake", TmpFile),
    file:delete(TmpFile),
    io:format("\033[32mService waking: ~s\033[0m~n", [ServiceId]);

service_command(["--destroy", ServiceId | _]) ->
    ApiKey = get_api_key(),
    _ = curl_delete(ApiKey, "/services/" ++ ServiceId),
    io:format("\033[32mService destroyed: ~s\033[0m~n", [ServiceId]);

service_command(["--execute", ServiceId, "--command", Command | _]) ->
    ApiKey = get_api_key(),
    Json = "{\"command\":\"" ++ escape_json(Command) ++ "\"}",
    TmpFile = write_temp_file(Json),
    Response = curl_post(ApiKey, "/services/" ++ ServiceId ++ "/execute", TmpFile),
    file:delete(TmpFile),
    case extract_json_field(Response, "stdout") of
        "" -> ok;
        Stdout -> io:format("\033[34m~s\033[0m", [Stdout])
    end;

service_command(["--dump-bootstrap", ServiceId, File | _]) ->
    ApiKey = get_api_key(),
    io:format(standard_error, "Fetching bootstrap script from ~s...~n", [ServiceId]),
    Json = "{\"command\":\"cat /tmp/bootstrap.sh\"}",
    TmpFile = write_temp_file(Json),
    Response = curl_post(ApiKey, "/services/" ++ ServiceId ++ "/execute", TmpFile),
    file:delete(TmpFile),
    case extract_json_field(Response, "stdout") of
        "" ->
            io:format(standard_error, "\033[31mError: Failed to fetch bootstrap (service not running or no bootstrap file)\033[0m~n"),
            halt(1);
        Script ->
            file:write_file(File, Script),
            os:cmd("chmod 755 " ++ File),
            io:format("Bootstrap saved to ~s~n", [File])
    end;

service_command(["--dump-bootstrap", ServiceId | _]) ->
    ApiKey = get_api_key(),
    io:format(standard_error, "Fetching bootstrap script from ~s...~n", [ServiceId]),
    Json = "{\"command\":\"cat /tmp/bootstrap.sh\"}",
    TmpFile = write_temp_file(Json),
    Response = curl_post(ApiKey, "/services/" ++ ServiceId ++ "/execute", TmpFile),
    file:delete(TmpFile),
    case extract_json_field(Response, "stdout") of
        "" ->
            io:format(standard_error, "\033[31mError: Failed to fetch bootstrap (service not running or no bootstrap file)\033[0m~n"),
            halt(1);
        Script ->
            io:format("~s", [Script])
    end;

service_command(Args) ->
    case get_service_name(Args) of
        undefined ->
            io:format("Error: --name required to create service~n"),
            halt(1);
        Name ->
            ApiKey = get_api_key(),
            Ports = get_service_ports(Args),
            Bootstrap = get_service_bootstrap(Args),
            Type = get_service_type(Args),
            PortsJson = case Ports of
                undefined -> "";
                P -> ",\"ports\":[" ++ P ++ "]"
            end,
            BootstrapJson = case Bootstrap of
                undefined -> "";
                B -> ",\"bootstrap\":\"" ++ escape_json(B) ++ "\""
            end,
            TypeJson = case Type of
                undefined -> "";
                T -> ",\"service_type\":\"" ++ T ++ "\""
            end,
            Json = "{\"name\":\"" ++ Name ++ "\"" ++ PortsJson ++ BootstrapJson ++ TypeJson ++ "}",
            TmpFile = write_temp_file(Json),
            Response = curl_post(ApiKey, "/services", TmpFile),
            file:delete(TmpFile),
            io:format("\033[32mService created\033[0m~n"),
            io:format("~s~n", [Response])
    end.

%% Key command
key_command(Args) ->
    ApiKey = get_api_key(),
    case has_extend_flag(Args) of
        true ->
            validate_and_extend_key(ApiKey);
        false ->
            validate_key(ApiKey)
    end.

validate_key(ApiKey) ->
    Response = curl_post_portal(ApiKey, "/keys/validate", "{}"),
    parse_and_display_key_status(Response, false).

validate_and_extend_key(ApiKey) ->
    Response = curl_post_portal(ApiKey, "/keys/validate", "{}"),
    parse_and_display_key_status(Response, true).

parse_and_display_key_status(Response, ShouldExtend) ->
    %% Parse JSON response (simple extraction for fields we need)
    Status = extract_json_field(Response, "status"),
    PublicKey = extract_json_field(Response, "public_key"),
    Tier = extract_json_field(Response, "tier"),
    ExpiresAt = extract_json_field(Response, "expires_at"),
    TimeRemaining = extract_json_field(Response, "time_remaining"),
    RateLimit = extract_json_field(Response, "rate_limit"),
    Burst = extract_json_field(Response, "burst"),
    Concurrency = extract_json_field(Response, "concurrency"),

    case Status of
        "valid" ->
            io:format("\033[32mValid\033[0m~n"),
            io:format("Public Key: ~s~n", [PublicKey]),
            io:format("Tier: ~s~n", [Tier]),
            io:format("Status: ~s~n", [Status]),
            io:format("Expires: ~s~n", [ExpiresAt]),
            if TimeRemaining =/= "" -> io:format("Time Remaining: ~s~n", [TimeRemaining]); true -> ok end,
            if RateLimit =/= "" -> io:format("Rate Limit: ~s~n", [RateLimit]); true -> ok end,
            if Burst =/= "" -> io:format("Burst: ~s~n", [Burst]); true -> ok end,
            if Concurrency =/= "" -> io:format("Concurrency: ~s~n", [Concurrency]); true -> ok end,
            if ShouldExtend ->
                open_extend_page(PublicKey);
            true -> ok
            end;
        "expired" ->
            io:format("\033[31mExpired\033[0m~n"),
            io:format("Public Key: ~s~n", [PublicKey]),
            io:format("Tier: ~s~n", [Tier]),
            io:format("Expired: ~s~n", [ExpiresAt]),
            io:format("\033[33mTo renew: Visit https://unsandbox.com/keys/extend\033[0m~n"),
            if ShouldExtend ->
                open_extend_page(PublicKey);
            true -> ok
            end;
        "invalid" ->
            io:format("\033[31mInvalid\033[0m~n"),
            io:format("The API key is not valid.~n");
        _ ->
            io:format("~s~n", [Response])
    end.

open_extend_page(PublicKey) ->
    Url = "https://unsandbox.com/keys/extend?pk=" ++ PublicKey,
    io:format("\033[33mOpening browser to extend key...\033[0m~n"),
    case os:type() of
        {unix, darwin} ->
            os:cmd("open '" ++ Url ++ "'");
        {unix, _} ->
            os:cmd("xdg-open '" ++ Url ++ "' 2>/dev/null || sensible-browser '" ++ Url ++ "' 2>/dev/null &");
        {win32, _} ->
            os:cmd("start " ++ Url)
    end.

%% Helpers
get_api_keys() ->
    PublicKey = os:getenv("UNSANDBOX_PUBLIC_KEY"),
    SecretKey = os:getenv("UNSANDBOX_SECRET_KEY"),
    ApiKey = os:getenv("UNSANDBOX_API_KEY"),

    if
        PublicKey =/= false andalso SecretKey =/= false ->
            {PublicKey, SecretKey};
        ApiKey =/= false ->
            {ApiKey, false};
        true ->
            io:format("Error: UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY not set (or UNSANDBOX_API_KEY for backwards compat)~n"),
            halt(1)
    end.

get_api_key() ->
    {PublicKey, _} = get_api_keys(),
    PublicKey.

hmac_sha256(Secret, Message) ->
    string:lowercase(
        lists:flatten([io_lib:format("~2.16.0b", [X]) || X <- binary_to_list(crypto:mac(hmac, sha256, Secret, Message))])
    ).

make_signature(SecretKey, Timestamp, Method, Path, Body) ->
    Message = Timestamp ++ ":" ++ Method ++ ":" ++ Path ++ ":" ++ Body,
    hmac_sha256(SecretKey, Message).

check_clock_drift_error(Response) ->
    HasTimestamp = string:str(Response, "timestamp") > 0 orelse string:str(Response, "\"timestamp\"") > 0,
    Has401 = string:str(Response, "401") > 0,
    HasExpired = string:str(Response, "expired") > 0,
    HasInvalid = string:str(Response, "invalid") > 0,

    case HasTimestamp andalso (Has401 orelse HasExpired orelse HasInvalid) of
        true ->
            io:format(standard_error, "\033[31mError: Request timestamp expired (must be within 5 minutes of server time)\033[0m~n", []),
            io:format(standard_error, "\033[33mYour computer's clock may have drifted.\033[0m~n", []),
            io:format(standard_error, "Check your system time and sync with NTP if needed:~n", []),
            io:format(standard_error, "  Linux:   sudo ntpdate -s time.nist.gov~n", []),
            io:format(standard_error, "  macOS:   sudo sntp -sS time.apple.com~n", []),
            io:format(standard_error, "  Windows: w32tm /resync~n", []),
            halt(1);
        false ->
            ok
    end.

build_auth_headers(PublicKey, SecretKey, Method, Path, Body) ->
    if
        SecretKey =/= false ->
            Timestamp = integer_to_list(erlang:system_time(second)),
            Signature = make_signature(SecretKey, Timestamp, Method, Path, Body),
            " -H 'Authorization: Bearer " ++ PublicKey ++ "'"
            ++ " -H 'X-Timestamp: " ++ Timestamp ++ "'"
            ++ " -H 'X-Signature: " ++ Signature ++ "'";
        true ->
            " -H 'Authorization: Bearer " ++ PublicKey ++ "'"
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
    {ok, Body} = file:read_file(TmpFile),
    BodyStr = binary_to_list(Body),
    {PublicKey, SecretKey} = get_api_keys(),
    AuthHeaders = build_auth_headers(PublicKey, SecretKey, "POST", Endpoint, BodyStr),
    Cmd = "curl -s -X POST https://api.unsandbox.com" ++ Endpoint ++
          " -H 'Content-Type: application/json'" ++
          AuthHeaders ++
          " -d @" ++ TmpFile,
    Result = os:cmd(Cmd),
    check_clock_drift_error(Result),
    Result.

curl_post_portal(ApiKey, Endpoint, Data) ->
    TmpFile = write_temp_file(Data),
    {PublicKey, SecretKey} = get_api_keys(),
    AuthHeaders = build_auth_headers(PublicKey, SecretKey, "POST", Endpoint, Data),
    Cmd = "curl -s -X POST https://unsandbox.com" ++ Endpoint ++
          " -H 'Content-Type: application/json'" ++
          AuthHeaders ++
          " -d @" ++ TmpFile,
    Result = os:cmd(Cmd),
    file:delete(TmpFile),
    check_clock_drift_error(Result),
    Result.

curl_get(ApiKey, Endpoint) ->
    {PublicKey, SecretKey} = get_api_keys(),
    AuthHeaders = build_auth_headers(PublicKey, SecretKey, "GET", Endpoint, ""),
    Cmd = "curl -s https://api.unsandbox.com" ++ Endpoint ++
          AuthHeaders,
    Result = os:cmd(Cmd),
    check_clock_drift_error(Result),
    Result.

curl_delete(ApiKey, Endpoint) ->
    {PublicKey, SecretKey} = get_api_keys(),
    AuthHeaders = build_auth_headers(PublicKey, SecretKey, "DELETE", Endpoint, ""),
    Cmd = "curl -s -X DELETE https://api.unsandbox.com" ++ Endpoint ++
          AuthHeaders,
    Result = os:cmd(Cmd),
    check_clock_drift_error(Result),
    Result.

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

get_service_type([]) -> undefined;
get_service_type(["--type", Type | _]) -> Type;
get_service_type([_ | Rest]) -> get_service_type(Rest).

has_extend_flag([]) -> false;
has_extend_flag(["--extend" | _]) -> true;
has_extend_flag([_ | Rest]) -> has_extend_flag(Rest).

%% Simple JSON field extraction (works for simple string fields)
extract_json_field(Json, Field) ->
    Pattern = "\"" ++ Field ++ "\":\"",
    case string:str(Json, Pattern) of
        0 -> "";
        Pos ->
            Start = Pos + length(Pattern),
            Rest = lists:nthtail(Start - 1, Json),
            extract_until_quote(Rest)
    end.

extract_until_quote(Str) ->
    extract_until_quote(Str, []).

extract_until_quote([], Acc) ->
    lists:reverse(Acc);
extract_until_quote([$\" | _], Acc) ->
    lists:reverse(Acc);
extract_until_quote([$\\, $\" | Rest], Acc) ->
    extract_until_quote(Rest, [$\" | Acc]);
extract_until_quote([C | Rest], Acc) ->
    extract_until_quote(Rest, [C | Acc]).
