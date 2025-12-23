% PUBLIC DOMAIN - NO LICENSE, NO WARRANTY
%
% This is free public domain software for the public good of a permacomputer hosted
% at permacomputer.com - an always-on computer by the people, for the people. One
% which is durable, easy to repair, and distributed like tap water for machine
% learning intelligence.
%
% The permacomputer is community-owned infrastructure optimized around four values:
%
%   TRUTH    - Source code must be open source & freely distributed
%   FREEDOM  - Voluntary participation without corporate control
%   HARMONY  - Systems operating with minimal waste that self-renew
%   LOVE     - Individual rights protected while fostering cooperation
%
% This software contributes to that vision by enabling code execution across 42+
% programming languages through a unified interface, accessible to all. Code is
% seeds to sprout on any abandoned technology.
%
% Learn more: https://www.permacomputer.com
%
% Anyone is free to copy, modify, publish, use, compile, sell, or distribute this
% software, either in source code form or as a compiled binary, for any purpose,
% commercial or non-commercial, and by any means.
%
% NO WARRANTY. THE SOFTWARE IS PROVIDED "AS IS" WITHOUT WARRANTY OF ANY KIND.
%
% That said, our permacomputer's digital membrane stratum continuously runs unit,
% integration, and functional tests on all of it's own software - with our
% permacomputer monitoring itself, repairing itself, with minimal human in the
% loop guidance. Our agents do their best.
%
% Copyright 2025 TimeHexOn & foxhop & russell@unturf
% https://www.timehexon.com
% https://www.foxhop.net
% https://www.unturf.com/software


#!/usr/bin/env swipl

:- initialization(main, main).

% Extension to language mapping
ext_lang('.jl', 'julia').
ext_lang('.r', 'r').
ext_lang('.cr', 'crystal').
ext_lang('.f90', 'fortran').
ext_lang('.cob', 'cobol').
ext_lang('.pro', 'prolog').
ext_lang('.forth', 'forth').
ext_lang('.4th', 'forth').
ext_lang('.py', 'python').
ext_lang('.js', 'javascript').
ext_lang('.ts', 'typescript').
ext_lang('.rb', 'ruby').
ext_lang('.php', 'php').
ext_lang('.pl', 'perl').
ext_lang('.lua', 'lua').
ext_lang('.sh', 'bash').
ext_lang('.go', 'go').
ext_lang('.rs', 'rust').
ext_lang('.c', 'c').
ext_lang('.cpp', 'cpp').
ext_lang('.java', 'java').

% Detect language from filename
detect_language(Filename, Language) :-
    file_name_extension(_, Ext, Filename),
    downcase_atom(Ext, ExtLower),
    atomic_list_concat(['.', ExtLower], ExtWithDot),
    ext_lang(ExtWithDot, Language), !.
detect_language(_, 'unknown').

% Read entire file into string
read_file_content(Filename, Content) :-
    open(Filename, read, Stream),
    read_string(Stream, _, Content),
    close(Stream).

% Get API key from environment
get_api_key(ApiKey) :-
    (   getenv('UNSANDBOX_API_KEY', ApiKey),
        ApiKey \= ''
    ->  true
    ;   write(user_error, 'Error: UNSANDBOX_API_KEY environment variable not set\n'),
        halt(1)
    ).

% Execute command using curl
execute_file(Filename) :-
    % Check file exists
    (   exists_file(Filename)
    ->  true
    ;   format(user_error, 'Error: File not found: ~w~n', [Filename]),
        halt(1)
    ),

    % Detect language
    detect_language(Filename, Language),
    (   Language \= 'unknown'
    ->  true
    ;   format(user_error, 'Error: Unknown language for file: ~w~n', [Filename]),
        halt(1)
    ),

    % Get API key
    get_api_key(ApiKey),

    % Build and execute curl command
    format(atom(Cmd),
        'curl -s -X POST https://api.unsandbox.com/execute -H "Content-Type: application/json" -H "Authorization: Bearer ~w" --data-binary @- -o /tmp/unsandbox_resp.json < <(jq -Rs \'\'\''{language: "~w", code: .}\'\'\'\' < "~w"); jq -r ".stdout // empty" /tmp/unsandbox_resp.json | sed "s/^/\\x1b[34m/" | sed "s/$/\\x1b[0m/"; jq -r ".stderr // empty" /tmp/unsandbox_resp.json | sed "s/^/\\x1b[31m/" | sed "s/$/\\x1b[0m/" >&2; rm -f /tmp/unsandbox_resp.json',
        [ApiKey, Language, Filename]),
    shell(Cmd, 0).

% Session list
session_list :-
    get_api_key(ApiKey),
    format(atom(Cmd),
        'curl -s -X GET https://api.unsandbox.com/sessions -H "Authorization: Bearer ~w" | jq -r \'.sessions[] | "\\(.id) \\(.shell) \\(.status) \\(.created_at)"\' 2>/dev/null || echo "No active sessions"',
        [ApiKey]),
    shell(Cmd, 0).

% Session kill
session_kill(SessionId) :-
    get_api_key(ApiKey),
    format(atom(Cmd),
        'curl -s -X DELETE https://api.unsandbox.com/sessions/~w -H "Authorization: Bearer ~w" >/dev/null && echo -e "\\x1b[32mSession terminated: ~w\\x1b[0m"',
        [SessionId, ApiKey, SessionId]),
    shell(Cmd, 0).

% Service list
service_list :-
    get_api_key(ApiKey),
    format(atom(Cmd),
        'curl -s -X GET https://api.unsandbox.com/services -H "Authorization: Bearer ~w" | jq -r \'.services[] | "\\(.id) \\(.name) \\(.status)"\' 2>/dev/null || echo "No services"',
        [ApiKey]),
    shell(Cmd, 0).

% Service info
service_info(ServiceId) :-
    get_api_key(ApiKey),
    format(atom(Cmd),
        'curl -s -X GET https://api.unsandbox.com/services/~w -H "Authorization: Bearer ~w" | jq .',
        [ServiceId, ApiKey]),
    shell(Cmd, 0).

% Service logs
service_logs(ServiceId) :-
    get_api_key(ApiKey),
    format(atom(Cmd),
        'curl -s -X GET https://api.unsandbox.com/services/~w/logs -H "Authorization: Bearer ~w" | jq -r ".logs"',
        [ServiceId, ApiKey]),
    shell(Cmd, 0).

% Service sleep
service_sleep(ServiceId) :-
    get_api_key(ApiKey),
    format(atom(Cmd),
        'curl -s -X POST https://api.unsandbox.com/services/~w/sleep -H "Authorization: Bearer ~w" >/dev/null && echo -e "\\x1b[32mService sleeping: ~w\\x1b[0m"',
        [ServiceId, ApiKey, ServiceId]),
    shell(Cmd, 0).

% Service wake
service_wake(ServiceId) :-
    get_api_key(ApiKey),
    format(atom(Cmd),
        'curl -s -X POST https://api.unsandbox.com/services/~w/wake -H "Authorization: Bearer ~w" >/dev/null && echo -e "\\x1b[32mService waking: ~w\\x1b[0m"',
        [ServiceId, ApiKey, ServiceId]),
    shell(Cmd, 0).

% Service destroy
service_destroy(ServiceId) :-
    get_api_key(ApiKey),
    format(atom(Cmd),
        'curl -s -X DELETE https://api.unsandbox.com/services/~w -H "Authorization: Bearer ~w" >/dev/null && echo -e "\\x1b[32mService destroyed: ~w\\x1b[0m"',
        [ServiceId, ApiKey, ServiceId]),
    shell(Cmd, 0).

% Handle session subcommand
handle_session(['--list'|_]) :- session_list.
handle_session(['-l'|_]) :- session_list.
handle_session(['--kill', SessionId|_]) :- session_kill(SessionId).
handle_session(_) :-
    write(user_error, 'Error: Use --list or --kill ID\n'),
    halt(1).

% Handle service subcommand
handle_service(['--list'|_]) :- service_list.
handle_service(['-l'|_]) :- service_list.
handle_service(['--info', ServiceId|_]) :- service_info(ServiceId).
handle_service(['--logs', ServiceId|_]) :- service_logs(ServiceId).
handle_service(['--sleep', ServiceId|_]) :- service_sleep(ServiceId).
handle_service(['--wake', ServiceId|_]) :- service_wake(ServiceId).
handle_service(['--destroy', ServiceId|_]) :- service_destroy(ServiceId).
handle_service(_) :-
    write(user_error, 'Error: Use --list, --info, --logs, --sleep, --wake, or --destroy\n'),
    halt(1).

% Main program
main(Argv) :-
    % Check arguments
    (   Argv = []
    ->  write(user_error, 'Usage: un.pro [options] <source_file>\n'),
        write(user_error, '       un.pro session [options]\n'),
        write(user_error, '       un.pro service [options]\n'),
        halt(1)
    ;   true
    ),

    % Parse subcommands
    (   Argv = ['session'|Rest]
    ->  handle_session(Rest)
    ;   Argv = ['service'|Rest]
    ->  handle_service(Rest)
    ;   Argv = [Filename|_]
    ->  execute_file(Filename)
    ;   write(user_error, 'Error: Invalid arguments\n'),
        halt(1)
    ).
