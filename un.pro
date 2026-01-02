% PUBLIC DOMAIN - NO LICENSE, NO WARRANTY
%
% This is free public domain software for the public good of a permacomputer hosted
% at permacomputer.com - an always-on computer by the people, for the people. One
% which is durable, easy to repair, and distributed like tap water for machine
% learning intelligence.
%
% The permacomputer is community-owned infrastructure optimized around four values:
%
%   TRUTH    - First principles, math & science, open source code freely distributed
%   FREEDOM  - Voluntary partnerships, freedom from tyranny & corporate control
%   HARMONY  - Minimal waste, self-renewing systems with diverse thriving connections
%   LOVE     - Be yourself without hurting others, cooperation through natural law
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

% Constants
portal_base('https://unsandbox.com').

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

% Get API keys from environment (HMAC or legacy)
get_public_key(PublicKey) :-
    (   getenv('UNSANDBOX_PUBLIC_KEY', PublicKey),
        PublicKey \= ''
    ->  true
    ;   getenv('UNSANDBOX_API_KEY', PublicKey),
        PublicKey \= ''
    ->  true
    ;   write(user_error, 'Error: UNSANDBOX_PUBLIC_KEY or UNSANDBOX_API_KEY environment variable not set\n'),
        halt(1)
    ).

get_secret_key(SecretKey) :-
    (   getenv('UNSANDBOX_SECRET_KEY', SecretKey),
        SecretKey \= ''
    ->  true
    ;   getenv('UNSANDBOX_API_KEY', SecretKey),
        SecretKey \= ''
    ->  true
    ;   SecretKey = ''
    ).

% Get API key (legacy compatibility)
get_api_key(ApiKey) :-
    get_public_key(ApiKey).

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

    % Get API keys
    get_public_key(PublicKey),
    get_secret_key(SecretKey),

    % Build and execute curl command with HMAC
    format(atom(Cmd),
        'BODY=$(jq -Rs \'\'\''{language: "~w", code: .}\'\'\'\' < "~w"); TIMESTAMP=$(date +%s); MESSAGE="$TIMESTAMP:POST:/execute:$BODY"; SIGNATURE=$(echo -n "$MESSAGE" | openssl dgst -sha256 -hmac "~w" -hex | sed \'\'\'\'s/.*= //\'\'\'\'); curl -s -X POST https://api.unsandbox.com/execute -H "Content-Type: application/json" -H "Authorization: Bearer ~w" -H "X-Timestamp: $TIMESTAMP" -H "X-Signature: $SIGNATURE" -d "$BODY" -o /tmp/unsandbox_resp.json; RESP=$(cat /tmp/unsandbox_resp.json); if echo "$RESP" | grep -qi "timestamp" && echo "$RESP" | grep -Eqi "(401|expired|invalid)"; then echo -e "\\x1b[31mError: Request timestamp expired (must be within 5 minutes of server time)\\x1b[0m" >&2; echo -e "\\x1b[33mYour computer'\''s clock may have drifted.\\x1b[0m" >&2; echo "Check your system time and sync with NTP if needed:" >&2; echo "  Linux:   sudo ntpdate -s time.nist.gov" >&2; echo "  macOS:   sudo sntp -sS time.apple.com" >&2; echo "  Windows: w32tm /resync\\x1b[0m" >&2; rm -f /tmp/unsandbox_resp.json; exit 1; fi; jq -r ".stdout // empty" /tmp/unsandbox_resp.json | sed "s/^/\\x1b[34m/" | sed "s/$/\\x1b[0m/"; jq -r ".stderr // empty" /tmp/unsandbox_resp.json | sed "s/^/\\x1b[31m/" | sed "s/$/\\x1b[0m/" >&2; rm -f /tmp/unsandbox_resp.json',
        [Language, Filename, SecretKey, PublicKey]),
    shell(Cmd, 0).

% Session list
session_list :-
    get_public_key(PublicKey),
    get_secret_key(SecretKey),
    format(atom(Cmd),
        'TIMESTAMP=$(date +%s); MESSAGE="$TIMESTAMP:GET:/sessions:"; SIGNATURE=$(echo -n "$MESSAGE" | openssl dgst -sha256 -hmac "~w" -hex | sed \'\'s/.*= //\'\'); RESP=$(curl -s -X GET https://api.unsandbox.com/sessions -H "Authorization: Bearer ~w" -H "X-Timestamp: $TIMESTAMP" -H "X-Signature: $SIGNATURE"); if echo "$RESP" | grep -qi "timestamp" && echo "$RESP" | grep -Eqi "(401|expired|invalid)"; then echo -e "\\x1b[31mError: Request timestamp expired (must be within 5 minutes of server time)\\x1b[0m" >&2; echo -e "\\x1b[33mYour computer'\''s clock may have drifted.\\x1b[0m" >&2; echo "Check your system time and sync with NTP if needed:" >&2; echo "  Linux:   sudo ntpdate -s time.nist.gov" >&2; echo "  macOS:   sudo sntp -sS time.apple.com" >&2; echo "  Windows: w32tm /resync\\x1b[0m" >&2; exit 1; fi; echo "$RESP" | jq -r \'.sessions[] | "\\(.id) \\(.shell) \\(.status) \\(.created_at)"\' 2>/dev/null || echo "No active sessions"',
        [SecretKey, PublicKey]),
    shell(Cmd, 0).

% Session kill
session_kill(SessionId) :-
    get_public_key(PublicKey),
    get_secret_key(SecretKey),
    format(atom(Cmd),
        'TIMESTAMP=$(date +%s); MESSAGE="$TIMESTAMP:DELETE:/sessions/~w:"; SIGNATURE=$(echo -n "$MESSAGE" | openssl dgst -sha256 -hmac "~w" -hex | sed \'\'s/.*= //\'\'); curl -s -X DELETE https://api.unsandbox.com/sessions/~w -H "Authorization: Bearer ~w" -H "X-Timestamp: $TIMESTAMP" -H "X-Signature: $SIGNATURE" >/dev/null && echo -e "\\x1b[32mSession terminated: ~w\\x1b[0m"',
        [SessionId, SecretKey, SessionId, PublicKey, SessionId]),
    shell(Cmd, 0).

% Session create with optional input files
session_create(Shell, InputFiles) :-
    get_public_key(PublicKey),
    get_secret_key(SecretKey),
    (   Shell \= ''
    ->  ShellVal = Shell
    ;   ShellVal = 'bash'
    ),
    % Build file arguments for bash script
    build_file_args(InputFiles, FileArgs),
    format(atom(Cmd),
        'echo -e "\\x1b[33mCreating session...\\x1b[0m"; SHELL_VAL="~w"; INPUT_FILES=""; ~w if [ -n "$INPUT_FILES" ]; then BODY="{\\\"shell\\\":\\\"$SHELL_VAL\\\",\\\"input_files\\\":[$INPUT_FILES]}"; else BODY="{\\\"shell\\\":\\\"$SHELL_VAL\\\"}"; fi; TIMESTAMP=$(date +%s); MESSAGE="$TIMESTAMP:POST:/sessions:$BODY"; SIGNATURE=$(echo -n "$MESSAGE" | openssl dgst -sha256 -hmac "~w" -hex | sed \'\'s/.*= //\'\'); curl -s -X POST https://api.unsandbox.com/sessions -H "Content-Type: application/json" -H "Authorization: Bearer ~w" -H "X-Timestamp: $TIMESTAMP" -H "X-Signature: $SIGNATURE" -d "$BODY" | jq .',
        [ShellVal, FileArgs, SecretKey, PublicKey]),
    shell(Cmd, 0).

% Build bash commands to base64 encode files
build_file_args([], '').
build_file_args(Files, Args) :-
    Files \= [],
    maplist(build_single_file_arg, Files, ArgList),
    atomic_list_concat(ArgList, ' ', Args).

build_single_file_arg(FilePath, Arg) :-
    file_base_name(FilePath, Basename),
    format(atom(Arg), 'CONTENT=$(base64 -w0 "~w"); if [ -z "$INPUT_FILES" ]; then INPUT_FILES="{\\\"filename\\\":\\\"~w\\\",\\\"content\\\":\\\"$CONTENT\\\"}"; else INPUT_FILES="$INPUT_FILES,{\\\"filename\\\":\\\"~w\\\",\\\"content\\\":\\\"$CONTENT\\\"}"; fi;', [FilePath, Basename, Basename]).

% Service list
service_list :-
    get_public_key(PublicKey),
    get_secret_key(SecretKey),
    format(atom(Cmd),
        'TIMESTAMP=$(date +%s); MESSAGE="$TIMESTAMP:GET:/services:"; SIGNATURE=$(echo -n "$MESSAGE" | openssl dgst -sha256 -hmac "~w" -hex | sed \'\'s/.*= //\'\'); RESP=$(curl -s -X GET https://api.unsandbox.com/services -H "Authorization: Bearer ~w" -H "X-Timestamp: $TIMESTAMP" -H "X-Signature: $SIGNATURE"); if echo "$RESP" | grep -qi "timestamp" && echo "$RESP" | grep -Eqi "(401|expired|invalid)"; then echo -e "\\x1b[31mError: Request timestamp expired (must be within 5 minutes of server time)\\x1b[0m" >&2; echo -e "\\x1b[33mYour computer'\''s clock may have drifted.\\x1b[0m" >&2; echo "Check your system time and sync with NTP if needed:" >&2; echo "  Linux:   sudo ntpdate -s time.nist.gov" >&2; echo "  macOS:   sudo sntp -sS time.apple.com" >&2; echo "  Windows: w32tm /resync\\x1b[0m" >&2; exit 1; fi; echo "$RESP" | jq -r \'.services[] | "\\(.id) \\(.name) \\(.status)"\' 2>/dev/null || echo "No services"',
        [SecretKey, PublicKey]),
    shell(Cmd, 0).

% Service info
service_info(ServiceId) :-
    get_public_key(PublicKey),
    get_secret_key(SecretKey),
    format(atom(Cmd),
        'TIMESTAMP=$(date +%s); MESSAGE="$TIMESTAMP:GET:/services/~w:"; SIGNATURE=$(echo -n "$MESSAGE" | openssl dgst -sha256 -hmac "~w" -hex | sed \'\'s/.*= //\'\'); curl -s -X GET https://api.unsandbox.com/services/~w -H "Authorization: Bearer ~w" -H "X-Timestamp: $TIMESTAMP" -H "X-Signature: $SIGNATURE" | jq .',
        [ServiceId, SecretKey, ServiceId, PublicKey]),
    shell(Cmd, 0).

% Service logs
service_logs(ServiceId) :-
    get_public_key(PublicKey),
    get_secret_key(SecretKey),
    format(atom(Cmd),
        'TIMESTAMP=$(date +%s); MESSAGE="$TIMESTAMP:GET:/services/~w/logs:"; SIGNATURE=$(echo -n "$MESSAGE" | openssl dgst -sha256 -hmac "~w" -hex | sed \'\'s/.*= //\'\'); curl -s -X GET https://api.unsandbox.com/services/~w/logs -H "Authorization: Bearer ~w" -H "X-Timestamp: $TIMESTAMP" -H "X-Signature: $SIGNATURE" | jq -r ".logs"',
        [ServiceId, SecretKey, ServiceId, PublicKey]),
    shell(Cmd, 0).

% Service sleep
service_sleep(ServiceId) :-
    get_public_key(PublicKey),
    get_secret_key(SecretKey),
    format(atom(Cmd),
        'TIMESTAMP=$(date +%s); MESSAGE="$TIMESTAMP:POST:/services/~w/sleep:"; SIGNATURE=$(echo -n "$MESSAGE" | openssl dgst -sha256 -hmac "~w" -hex | sed \'\'s/.*= //\'\'); curl -s -X POST https://api.unsandbox.com/services/~w/sleep -H "Authorization: Bearer ~w" -H "X-Timestamp: $TIMESTAMP" -H "X-Signature: $SIGNATURE" >/dev/null && echo -e "\\x1b[32mService sleeping: ~w\\x1b[0m"',
        [ServiceId, SecretKey, ServiceId, PublicKey, ServiceId]),
    shell(Cmd, 0).

% Service wake
service_wake(ServiceId) :-
    get_public_key(PublicKey),
    get_secret_key(SecretKey),
    format(atom(Cmd),
        'TIMESTAMP=$(date +%s); MESSAGE="$TIMESTAMP:POST:/services/~w/wake:"; SIGNATURE=$(echo -n "$MESSAGE" | openssl dgst -sha256 -hmac "~w" -hex | sed \'\'s/.*= //\'\'); curl -s -X POST https://api.unsandbox.com/services/~w/wake -H "Authorization: Bearer ~w" -H "X-Timestamp: $TIMESTAMP" -H "X-Signature: $SIGNATURE" >/dev/null && echo -e "\\x1b[32mService waking: ~w\\x1b[0m"',
        [ServiceId, SecretKey, ServiceId, PublicKey, ServiceId]),
    shell(Cmd, 0).

% Service destroy
service_destroy(ServiceId) :-
    get_public_key(PublicKey),
    get_secret_key(SecretKey),
    format(atom(Cmd),
        'TIMESTAMP=$(date +%s); MESSAGE="$TIMESTAMP:DELETE:/services/~w:"; SIGNATURE=$(echo -n "$MESSAGE" | openssl dgst -sha256 -hmac "~w" -hex | sed \'\'s/.*= //\'\'); curl -s -X DELETE https://api.unsandbox.com/services/~w -H "Authorization: Bearer ~w" -H "X-Timestamp: $TIMESTAMP" -H "X-Signature: $SIGNATURE" >/dev/null && echo -e "\\x1b[32mService destroyed: ~w\\x1b[0m"',
        [ServiceId, SecretKey, ServiceId, PublicKey, ServiceId]),
    shell(Cmd, 0).

% Service dump bootstrap
service_dump_bootstrap(ServiceId, DumpFile) :-
    get_public_key(PublicKey),
    get_secret_key(SecretKey),
    (   DumpFile = ''
    ->  % No file specified, print to stdout
        format(atom(Cmd),
            'echo "Fetching bootstrap script from ~w..." >&2; BODY=\'\'\'\'{"command":"cat /tmp/bootstrap.sh"}\'\'\'\'; TIMESTAMP=$(date +%s); MESSAGE="$TIMESTAMP:POST:/services/~w/execute:$BODY"; SIGNATURE=$(echo -n "$MESSAGE" | openssl dgst -sha256 -hmac "~w" -hex | sed \'\'\'\'s/.*= //\'\'\'\'); RESP=$(curl -s -X POST https://api.unsandbox.com/services/~w/execute -H "Content-Type: application/json" -H "Authorization: Bearer ~w" -H "X-Timestamp: $TIMESTAMP" -H "X-Signature: $SIGNATURE" -d "$BODY"); STDOUT=$(echo "$RESP" | jq -r ".stdout // empty"); if [ -n "$STDOUT" ]; then echo "$STDOUT"; else echo -e "\\x1b[31mError: Failed to fetch bootstrap\\x1b[0m" >&2; exit 1; fi',
            [ServiceId, ServiceId, SecretKey, ServiceId, PublicKey])
    ;   % File specified, save to file
        format(atom(Cmd),
            'echo "Fetching bootstrap script from ~w..." >&2; BODY=\'\'\'\'{"command":"cat /tmp/bootstrap.sh"}\'\'\'\'; TIMESTAMP=$(date +%s); MESSAGE="$TIMESTAMP:POST:/services/~w/execute:$BODY"; SIGNATURE=$(echo -n "$MESSAGE" | openssl dgst -sha256 -hmac "~w" -hex | sed \'\'\'\'s/.*= //\'\'\'\'); RESP=$(curl -s -X POST https://api.unsandbox.com/services/~w/execute -H "Content-Type: application/json" -H "Authorization: Bearer ~w" -H "X-Timestamp: $TIMESTAMP" -H "X-Signature: $SIGNATURE" -d "$BODY"); STDOUT=$(echo "$RESP" | jq -r ".stdout // empty"); if [ -n "$STDOUT" ]; then echo "$STDOUT" > "~w" && chmod 755 "~w" && echo "Bootstrap saved to ~w"; else echo -e "\\x1b[31mError: Failed to fetch bootstrap\\x1b[0m" >&2; exit 1; fi',
            [ServiceId, ServiceId, SecretKey, ServiceId, PublicKey, DumpFile, DumpFile, DumpFile])
    ),
    shell(Cmd, 0).

% Service create with optional input files
service_create(Name, Ports, Bootstrap, BootstrapFile, ServiceType, InputFiles) :-
    get_public_key(PublicKey),
    get_secret_key(SecretKey),
    % Build JSON payload
    (   Ports \= ''
    ->  format(atom(PortsJson), ',"ports":[~w]', [Ports])
    ;   PortsJson = ''
    ),
    (   Bootstrap \= ''
    ->  format(atom(BootstrapJson), ',"bootstrap":"~w"', [Bootstrap])
    ;   BootstrapJson = ''
    ),
    (   BootstrapFile \= ''
    ->  (   exists_file(BootstrapFile)
        ->  read_file_content(BootstrapFile, BootstrapContent),
            format(atom(BootstrapContentJson), ',"bootstrap_content":"~w"', [BootstrapContent])
        ;   format(user_error, 'Error: Bootstrap file not found: ~w~n', [BootstrapFile]),
            halt(1)
        )
    ;   BootstrapContentJson = ''
    ),
    (   ServiceType \= ''
    ->  format(atom(ServiceTypeJson), ',"service_type":"~w"', [ServiceType])
    ;   ServiceTypeJson = ''
    ),
    % Build file arguments for bash script
    build_file_args(InputFiles, FileArgs),
    format(atom(Cmd),
        'echo -e "\\x1b[33mCreating service...\\x1b[0m"; INPUT_FILES=""; ~w if [ -n "$INPUT_FILES" ]; then INPUT_FILES_JSON=",\\\"input_files\\\":[$INPUT_FILES]"; else INPUT_FILES_JSON=""; fi; BODY="{\\\"name\\\":\\\"~w\\\"~w~w~w~w$INPUT_FILES_JSON}"; TIMESTAMP=$(date +%s); MESSAGE="$TIMESTAMP:POST:/services:$BODY"; SIGNATURE=$(echo -n "$MESSAGE" | openssl dgst -sha256 -hmac "~w" -hex | sed \'\'s/.*= //\'\'); curl -s -X POST https://api.unsandbox.com/services -H "Content-Type: application/json" -H "Authorization: Bearer ~w" -H "X-Timestamp: $TIMESTAMP" -H "X-Signature: $SIGNATURE" -d "$BODY" | jq . && echo -e "\\x1b[32mService created\\x1b[0m"',
        [FileArgs, Name, PortsJson, BootstrapJson, BootstrapContentJson, ServiceTypeJson, SecretKey, PublicKey]),
    shell(Cmd, 0).

% Key validate
validate_key(Extend) :-
    get_public_key(PublicKey),
    get_secret_key(SecretKey),
    portal_base(PortalBase),
    (   Extend = true
    ->  % Build command for --extend mode
        format(atom(Cmd),
            'BODY=\'\'{}\'\'; TIMESTAMP=$(date +%s); MESSAGE="$TIMESTAMP:POST:/keys/validate:$BODY"; SIGNATURE=$(echo -n "$MESSAGE" | openssl dgst -sha256 -hmac "~w" -hex | sed \'\'s/.*= //\'\'); RESP=$(curl -s -X POST ~w/keys/validate -H "Content-Type: application/json" -H "Authorization: Bearer ~w" -H "X-Timestamp: $TIMESTAMP" -H "X-Signature: $SIGNATURE" -d "$BODY"); if echo "$RESP" | grep -qi "timestamp" && echo "$RESP" | grep -Eqi "(401|expired|invalid)"; then echo -e "\\x1b[31mError: Request timestamp expired (must be within 5 minutes of server time)\\x1b[0m" >&2; echo -e "\\x1b[33mYour computer'\''s clock may have drifted.\\x1b[0m" >&2; echo "Check your system time and sync with NTP if needed:" >&2; echo "  Linux:   sudo ntpdate -s time.nist.gov" >&2; echo "  macOS:   sudo sntp -sS time.apple.com" >&2; echo "  Windows: w32tm /resync\\x1b[0m" >&2; exit 1; fi; PUBLIC_KEY=$(echo "$RESP" | jq -r ".public_key // \\"N/A\\""); xdg-open "~w/keys/extend?pk=$PUBLIC_KEY" 2>/dev/null',
            [SecretKey, PortalBase, PublicKey, PortalBase])
    ;   % Build command for normal validation
        format(atom(Cmd),
            'BODY=\'\'{}\'\'; TIMESTAMP=$(date +%s); MESSAGE="$TIMESTAMP:POST:/keys/validate:$BODY"; SIGNATURE=$(echo -n "$MESSAGE" | openssl dgst -sha256 -hmac "~w" -hex | sed \'\'s/.*= //\'\'); curl -s -X POST ~w/keys/validate -H "Content-Type: application/json" -H "Authorization: Bearer ~w" -H "X-Timestamp: $TIMESTAMP" -H "X-Signature: $SIGNATURE" -d "$BODY" -o /tmp/unsandbox_key_resp.json; STATUS=$?; if [ $STATUS -ne 0 ]; then echo -e "\\x1b[31mInvalid\\x1b[0m"; exit 1; fi; RESP=$(cat /tmp/unsandbox_key_resp.json); if echo "$RESP" | grep -qi "timestamp" && echo "$RESP" | grep -Eqi "(401|expired|invalid)"; then echo -e "\\x1b[31mError: Request timestamp expired (must be within 5 minutes of server time)\\x1b[0m" >&2; echo -e "\\x1b[33mYour computer'\''s clock may have drifted.\\x1b[0m" >&2; echo "Check your system time and sync with NTP if needed:" >&2; echo "  Linux:   sudo ntpdate -s time.nist.gov" >&2; echo "  macOS:   sudo sntp -sS time.apple.com" >&2; echo "  Windows: w32tm /resync\\x1b[0m" >&2; rm -f /tmp/unsandbox_key_resp.json; exit 1; fi; EXPIRED=$(jq -r ".expired // false" /tmp/unsandbox_key_resp.json); if [ "$EXPIRED" = "true" ]; then echo -e "\\x1b[31mExpired\\x1b[0m"; echo "Public Key: $(jq -r ".public_key // \\"N/A\\"" /tmp/unsandbox_key_resp.json)"; echo "Tier: $(jq -r ".tier // \\"N/A\\"" /tmp/unsandbox_key_resp.json)"; echo "Expired: $(jq -r ".expires_at // \\"N/A\\"" /tmp/unsandbox_key_resp.json)"; echo -e "\\x1b[33mTo renew: Visit https://unsandbox.com/keys/extend\\x1b[0m"; rm -f /tmp/unsandbox_key_resp.json; exit 1; else echo -e "\\x1b[32mValid\\x1b[0m"; echo "Public Key: $(jq -r ".public_key // \\"N/A\\"" /tmp/unsandbox_key_resp.json)"; echo "Tier: $(jq -r ".tier // \\"N/A\\"" /tmp/unsandbox_key_resp.json)"; echo "Status: $(jq -r ".status // \\"N/A\\"" /tmp/unsandbox_key_resp.json)"; echo "Expires: $(jq -r ".expires_at // \\"N/A\\"" /tmp/unsandbox_key_resp.json)"; echo "Time Remaining: $(jq -r ".time_remaining // \\"N/A\\"" /tmp/unsandbox_key_resp.json)"; echo "Rate Limit: $(jq -r ".rate_limit // \\"N/A\\"" /tmp/unsandbox_key_resp.json)"; echo "Burst: $(jq -r ".burst // \\"N/A\\"" /tmp/unsandbox_key_resp.json)"; echo "Concurrency: $(jq -r ".concurrency // \\"N/A\\"" /tmp/unsandbox_key_resp.json)"; fi; rm -f /tmp/unsandbox_key_resp.json',
            [SecretKey, PortalBase, PublicKey])
    ),
    shell(Cmd, 0).

% Handle key subcommand
handle_key(['--extend'|_]) :- validate_key(true).
handle_key(_) :- validate_key(false).

% Handle session subcommand
handle_session(['--list'|_]) :- session_list.
handle_session(['-l'|_]) :- session_list.
handle_session(['--kill', SessionId|_]) :- session_kill(SessionId).
handle_session(Args) :-
    parse_session_args(Args, '', [], Shell, InputFiles),
    session_create(Shell, InputFiles).

% Parse session arguments for -f and --shell
parse_session_args([], Shell, Files, Shell, Files).
parse_session_args(['--shell', ShellVal|Rest], _, Files, Shell, InputFiles) :-
    parse_session_args(Rest, ShellVal, Files, Shell, InputFiles).
parse_session_args(['-s', ShellVal|Rest], _, Files, Shell, InputFiles) :-
    parse_session_args(Rest, ShellVal, Files, Shell, InputFiles).
parse_session_args(['-f', FilePath|Rest], Shell, Files, ShellOut, InputFiles) :-
    (   exists_file(FilePath)
    ->  append(Files, [FilePath], NewFiles),
        parse_session_args(Rest, Shell, NewFiles, ShellOut, InputFiles)
    ;   format(user_error, 'Error: File not found: ~w~n', [FilePath]),
        halt(1)
    ).
parse_session_args([_|Rest], Shell, Files, ShellOut, InputFiles) :-
    parse_session_args(Rest, Shell, Files, ShellOut, InputFiles).

% Handle service subcommand
handle_service(Args) :-
    parse_service_args(Args, '', '', '', '', '', [], Action, InputFiles),
    execute_service_action(Action, InputFiles).

% Parse service arguments
parse_service_args([], Name, Ports, Bootstrap, BootstrapFile, ServiceType, InputFiles, create, InputFiles) :-
    (   Name \= ''
    ->  service_create(Name, Ports, Bootstrap, BootstrapFile, ServiceType, InputFiles)
    ;   write(user_error, 'Error: --name required for service creation\n'),
        halt(1)
    ).
parse_service_args([], _, _, _, _, _, InputFiles, Action, InputFiles) :-
    (   Action = list
    ->  service_list
    ;   write(user_error, 'Error: Use --list, --info, --logs, --freeze, --unfreeze, --destroy, or --name\n'),
        halt(1)
    ).
parse_service_args(['--list'|_], _, _, _, _, _, _, _, _) :- service_list.
parse_service_args(['-l'|_], _, _, _, _, _, _, _, _) :- service_list.
parse_service_args(['--info', ServiceId|_], _, _, _, _, _, _, _, _) :- service_info(ServiceId).
parse_service_args(['--logs', ServiceId|_], _, _, _, _, _, _, _, _) :- service_logs(ServiceId).
parse_service_args(['--freeze', ServiceId|_], _, _, _, _, _, _, _, _) :- service_sleep(ServiceId).
parse_service_args(['--unfreeze', ServiceId|_], _, _, _, _, _, _, _, _) :- service_wake(ServiceId).
parse_service_args(['--destroy', ServiceId|_], _, _, _, _, _, _, _, _) :- service_destroy(ServiceId).
parse_service_args(['--dump-bootstrap', ServiceId|Rest], _, _, _, _, _, _, _, _) :-
    (   Rest = ['--dump-file', DumpFile|_]
    ->  service_dump_bootstrap(ServiceId, DumpFile)
    ;   service_dump_bootstrap(ServiceId, '')
    ).
parse_service_args(['--name', Name|Rest], _, Ports, Bootstrap, BootstrapFile, ServiceType, InputFiles, _, InputFilesOut) :-
    parse_service_args(Rest, Name, Ports, Bootstrap, BootstrapFile, ServiceType, InputFiles, create, InputFilesOut).
parse_service_args(['--ports', PortsList|Rest], Name, _, Bootstrap, BootstrapFile, ServiceType, InputFiles, Action, InputFilesOut) :-
    parse_service_args(Rest, Name, PortsList, Bootstrap, BootstrapFile, ServiceType, InputFiles, Action, InputFilesOut).
parse_service_args(['--bootstrap', BootstrapVal|Rest], Name, Ports, _, BootstrapFile, ServiceType, InputFiles, Action, InputFilesOut) :-
    parse_service_args(Rest, Name, Ports, BootstrapVal, BootstrapFile, ServiceType, InputFiles, Action, InputFilesOut).
parse_service_args(['--bootstrap-file', BootstrapFileVal|Rest], Name, Ports, Bootstrap, _, ServiceType, InputFiles, Action, InputFilesOut) :-
    parse_service_args(Rest, Name, Ports, Bootstrap, BootstrapFileVal, ServiceType, InputFiles, Action, InputFilesOut).
parse_service_args(['--type', Type|Rest], Name, Ports, Bootstrap, BootstrapFile, _, InputFiles, Action, InputFilesOut) :-
    parse_service_args(Rest, Name, Ports, Bootstrap, BootstrapFile, Type, InputFiles, Action, InputFilesOut).
parse_service_args(['-f', FilePath|Rest], Name, Ports, Bootstrap, BootstrapFile, ServiceType, InputFiles, Action, InputFilesOut) :-
    (   exists_file(FilePath)
    ->  append(InputFiles, [FilePath], NewInputFiles),
        parse_service_args(Rest, Name, Ports, Bootstrap, BootstrapFile, ServiceType, NewInputFiles, Action, InputFilesOut)
    ;   format(user_error, 'Error: File not found: ~w~n', [FilePath]),
        halt(1)
    ).
parse_service_args([_|Rest], Name, Ports, Bootstrap, BootstrapFile, ServiceType, InputFiles, Action, InputFilesOut) :-
    parse_service_args(Rest, Name, Ports, Bootstrap, BootstrapFile, ServiceType, InputFiles, Action, InputFilesOut).

% Execute service action (not used, but kept for structure)
execute_service_action(_, _).

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
    ;   Argv = ['key'|Rest]
    ->  handle_key(Rest)
    ;   Argv = [Filename|_]
    ->  execute_file(Filename)
    ;   write(user_error, 'Error: Invalid arguments\n'),
        halt(1)
    ).
