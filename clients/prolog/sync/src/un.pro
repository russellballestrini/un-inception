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
        'TIMESTAMP=$(date +%s); MESSAGE="$TIMESTAMP:POST:/services/~w/freeze:"; SIGNATURE=$(echo -n "$MESSAGE" | openssl dgst -sha256 -hmac "~w" -hex | sed \'\'s/.*= //\'\'); curl -s -X POST https://api.unsandbox.com/services/~w/freeze -H "Authorization: Bearer ~w" -H "X-Timestamp: $TIMESTAMP" -H "X-Signature: $SIGNATURE" >/dev/null && echo -e "\\x1b[32mService frozen: ~w\\x1b[0m"',
        [ServiceId, SecretKey, ServiceId, PublicKey, ServiceId]),
    shell(Cmd, 0).

% Service wake
service_wake(ServiceId) :-
    get_public_key(PublicKey),
    get_secret_key(SecretKey),
    format(atom(Cmd),
        'TIMESTAMP=$(date +%s); MESSAGE="$TIMESTAMP:POST:/services/~w/unfreeze:"; SIGNATURE=$(echo -n "$MESSAGE" | openssl dgst -sha256 -hmac "~w" -hex | sed \'\'s/.*= //\'\'); curl -s -X POST https://api.unsandbox.com/services/~w/unfreeze -H "Authorization: Bearer ~w" -H "X-Timestamp: $TIMESTAMP" -H "X-Signature: $SIGNATURE" >/dev/null && echo -e "\\x1b[32mService unfreezing: ~w\\x1b[0m"',
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

% Service resize
service_resize(ServiceId, Vcpu) :-
    get_public_key(PublicKey),
    get_secret_key(SecretKey),
    Ram is Vcpu * 2,
    format(atom(Cmd),
        'BODY=\'\'{\"vcpu\":~w}\'\'; TIMESTAMP=$(date +%s); MESSAGE="$TIMESTAMP:PATCH:/services/~w:$BODY"; SIGNATURE=$(echo -n "$MESSAGE" | openssl dgst -sha256 -hmac "~w" -hex | sed \'\'s/.*= //\'\'); curl -s -X PATCH https://api.unsandbox.com/services/~w -H "Content-Type: application/json" -H "Authorization: Bearer ~w" -H "X-Timestamp: $TIMESTAMP" -H "X-Signature: $SIGNATURE" -d "$BODY" >/dev/null && echo -e "\\x1b[32mService resized to ~w vCPU, ~w GB RAM\\x1b[0m"',
        [Vcpu, ServiceId, SecretKey, ServiceId, PublicKey, Vcpu, Ram]),
    shell(Cmd, 0).

% Service env status
service_env_status(ServiceId) :-
    get_public_key(PublicKey),
    get_secret_key(SecretKey),
    format(atom(Cmd),
        'TIMESTAMP=$(date +%s); MESSAGE="$TIMESTAMP:GET:/services/~w/env:"; SIGNATURE=$(echo -n "$MESSAGE" | openssl dgst -sha256 -hmac "~w" -hex | sed \'\'s/.*= //\'\'); curl -s -X GET "https://api.unsandbox.com/services/~w/env" -H "Authorization: Bearer ~w" -H "X-Timestamp: $TIMESTAMP" -H "X-Signature: $SIGNATURE" | jq .',
        [ServiceId, SecretKey, ServiceId, PublicKey]),
    shell(Cmd, 0).

% Service env set
service_env_set(ServiceId, Envs, EnvFile) :-
    get_public_key(PublicKey),
    get_secret_key(SecretKey),
    format(atom(Cmd),
        'ENV_CONTENT=""; ENV_LINES="~w"; if [ -n "$ENV_LINES" ]; then ENV_CONTENT="$ENV_LINES"; fi; ENV_FILE="~w"; if [ -n "$ENV_FILE" ] && [ -f "$ENV_FILE" ]; then while IFS= read -r line || [ -n "$line" ]; do case "$line" in "#"*|"") continue ;; esac; if [ -n "$ENV_CONTENT" ]; then ENV_CONTENT="$ENV_CONTENT\\n"; fi; ENV_CONTENT="$ENV_CONTENT$line"; done < "$ENV_FILE"; fi; if [ -z "$ENV_CONTENT" ]; then echo -e "\\x1b[31mError: No environment variables to set\\x1b[0m" >&2; exit 1; fi; TIMESTAMP=$(date +%s); MESSAGE="$TIMESTAMP:PUT:/services/~w/env:$ENV_CONTENT"; SIGNATURE=$(echo -n "$MESSAGE" | openssl dgst -sha256 -hmac "~w" -hex | sed \'\'s/.*= //\'\'); curl -s -X PUT "https://api.unsandbox.com/services/~w/env" -H "Authorization: Bearer ~w" -H "X-Timestamp: $TIMESTAMP" -H "X-Signature: $SIGNATURE" -H "Content-Type: text/plain" --data-binary "$ENV_CONTENT" | jq .',
        [Envs, EnvFile, ServiceId, SecretKey, ServiceId, PublicKey]),
    shell(Cmd, 0).

% Service env export
service_env_export(ServiceId) :-
    get_public_key(PublicKey),
    get_secret_key(SecretKey),
    format(atom(Cmd),
        'TIMESTAMP=$(date +%s); MESSAGE="$TIMESTAMP:POST:/services/~w/env/export:"; SIGNATURE=$(echo -n "$MESSAGE" | openssl dgst -sha256 -hmac "~w" -hex | sed \'\'s/.*= //\'\'); curl -s -X POST "https://api.unsandbox.com/services/~w/env/export" -H "Authorization: Bearer ~w" -H "X-Timestamp: $TIMESTAMP" -H "X-Signature: $SIGNATURE" | jq -r ".content // empty"',
        [ServiceId, SecretKey, ServiceId, PublicKey]),
    shell(Cmd, 0).

% Service env delete
service_env_delete(ServiceId) :-
    get_public_key(PublicKey),
    get_secret_key(SecretKey),
    format(atom(Cmd),
        'TIMESTAMP=$(date +%s); MESSAGE="$TIMESTAMP:DELETE:/services/~w/env:"; SIGNATURE=$(echo -n "$MESSAGE" | openssl dgst -sha256 -hmac "~w" -hex | sed \'\'s/.*= //\'\'); curl -s -X DELETE "https://api.unsandbox.com/services/~w/env" -H "Authorization: Bearer ~w" -H "X-Timestamp: $TIMESTAMP" -H "X-Signature: $SIGNATURE" >/dev/null && echo -e "\\x1b[32mVault deleted for: ~w\\x1b[0m"',
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

% Languages command
languages_command(JsonOutput) :-
    get_public_key(PublicKey),
    get_secret_key(SecretKey),
    (   JsonOutput = true
    ->  % Output as JSON array
        format(atom(Cmd),
            'TIMESTAMP=$(date +%s); MESSAGE="$TIMESTAMP:GET:/languages:"; SIGNATURE=$(echo -n "$MESSAGE" | openssl dgst -sha256 -hmac "~w" -hex | sed \'\'s/.*= //\'\'); curl -s -X GET https://api.unsandbox.com/languages -H "Authorization: Bearer ~w" -H "X-Timestamp: $TIMESTAMP" -H "X-Signature: $SIGNATURE" | jq -c ".languages // []"',
            [SecretKey, PublicKey])
    ;   % Output one language per line
        format(atom(Cmd),
            'TIMESTAMP=$(date +%s); MESSAGE="$TIMESTAMP:GET:/languages:"; SIGNATURE=$(echo -n "$MESSAGE" | openssl dgst -sha256 -hmac "~w" -hex | sed \'\'s/.*= //\'\'); curl -s -X GET https://api.unsandbox.com/languages -H "Authorization: Bearer ~w" -H "X-Timestamp: $TIMESTAMP" -H "X-Signature: $SIGNATURE" | jq -r ".languages[]"',
            [SecretKey, PublicKey])
    ),
    shell(Cmd, 0).

% Handle languages subcommand
handle_languages(['--json'|_]) :- languages_command(true).
handle_languages(_) :- languages_command(false).

% Handle key subcommand
handle_key(['--extend'|_]) :- validate_key(true).
handle_key(_) :- validate_key(false).

% Image list
image_list :-
    get_public_key(PublicKey),
    get_secret_key(SecretKey),
    format(atom(Cmd),
        'TIMESTAMP=$(date +%s); MESSAGE="$TIMESTAMP:GET:/images:"; SIGNATURE=$(echo -n "$MESSAGE" | openssl dgst -sha256 -hmac "~w" -hex | sed \'\'s/.*= //\'\'); curl -s -X GET https://api.unsandbox.com/images -H "Authorization: Bearer ~w" -H "X-Timestamp: $TIMESTAMP" -H "X-Signature: $SIGNATURE" | jq .',
        [SecretKey, PublicKey]),
    shell(Cmd, 0).

% Image info
image_info(ImageId) :-
    get_public_key(PublicKey),
    get_secret_key(SecretKey),
    format(atom(Cmd),
        'TIMESTAMP=$(date +%s); MESSAGE="$TIMESTAMP:GET:/images/~w:"; SIGNATURE=$(echo -n "$MESSAGE" | openssl dgst -sha256 -hmac "~w" -hex | sed \'\'s/.*= //\'\'); curl -s -X GET https://api.unsandbox.com/images/~w -H "Authorization: Bearer ~w" -H "X-Timestamp: $TIMESTAMP" -H "X-Signature: $SIGNATURE" | jq .',
        [ImageId, SecretKey, ImageId, PublicKey]),
    shell(Cmd, 0).

% Image delete
image_delete(ImageId) :-
    get_public_key(PublicKey),
    get_secret_key(SecretKey),
    format(atom(Cmd),
        'TIMESTAMP=$(date +%s); MESSAGE="$TIMESTAMP:DELETE:/images/~w:"; SIGNATURE=$(echo -n "$MESSAGE" | openssl dgst -sha256 -hmac "~w" -hex | sed \'\'s/.*= //\'\'); curl -s -X DELETE https://api.unsandbox.com/images/~w -H "Authorization: Bearer ~w" -H "X-Timestamp: $TIMESTAMP" -H "X-Signature: $SIGNATURE" >/dev/null && echo -e "\\x1b[32mImage deleted: ~w\\x1b[0m"',
        [ImageId, SecretKey, ImageId, PublicKey, ImageId]),
    shell(Cmd, 0).

% Image lock
image_lock(ImageId) :-
    get_public_key(PublicKey),
    get_secret_key(SecretKey),
    format(atom(Cmd),
        'TIMESTAMP=$(date +%s); MESSAGE="$TIMESTAMP:POST:/images/~w/lock:"; SIGNATURE=$(echo -n "$MESSAGE" | openssl dgst -sha256 -hmac "~w" -hex | sed \'\'s/.*= //\'\'); curl -s -X POST https://api.unsandbox.com/images/~w/lock -H "Authorization: Bearer ~w" -H "X-Timestamp: $TIMESTAMP" -H "X-Signature: $SIGNATURE" >/dev/null && echo -e "\\x1b[32mImage locked: ~w\\x1b[0m"',
        [ImageId, SecretKey, ImageId, PublicKey, ImageId]),
    shell(Cmd, 0).

% Image unlock
image_unlock(ImageId) :-
    get_public_key(PublicKey),
    get_secret_key(SecretKey),
    format(atom(Cmd),
        'TIMESTAMP=$(date +%s); MESSAGE="$TIMESTAMP:POST:/images/~w/unlock:"; SIGNATURE=$(echo -n "$MESSAGE" | openssl dgst -sha256 -hmac "~w" -hex | sed \'\'s/.*= //\'\'); curl -s -X POST https://api.unsandbox.com/images/~w/unlock -H "Authorization: Bearer ~w" -H "X-Timestamp: $TIMESTAMP" -H "X-Signature: $SIGNATURE" >/dev/null && echo -e "\\x1b[32mImage unlocked: ~w\\x1b[0m"',
        [ImageId, SecretKey, ImageId, PublicKey, ImageId]),
    shell(Cmd, 0).

% Image publish
image_publish(SourceId, SourceType, Name) :-
    get_public_key(PublicKey),
    get_secret_key(SecretKey),
    (   Name \= ''
    ->  format(atom(NameJson), ',\\\"name\\\":\\\"~w\\\"', [Name])
    ;   NameJson = ''
    ),
    format(atom(Cmd),
        'BODY="{\\\"source_type\\\":\\\"~w\\\",\\\"source_id\\\":\\\"~w\\\"~w}"; TIMESTAMP=$(date +%s); MESSAGE="$TIMESTAMP:POST:/images/publish:$BODY"; SIGNATURE=$(echo -n "$MESSAGE" | openssl dgst -sha256 -hmac "~w" -hex | sed \'\'s/.*= //\'\'); curl -s -X POST https://api.unsandbox.com/images/publish -H "Content-Type: application/json" -H "Authorization: Bearer ~w" -H "X-Timestamp: $TIMESTAMP" -H "X-Signature: $SIGNATURE" -d "$BODY" | jq . && echo -e "\\x1b[32mImage published\\x1b[0m"',
        [SourceType, SourceId, NameJson, SecretKey, PublicKey]),
    shell(Cmd, 0).

% Image visibility
image_visibility(ImageId, Mode) :-
    get_public_key(PublicKey),
    get_secret_key(SecretKey),
    format(atom(Cmd),
        'BODY="{\\\"visibility\\\":\\\"~w\\\"}"; TIMESTAMP=$(date +%s); MESSAGE="$TIMESTAMP:POST:/images/~w/visibility:$BODY"; SIGNATURE=$(echo -n "$MESSAGE" | openssl dgst -sha256 -hmac "~w" -hex | sed \'\'s/.*= //\'\'); curl -s -X POST https://api.unsandbox.com/images/~w/visibility -H "Content-Type: application/json" -H "Authorization: Bearer ~w" -H "X-Timestamp: $TIMESTAMP" -H "X-Signature: $SIGNATURE" -d "$BODY" >/dev/null && echo -e "\\x1b[32mImage visibility set to ~w: ~w\\x1b[0m"',
        [Mode, ImageId, SecretKey, ImageId, PublicKey, Mode, ImageId]),
    shell(Cmd, 0).

% Image spawn
image_spawn(ImageId, Name, Ports) :-
    get_public_key(PublicKey),
    get_secret_key(SecretKey),
    (   Name \= ''
    ->  format(atom(NameJson), '\\\"name\\\":\\\"~w\\\"', [Name])
    ;   NameJson = ''
    ),
    (   Ports \= ''
    ->  (   Name \= ''
        ->  format(atom(PortsJson), ',\\\"ports\\\":[~w]', [Ports])
        ;   format(atom(PortsJson), '\\\"ports\\\":[~w]', [Ports])
        )
    ;   PortsJson = ''
    ),
    format(atom(Cmd),
        'BODY="{~w~w}"; TIMESTAMP=$(date +%s); MESSAGE="$TIMESTAMP:POST:/images/~w/spawn:$BODY"; SIGNATURE=$(echo -n "$MESSAGE" | openssl dgst -sha256 -hmac "~w" -hex | sed \'\'s/.*= //\'\'); curl -s -X POST https://api.unsandbox.com/images/~w/spawn -H "Content-Type: application/json" -H "Authorization: Bearer ~w" -H "X-Timestamp: $TIMESTAMP" -H "X-Signature: $SIGNATURE" -d "$BODY" | jq . && echo -e "\\x1b[32mService spawned from image\\x1b[0m"',
        [NameJson, PortsJson, ImageId, SecretKey, ImageId, PublicKey]),
    shell(Cmd, 0).

% Image clone
image_clone(ImageId, Name) :-
    get_public_key(PublicKey),
    get_secret_key(SecretKey),
    (   Name \= ''
    ->  format(atom(NameJson), '\\\"name\\\":\\\"~w\\\"', [Name])
    ;   NameJson = ''
    ),
    format(atom(Cmd),
        'BODY="{~w}"; TIMESTAMP=$(date +%s); MESSAGE="$TIMESTAMP:POST:/images/~w/clone:$BODY"; SIGNATURE=$(echo -n "$MESSAGE" | openssl dgst -sha256 -hmac "~w" -hex | sed \'\'s/.*= //\'\'); curl -s -X POST https://api.unsandbox.com/images/~w/clone -H "Content-Type: application/json" -H "Authorization: Bearer ~w" -H "X-Timestamp: $TIMESTAMP" -H "X-Signature: $SIGNATURE" -d "$BODY" | jq . && echo -e "\\x1b[32mImage cloned\\x1b[0m"',
        [NameJson, ImageId, SecretKey, ImageId, PublicKey]),
    shell(Cmd, 0).

% Handle image subcommand
handle_image(['--list'|_]) :- !, image_list.
handle_image(['-l'|_]) :- !, image_list.
handle_image(['--info', ImageId|_]) :- !, image_info(ImageId).
handle_image(['--delete', ImageId|_]) :- !, image_delete(ImageId).
handle_image(['--lock', ImageId|_]) :- !, image_lock(ImageId).
handle_image(['--unlock', ImageId|_]) :- !, image_unlock(ImageId).
handle_image(['--publish', SourceId, '--source-type', SourceType|Rest]) :- !,
    parse_image_name(Rest, '', Name),
    image_publish(SourceId, SourceType, Name).
handle_image(['--publish', _|_]) :- !,
    write(user_error, '\x1b[31mError: --publish requires --source-type (service or snapshot)\x1b[0m\n'),
    halt(1).
handle_image(['--visibility', ImageId, Mode|_]) :- !, image_visibility(ImageId, Mode).
handle_image(['--spawn', ImageId|Rest]) :- !,
    parse_spawn_args(Rest, '', '', Name, Ports),
    image_spawn(ImageId, Name, Ports).
handle_image(['--clone', ImageId|Rest]) :- !,
    parse_image_name(Rest, '', Name),
    image_clone(ImageId, Name).
handle_image(_) :-
    write(user_error, '\x1b[31mError: Use --list, --info, --delete, --lock, --unlock, --publish, --visibility, --spawn, or --clone\x1b[0m\n'),
    halt(1).

% Parse --name from arguments
parse_image_name([], Name, Name).
parse_image_name(['--name', N|_], _, N) :- !.
parse_image_name([_|Rest], Name, NameOut) :- parse_image_name(Rest, Name, NameOut).

% Parse --name and --ports for spawn
parse_spawn_args([], Name, Ports, Name, Ports).
parse_spawn_args(['--name', N|Rest], _, Ports, NameOut, PortsOut) :- !,
    parse_spawn_args(Rest, N, Ports, NameOut, PortsOut).
parse_spawn_args(['--ports', P|Rest], Name, _, NameOut, PortsOut) :- !,
    parse_spawn_args(Rest, Name, P, NameOut, PortsOut).
parse_spawn_args([_|Rest], Name, Ports, NameOut, PortsOut) :-
    parse_spawn_args(Rest, Name, Ports, NameOut, PortsOut).

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
parse_session_args([Arg|Rest], Shell, Files, ShellOut, InputFiles) :-
    (   atom_chars(Arg, ['-'|_])
    ->  format(user_error, 'Unknown option: ~w~n', [Arg]),
        format(user_error, 'Usage: un.pro session [options]~n', []),
        halt(1)
    ;   parse_session_args(Rest, Shell, Files, ShellOut, InputFiles)
    ).

% Handle service subcommand
handle_service(['env', Action, ServiceId|Rest]) :-
    !,
    parse_env_args(Rest, '', '', Envs, EnvFile),
    handle_env_action(Action, ServiceId, Envs, EnvFile).
handle_service(Args) :-
    parse_service_args(Args, '', '', '', '', '', [], '', '', Action, InputFiles),
    execute_service_action(Action, InputFiles).

% Handle env action
handle_env_action('status', ServiceId, _, _) :- service_env_status(ServiceId).
handle_env_action('set', ServiceId, Envs, EnvFile) :- service_env_set(ServiceId, Envs, EnvFile).
handle_env_action('export', ServiceId, _, _) :- service_env_export(ServiceId).
handle_env_action('delete', ServiceId, _, _) :- service_env_delete(ServiceId).
handle_env_action(Action, _, _, _) :-
    format(user_error, 'Error: Unknown env action: ~w~n', [Action]),
    write(user_error, 'Usage: un.pro service env <status|set|export|delete> <service_id>\n'),
    halt(1).

% Parse env arguments for -e and --env-file
parse_env_args([], Envs, EnvFile, Envs, EnvFile).
parse_env_args(['-e', EnvVal|Rest], Envs, EnvFile, EnvsOut, EnvFileOut) :-
    (   Envs \= ''
    ->  format(atom(NewEnvs), '~w\\n~w', [Envs, EnvVal])
    ;   NewEnvs = EnvVal
    ),
    parse_env_args(Rest, NewEnvs, EnvFile, EnvsOut, EnvFileOut).
parse_env_args(['--env-file', EnvFileVal|Rest], Envs, _, EnvsOut, EnvFileOut) :-
    parse_env_args(Rest, Envs, EnvFileVal, EnvsOut, EnvFileOut).
parse_env_args([_|Rest], Envs, EnvFile, EnvsOut, EnvFileOut) :-
    parse_env_args(Rest, Envs, EnvFile, EnvsOut, EnvFileOut).

% Parse service arguments
parse_service_args([], Name, Ports, Bootstrap, BootstrapFile, ServiceType, InputFiles, Envs, EnvFile, create, InputFiles) :-
    (   Name \= ''
    ->  service_create_with_vault(Name, Ports, Bootstrap, BootstrapFile, ServiceType, InputFiles, Envs, EnvFile)
    ;   write(user_error, 'Error: --name required for service creation\n'),
        halt(1)
    ).
parse_service_args([], _, _, _, _, _, InputFiles, _, _, Action, InputFiles) :-
    (   Action = list
    ->  service_list
    ;   write(user_error, 'Error: Use --list, --info, --logs, --freeze, --unfreeze, --destroy, --name, or env\n'),
        halt(1)
    ).
parse_service_args(['--list'|_], _, _, _, _, _, _, _, _, _, _) :- service_list.
parse_service_args(['-l'|_], _, _, _, _, _, _, _, _, _, _) :- service_list.
parse_service_args(['--info', ServiceId|_], _, _, _, _, _, _, _, _, _, _) :- service_info(ServiceId).
parse_service_args(['--logs', ServiceId|_], _, _, _, _, _, _, _, _, _, _) :- service_logs(ServiceId).
parse_service_args(['--freeze', ServiceId|_], _, _, _, _, _, _, _, _, _, _) :- service_sleep(ServiceId).
parse_service_args(['--unfreeze', ServiceId|_], _, _, _, _, _, _, _, _, _, _) :- service_wake(ServiceId).
parse_service_args(['--destroy', ServiceId|_], _, _, _, _, _, _, _, _, _, _) :- service_destroy(ServiceId).
parse_service_args(['--resize', ServiceId, '--vcpu', VcpuAtom|_], _, _, _, _, _, _, _, _, _, _) :-
    atom_number(VcpuAtom, Vcpu),
    (   Vcpu >= 1, Vcpu =< 8
    ->  service_resize(ServiceId, Vcpu)
    ;   write(user_error, '\x1b[31mError: vCPU must be between 1 and 8\x1b[0m\n'),
        halt(1)
    ).
parse_service_args(['--resize', ServiceId, '-v', VcpuAtom|_], _, _, _, _, _, _, _, _, _, _) :-
    atom_number(VcpuAtom, Vcpu),
    (   Vcpu >= 1, Vcpu =< 8
    ->  service_resize(ServiceId, Vcpu)
    ;   write(user_error, '\x1b[31mError: vCPU must be between 1 and 8\x1b[0m\n'),
        halt(1)
    ).
parse_service_args(['--resize', _|_], _, _, _, _, _, _, _, _, _, _) :-
    write(user_error, '\x1b[31mError: --resize requires --vcpu or -v\x1b[0m\n'),
    halt(1).
parse_service_args(['--dump-bootstrap', ServiceId|Rest], _, _, _, _, _, _, _, _, _, _) :-
    (   Rest = ['--dump-file', DumpFile|_]
    ->  service_dump_bootstrap(ServiceId, DumpFile)
    ;   service_dump_bootstrap(ServiceId, '')
    ).
parse_service_args(['--name', Name|Rest], _, Ports, Bootstrap, BootstrapFile, ServiceType, InputFiles, Envs, EnvFile, _, InputFilesOut) :-
    parse_service_args(Rest, Name, Ports, Bootstrap, BootstrapFile, ServiceType, InputFiles, Envs, EnvFile, create, InputFilesOut).
parse_service_args(['--ports', PortsList|Rest], Name, _, Bootstrap, BootstrapFile, ServiceType, InputFiles, Envs, EnvFile, Action, InputFilesOut) :-
    parse_service_args(Rest, Name, PortsList, Bootstrap, BootstrapFile, ServiceType, InputFiles, Envs, EnvFile, Action, InputFilesOut).
parse_service_args(['--bootstrap', BootstrapVal|Rest], Name, Ports, _, BootstrapFile, ServiceType, InputFiles, Envs, EnvFile, Action, InputFilesOut) :-
    parse_service_args(Rest, Name, Ports, BootstrapVal, BootstrapFile, ServiceType, InputFiles, Envs, EnvFile, Action, InputFilesOut).
parse_service_args(['--bootstrap-file', BootstrapFileVal|Rest], Name, Ports, Bootstrap, _, ServiceType, InputFiles, Envs, EnvFile, Action, InputFilesOut) :-
    parse_service_args(Rest, Name, Ports, Bootstrap, BootstrapFileVal, ServiceType, InputFiles, Envs, EnvFile, Action, InputFilesOut).
parse_service_args(['--type', Type|Rest], Name, Ports, Bootstrap, BootstrapFile, _, InputFiles, Envs, EnvFile, Action, InputFilesOut) :-
    parse_service_args(Rest, Name, Ports, Bootstrap, BootstrapFile, Type, InputFiles, Envs, EnvFile, Action, InputFilesOut).
parse_service_args(['-e', EnvVal|Rest], Name, Ports, Bootstrap, BootstrapFile, ServiceType, InputFiles, Envs, EnvFile, Action, InputFilesOut) :-
    (   Envs \= ''
    ->  format(atom(NewEnvs), '~w\\n~w', [Envs, EnvVal])
    ;   NewEnvs = EnvVal
    ),
    parse_service_args(Rest, Name, Ports, Bootstrap, BootstrapFile, ServiceType, InputFiles, NewEnvs, EnvFile, Action, InputFilesOut).
parse_service_args(['--env-file', EnvFileVal|Rest], Name, Ports, Bootstrap, BootstrapFile, ServiceType, InputFiles, Envs, _, Action, InputFilesOut) :-
    parse_service_args(Rest, Name, Ports, Bootstrap, BootstrapFile, ServiceType, InputFiles, Envs, EnvFileVal, Action, InputFilesOut).
parse_service_args(['-f', FilePath|Rest], Name, Ports, Bootstrap, BootstrapFile, ServiceType, InputFiles, Envs, EnvFile, Action, InputFilesOut) :-
    (   exists_file(FilePath)
    ->  append(InputFiles, [FilePath], NewInputFiles),
        parse_service_args(Rest, Name, Ports, Bootstrap, BootstrapFile, ServiceType, NewInputFiles, Envs, EnvFile, Action, InputFilesOut)
    ;   format(user_error, 'Error: File not found: ~w~n', [FilePath]),
        halt(1)
    ).
parse_service_args([_|Rest], Name, Ports, Bootstrap, BootstrapFile, ServiceType, InputFiles, Envs, EnvFile, Action, InputFilesOut) :-
    parse_service_args(Rest, Name, Ports, Bootstrap, BootstrapFile, ServiceType, InputFiles, Envs, EnvFile, Action, InputFilesOut).

% Execute service action (not used, but kept for structure)
execute_service_action(_, _).

% Service create with auto-vault
service_create_with_vault(Name, Ports, Bootstrap, BootstrapFile, ServiceType, InputFiles, Envs, EnvFile) :-
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
        'echo -e "\\x1b[33mCreating service...\\x1b[0m"; INPUT_FILES=""; ~w if [ -n "$INPUT_FILES" ]; then INPUT_FILES_JSON=",\\\"input_files\\\":[$INPUT_FILES]"; else INPUT_FILES_JSON=""; fi; BODY="{\\\"name\\\":\\\"~w\\\"~w~w~w~w$INPUT_FILES_JSON}"; TIMESTAMP=$(date +%s); MESSAGE="$TIMESTAMP:POST:/services:$BODY"; SIGNATURE=$(echo -n "$MESSAGE" | openssl dgst -sha256 -hmac "~w" -hex | sed \'\'s/.*= //\'\'); RESP=$(curl -s -X POST https://api.unsandbox.com/services -H "Content-Type: application/json" -H "Authorization: Bearer ~w" -H "X-Timestamp: $TIMESTAMP" -H "X-Signature: $SIGNATURE" -d "$BODY"); SVC_ID=$(echo "$RESP" | jq -r ".id // empty"); if [ -n "$SVC_ID" ]; then echo -e "\\x1b[32m$SVC_ID created\\x1b[0m"; ENV_CONTENT=""; ENV_LINES="~w"; if [ -n "$ENV_LINES" ]; then ENV_CONTENT="$ENV_LINES"; fi; ENV_FILE="~w"; if [ -n "$ENV_FILE" ] && [ -f "$ENV_FILE" ]; then while IFS= read -r line || [ -n "$line" ]; do case "$line" in "#"*|"") continue ;; esac; if [ -n "$ENV_CONTENT" ]; then ENV_CONTENT="$ENV_CONTENT\\n"; fi; ENV_CONTENT="$ENV_CONTENT$line"; done < "$ENV_FILE"; fi; if [ -n "$ENV_CONTENT" ]; then TS2=$(date +%s); SIG2=$(echo -n "$TS2:PUT:/services/$SVC_ID/env:$ENV_CONTENT" | openssl dgst -sha256 -hmac "~w" -hex | sed \'\'s/.*= //\'\'); curl -s -X PUT "https://api.unsandbox.com/services/$SVC_ID/env" -H "Authorization: Bearer ~w" -H "X-Timestamp: $TS2" -H "X-Signature: $SIG2" -H "Content-Type: text/plain" --data-binary "$ENV_CONTENT" >/dev/null && echo -e "\\x1b[32mVault configured\\x1b[0m"; fi; else echo "$RESP" | jq .; fi',
        [FileArgs, Name, PortsJson, BootstrapJson, BootstrapContentJson, ServiceTypeJson, SecretKey, PublicKey, Envs, EnvFile, SecretKey, PublicKey]),
    shell(Cmd, 0).

% Main program
main(Argv) :-
    % Check arguments
    (   Argv = []
    ->  write(user_error, 'Usage: un.pro [options] <source_file>\n'),
        write(user_error, '       un.pro session [options]\n'),
        write(user_error, '       un.pro service [options]\n'),
        write(user_error, '       un.pro image [options]\n'),
        write(user_error, '       un.pro languages [--json]\n'),
        write(user_error, '       un.pro key [options]\n'),
        write(user_error, '\n'),
        write(user_error, 'Image options:\n'),
        write(user_error, '  --list, -l          List all images\n'),
        write(user_error, '  --info ID           Get image details\n'),
        write(user_error, '  --delete ID         Delete an image\n'),
        write(user_error, '  --lock ID           Lock image\n'),
        write(user_error, '  --unlock ID         Unlock image\n'),
        write(user_error, '  --publish ID --source-type TYPE  Publish image\n'),
        write(user_error, '  --visibility ID MODE Set visibility\n'),
        write(user_error, '  --spawn ID          Spawn service from image\n'),
        write(user_error, '  --clone ID          Clone an image\n'),
        write(user_error, '  --name NAME         Name for spawned/cloned\n'),
        write(user_error, '  --ports PORTS       Ports for spawned service\n'),
        halt(1)
    ;   true
    ),

    % Parse subcommands
    (   Argv = ['session'|Rest]
    ->  handle_session(Rest)
    ;   Argv = ['service'|Rest]
    ->  handle_service(Rest)
    ;   Argv = ['image'|Rest]
    ->  handle_image(Rest)
    ;   Argv = ['languages'|Rest]
    ->  handle_languages(Rest)
    ;   Argv = ['key'|Rest]
    ->  handle_key(Rest)
    ;   Argv = [Filename|_]
    ->  execute_file(Filename)
    ;   write(user_error, 'Error: Invalid arguments\n'),
        halt(1)
    ).
