#!/usr/bin/env swipl
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


:- initialization(main, main).

% Initialize global account index to -1 (not set)
:- nb_setval(account_index, -1).

% Constants
portal_base('https://unsandbox.com').
languages_cache_ttl(3600).  % 1 hour cache TTL

% Get languages cache file path
languages_cache_file(Path) :-
    getenv('HOME', Home),
    atomic_list_concat([Home, '/.unsandbox/languages.json'], Path).

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

% Load credentials from accounts.csv at 0-indexed row
% Tries ~/.unsandbox/accounts.csv first, then ./accounts.csv
load_accounts_csv(Index, PK, SK) :-
    getenv('HOME', Home),
    atomic_list_concat([Home, '/.unsandbox/accounts.csv'], Path1),
    (   exists_file(Path1)
    ->  load_accounts_csv_file(Path1, Index, PK, SK)
    ;   exists_file('accounts.csv')
    ->  load_accounts_csv_file('accounts.csv', Index, PK, SK)
    ;   fail
    ).

load_accounts_csv_file(Path, Index, PK, SK) :-
    setup_call_cleanup(
        open(Path, read, Stream),
        load_accounts_csv_stream(Stream, 0, Index, PK, SK),
        close(Stream)
    ).

load_accounts_csv_stream(Stream, Count, Index, PK, SK) :-
    read_line_to_string(Stream, Line),
    Line \= end_of_file,
    !,
    % Skip blank lines and comments
    (   (Line = "" ; string_code(1, Line, 35))  % 35 = '#'
    ->  load_accounts_csv_stream(Stream, Count, Index, PK, SK)
    ;   Count =:= Index
    ->  split_string(Line, ",", " \t", [PKStr, SKStr|_]),
        atom_string(PK, PKStr),
        atom_string(SK, SKStr)
    ;   Next is Count + 1,
        load_accounts_csv_stream(Stream, Next, Index, PK, SK)
    ).

% Get API keys from environment (HMAC or legacy), respecting --account N
get_public_key(PublicKey) :-
    (   nb_getval(account_index, Idx), Idx >= 0
    ->  (   load_accounts_csv(Idx, PublicKey, _)
        ->  true
        ;   format(user_error, 'Error: Account index ~w not found in accounts.csv~n', [Idx]),
            halt(1)
        )
    ;   (   getenv('UNSANDBOX_PUBLIC_KEY', PublicKey),
            PublicKey \= ''
        ->  true
        ;   getenv('UNSANDBOX_API_KEY', PublicKey),
            PublicKey \= ''
        ->  true
        ;   % Try accounts.csv with UNSANDBOX_ACCOUNT or row 0
            (   getenv('UNSANDBOX_ACCOUNT', IdxStr),
                atom_number(IdxStr, DefaultIdx)
            ->  true
            ;   DefaultIdx = 0
            ),
            (   load_accounts_csv(DefaultIdx, PublicKey, _)
            ->  true
            ;   write(user_error, 'Error: UNSANDBOX_PUBLIC_KEY or UNSANDBOX_API_KEY environment variable not set\n'),
                halt(1)
            )
        )
    ).

get_secret_key(SecretKey) :-
    (   nb_getval(account_index, Idx), Idx >= 0
    ->  (   load_accounts_csv(Idx, _, SecretKey)
        ->  true
        ;   SecretKey = ''
        )
    ;   (   getenv('UNSANDBOX_SECRET_KEY', SecretKey),
            SecretKey \= ''
        ->  true
        ;   getenv('UNSANDBOX_API_KEY', SecretKey),
            SecretKey \= ''
        ->  true
        ;   % Try accounts.csv with UNSANDBOX_ACCOUNT or row 0
            (   getenv('UNSANDBOX_ACCOUNT', IdxStr),
                atom_number(IdxStr, DefaultIdx)
            ->  true
            ;   DefaultIdx = 0
            ),
            (   load_accounts_csv(DefaultIdx, _, SecretKey)
            ->  true
            ;   SecretKey = ''
            )
        )
    ).

% Get API key (legacy compatibility)
get_api_key(ApiKey) :-
    get_public_key(ApiKey).

% API base URL: UNSANDBOX_API_URL (default https://api.unsandbox.com), no trailing slash.
% main/1 exports it as UN_API, which every request command references.
api_base(Base) :-
    (   getenv('UNSANDBOX_API_URL', Url), Url \== ''
    ->  true
    ;   Url = 'https://api.unsandbox.com'
    ),
    strip_trailing_slashes(Url, Base).

strip_trailing_slashes(Url, Base) :-
    (   atom_concat(Prefix, '/', Url)
    ->  strip_trailing_slashes(Prefix, Base)
    ;   Base = Url
    ).

% accounts.csv the credentials came from ('' for flags / env vars).
% Mirrors get_public_key/get_secret_key and load_accounts_csv tiers.
credentials_csv_path(Path) :-
    (   nb_getval(account_index, Idx), Idx >= 0
    ->  accounts_csv_file(Path)
    ;   getenv('UNSANDBOX_PUBLIC_KEY', PK), PK \= ''
    ->  Path = ''
    ;   getenv('UNSANDBOX_API_KEY', K), K \= ''
    ->  Path = ''
    ;   accounts_csv_file(Path)
    ).

accounts_csv_file(Path) :-
    getenv('HOME', Home),
    atomic_list_concat([Home, '/.unsandbox/accounts.csv'], Path1),
    (   exists_file(Path1)
    ->  Path = Path1
    ;   exists_file('accounts.csv')
    ->  Path = 'accounts.csv'
    ;   Path = ''
    ).

% un key --rotate [--otp CODE]: rotate this key's secret (see CLI_SPEC.md).
% Credentials reach the static bash flow through the environment, never argv.
rotate_key(Args) :-
    get_public_key(PublicKey),
    get_secret_key(SecretKey),
    (   append(_, ['--otp', Otp|_], Args) -> true ; Otp = '' ),
    credentials_csv_path(CsvPath),
    setenv('UN_PK', PublicKey),
    setenv('UN_SK', SecretKey),
    setenv('UN_CSV', CsvPath),
    setenv('UN_OTP', Otp),
    rotate_script(Script),
    process_create(path(bash), ['-c', Script],
                   [stdin(std), stdout(std), stderr(std), process(Pid)]),
    process_wait(Pid, Status),
    (   Status = exit(Code) -> halt(Code) ; halt(1) ).

% bash prelude defining hmac KEY MSG -> hex HMAC-SHA256 (RFC 2104), built from
% plain SHA-256 over stdin with bash builtins: our secret never reaches an argv
% (openssl -hmac would put it there) and never touches a file.
hmac_prelude(Prelude) :-
    atomic_list_concat([
    'if command -v sha256sum >/dev/null 2>&1; then S=sha256sum; else S="shasum -a 256"; fi',
    'hmac() { local k=$1 i c ip= op= ih= x= kh= kb=()',
    '  if ((${#k}>64)); then kh=$(printf %s "$k" | $S | cut -c1-64); for ((i=0;i<32;i++)); do kb[i]=$((16#${kh:2*i:2})); done',
    '  else for ((i=0;i<${#k};i++)); do printf -v c %d "\'${k:i:1}"; kb[i]=$c; done; fi',
    '  for ((i=0;i<64;i++)); do c=${kb[i]:-0}',
    '    printf -v ip \'%s\\\\x%02x\' "$ip" $((c^54)); printf -v op \'%s\\\\x%02x\' "$op" $((c^92)); done',
    '  ih=$( { printf "$ip"; printf %s "$2"; } | $S | cut -c1-64)',
    '  for ((i=0;i<64;i+=2)); do x="$x\\\\x${ih:i:2}"; done',
    '  { printf "$op"; printf "$x"; } | $S | cut -c1-64; }'
    ], '\n', Prelude).

% Run a request command under bash with our hmac helper. Our secret reaches
% it only through the UN_SK environment variable, never a command line.
signed_shell(SecretKey, Cmd) :-
    signed_shell(SecretKey, Cmd, 0).

signed_shell(SecretKey, Cmd, Status) :-
    setenv('UN_SK', SecretKey),
    hmac_prelude(Prelude),
    atomic_list_concat([Prelude, Cmd], '\n', Script),
    process_create(path(bash), ['-c', Script],
                   [stdin(std), stdout(std), stderr(std), process(Pid)]),
    process_wait(Pid, exit(Status)).

rotate_script(Script) :-
    hmac_prelude(Prelude),
    atomic_list_concat([
    'A=${UNSANDBOX_API_URL:-https://api.unsandbox.com}; while [ "${A%/}" != "$A" ]; do A=${A%/}; done',
    'P=/keys/self/rotate-secret; D="$HOME/.unsandbox"',
    'CF="$D/rotate-$(printf %s "$UN_PK" | $S | cut -c1-16).challenge"',
    'RB=$(mktemp) || exit 1; trap \'rm -f "$RB"\' EXIT',
    'post() { local ts sig; ts=$(date +%s); sig=$(hmac "$UN_SK" "$ts:POST:$P:{}")',
    '  { printf \'url = "%s"\\nrequest = "POST"\\n\' "$A$P"',
    '    printf \'header = "Content-Type: application/json"\\nheader = "Authorization: Bearer %s"\\n\' "$UN_PK"',
    '    printf \'header = "X-Timestamp: %s"\\nheader = "X-Signature: %s"\\n\' "$ts" "$sig"',
    '    if [ -n "$1" ]; then printf \'header = "X-Sudo-OTP: %s"\\nheader = "X-Sudo-Challenge: %s"\\n\' "$1" "$2"; fi',
    '    printf \'data = "{}"\\n\'; } | curl -s -K - -o "$RB" -w \'%{http_code}\'; }',
    'if [ -n "${UN_OTP:-}" ]; then',
    '  CH=$(cat "$CF" 2>/dev/null) || { echo "Error: no rotation in progress; run: un key --rotate" >&2; exit 2; }',
    '  CODE=$(post "$UN_OTP" "$CH"); rm -f "$CF"',
    'else',
    '  CODE=$(post)',
    '  if [ "$CODE" = 428 ]; then CH=$(jq -r \'.challenge_id // empty\' "$RB")',
    '    if [ -t 0 ]; then echo "One-time code emailed to the key owner." >&2; printf \'Code: \' >&2; read -r OTP',
    '      [ -n "$OTP" ] || { echo "Error: cancelled" >&2; exit 1; }; CODE=$(post "$OTP" "$CH")',
    '    else (umask 077; mkdir -p "$D" && printf %s "$CH" > "$CF") && chmod 600 "$CF" || exit 1',
    '      echo "One-time code emailed to the key owner. Finish with: un key --rotate --otp CODE"; exit 0; fi; fi',
    'fi',
    'if [ "$CODE" != 200 ]; then',
    '  if [ "$CODE" = 403 ] && [ "$(jq -r \'.error // empty\' "$RB" 2>/dev/null)" = email_required ]; then',
    '    echo "Error: this key has no email; add one or use POST /keys/recover" >&2; exit 3; fi',
    '  [ "$CODE" = 401 ] && { echo "Error: authentication failed" >&2; exit 3; }',
    '  M=$(jq -r \'.message // .error // empty\' "$RB" 2>/dev/null); echo "Error: ${M:-HTTP $CODE}" >&2; exit 4; fi',
    'NS=$(jq -r \'.secret // empty\' "$RB"); [ -n "$NS" ] || { echo "Error: API returned no secret" >&2; exit 4; }',
    'if [ -n "${UN_CSV:-}" ] && [ -f "$UN_CSV" ] && T=$(mktemp "$(dirname "$UN_CSV")/.accounts.csv.XXXXXX"); then',
    '  chmod 600 "$T"; NS="$NS" awk -F, -v OFS=, -v pk="$UN_PK" \'BEGIN{d=0} { t=$1; gsub(/^[ \\t]+|[ \\t\\r]+$/,"",t)',
    '    if (!d && $0 !~ /^[ \\t]*(#|$)/ && NF>=2 && t==pk) { $2=ENVIRON["NS"]; d=1 } print } END{exit d?0:1}\' "$UN_CSV" > "$T" \\',
    '    && mv -f "$T" "$UN_CSV" && { echo "Secret rotated. Updated $UN_CSV (public key $UN_PK)."; exit 0; }; rm -f "$T"; fi',
    'echo "New secret (shown once, store it now): $NS"',
    'echo "Warning: UNSANDBOX_SECRET_KEY / -k still hold the old, now invalid, secret."'
    ], '\n', Body),
    atomic_list_concat([Prelude, Body], '\n', Script).

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
        'BODY=$(jq -Rs \'{language: "~w", code: .}\' < "~w"); TIMESTAMP=$(date +%s); MESSAGE="$TIMESTAMP:POST:/execute:$BODY"; SIGNATURE=$(hmac "$UN_SK" "$MESSAGE"); curl -s -X POST $UN_API/execute -H "Content-Type: application/json" -H "Authorization: Bearer ~w" -H "X-Timestamp: $TIMESTAMP" -H "X-Signature: $SIGNATURE" -d "$BODY" -o /tmp/unsandbox_resp.json; RESP=$(cat /tmp/unsandbox_resp.json); if echo "$RESP" | grep -qi "timestamp" && echo "$RESP" | grep -Eqi "(401|expired|invalid)"; then echo -e "\\x1b[31mError: Request timestamp expired (must be within 5 minutes of server time)\\x1b[0m" >&2; echo -e "\\x1b[33mYour computer\'s clock may have drifted.\\x1b[0m" >&2; echo "Check your system time and sync with NTP if needed:" >&2; echo "  Linux:   sudo ntpdate -s time.nist.gov" >&2; echo "  macOS:   sudo sntp -sS time.apple.com" >&2; echo "  Windows: w32tm /resync\\x1b[0m" >&2; rm -f /tmp/unsandbox_resp.json; exit 1; fi; jq -r ".stdout // empty" /tmp/unsandbox_resp.json | sed "s/^/\\x1b[34m/" | sed "s/$/\\x1b[0m/"; jq -r ".stderr // empty" /tmp/unsandbox_resp.json | sed "s/^/\\x1b[31m/" | sed "s/$/\\x1b[0m/" >&2; rm -f /tmp/unsandbox_resp.json',
        [Language, Filename, PublicKey]),
    signed_shell(SecretKey, Cmd, 0).

% Session list
session_list :-
    get_public_key(PublicKey),
    get_secret_key(SecretKey),
    format(atom(Cmd),
        'TIMESTAMP=$(date +%s); MESSAGE="$TIMESTAMP:GET:/sessions:"; SIGNATURE=$(hmac "$UN_SK" "$MESSAGE"); RESP=$(curl -s -X GET $UN_API/sessions -H "Authorization: Bearer ~w" -H "X-Timestamp: $TIMESTAMP" -H "X-Signature: $SIGNATURE"); if echo "$RESP" | grep -qi "timestamp" && echo "$RESP" | grep -Eqi "(401|expired|invalid)"; then echo -e "\\x1b[31mError: Request timestamp expired (must be within 5 minutes of server time)\\x1b[0m" >&2; echo -e "\\x1b[33mYour computer\'s clock may have drifted.\\x1b[0m" >&2; echo "Check your system time and sync with NTP if needed:" >&2; echo "  Linux:   sudo ntpdate -s time.nist.gov" >&2; echo "  macOS:   sudo sntp -sS time.apple.com" >&2; echo "  Windows: w32tm /resync\\x1b[0m" >&2; exit 1; fi; echo "$RESP" | jq -r \'.sessions[] | "\\(.id) \\(.shell) \\(.status) \\(.created_at)"\' 2>/dev/null || echo "No active sessions"',
        [PublicKey]),
    signed_shell(SecretKey, Cmd, 0).

% Session kill
session_kill(SessionId) :-
    get_public_key(PublicKey),
    get_secret_key(SecretKey),
    format(atom(Cmd),
        'TIMESTAMP=$(date +%s); MESSAGE="$TIMESTAMP:DELETE:/sessions/~w:"; SIGNATURE=$(hmac "$UN_SK" "$MESSAGE"); curl -s -X DELETE $UN_API/sessions/~w -H "Authorization: Bearer ~w" -H "X-Timestamp: $TIMESTAMP" -H "X-Signature: $SIGNATURE" >/dev/null && echo -e "\\x1b[32mSession terminated: ~w\\x1b[0m"',
        [SessionId, SessionId, PublicKey, SessionId]),
    signed_shell(SecretKey, Cmd, 0).

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
        'echo -e "\\x1b[33mCreating session...\\x1b[0m"; SHELL_VAL="~w"; INPUT_FILES=""; ~w if [ -n "$INPUT_FILES" ]; then BODY="{\\\"shell\\\":\\\"$SHELL_VAL\\\",\\\"input_files\\\":[$INPUT_FILES]}"; else BODY="{\\\"shell\\\":\\\"$SHELL_VAL\\\"}"; fi; TIMESTAMP=$(date +%s); MESSAGE="$TIMESTAMP:POST:/sessions:$BODY"; SIGNATURE=$(hmac "$UN_SK" "$MESSAGE"); curl -s -X POST $UN_API/sessions -H "Content-Type: application/json" -H "Authorization: Bearer ~w" -H "X-Timestamp: $TIMESTAMP" -H "X-Signature: $SIGNATURE" -d "$BODY" | jq .',
        [ShellVal, FileArgs, PublicKey]),
    signed_shell(SecretKey, Cmd, 0).

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
        'TIMESTAMP=$(date +%s); MESSAGE="$TIMESTAMP:GET:/services:"; SIGNATURE=$(hmac "$UN_SK" "$MESSAGE"); RESP=$(curl -s -X GET $UN_API/services -H "Authorization: Bearer ~w" -H "X-Timestamp: $TIMESTAMP" -H "X-Signature: $SIGNATURE"); if echo "$RESP" | grep -qi "timestamp" && echo "$RESP" | grep -Eqi "(401|expired|invalid)"; then echo -e "\\x1b[31mError: Request timestamp expired (must be within 5 minutes of server time)\\x1b[0m" >&2; echo -e "\\x1b[33mYour computer\'s clock may have drifted.\\x1b[0m" >&2; echo "Check your system time and sync with NTP if needed:" >&2; echo "  Linux:   sudo ntpdate -s time.nist.gov" >&2; echo "  macOS:   sudo sntp -sS time.apple.com" >&2; echo "  Windows: w32tm /resync\\x1b[0m" >&2; exit 1; fi; echo "$RESP" | jq -r \'.services[] | "\\(.id) \\(.name) \\(.status)"\' 2>/dev/null || echo "No services"',
        [PublicKey]),
    signed_shell(SecretKey, Cmd, 0).

% Service info
service_info(ServiceId) :-
    get_public_key(PublicKey),
    get_secret_key(SecretKey),
    format(atom(Cmd),
        'TIMESTAMP=$(date +%s); MESSAGE="$TIMESTAMP:GET:/services/~w:"; SIGNATURE=$(hmac "$UN_SK" "$MESSAGE"); curl -s -X GET $UN_API/services/~w -H "Authorization: Bearer ~w" -H "X-Timestamp: $TIMESTAMP" -H "X-Signature: $SIGNATURE" | jq .',
        [ServiceId, ServiceId, PublicKey]),
    signed_shell(SecretKey, Cmd, 0).

% Service logs
service_logs(ServiceId) :-
    get_public_key(PublicKey),
    get_secret_key(SecretKey),
    format(atom(Cmd),
        'TIMESTAMP=$(date +%s); MESSAGE="$TIMESTAMP:GET:/services/~w/logs:"; SIGNATURE=$(hmac "$UN_SK" "$MESSAGE"); curl -s -X GET $UN_API/services/~w/logs -H "Authorization: Bearer ~w" -H "X-Timestamp: $TIMESTAMP" -H "X-Signature: $SIGNATURE" | jq -r ".logs"',
        [ServiceId, ServiceId, PublicKey]),
    signed_shell(SecretKey, Cmd, 0).

% Service sleep
service_sleep(ServiceId) :-
    get_public_key(PublicKey),
    get_secret_key(SecretKey),
    format(atom(Cmd),
        'TIMESTAMP=$(date +%s); MESSAGE="$TIMESTAMP:POST:/services/~w/freeze:"; SIGNATURE=$(hmac "$UN_SK" "$MESSAGE"); curl -s -X POST $UN_API/services/~w/freeze -H "Authorization: Bearer ~w" -H "X-Timestamp: $TIMESTAMP" -H "X-Signature: $SIGNATURE" >/dev/null && echo -e "\\x1b[32mService frozen: ~w\\x1b[0m"',
        [ServiceId, ServiceId, PublicKey, ServiceId]),
    signed_shell(SecretKey, Cmd, 0).

% Service wake
service_wake(ServiceId) :-
    get_public_key(PublicKey),
    get_secret_key(SecretKey),
    format(atom(Cmd),
        'TIMESTAMP=$(date +%s); MESSAGE="$TIMESTAMP:POST:/services/~w/unfreeze:"; SIGNATURE=$(hmac "$UN_SK" "$MESSAGE"); curl -s -X POST $UN_API/services/~w/unfreeze -H "Authorization: Bearer ~w" -H "X-Timestamp: $TIMESTAMP" -H "X-Signature: $SIGNATURE" >/dev/null && echo -e "\\x1b[32mService unfreezing: ~w\\x1b[0m"',
        [ServiceId, ServiceId, PublicKey, ServiceId]),
    signed_shell(SecretKey, Cmd, 0).

% Service destroy
service_destroy(ServiceId) :-
    get_public_key(PublicKey),
    get_secret_key(SecretKey),
    format(atom(Cmd),
        'TIMESTAMP=$(date +%s); MESSAGE="$TIMESTAMP:DELETE:/services/~w:"; SIGNATURE=$(hmac "$UN_SK" "$MESSAGE"); RESP=$(curl -s -w \'\\n%{http_code}\' -X DELETE $UN_API/services/~w -H "Authorization: Bearer ~w" -H "X-Timestamp: $TIMESTAMP" -H "X-Signature: $SIGNATURE"); HTTP_CODE=$(echo "$RESP" | tail -n1); BODY=$(echo "$RESP" | sed \'$d\'); if [ "$HTTP_CODE" = "428" ]; then CHALLENGE_ID=$(echo "$BODY" | jq -r \'.challenge_id // empty\'); echo -e "\\x1b[33mConfirmation required. Check your email for a one-time code.\\x1b[0m" >&2; echo -n "Enter OTP: " >&2; read OTP; if [ -z "$OTP" ]; then echo -e "\\x1b[31mError: Operation cancelled\\x1b[0m" >&2; exit 1; fi; TS2=$(date +%s); MSG2="$TS2:DELETE:/services/~w:"; SIG2=$(hmac "$UN_SK" "$MSG2"); RESP2=$(curl -s -w \'\\n%{http_code}\' -X DELETE $UN_API/services/~w -H "Authorization: Bearer ~w" -H "X-Timestamp: $TS2" -H "X-Signature: $SIG2" -H "X-Sudo-OTP: $OTP" -H "X-Sudo-Challenge: $CHALLENGE_ID"); HTTP2=$(echo "$RESP2" | tail -n1); if [ "$HTTP2" = "200" ] || [ "$HTTP2" = "204" ]; then echo -e "\\x1b[32mService destroyed: ~w\\x1b[0m"; else echo "$RESP2" | sed \'$d\' | jq . 2>/dev/null || echo "$RESP2" | sed \'$d\'; exit 1; fi; elif [ "$HTTP_CODE" = "200" ] || [ "$HTTP_CODE" = "204" ]; then echo -e "\\x1b[32mService destroyed: ~w\\x1b[0m"; else echo "$BODY" | jq . 2>/dev/null || echo "$BODY"; exit 1; fi',
        [ServiceId, ServiceId, PublicKey, ServiceId, ServiceId, PublicKey, ServiceId, ServiceId]),
    signed_shell(SecretKey, Cmd, 0).

% Service resize
service_resize(ServiceId, Vcpu) :-
    get_public_key(PublicKey),
    get_secret_key(SecretKey),
    Ram is Vcpu * 2,
    format(atom(Cmd),
        'BODY=\'{\"vcpu\":~w}\'; TIMESTAMP=$(date +%s); MESSAGE="$TIMESTAMP:PATCH:/services/~w:$BODY"; SIGNATURE=$(hmac "$UN_SK" "$MESSAGE"); curl -s -X PATCH $UN_API/services/~w -H "Content-Type: application/json" -H "Authorization: Bearer ~w" -H "X-Timestamp: $TIMESTAMP" -H "X-Signature: $SIGNATURE" -d "$BODY" >/dev/null && echo -e "\\x1b[32mService resized to ~w vCPU, ~w GB RAM\\x1b[0m"',
        [Vcpu, ServiceId, ServiceId, PublicKey, Vcpu, Ram]),
    signed_shell(SecretKey, Cmd, 0).

% Service set unfreeze on demand
service_set_unfreeze_on_demand(ServiceId, Enabled) :-
    get_public_key(PublicKey),
    get_secret_key(SecretKey),
    (   Enabled = true
    ->  EnabledStr = 'true', Msg = 'enabled'
    ;   EnabledStr = 'false', Msg = 'disabled'
    ),
    format(atom(Cmd),
        'BODY=\'{\"unfreeze_on_demand\":~w}\'; TIMESTAMP=$(date +%s); MESSAGE="$TIMESTAMP:PATCH:/services/~w:$BODY"; SIGNATURE=$(hmac "$UN_SK" "$MESSAGE"); curl -s -X PATCH $UN_API/services/~w -H "Content-Type: application/json" -H "Authorization: Bearer ~w" -H "X-Timestamp: $TIMESTAMP" -H "X-Signature: $SIGNATURE" -d "$BODY" >/dev/null && echo -e "\\x1b[32mUnfreeze-on-demand ~w for service: ~w\\x1b[0m"',
        [EnabledStr, ServiceId, ServiceId, PublicKey, Msg, ServiceId]),
    signed_shell(SecretKey, Cmd, 0).

% Service env status
service_env_status(ServiceId) :-
    get_public_key(PublicKey),
    get_secret_key(SecretKey),
    format(atom(Cmd),
        'TIMESTAMP=$(date +%s); MESSAGE="$TIMESTAMP:GET:/services/~w/env:"; SIGNATURE=$(hmac "$UN_SK" "$MESSAGE"); curl -s -X GET "$UN_API/services/~w/env" -H "Authorization: Bearer ~w" -H "X-Timestamp: $TIMESTAMP" -H "X-Signature: $SIGNATURE" | jq .',
        [ServiceId, ServiceId, PublicKey]),
    signed_shell(SecretKey, Cmd, 0).

% Service env set
service_env_set(ServiceId, Envs, EnvFile) :-
    get_public_key(PublicKey),
    get_secret_key(SecretKey),
    format(atom(Cmd),
        'ENV_CONTENT=""; ENV_LINES="~w"; if [ -n "$ENV_LINES" ]; then ENV_CONTENT="$ENV_LINES"; fi; ENV_FILE="~w"; if [ -n "$ENV_FILE" ] && [ -f "$ENV_FILE" ]; then while IFS= read -r line || [ -n "$line" ]; do case "$line" in "#"*|"") continue ;; esac; if [ -n "$ENV_CONTENT" ]; then ENV_CONTENT="$ENV_CONTENT\\n"; fi; ENV_CONTENT="$ENV_CONTENT$line"; done < "$ENV_FILE"; fi; if [ -z "$ENV_CONTENT" ]; then echo -e "\\x1b[31mError: No environment variables to set\\x1b[0m" >&2; exit 1; fi; TIMESTAMP=$(date +%s); MESSAGE="$TIMESTAMP:PUT:/services/~w/env:$ENV_CONTENT"; SIGNATURE=$(hmac "$UN_SK" "$MESSAGE"); curl -s -X PUT "$UN_API/services/~w/env" -H "Authorization: Bearer ~w" -H "X-Timestamp: $TIMESTAMP" -H "X-Signature: $SIGNATURE" -H "Content-Type: text/plain" --data-binary "$ENV_CONTENT" | jq .',
        [Envs, EnvFile, ServiceId, ServiceId, PublicKey]),
    signed_shell(SecretKey, Cmd, 0).

% Service env export
service_env_export(ServiceId) :-
    get_public_key(PublicKey),
    get_secret_key(SecretKey),
    format(atom(Cmd),
        'TIMESTAMP=$(date +%s); MESSAGE="$TIMESTAMP:POST:/services/~w/env/export:"; SIGNATURE=$(hmac "$UN_SK" "$MESSAGE"); curl -s -X POST "$UN_API/services/~w/env/export" -H "Authorization: Bearer ~w" -H "X-Timestamp: $TIMESTAMP" -H "X-Signature: $SIGNATURE" | jq -r ".content // empty"',
        [ServiceId, ServiceId, PublicKey]),
    signed_shell(SecretKey, Cmd, 0).

% Service env delete
service_env_delete(ServiceId) :-
    get_public_key(PublicKey),
    get_secret_key(SecretKey),
    format(atom(Cmd),
        'TIMESTAMP=$(date +%s); MESSAGE="$TIMESTAMP:DELETE:/services/~w/env:"; SIGNATURE=$(hmac "$UN_SK" "$MESSAGE"); curl -s -X DELETE "$UN_API/services/~w/env" -H "Authorization: Bearer ~w" -H "X-Timestamp: $TIMESTAMP" -H "X-Signature: $SIGNATURE" >/dev/null && echo -e "\\x1b[32mVault deleted for: ~w\\x1b[0m"',
        [ServiceId, ServiceId, PublicKey, ServiceId]),
    signed_shell(SecretKey, Cmd, 0).

% Service dump bootstrap
service_dump_bootstrap(ServiceId, DumpFile) :-
    get_public_key(PublicKey),
    get_secret_key(SecretKey),
    (   DumpFile = ''
    ->  % No file specified, print to stdout
        format(atom(Cmd),
            'echo "Fetching bootstrap script from ~w..." >&2; BODY=\'{"command":"cat /tmp/bootstrap.sh"}\'; TIMESTAMP=$(date +%s); MESSAGE="$TIMESTAMP:POST:/services/~w/execute:$BODY"; SIGNATURE=$(hmac "$UN_SK" "$MESSAGE"); RESP=$(curl -s -X POST $UN_API/services/~w/execute -H "Content-Type: application/json" -H "Authorization: Bearer ~w" -H "X-Timestamp: $TIMESTAMP" -H "X-Signature: $SIGNATURE" -d "$BODY"); STDOUT=$(echo "$RESP" | jq -r ".stdout // empty"); if [ -n "$STDOUT" ]; then echo "$STDOUT"; else echo -e "\\x1b[31mError: Failed to fetch bootstrap\\x1b[0m" >&2; exit 1; fi',
            [ServiceId, ServiceId, ServiceId, PublicKey])
    ;   % File specified, save to file
        format(atom(Cmd),
            'echo "Fetching bootstrap script from ~w..." >&2; BODY=\'{"command":"cat /tmp/bootstrap.sh"}\'; TIMESTAMP=$(date +%s); MESSAGE="$TIMESTAMP:POST:/services/~w/execute:$BODY"; SIGNATURE=$(hmac "$UN_SK" "$MESSAGE"); RESP=$(curl -s -X POST $UN_API/services/~w/execute -H "Content-Type: application/json" -H "Authorization: Bearer ~w" -H "X-Timestamp: $TIMESTAMP" -H "X-Signature: $SIGNATURE" -d "$BODY"); STDOUT=$(echo "$RESP" | jq -r ".stdout // empty"); if [ -n "$STDOUT" ]; then echo "$STDOUT" > "~w" && chmod 755 "~w" && echo "Bootstrap saved to ~w"; else echo -e "\\x1b[31mError: Failed to fetch bootstrap\\x1b[0m" >&2; exit 1; fi',
            [ServiceId, ServiceId, ServiceId, PublicKey, DumpFile, DumpFile, DumpFile])
    ),
    signed_shell(SecretKey, Cmd, 0).

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
        'echo -e "\\x1b[33mCreating service...\\x1b[0m"; INPUT_FILES=""; ~w if [ -n "$INPUT_FILES" ]; then INPUT_FILES_JSON=",\\\"input_files\\\":[$INPUT_FILES]"; else INPUT_FILES_JSON=""; fi; BODY="{\\\"name\\\":\\\"~w\\\"~w~w~w~w$INPUT_FILES_JSON}"; TIMESTAMP=$(date +%s); MESSAGE="$TIMESTAMP:POST:/services:$BODY"; SIGNATURE=$(hmac "$UN_SK" "$MESSAGE"); curl -s -X POST $UN_API/services -H "Content-Type: application/json" -H "Authorization: Bearer ~w" -H "X-Timestamp: $TIMESTAMP" -H "X-Signature: $SIGNATURE" -d "$BODY" | jq . && echo -e "\\x1b[32mService created\\x1b[0m"',
        [FileArgs, Name, PortsJson, BootstrapJson, BootstrapContentJson, ServiceTypeJson, PublicKey]),
    signed_shell(SecretKey, Cmd, 0).

% Key validate
validate_key(Extend) :-
    get_public_key(PublicKey),
    get_secret_key(SecretKey),
    portal_base(PortalBase),
    (   Extend = true
    ->  % Build command for --extend mode
        format(atom(Cmd),
            'BODY=\'{}\'; TIMESTAMP=$(date +%s); MESSAGE="$TIMESTAMP:POST:/keys/validate:$BODY"; SIGNATURE=$(hmac "$UN_SK" "$MESSAGE"); RESP=$(curl -s -X POST ~w/keys/validate -H "Content-Type: application/json" -H "Authorization: Bearer ~w" -H "X-Timestamp: $TIMESTAMP" -H "X-Signature: $SIGNATURE" -d "$BODY"); if echo "$RESP" | grep -qi "timestamp" && echo "$RESP" | grep -Eqi "(401|expired|invalid)"; then echo -e "\\x1b[31mError: Request timestamp expired (must be within 5 minutes of server time)\\x1b[0m" >&2; echo -e "\\x1b[33mYour computer\'s clock may have drifted.\\x1b[0m" >&2; echo "Check your system time and sync with NTP if needed:" >&2; echo "  Linux:   sudo ntpdate -s time.nist.gov" >&2; echo "  macOS:   sudo sntp -sS time.apple.com" >&2; echo "  Windows: w32tm /resync\\x1b[0m" >&2; exit 1; fi; PUBLIC_KEY=$(echo "$RESP" | jq -r ".public_key // \\"N/A\\""); xdg-open "~w/keys/extend?pk=$PUBLIC_KEY" 2>/dev/null',
            [PortalBase, PublicKey, PortalBase])
    ;   % Build command for normal validation
        format(atom(Cmd),
            'BODY=\'{}\'; TIMESTAMP=$(date +%s); MESSAGE="$TIMESTAMP:POST:/keys/validate:$BODY"; SIGNATURE=$(hmac "$UN_SK" "$MESSAGE"); curl -s -X POST ~w/keys/validate -H "Content-Type: application/json" -H "Authorization: Bearer ~w" -H "X-Timestamp: $TIMESTAMP" -H "X-Signature: $SIGNATURE" -d "$BODY" -o /tmp/unsandbox_key_resp.json; STATUS=$?; if [ $STATUS -ne 0 ]; then echo -e "\\x1b[31mInvalid\\x1b[0m"; exit 1; fi; RESP=$(cat /tmp/unsandbox_key_resp.json); if echo "$RESP" | grep -qi "timestamp" && echo "$RESP" | grep -Eqi "(401|expired|invalid)"; then echo -e "\\x1b[31mError: Request timestamp expired (must be within 5 minutes of server time)\\x1b[0m" >&2; echo -e "\\x1b[33mYour computer\'s clock may have drifted.\\x1b[0m" >&2; echo "Check your system time and sync with NTP if needed:" >&2; echo "  Linux:   sudo ntpdate -s time.nist.gov" >&2; echo "  macOS:   sudo sntp -sS time.apple.com" >&2; echo "  Windows: w32tm /resync\\x1b[0m" >&2; rm -f /tmp/unsandbox_key_resp.json; exit 1; fi; EXPIRED=$(jq -r ".expired // false" /tmp/unsandbox_key_resp.json); if [ "$EXPIRED" = "true" ]; then echo -e "\\x1b[31mExpired\\x1b[0m"; echo "Public Key: $(jq -r ".public_key // \\"N/A\\"" /tmp/unsandbox_key_resp.json)"; echo "Tier: $(jq -r ".tier // \\"N/A\\"" /tmp/unsandbox_key_resp.json)"; echo "Expired: $(jq -r ".expires_at // \\"N/A\\"" /tmp/unsandbox_key_resp.json)"; echo -e "\\x1b[33mTo renew: Visit https://unsandbox.com/keys/extend\\x1b[0m"; rm -f /tmp/unsandbox_key_resp.json; exit 1; else echo -e "\\x1b[32mValid\\x1b[0m"; echo "Public Key: $(jq -r ".public_key // \\"N/A\\"" /tmp/unsandbox_key_resp.json)"; echo "Tier: $(jq -r ".tier // \\"N/A\\"" /tmp/unsandbox_key_resp.json)"; echo "Status: $(jq -r ".status // \\"N/A\\"" /tmp/unsandbox_key_resp.json)"; echo "Expires: $(jq -r ".expires_at // \\"N/A\\"" /tmp/unsandbox_key_resp.json)"; echo "Time Remaining: $(jq -r ".time_remaining // \\"N/A\\"" /tmp/unsandbox_key_resp.json)"; echo "Rate Limit: $(jq -r ".rate_limit // \\"N/A\\"" /tmp/unsandbox_key_resp.json)"; echo "Burst: $(jq -r ".burst // \\"N/A\\"" /tmp/unsandbox_key_resp.json)"; echo "Concurrency: $(jq -r ".concurrency // \\"N/A\\"" /tmp/unsandbox_key_resp.json)"; fi; rm -f /tmp/unsandbox_key_resp.json',
            [PortalBase, PublicKey])
    ),
    signed_shell(SecretKey, Cmd, 0).

% Read languages cache
read_languages_cache(Languages) :-
    languages_cache_file(CacheFile),
    exists_file(CacheFile),
    languages_cache_ttl(TTL),
    % Read cache file and check timestamp
    format(atom(Cmd),
        'CACHE=$(cat "~w" 2>/dev/null); if [ -n "$CACHE" ]; then TIMESTAMP=$(echo "$CACHE" | jq -r ".timestamp // 0"); CURRENT=$(date +%s); AGE=$((CURRENT - TIMESTAMP)); if [ $AGE -lt ~w ]; then echo "$CACHE" | jq -c ".languages // []"; fi; fi',
        [CacheFile, TTL]),
    setup_call_cleanup(
        process_create(path(sh), ['-c', Cmd], [stdout(pipe(Out))]),
        read_string(Out, _, Output),
        close(Out)
    ),
    Output \= '',
    Output \= '\n',
    Languages = Output.

% Write languages cache
write_languages_cache(LanguagesJson) :-
    languages_cache_file(CacheFile),
    getenv('HOME', Home),
    atomic_list_concat([Home, '/.unsandbox'], CacheDir),
    format(atom(MkdirCmd), 'mkdir -p "~w"', [CacheDir]),
    shell(MkdirCmd, 0),
    format(atom(Cmd),
        'TIMESTAMP=$(date +%s); echo \'{"languages":~w,"timestamp":\'$TIMESTAMP\'}\'> "~w"',
        [LanguagesJson, CacheFile]),
    shell(Cmd, 0).

% Languages command with caching
languages_command(JsonOutput) :-
    % Try to read from cache first
    (   read_languages_cache(CachedLangs),
        CachedLangs \= ''
    ->  % Use cached data
        (   JsonOutput = true
        ->  format('~w~n', [CachedLangs])
        ;   % Parse and print one per line
            format(atom(Cmd), 'echo \'~w\' | jq -r ".[]"', [CachedLangs]),
            shell(Cmd, 0)
        )
    ;   % No valid cache, fetch from API
        get_public_key(PublicKey),
        get_secret_key(SecretKey),
        (   JsonOutput = true
        ->  % Output as JSON array and cache
            format(atom(Cmd),
                'TIMESTAMP=$(date +%s); MESSAGE="$TIMESTAMP:GET:/languages:"; SIGNATURE=$(hmac "$UN_SK" "$MESSAGE"); RESP=$(curl -s -X GET $UN_API/languages -H "Authorization: Bearer ~w" -H "X-Timestamp: $TIMESTAMP" -H "X-Signature: $SIGNATURE"); LANGS=$(echo "$RESP" | jq -c ".languages // []"); echo "$LANGS"; CACHE_DIR="$HOME/.unsandbox"; mkdir -p "$CACHE_DIR"; echo "{\\\"languages\\\":$LANGS,\\\"timestamp\\\":$(date +%s)}" > "$CACHE_DIR/languages.json"',
                [PublicKey])
        ;   % Output one language per line and cache
            format(atom(Cmd),
                'TIMESTAMP=$(date +%s); MESSAGE="$TIMESTAMP:GET:/languages:"; SIGNATURE=$(hmac "$UN_SK" "$MESSAGE"); RESP=$(curl -s -X GET $UN_API/languages -H "Authorization: Bearer ~w" -H "X-Timestamp: $TIMESTAMP" -H "X-Signature: $SIGNATURE"); LANGS=$(echo "$RESP" | jq -c ".languages // []"); echo "$RESP" | jq -r ".languages[]"; CACHE_DIR="$HOME/.unsandbox"; mkdir -p "$CACHE_DIR"; echo "{\\\"languages\\\":$LANGS,\\\"timestamp\\\":$(date +%s)}" > "$CACHE_DIR/languages.json"',
                [PublicKey])
        ),
        signed_shell(SecretKey, Cmd, 0)
    ).

% Handle languages subcommand
handle_languages(['--json'|_]) :- languages_command(true).
handle_languages(_) :- languages_command(false).

% Handle key subcommand
handle_key(Args) :- memberchk('--rotate', Args), !, rotate_key(Args).
handle_key(['--extend'|_]) :- validate_key(true).
handle_key(_) :- validate_key(false).

% Image list
image_list :-
    get_public_key(PublicKey),
    get_secret_key(SecretKey),
    format(atom(Cmd),
        'TIMESTAMP=$(date +%s); MESSAGE="$TIMESTAMP:GET:/images:"; SIGNATURE=$(hmac "$UN_SK" "$MESSAGE"); curl -s -X GET $UN_API/images -H "Authorization: Bearer ~w" -H "X-Timestamp: $TIMESTAMP" -H "X-Signature: $SIGNATURE" | jq .',
        [PublicKey]),
    signed_shell(SecretKey, Cmd, 0).

% Image info
image_info(ImageId) :-
    get_public_key(PublicKey),
    get_secret_key(SecretKey),
    format(atom(Cmd),
        'TIMESTAMP=$(date +%s); MESSAGE="$TIMESTAMP:GET:/images/~w:"; SIGNATURE=$(hmac "$UN_SK" "$MESSAGE"); curl -s -X GET $UN_API/images/~w -H "Authorization: Bearer ~w" -H "X-Timestamp: $TIMESTAMP" -H "X-Signature: $SIGNATURE" | jq .',
        [ImageId, ImageId, PublicKey]),
    signed_shell(SecretKey, Cmd, 0).

% Image delete
image_delete(ImageId) :-
    get_public_key(PublicKey),
    get_secret_key(SecretKey),
    format(atom(Cmd),
        'TIMESTAMP=$(date +%s); MESSAGE="$TIMESTAMP:DELETE:/images/~w:"; SIGNATURE=$(hmac "$UN_SK" "$MESSAGE"); RESP=$(curl -s -w \'\\n%{http_code}\' -X DELETE $UN_API/images/~w -H "Authorization: Bearer ~w" -H "X-Timestamp: $TIMESTAMP" -H "X-Signature: $SIGNATURE"); HTTP_CODE=$(echo "$RESP" | tail -n1); BODY=$(echo "$RESP" | sed \'$d\'); if [ "$HTTP_CODE" = "428" ]; then CHALLENGE_ID=$(echo "$BODY" | jq -r \'.challenge_id // empty\'); echo -e "\\x1b[33mConfirmation required. Check your email for a one-time code.\\x1b[0m" >&2; echo -n "Enter OTP: " >&2; read OTP; if [ -z "$OTP" ]; then echo -e "\\x1b[31mError: Operation cancelled\\x1b[0m" >&2; exit 1; fi; TS2=$(date +%s); MSG2="$TS2:DELETE:/images/~w:"; SIG2=$(hmac "$UN_SK" "$MSG2"); RESP2=$(curl -s -w \'\\n%{http_code}\' -X DELETE $UN_API/images/~w -H "Authorization: Bearer ~w" -H "X-Timestamp: $TS2" -H "X-Signature: $SIG2" -H "X-Sudo-OTP: $OTP" -H "X-Sudo-Challenge: $CHALLENGE_ID"); HTTP2=$(echo "$RESP2" | tail -n1); if [ "$HTTP2" = "200" ] || [ "$HTTP2" = "204" ]; then echo -e "\\x1b[32mImage deleted: ~w\\x1b[0m"; else echo "$RESP2" | sed \'$d\' | jq . 2>/dev/null || echo "$RESP2" | sed \'$d\'; exit 1; fi; elif [ "$HTTP_CODE" = "200" ] || [ "$HTTP_CODE" = "204" ]; then echo -e "\\x1b[32mImage deleted: ~w\\x1b[0m"; else echo "$BODY" | jq . 2>/dev/null || echo "$BODY"; exit 1; fi',
        [ImageId, ImageId, PublicKey, ImageId, ImageId, PublicKey, ImageId, ImageId]),
    signed_shell(SecretKey, Cmd, 0).

% Image lock
image_lock(ImageId) :-
    get_public_key(PublicKey),
    get_secret_key(SecretKey),
    format(atom(Cmd),
        'TIMESTAMP=$(date +%s); MESSAGE="$TIMESTAMP:POST:/images/~w/lock:"; SIGNATURE=$(hmac "$UN_SK" "$MESSAGE"); curl -s -X POST $UN_API/images/~w/lock -H "Authorization: Bearer ~w" -H "X-Timestamp: $TIMESTAMP" -H "X-Signature: $SIGNATURE" >/dev/null && echo -e "\\x1b[32mImage locked: ~w\\x1b[0m"',
        [ImageId, ImageId, PublicKey, ImageId]),
    signed_shell(SecretKey, Cmd, 0).

% Image unlock
image_unlock(ImageId) :-
    get_public_key(PublicKey),
    get_secret_key(SecretKey),
    format(atom(Cmd),
        'TIMESTAMP=$(date +%s); MESSAGE="$TIMESTAMP:POST:/images/~w/unlock:"; SIGNATURE=$(hmac "$UN_SK" "$MESSAGE"); RESP=$(curl -s -w \'\\n%{http_code}\' -X POST $UN_API/images/~w/unlock -H "Authorization: Bearer ~w" -H "X-Timestamp: $TIMESTAMP" -H "X-Signature: $SIGNATURE"); HTTP_CODE=$(echo "$RESP" | tail -n1); BODY=$(echo "$RESP" | sed \'$d\'); if [ "$HTTP_CODE" = "428" ]; then CHALLENGE_ID=$(echo "$BODY" | jq -r \'.challenge_id // empty\'); echo -e "\\x1b[33mConfirmation required. Check your email for a one-time code.\\x1b[0m" >&2; echo -n "Enter OTP: " >&2; read OTP; if [ -z "$OTP" ]; then echo -e "\\x1b[31mError: Operation cancelled\\x1b[0m" >&2; exit 1; fi; TS2=$(date +%s); MSG2="$TS2:POST:/images/~w/unlock:"; SIG2=$(hmac "$UN_SK" "$MSG2"); RESP2=$(curl -s -w \'\\n%{http_code}\' -X POST $UN_API/images/~w/unlock -H "Authorization: Bearer ~w" -H "X-Timestamp: $TS2" -H "X-Signature: $SIG2" -H "X-Sudo-OTP: $OTP" -H "X-Sudo-Challenge: $CHALLENGE_ID"); HTTP2=$(echo "$RESP2" | tail -n1); if [ "$HTTP2" = "200" ] || [ "$HTTP2" = "204" ]; then echo -e "\\x1b[32mImage unlocked: ~w\\x1b[0m"; else echo "$RESP2" | sed \'$d\' | jq . 2>/dev/null || echo "$RESP2" | sed \'$d\'; exit 1; fi; elif [ "$HTTP_CODE" = "200" ] || [ "$HTTP_CODE" = "204" ]; then echo -e "\\x1b[32mImage unlocked: ~w\\x1b[0m"; else echo "$BODY" | jq . 2>/dev/null || echo "$BODY"; exit 1; fi',
        [ImageId, ImageId, PublicKey, ImageId, ImageId, PublicKey, ImageId, ImageId]),
    signed_shell(SecretKey, Cmd, 0).

% Image publish
image_publish(SourceId, SourceType, Name) :-
    get_public_key(PublicKey),
    get_secret_key(SecretKey),
    (   Name \= ''
    ->  format(atom(NameJson), ',\\\"name\\\":\\\"~w\\\"', [Name])
    ;   NameJson = ''
    ),
    format(atom(Cmd),
        'BODY="{\\\"source_type\\\":\\\"~w\\\",\\\"source_id\\\":\\\"~w\\\"~w}"; TIMESTAMP=$(date +%s); MESSAGE="$TIMESTAMP:POST:/images/publish:$BODY"; SIGNATURE=$(hmac "$UN_SK" "$MESSAGE"); curl -s -X POST $UN_API/images/publish -H "Content-Type: application/json" -H "Authorization: Bearer ~w" -H "X-Timestamp: $TIMESTAMP" -H "X-Signature: $SIGNATURE" -d "$BODY" | jq . && echo -e "\\x1b[32mImage published\\x1b[0m"',
        [SourceType, SourceId, NameJson, PublicKey]),
    signed_shell(SecretKey, Cmd, 0).

% Image visibility
image_visibility(ImageId, Mode) :-
    get_public_key(PublicKey),
    get_secret_key(SecretKey),
    format(atom(Cmd),
        'BODY="{\\\"visibility\\\":\\\"~w\\\"}"; TIMESTAMP=$(date +%s); MESSAGE="$TIMESTAMP:POST:/images/~w/visibility:$BODY"; SIGNATURE=$(hmac "$UN_SK" "$MESSAGE"); curl -s -X POST $UN_API/images/~w/visibility -H "Content-Type: application/json" -H "Authorization: Bearer ~w" -H "X-Timestamp: $TIMESTAMP" -H "X-Signature: $SIGNATURE" -d "$BODY" >/dev/null && echo -e "\\x1b[32mImage visibility set to ~w: ~w\\x1b[0m"',
        [Mode, ImageId, ImageId, PublicKey, Mode, ImageId]),
    signed_shell(SecretKey, Cmd, 0).

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
        'BODY="{~w~w}"; TIMESTAMP=$(date +%s); MESSAGE="$TIMESTAMP:POST:/images/~w/spawn:$BODY"; SIGNATURE=$(hmac "$UN_SK" "$MESSAGE"); curl -s -X POST $UN_API/images/~w/spawn -H "Content-Type: application/json" -H "Authorization: Bearer ~w" -H "X-Timestamp: $TIMESTAMP" -H "X-Signature: $SIGNATURE" -d "$BODY" | jq . && echo -e "\\x1b[32mService spawned from image\\x1b[0m"',
        [NameJson, PortsJson, ImageId, ImageId, PublicKey]),
    signed_shell(SecretKey, Cmd, 0).

% Image clone
image_clone(ImageId, Name) :-
    get_public_key(PublicKey),
    get_secret_key(SecretKey),
    (   Name \= ''
    ->  format(atom(NameJson), '\\\"name\\\":\\\"~w\\\"', [Name])
    ;   NameJson = ''
    ),
    format(atom(Cmd),
        'BODY="{~w}"; TIMESTAMP=$(date +%s); MESSAGE="$TIMESTAMP:POST:/images/~w/clone:$BODY"; SIGNATURE=$(hmac "$UN_SK" "$MESSAGE"); curl -s -X POST $UN_API/images/~w/clone -H "Content-Type: application/json" -H "Authorization: Bearer ~w" -H "X-Timestamp: $TIMESTAMP" -H "X-Signature: $SIGNATURE" -d "$BODY" | jq . && echo -e "\\x1b[32mImage cloned\\x1b[0m"',
        [NameJson, ImageId, ImageId, PublicKey]),
    signed_shell(SecretKey, Cmd, 0).

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
parse_service_args(['--set-unfreeze-on-demand', ServiceId, EnabledAtom|_], _, _, _, _, _, _, _, _, _, _) :-
    (   (EnabledAtom = 'true' ; EnabledAtom = '1')
    ->  service_set_unfreeze_on_demand(ServiceId, true)
    ;   (EnabledAtom = 'false' ; EnabledAtom = '0')
    ->  service_set_unfreeze_on_demand(ServiceId, false)
    ;   write(user_error, '\x1b[31mError: --set-unfreeze-on-demand requires true/false or 1/0\x1b[0m\n'),
        halt(1)
    ).
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
        'echo -e "\\x1b[33mCreating service...\\x1b[0m"; INPUT_FILES=""; ~w if [ -n "$INPUT_FILES" ]; then INPUT_FILES_JSON=",\\\"input_files\\\":[$INPUT_FILES]"; else INPUT_FILES_JSON=""; fi; BODY="{\\\"name\\\":\\\"~w\\\"~w~w~w~w$INPUT_FILES_JSON}"; TIMESTAMP=$(date +%s); MESSAGE="$TIMESTAMP:POST:/services:$BODY"; SIGNATURE=$(hmac "$UN_SK" "$MESSAGE"); RESP=$(curl -s -X POST $UN_API/services -H "Content-Type: application/json" -H "Authorization: Bearer ~w" -H "X-Timestamp: $TIMESTAMP" -H "X-Signature: $SIGNATURE" -d "$BODY"); SVC_ID=$(echo "$RESP" | jq -r ".id // empty"); if [ -n "$SVC_ID" ]; then echo -e "\\x1b[32m$SVC_ID created\\x1b[0m"; ENV_CONTENT=""; ENV_LINES="~w"; if [ -n "$ENV_LINES" ]; then ENV_CONTENT="$ENV_LINES"; fi; ENV_FILE="~w"; if [ -n "$ENV_FILE" ] && [ -f "$ENV_FILE" ]; then while IFS= read -r line || [ -n "$line" ]; do case "$line" in "#"*|"") continue ;; esac; if [ -n "$ENV_CONTENT" ]; then ENV_CONTENT="$ENV_CONTENT\\n"; fi; ENV_CONTENT="$ENV_CONTENT$line"; done < "$ENV_FILE"; fi; if [ -n "$ENV_CONTENT" ]; then TS2=$(date +%s); SIG2=$(hmac "$UN_SK" "$TS2:PUT:/services/$SVC_ID/env:$ENV_CONTENT"); curl -s -X PUT "$UN_API/services/$SVC_ID/env" -H "Authorization: Bearer ~w" -H "X-Timestamp: $TS2" -H "X-Signature: $SIG2" -H "Content-Type: text/plain" --data-binary "$ENV_CONTENT" >/dev/null && echo -e "\\x1b[32mVault configured\\x1b[0m"; fi; else echo "$RESP" | jq .; fi',
        [FileArgs, Name, PortsJson, BootstrapJson, BootstrapContentJson, ServiceTypeJson, PublicKey, Envs, EnvFile, PublicKey]),
    signed_shell(SecretKey, Cmd, 0).

% Main program
main(Argv) :-
    % Initialize account_index global to -1 (not set)
    nb_setval(account_index, -1),
    api_base(ApiBase),
    setenv('UN_API', ApiBase),

    % Parse --account N global flag if present
    (   Argv = ['--account', NStr|Rest]
    ->  (   atom_number(NStr, N), integer(N)
        ->  nb_setval(account_index, N),
            ActualArgv = Rest
        ;   write(user_error, 'Error: --account requires an integer argument\n'),
            halt(1)
        )
    ;   ActualArgv = Argv
    ),

    % Check arguments
    (   ActualArgv = []
    ->  write(user_error, 'Usage: un.pro [options] <source_file>\n'),
        write(user_error, '       un.pro session [options]\n'),
        write(user_error, '       un.pro service [options]\n'),
        write(user_error, '       un.pro snapshot [options]\n'),
        write(user_error, '       un.pro image [options]\n'),
        write(user_error, '       un.pro languages [--json]\n'),
        write(user_error, '       un.pro key [options]\n'),
        write(user_error, '       un.pro key --rotate [--otp CODE]\n'),
        write(user_error, '\n'),
        write(user_error, 'Snapshot options:\n'),
        write(user_error, '  --list, -l          List all snapshots\n'),
        write(user_error, '  --info ID           Get snapshot details\n'),
        write(user_error, '  --delete ID         Delete a snapshot\n'),
        write(user_error, '  --lock ID           Lock snapshot\n'),
        write(user_error, '  --unlock ID         Unlock snapshot\n'),
        write(user_error, '  --restore ID        Restore from snapshot\n'),
        write(user_error, '  --clone ID          Clone snapshot (--type required)\n'),
        write(user_error, '  --type TYPE         Clone type: session or service\n'),
        write(user_error, '  --name NAME         Name for cloned resource\n'),
        write(user_error, '  --shell SHELL       Shell for cloned session\n'),
        write(user_error, '  --ports PORTS       Ports for cloned service\n'),
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
    (   ActualArgv = ['session'|Rest]
    ->  handle_session(Rest)
    ;   ActualArgv = ['service'|Rest]
    ->  handle_service(Rest)
    ;   ActualArgv = ['snapshot'|Rest]
    ->  handle_snapshot(Rest)
    ;   ActualArgv = ['image'|Rest]
    ->  handle_image(Rest)
    ;   ActualArgv = ['languages'|Rest]
    ->  handle_languages(Rest)
    ;   ActualArgv = ['key'|Rest]
    ->  handle_key(Rest)
    ;   ActualArgv = [Filename|_]
    ->  execute_file(Filename)
    ;   write(user_error, 'Error: Invalid arguments\n'),
        halt(1)
    ).

% =============================================================================
% Snapshot handlers
% =============================================================================

handle_snapshot(Args) :-
    get_public_key(PublicKey),
    get_secret_key(SecretKey),
    handle_snapshot_cmd(Args, PublicKey, SecretKey).

handle_snapshot_cmd(['--list'|_], PublicKey, SecretKey) :- !,
    atomic_list_concat([
        'TS=$(date +%s); ',
        'SIG=$(hmac "$UN_SK" "$TS:GET:/snapshots:"); ',
        'curl -s -X GET "$UN_API/snapshots" ',
        '-H "Authorization: Bearer ', PublicKey, '" ',
        '-H "X-Timestamp: $TS" ',
        '-H "X-Signature: $SIG" | jq .'
    ], Cmd),
    signed_shell(SecretKey, Cmd).

handle_snapshot_cmd(['-l'|_], PublicKey, SecretKey) :- !,
    handle_snapshot_cmd(['--list'], PublicKey, SecretKey).

handle_snapshot_cmd(['--info', Id|_], PublicKey, SecretKey) :- !,
    atomic_list_concat([
        'TS=$(date +%s); ',
        'SIG=$(hmac "$UN_SK" "$TS:GET:/snapshots/', Id, ':"); ',
        'curl -s -X GET "$UN_API/snapshots/', Id, '" ',
        '-H "Authorization: Bearer ', PublicKey, '" ',
        '-H "X-Timestamp: $TS" ',
        '-H "X-Signature: $SIG" | jq .'
    ], Cmd),
    signed_shell(SecretKey, Cmd).

handle_snapshot_cmd(['--delete', Id|_], PublicKey, SecretKey) :- !,
    atomic_list_concat([
        'TS=$(date +%s); ',
        'SIG=$(hmac "$UN_SK" "$TS:DELETE:/snapshots/', Id, ':"); ',
        'RESP=$(curl -s -w "\\n%{http_code}" -X DELETE "$UN_API/snapshots/', Id, '" ',
        '-H "Authorization: Bearer ', PublicKey, '" ',
        '-H "X-Timestamp: $TS" ',
        '-H "X-Signature: $SIG"); ',
        'HTTP_CODE=$(echo "$RESP" | tail -1); ',
        'BODY=$(echo "$RESP" | head -n -1); ',
        'if [ "$HTTP_CODE" = "428" ]; then ',
        'OTP=$(echo "$BODY" | jq -r ".otp // empty"); ',
        'if [ -n "$OTP" ]; then ',
        'TS2=$(date +%s); ',
        'SIG2=$(hmac "$UN_SK" "$TS2:DELETE:/snapshots/', Id, ':"); ',
        'curl -s -X DELETE "$UN_API/snapshots/', Id, '" ',
        '-H "Authorization: Bearer ', PublicKey, '" ',
        '-H "X-Timestamp: $TS2" ',
        '-H "X-Signature: $SIG2" ',
        '-H "X-Sudo-OTP: $OTP" | jq .; ',
        'echo -e "\\x1b[32mSnapshot deleted\\x1b[0m"; fi; ',
        'else echo "$BODY" | jq .; fi'
    ], Cmd),
    signed_shell(SecretKey, Cmd).

handle_snapshot_cmd(['--lock', Id|_], PublicKey, SecretKey) :- !,
    atomic_list_concat([
        'TS=$(date +%s); ',
        'SIG=$(hmac "$UN_SK" "$TS:POST:/snapshots/', Id, '/lock:"); ',
        'curl -s -X POST "$UN_API/snapshots/', Id, '/lock" ',
        '-H "Authorization: Bearer ', PublicKey, '" ',
        '-H "X-Timestamp: $TS" ',
        '-H "X-Signature: $SIG" | jq . && ',
        'echo -e "\\x1b[32mSnapshot locked\\x1b[0m"'
    ], Cmd),
    signed_shell(SecretKey, Cmd).

handle_snapshot_cmd(['--unlock', Id|_], PublicKey, SecretKey) :- !,
    atomic_list_concat([
        'TS=$(date +%s); ',
        'BODY="{}"; ',
        'SIG=$(hmac "$UN_SK" "$TS:POST:/snapshots/', Id, '/unlock:$BODY"); ',
        'RESP=$(curl -s -w "\\n%{http_code}" -X POST "$UN_API/snapshots/', Id, '/unlock" ',
        '-H "Content-Type: application/json" ',
        '-H "Authorization: Bearer ', PublicKey, '" ',
        '-H "X-Timestamp: $TS" ',
        '-H "X-Signature: $SIG" ',
        '-d "$BODY"); ',
        'HTTP_CODE=$(echo "$RESP" | tail -1); ',
        'BODY_RESP=$(echo "$RESP" | head -n -1); ',
        'if [ "$HTTP_CODE" = "428" ]; then ',
        'OTP=$(echo "$BODY_RESP" | jq -r ".otp // empty"); ',
        'if [ -n "$OTP" ]; then ',
        'TS2=$(date +%s); ',
        'SIG2=$(hmac "$UN_SK" "$TS2:POST:/snapshots/', Id, '/unlock:$BODY"); ',
        'curl -s -X POST "$UN_API/snapshots/', Id, '/unlock" ',
        '-H "Content-Type: application/json" ',
        '-H "Authorization: Bearer ', PublicKey, '" ',
        '-H "X-Timestamp: $TS2" ',
        '-H "X-Signature: $SIG2" ',
        '-H "X-Sudo-OTP: $OTP" ',
        '-d "$BODY" | jq .; ',
        'echo -e "\\x1b[32mSnapshot unlocked\\x1b[0m"; fi; ',
        'else echo "$BODY_RESP" | jq .; fi'
    ], Cmd),
    signed_shell(SecretKey, Cmd).

handle_snapshot_cmd(['--restore', Id|_], PublicKey, SecretKey) :- !,
    atomic_list_concat([
        'TS=$(date +%s); ',
        'BODY="{}"; ',
        'SIG=$(hmac "$UN_SK" "$TS:POST:/snapshots/', Id, '/restore:$BODY"); ',
        'curl -s -X POST "$UN_API/snapshots/', Id, '/restore" ',
        '-H "Content-Type: application/json" ',
        '-H "Authorization: Bearer ', PublicKey, '" ',
        '-H "X-Timestamp: $TS" ',
        '-H "X-Signature: $SIG" ',
        '-d "$BODY" | jq . && ',
        'echo -e "\\x1b[32mSnapshot restored\\x1b[0m"'
    ], Cmd),
    signed_shell(SecretKey, Cmd).

handle_snapshot_cmd(['--clone', Id|Rest], PublicKey, SecretKey) :- !,
    parse_snapshot_clone_args(Rest, Type, Name, Ports, Shell),
    (   Type = ''
    ->  write(user_error, '\x1b[31mError: --type required for --clone (session or service)\x1b[0m\n'),
        halt(1)
    ;   true
    ),
    build_snapshot_clone_body(Type, Name, Ports, Shell, Body),
    atomic_list_concat([
        'TS=$(date +%s); ',
        'BODY=''', Body, '''; ',
        'SIG=$(hmac "$UN_SK" "$TS:POST:/snapshots/', Id, '/clone:$BODY"); ',
        'curl -s -X POST "$UN_API/snapshots/', Id, '/clone" ',
        '-H "Content-Type: application/json" ',
        '-H "Authorization: Bearer ', PublicKey, '" ',
        '-H "X-Timestamp: $TS" ',
        '-H "X-Signature: $SIG" ',
        '-d "$BODY" | jq . && ',
        'echo -e "\\x1b[32mSnapshot cloned\\x1b[0m"'
    ], Cmd),
    signed_shell(SecretKey, Cmd).

handle_snapshot_cmd(_, _, _) :-
    write(user_error, '\x1b[31mError: Use --list, --info, --delete, --lock, --unlock, --restore, or --clone\x1b[0m\n'),
    halt(1).

parse_snapshot_clone_args([], '', '', '', '').
parse_snapshot_clone_args(['--type', Type|Rest], Type, Name, Ports, Shell) :- !,
    parse_snapshot_clone_args(Rest, _, Name, Ports, Shell).
parse_snapshot_clone_args(['--name', Name|Rest], Type, Name, Ports, Shell) :- !,
    parse_snapshot_clone_args(Rest, Type, _, Ports, Shell).
parse_snapshot_clone_args(['--ports', Ports|Rest], Type, Name, Ports, Shell) :- !,
    parse_snapshot_clone_args(Rest, Type, Name, _, Shell).
parse_snapshot_clone_args(['--shell', Shell|Rest], Type, Name, Ports, Shell) :- !,
    parse_snapshot_clone_args(Rest, Type, Name, Ports, _).
parse_snapshot_clone_args([_|Rest], Type, Name, Ports, Shell) :-
    parse_snapshot_clone_args(Rest, Type, Name, Ports, Shell).

build_snapshot_clone_body(Type, Name, Ports, Shell, Body) :-
    atomic_list_concat(['{"type":"', Type, '"'], Base),
    (   Name \= ''
    ->  atomic_list_concat([Base, ',"name":"', Name, '"'], Base2)
    ;   Base2 = Base
    ),
    (   Ports \= ''
    ->  atomic_list_concat([Base2, ',"ports":[', Ports, ']'], Base3)
    ;   Base3 = Base2
    ),
    (   Shell \= ''
    ->  atomic_list_concat([Base3, ',"shell":"', Shell, '"'], Base4)
    ;   Base4 = Base3
    ),
    atomic_list_concat([Base4, '}'], Body).
