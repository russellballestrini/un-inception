! PUBLIC DOMAIN - NO LICENSE, NO WARRANTY
!
! This is free public domain software for the public good of a permacomputer hosted
! at permacomputer.com - an always-on computer by the people, for the people. One
! which is durable, easy to repair, and distributed like tap water for machine
! learning intelligence.
!
! The permacomputer is community-owned infrastructure optimized around four values:
!
!   TRUTH    - First principles, math & science, open source code freely distributed
!   FREEDOM  - Voluntary partnerships, freedom from tyranny & corporate control
!   HARMONY  - Minimal waste, self-renewing systems with diverse thriving connections
!   LOVE     - Be yourself without hurting others, cooperation through natural law
!
! This software contributes to that vision by enabling code execution across 42+
! programming languages through a unified interface, accessible to all. Code is
! seeds to sprout on any abandoned technology.
!
! Learn more: https://www.permacomputer.com
!
! Anyone is free to copy, modify, publish, use, compile, sell, or distribute this
! software, either in source code form or as a compiled binary, for any purpose,
! commercial or non-commercial, and by any means.
!
! NO WARRANTY. THE SOFTWARE IS PROVIDED "AS IS" WITHOUT WARRANTY OF ANY KIND.
!
! That said, our permacomputer's digital membrane stratum continuously runs unit,
! integration, and functional tests on all of it's own software - with our
! permacomputer monitoring itself, repairing itself, with minimal human in the
! loop guidance. Our agents do their best.
!
! Copyright 2025 TimeHexOn & foxhop & russell@unturf
! https://www.timehexon.com
! https://www.foxhop.net
! https://www.unturf.com/software


program unsandbox_cli
    implicit none
    character(len=2048) :: cmd_line, curl_cmd
    character(len=1024) :: filename, language, api_key, ext, arg, subcommand
    character(len=256) :: session_id, service_id
    integer :: stat, i, nargs, dot_pos
    logical :: list_flag, is_session, is_service, is_key

    ! Initialize
    subcommand = ''
    list_flag = .false.
    is_session = .false.
    is_service = .false.
    is_key = .false.
    session_id = ''
    service_id = ''

    ! Get command line arguments count
    nargs = command_argument_count()
    if (nargs < 1) then
        write(0, '(A)') 'Usage: un.f90 [options] <source_file>'
        write(0, '(A)') '       un.f90 session [options]'
        write(0, '(A)') '       un.f90 service [options]'
        write(0, '(A)') '       un.f90 key [--extend]'
        stop 1
    end if

    ! Check for subcommands
    call get_command_argument(1, arg, status=stat)
    if (trim(arg) == 'session') then
        is_session = .true.
        call handle_session()
        stop 0
    else if (trim(arg) == 'service') then
        is_service = .true.
        call handle_service()
        stop 0
    else if (trim(arg) == 'key') then
        is_key = .true.
        call handle_key()
        stop 0
    else
        ! Default execute command
        filename = trim(arg)
        call handle_execute(filename)
        stop 0
    end if

contains

    subroutine handle_execute(fname)
        character(len=*), intent(in) :: fname
        character(len=4096) :: full_cmd
        character(len=1024) :: env_opts, file_opts, net_opt, public_key, secret_key
        integer :: i, arg_idx
        logical :: artifacts, has_env, has_files

        ! Check if file exists
        inquire(file=trim(fname), exist=stat)
        if (.not. stat) then
            write(0, '(A,A)') 'Error: File not found: ', trim(fname)
            stop 1
        end if

        ! Detect language from extension
        dot_pos = index(trim(fname), '.', back=.true.)
        if (dot_pos == 0) then
            write(0, '(A)') 'Error: No file extension found'
            stop 1
        end if
        ext = fname(dot_pos:)

        ! Simple extension mapping
        language = 'unknown'
        if (trim(ext) == '.jl') language = 'julia'
        if (trim(ext) == '.r') language = 'r'
        if (trim(ext) == '.cr') language = 'crystal'
        if (trim(ext) == '.f90') language = 'fortran'
        if (trim(ext) == '.cob') language = 'cobol'
        if (trim(ext) == '.pro') language = 'prolog'
        if (trim(ext) == '.forth' .or. trim(ext) == '.4th') language = 'forth'
        if (trim(ext) == '.py') language = 'python'
        if (trim(ext) == '.js') language = 'javascript'
        if (trim(ext) == '.rb') language = 'ruby'
        if (trim(ext) == '.go') language = 'go'
        if (trim(ext) == '.rs') language = 'rust'
        if (trim(ext) == '.c') language = 'c'
        if (trim(ext) == '.cpp') language = 'cpp'
        if (trim(ext) == '.java') language = 'java'
        if (trim(ext) == '.sh') language = 'bash'

        if (trim(language) == 'unknown') then
            write(0, '(A,A)') 'Error: Unknown language for file: ', trim(fname)
            stop 1
        end if

        ! Get API keys (try new format first, fall back to old)
        call get_environment_variable('UNSANDBOX_PUBLIC_KEY', public_key, status=stat)
        if (stat == 0 .and. len_trim(public_key) > 0) then
            call get_environment_variable('UNSANDBOX_SECRET_KEY', secret_key, status=stat)
            if (stat /= 0 .or. len_trim(secret_key) == 0) then
                write(0, '(A)') 'Error: UNSANDBOX_SECRET_KEY not set'
                stop 1
            end if
        else
            ! Fall back to old-style single key
            call get_environment_variable('UNSANDBOX_API_KEY', api_key, status=stat)
            if (stat /= 0 .or. len_trim(api_key) == 0) then
                write(0, '(A)') 'Error: UNSANDBOX_PUBLIC_KEY/UNSANDBOX_SECRET_KEY or UNSANDBOX_API_KEY not set'
                stop 1
            end if
            public_key = api_key
            secret_key = api_key
        end if

        ! Parse additional arguments (simple version - only support basic flags)
        env_opts = ''
        file_opts = ''
        net_opt = ''
        artifacts = .false.

        ! Build curl command with HMAC auth (use bash to compute signature)
        write(full_cmd, '(30A)') &
            'TS=$(date +%s); ', &
            'BODY=$(jq -Rs ''{language: "', trim(language), '", code: .}'' < "', trim(fname), '"); ', &
            'SIG=$(echo -n "$TS:POST:/execute:$BODY" | openssl dgst -sha256 -hmac "', trim(secret_key), '" | cut -d" " -f2); ', &
            'curl -s -X POST https://api.unsandbox.com/execute ', &
            '-H "Content-Type: application/json" ', &
            '-H "Authorization: Bearer ', trim(public_key), '" ', &
            '-H "X-Timestamp: $TS" ', &
            '-H "X-Signature: $SIG" ', &
            '--data-binary "$BODY" -o /tmp/unsandbox_resp.json; ', &
            'RESP=$(cat /tmp/unsandbox_resp.json); ', &
            'if echo "$RESP" | grep -q "timestamp" && ', &
            '(echo "$RESP" | grep -Eq "(401|expired|invalid)"); then ', &
            'echo -e "\x1b[31mError: Request timestamp expired (must be within 5 minutes of server time)\x1b[0m" >&2; ', &
            'echo -e "\x1b[33mYour computer'\''s clock may have drifted.\x1b[0m" >&2; ', &
            'echo "Check your system time and sync with NTP if needed:" >&2; ', &
            'echo "  Linux:   sudo ntpdate -s time.nist.gov" >&2; ', &
            'echo "  macOS:   sudo sntp -sS time.apple.com" >&2; ', &
            'echo -e "  Windows: w32tm /resync\x1b[0m" >&2; ', &
            'rm -f /tmp/unsandbox_resp.json; exit 1; fi; ', &
            'jq -r ".stdout // empty" /tmp/unsandbox_resp.json | ', &
            'sed "s/^/\x1b[34m/" | sed "s/$/\x1b[0m/"; ', &
            'jq -r ".stderr // empty" /tmp/unsandbox_resp.json | ', &
            'sed "s/^/\x1b[31m/" | sed "s/$/\x1b[0m/" >&2; ', &
            'rm -f /tmp/unsandbox_resp.json'

        ! Execute command
        call execute_command_line(trim(full_cmd), wait=.true., exitstat=stat)
        if (stat /= 0) then
            write(0, '(A)') 'Error: Request failed'
            stop 1
        end if
    end subroutine handle_execute

    subroutine handle_session()
        character(len=8192) :: full_cmd
        character(len=256) :: arg, session_id
        character(len=1024) :: public_key, secret_key, input_files
        integer :: i, stat
        logical :: list_mode, kill_mode

        list_mode = .false.
        kill_mode = .false.
        session_id = ''
        input_files = ''

        ! Parse session arguments
        do i = 2, command_argument_count()
            call get_command_argument(i, arg)
            if (trim(arg) == '-l' .or. trim(arg) == '--list') then
                list_mode = .true.
            else if (trim(arg) == '--kill') then
                kill_mode = .true.
                if (i+1 <= command_argument_count()) then
                    call get_command_argument(i+1, session_id)
                end if
            else if (trim(arg) == '-f') then
                if (i+1 <= command_argument_count()) then
                    call get_command_argument(i+1, arg)
                    if (len_trim(input_files) > 0) then
                        input_files = trim(input_files) // ',' // trim(arg)
                    else
                        input_files = trim(arg)
                    end if
                end if
            else
                if (len_trim(arg) > 0) then
                    if (arg(1:1) == '-') then
                        write(0, '(A,A)') 'Unknown option: ', trim(arg)
                        write(0, '(A)') 'Usage: un.f90 session [options]'
                        stop 1
                    end if
                end if
            end if
        end do

        ! Get API keys (try new format first, fall back to old)
        call get_environment_variable('UNSANDBOX_PUBLIC_KEY', public_key, status=stat)
        if (stat == 0 .and. len_trim(public_key) > 0) then
            call get_environment_variable('UNSANDBOX_SECRET_KEY', secret_key, status=stat)
            if (stat /= 0 .or. len_trim(secret_key) == 0) then
                write(0, '(A)') 'Error: UNSANDBOX_SECRET_KEY not set'
                stop 1
            end if
        else
            call get_environment_variable('UNSANDBOX_API_KEY', api_key, status=stat)
            if (stat /= 0 .or. len_trim(api_key) == 0) then
                write(0, '(A)') 'Error: UNSANDBOX_PUBLIC_KEY/UNSANDBOX_SECRET_KEY or UNSANDBOX_API_KEY not set'
                stop 1
            end if
            public_key = api_key
            secret_key = api_key
        end if

        if (list_mode) then
            ! List sessions - GET request with empty body
            write(full_cmd, '(20A)') &
                'TS=$(date +%s); ', &
                'SIG=$(echo -n "$TS:GET:/sessions:" | openssl dgst -sha256 -hmac "', trim(secret_key), '" | cut -d" " -f2); ', &
                'curl -s -X GET https://api.unsandbox.com/sessions ', &
                '-H "Authorization: Bearer ', trim(public_key), '" ', &
                '-H "X-Timestamp: $TS" ', &
                '-H "X-Signature: $SIG" | ', &
                'jq -r ''.sessions[] | "\(.id) \(.shell) \(.status) \(.created_at)"'' ', &
                '2>/dev/null || echo "No active sessions"'
            call execute_command_line(trim(full_cmd), wait=.true.)
        else if (kill_mode .and. len_trim(session_id) > 0) then
            ! Kill session - DELETE request with empty body
            write(full_cmd, '(20A)') &
                'TS=$(date +%s); ', &
                'SIG=$(echo -n "$TS:DELETE:/sessions/', trim(session_id), ':" | openssl dgst -sha256 -hmac "', trim(secret_key), '" | cut -d" " -f2); ', &
                'curl -s -X DELETE https://api.unsandbox.com/sessions/', &
                trim(session_id), ' ', &
                '-H "Authorization: Bearer ', trim(public_key), '" ', &
                '-H "X-Timestamp: $TS" ', &
                '-H "X-Signature: $SIG" >/dev/null && ', &
                'echo -e "\x1b[32mSession terminated: ', trim(session_id), '\x1b[0m"'
            call execute_command_line(trim(full_cmd), wait=.true.)
        else
            ! Create session with optional input_files
            if (len_trim(input_files) > 0) then
                write(full_cmd, '(30A)') &
                    'INPUT_FILES=""; ', &
                    'IFS='','' read -ra FILES <<< "', trim(input_files), '"; ', &
                    'for f in "${FILES[@]}"; do ', &
                    'b64=$(base64 -w0 "$f" 2>/dev/null || base64 "$f"); ', &
                    'name=$(basename "$f"); ', &
                    'if [ -n "$INPUT_FILES" ]; then INPUT_FILES="$INPUT_FILES,"; fi; ', &
                    'INPUT_FILES="$INPUT_FILES{\"filename\":\"$name\",\"content\":\"$b64\"}"; ', &
                    'done; ', &
                    'BODY=''{"shell":"bash","input_files":[''"$INPUT_FILES"'']}''; ', &
                    'TS=$(date +%s); ', &
                    'SIG=$(echo -n "$TS:POST:/sessions:$BODY" | openssl dgst -sha256 -hmac "', trim(secret_key), '" | cut -d" " -f2); ', &
                    'curl -s -X POST https://api.unsandbox.com/sessions ', &
                    '-H "Content-Type: application/json" ', &
                    '-H "Authorization: Bearer ', trim(public_key), '" ', &
                    '-H "X-Timestamp: $TS" ', &
                    '-H "X-Signature: $SIG" ', &
                    '-d "$BODY" && ', &
                    'echo -e "\x1b[33mSession created (WebSocket required)\x1b[0m"'
            else
                write(full_cmd, '(20A)') &
                    'BODY=''{"shell":"bash"}''; ', &
                    'TS=$(date +%s); ', &
                    'SIG=$(echo -n "$TS:POST:/sessions:$BODY" | openssl dgst -sha256 -hmac "', trim(secret_key), '" | cut -d" " -f2); ', &
                    'curl -s -X POST https://api.unsandbox.com/sessions ', &
                    '-H "Content-Type: application/json" ', &
                    '-H "Authorization: Bearer ', trim(public_key), '" ', &
                    '-H "X-Timestamp: $TS" ', &
                    '-H "X-Signature: $SIG" ', &
                    '-d "$BODY" && ', &
                    'echo -e "\x1b[33mSession created (WebSocket required)\x1b[0m"'
            end if
            call execute_command_line(trim(full_cmd), wait=.true.)
        end if
    end subroutine handle_session

    subroutine handle_service()
        character(len=8192) :: full_cmd
        character(len=256) :: arg, service_id, operation, service_type, service_name
        character(len=1024) :: input_files, public_key, secret_key
        character(len=2048) :: svc_envs, svc_env_file, env_action, env_target
        integer :: i, stat, resize_vcpu
        logical :: list_mode

        list_mode = .false.
        operation = ''
        service_id = ''
        service_type = ''
        service_name = ''
        input_files = ''
        svc_envs = ''
        svc_env_file = ''
        env_action = ''
        env_target = ''
        resize_vcpu = 0

        ! Parse service arguments
        i = 2
        do while (i <= command_argument_count())
            call get_command_argument(i, arg)
            if (trim(arg) == '-l' .or. trim(arg) == '--list') then
                list_mode = .true.
            else if (trim(arg) == 'env') then
                ! service env <action> <service_id>
                if (i+2 <= command_argument_count()) then
                    call get_command_argument(i+1, env_action)
                    call get_command_argument(i+2, env_target)
                    i = i + 2
                end if
            else if (trim(arg) == '--name') then
                operation = 'create'
                if (i+1 <= command_argument_count()) then
                    call get_command_argument(i+1, service_name)
                    i = i + 1
                end if
            else if (trim(arg) == '--type') then
                if (i+1 <= command_argument_count()) then
                    call get_command_argument(i+1, service_type)
                    i = i + 1
                end if
            else if (trim(arg) == '-e') then
                if (i+1 <= command_argument_count()) then
                    call get_command_argument(i+1, arg)
                    if (len_trim(svc_envs) > 0) then
                        svc_envs = trim(svc_envs) // char(10) // trim(arg)
                    else
                        svc_envs = trim(arg)
                    end if
                    i = i + 1
                end if
            else if (trim(arg) == '--env-file') then
                if (i+1 <= command_argument_count()) then
                    call get_command_argument(i+1, svc_env_file)
                    i = i + 1
                end if
            else if (trim(arg) == '-f') then
                if (i+1 <= command_argument_count()) then
                    call get_command_argument(i+1, arg)
                    if (len_trim(input_files) > 0) then
                        input_files = trim(input_files) // ',' // trim(arg)
                    else
                        input_files = trim(arg)
                    end if
                    i = i + 1
                end if
            else if (trim(arg) == '--info') then
                operation = 'info'
                if (i+1 <= command_argument_count()) then
                    call get_command_argument(i+1, service_id)
                    i = i + 1
                end if
            else if (trim(arg) == '--logs') then
                operation = 'logs'
                if (i+1 <= command_argument_count()) then
                    call get_command_argument(i+1, service_id)
                    i = i + 1
                end if
            else if (trim(arg) == '--freeze') then
                operation = 'sleep'
                if (i+1 <= command_argument_count()) then
                    call get_command_argument(i+1, service_id)
                    i = i + 1
                end if
            else if (trim(arg) == '--unfreeze') then
                operation = 'wake'
                if (i+1 <= command_argument_count()) then
                    call get_command_argument(i+1, service_id)
                    i = i + 1
                end if
            else if (trim(arg) == '--destroy') then
                operation = 'destroy'
                if (i+1 <= command_argument_count()) then
                    call get_command_argument(i+1, service_id)
                    i = i + 1
                end if
            else if (trim(arg) == '--resize') then
                operation = 'resize'
                if (i+1 <= command_argument_count()) then
                    call get_command_argument(i+1, service_id)
                    i = i + 1
                end if
            else if (trim(arg) == '--vcpu') then
                if (i+1 <= command_argument_count()) then
                    call get_command_argument(i+1, arg)
                    read(arg, *) resize_vcpu
                    i = i + 1
                end if
            else if (trim(arg) == '-v' .and. operation == 'resize') then
                if (i+1 <= command_argument_count()) then
                    call get_command_argument(i+1, arg)
                    read(arg, *) resize_vcpu
                    i = i + 1
                end if
            else if (trim(arg) == '--dump-bootstrap') then
                operation = 'dump-bootstrap'
                if (i+1 <= command_argument_count()) then
                    call get_command_argument(i+1, service_id)
                    i = i + 1
                end if
            else if (trim(arg) == '--dump-file') then
                operation = 'dump-file'
                if (i+1 <= command_argument_count()) then
                    call get_command_argument(i+1, service_type)
                    i = i + 1
                end if
            end if
            i = i + 1
        end do

        ! Get API keys (try new format first, fall back to old)
        call get_environment_variable('UNSANDBOX_PUBLIC_KEY', public_key, status=stat)
        if (stat == 0 .and. len_trim(public_key) > 0) then
            call get_environment_variable('UNSANDBOX_SECRET_KEY', secret_key, status=stat)
            if (stat /= 0 .or. len_trim(secret_key) == 0) then
                write(0, '(A)') 'Error: UNSANDBOX_SECRET_KEY not set'
                stop 1
            end if
        else
            call get_environment_variable('UNSANDBOX_API_KEY', api_key, status=stat)
            if (stat /= 0 .or. len_trim(api_key) == 0) then
                write(0, '(A)') 'Error: UNSANDBOX_PUBLIC_KEY/UNSANDBOX_SECRET_KEY or UNSANDBOX_API_KEY not set'
                stop 1
            end if
            public_key = api_key
            secret_key = api_key
        end if

        ! Handle env subcommand
        if (len_trim(env_action) > 0) then
            if (trim(env_action) == 'status') then
                write(full_cmd, '(20A)') &
                    'TS=$(date +%s); ', &
                    'SIG=$(echo -n "$TS:GET:/services/', trim(env_target), '/env:" | ', &
                    'openssl dgst -sha256 -hmac "', trim(secret_key), '" | cut -d" " -f2); ', &
                    'curl -s -X GET "https://api.unsandbox.com/services/', trim(env_target), '/env" ', &
                    '-H "Authorization: Bearer ', trim(public_key), '" ', &
                    '-H "X-Timestamp: $TS" ', &
                    '-H "X-Signature: $SIG" | jq .'
                call execute_command_line(trim(full_cmd), wait=.true.)
                return
            else if (trim(env_action) == 'set') then
                ! Build env content from -e flags and --env-file
                write(full_cmd, '(50A)') &
                    'ENV_CONTENT=""; ', &
                    'ENV_LINES="', trim(svc_envs), '"; ', &
                    'if [ -n "$ENV_LINES" ]; then ', &
                    'ENV_CONTENT="$ENV_LINES"; ', &
                    'fi; ', &
                    'ENV_FILE="', trim(svc_env_file), '"; ', &
                    'if [ -n "$ENV_FILE" ] && [ -f "$ENV_FILE" ]; then ', &
                    'while IFS= read -r line || [ -n "$line" ]; do ', &
                    'case "$line" in "#"*|"") continue ;; esac; ', &
                    'if [ -n "$ENV_CONTENT" ]; then ENV_CONTENT="$ENV_CONTENT', char(10), '"; fi; ', &
                    'ENV_CONTENT="$ENV_CONTENT$line"; ', &
                    'done < "$ENV_FILE"; fi; ', &
                    'if [ -z "$ENV_CONTENT" ]; then ', &
                    'echo -e "\x1b[31mError: No environment variables to set\x1b[0m" >&2; exit 1; fi; ', &
                    'TS=$(date +%s); ', &
                    'SIG=$(echo -n "$TS:PUT:/services/', trim(env_target), '/env:$ENV_CONTENT" | ', &
                    'openssl dgst -sha256 -hmac "', trim(secret_key), '" | cut -d" " -f2); ', &
                    'curl -s -X PUT "https://api.unsandbox.com/services/', trim(env_target), '/env" ', &
                    '-H "Authorization: Bearer ', trim(public_key), '" ', &
                    '-H "X-Timestamp: $TS" ', &
                    '-H "X-Signature: $SIG" ', &
                    '-H "Content-Type: text/plain" ', &
                    '--data-binary "$ENV_CONTENT" | jq .'
                call execute_command_line(trim(full_cmd), wait=.true.)
                return
            else if (trim(env_action) == 'export') then
                write(full_cmd, '(20A)') &
                    'TS=$(date +%s); ', &
                    'SIG=$(echo -n "$TS:POST:/services/', trim(env_target), '/env/export:" | ', &
                    'openssl dgst -sha256 -hmac "', trim(secret_key), '" | cut -d" " -f2); ', &
                    'curl -s -X POST "https://api.unsandbox.com/services/', trim(env_target), '/env/export" ', &
                    '-H "Authorization: Bearer ', trim(public_key), '" ', &
                    '-H "X-Timestamp: $TS" ', &
                    '-H "X-Signature: $SIG" | jq -r ".content // empty"'
                call execute_command_line(trim(full_cmd), wait=.true.)
                return
            else if (trim(env_action) == 'delete') then
                write(full_cmd, '(20A)') &
                    'TS=$(date +%s); ', &
                    'SIG=$(echo -n "$TS:DELETE:/services/', trim(env_target), '/env:" | ', &
                    'openssl dgst -sha256 -hmac "', trim(secret_key), '" | cut -d" " -f2); ', &
                    'curl -s -X DELETE "https://api.unsandbox.com/services/', trim(env_target), '/env" ', &
                    '-H "Authorization: Bearer ', trim(public_key), '" ', &
                    '-H "X-Timestamp: $TS" ', &
                    '-H "X-Signature: $SIG" >/dev/null && ', &
                    'echo -e "\x1b[32mVault deleted for: ', trim(env_target), '\x1b[0m"'
                call execute_command_line(trim(full_cmd), wait=.true.)
                return
            else
                write(0, '(A,A)') 'Error: Unknown env action: ', trim(env_action)
                write(0, '(A)') 'Usage: un.f90 service env <status|set|export|delete> <service_id>'
                stop 1
            end if
        end if

        if (list_mode) then
            ! List services
            write(full_cmd, '(20A)') &
                'TS=$(date +%s); ', &
                'SIG=$(echo -n "$TS:GET:/services:" | openssl dgst -sha256 -hmac "', trim(secret_key), '" | cut -d" " -f2); ', &
                'curl -s -X GET https://api.unsandbox.com/services ', &
                '-H "Authorization: Bearer ', trim(public_key), '" ', &
                '-H "X-Timestamp: $TS" ', &
                '-H "X-Signature: $SIG" | ', &
                'jq -r ''.services[] | "\(.id) \(.name) \(.status)"'' ', &
                '2>/dev/null || echo "No services"'
            call execute_command_line(trim(full_cmd), wait=.true.)
        else if (trim(operation) == 'info' .and. len_trim(service_id) > 0) then
            write(full_cmd, '(20A)') &
                'TS=$(date +%s); ', &
                'SIG=$(echo -n "$TS:GET:/services/', trim(service_id), ':" | openssl dgst -sha256 -hmac "', trim(secret_key), '" | cut -d" " -f2); ', &
                'curl -s -X GET https://api.unsandbox.com/services/', &
                trim(service_id), ' ', &
                '-H "Authorization: Bearer ', trim(public_key), '" ', &
                '-H "X-Timestamp: $TS" ', &
                '-H "X-Signature: $SIG" | jq .'
            call execute_command_line(trim(full_cmd), wait=.true.)
        else if (trim(operation) == 'logs' .and. len_trim(service_id) > 0) then
            write(full_cmd, '(20A)') &
                'TS=$(date +%s); ', &
                'SIG=$(echo -n "$TS:GET:/services/', trim(service_id), '/logs:" | openssl dgst -sha256 -hmac "', trim(secret_key), '" | cut -d" " -f2); ', &
                'curl -s -X GET https://api.unsandbox.com/services/', &
                trim(service_id), '/logs ', &
                '-H "Authorization: Bearer ', trim(public_key), '" ', &
                '-H "X-Timestamp: $TS" ', &
                '-H "X-Signature: $SIG" | jq -r ".logs"'
            call execute_command_line(trim(full_cmd), wait=.true.)
        else if (trim(operation) == 'sleep' .and. len_trim(service_id) > 0) then
            write(full_cmd, '(20A)') &
                'TS=$(date +%s); ', &
                'SIG=$(echo -n "$TS:POST:/services/', trim(service_id), '/freeze:" | openssl dgst -sha256 -hmac "', trim(secret_key), '" | cut -d" " -f2); ', &
                'curl -s -X POST https://api.unsandbox.com/services/', &
                trim(service_id), '/freeze ', &
                '-H "Authorization: Bearer ', trim(public_key), '" ', &
                '-H "X-Timestamp: $TS" ', &
                '-H "X-Signature: $SIG" >/dev/null && ', &
                'echo -e "\x1b[32mService frozen: ', trim(service_id), '\x1b[0m"'
            call execute_command_line(trim(full_cmd), wait=.true.)
        else if (trim(operation) == 'wake' .and. len_trim(service_id) > 0) then
            write(full_cmd, '(20A)') &
                'TS=$(date +%s); ', &
                'SIG=$(echo -n "$TS:POST:/services/', trim(service_id), '/unfreeze:" | openssl dgst -sha256 -hmac "', trim(secret_key), '" | cut -d" " -f2); ', &
                'curl -s -X POST https://api.unsandbox.com/services/', &
                trim(service_id), '/unfreeze ', &
                '-H "Authorization: Bearer ', trim(public_key), '" ', &
                '-H "X-Timestamp: $TS" ', &
                '-H "X-Signature: $SIG" >/dev/null && ', &
                'echo -e "\x1b[32mService unfreezing: ', trim(service_id), '\x1b[0m"'
            call execute_command_line(trim(full_cmd), wait=.true.)
        else if (trim(operation) == 'destroy' .and. len_trim(service_id) > 0) then
            write(full_cmd, '(20A)') &
                'TS=$(date +%s); ', &
                'SIG=$(echo -n "$TS:DELETE:/services/', trim(service_id), ':" | openssl dgst -sha256 -hmac "', trim(secret_key), '" | cut -d" " -f2); ', &
                'curl -s -X DELETE https://api.unsandbox.com/services/', &
                trim(service_id), ' ', &
                '-H "Authorization: Bearer ', trim(public_key), '" ', &
                '-H "X-Timestamp: $TS" ', &
                '-H "X-Signature: $SIG" >/dev/null && ', &
                'echo -e "\x1b[32mService destroyed: ', trim(service_id), '\x1b[0m"'
            call execute_command_line(trim(full_cmd), wait=.true.)
        else if (trim(operation) == 'resize' .and. len_trim(service_id) > 0) then
            if (resize_vcpu < 1 .or. resize_vcpu > 8) then
                write(0, '(A)') char(27)//'[31mError: --vcpu must be between 1 and 8'//char(27)//'[0m'
                stop 1
            end if
            write(full_cmd, '(30A,I0,A,I0,A,I0,A)') &
                'VCPU=', resize_vcpu, '; ', &
                'RAM=$((VCPU * 2)); ', &
                'BODY=''{"vcpu":''$VCPU''}''; ', &
                'TS=$(date +%s); ', &
                'SIG=$(echo -n "$TS:PATCH:/services/', trim(service_id), ':$BODY" | openssl dgst -sha256 -hmac "', trim(secret_key), '" | cut -d" " -f2); ', &
                'curl -s -X PATCH https://api.unsandbox.com/services/', &
                trim(service_id), ' ', &
                '-H "Content-Type: application/json" ', &
                '-H "Authorization: Bearer ', trim(public_key), '" ', &
                '-H "X-Timestamp: $TS" ', &
                '-H "X-Signature: $SIG" ', &
                '-d "$BODY" >/dev/null && ', &
                'echo -e "\x1b[32mService resized to ', resize_vcpu, ' vCPU, ', resize_vcpu * 2, ' GB RAM\x1b[0m"'
            call execute_command_line(trim(full_cmd), wait=.true.)
        else if (trim(operation) == 'dump-bootstrap' .and. len_trim(service_id) > 0) then
            write(full_cmd, '(30A)') &
                'echo "Fetching bootstrap script from ', trim(service_id), '..." >&2; ', &
                'BODY=''{"command":"cat /tmp/bootstrap.sh"}''; ', &
                'TS=$(date +%s); ', &
                'SIG=$(echo -n "$TS:POST:/services/', trim(service_id), '/execute:$BODY" | openssl dgst -sha256 -hmac "', trim(secret_key), '" | cut -d" " -f2); ', &
                'RESP=$(curl -s -X POST https://api.unsandbox.com/services/', &
                trim(service_id), '/execute ', &
                '-H "Content-Type: application/json" ', &
                '-H "Authorization: Bearer ', trim(public_key), '" ', &
                '-H "X-Timestamp: $TS" ', &
                '-H "X-Signature: $SIG" ', &
                '-d "$BODY"); ', &
                'STDOUT=$(echo "$RESP" | jq -r ".stdout // empty"); ', &
                'if [ -n "$STDOUT" ]; then ', &
                'if [ -n "', trim(service_type), '" ]; then ', &
                'echo "$STDOUT" > "', trim(service_type), '" && chmod 755 "', trim(service_type), '" && ', &
                'echo "Bootstrap saved to ', trim(service_type), '"; ', &
                'else echo "$STDOUT"; fi; ', &
                'else echo -e "\x1b[31mError: Failed to fetch bootstrap\x1b[0m" >&2; exit 1; fi'
            call execute_command_line(trim(full_cmd), wait=.true.)
        else if (trim(operation) == 'create' .and. len_trim(service_name) > 0) then
            ! Create service with optional input_files and auto-vault
            if (len_trim(input_files) > 0) then
                write(full_cmd, '(60A)') &
                    'INPUT_FILES=""; ', &
                    'IFS='','' read -ra FILES <<< "', trim(input_files), '"; ', &
                    'for f in "${FILES[@]}"; do ', &
                    'b64=$(base64 -w0 "$f" 2>/dev/null || base64 "$f"); ', &
                    'name=$(basename "$f"); ', &
                    'if [ -n "$INPUT_FILES" ]; then INPUT_FILES="$INPUT_FILES,"; fi; ', &
                    'INPUT_FILES="$INPUT_FILES{\"filename\":\"$name\",\"content\":\"$b64\"}"; ', &
                    'done; ', &
                    'BODY=''{"name":"', trim(service_name), '","input_files":[''"$INPUT_FILES"'']}''; ', &
                    'TS=$(date +%s); ', &
                    'SIG=$(echo -n "$TS:POST:/services:$BODY" | openssl dgst -sha256 -hmac "', trim(secret_key), '" | cut -d" " -f2); ', &
                    'RESP=$(curl -s -X POST https://api.unsandbox.com/services ', &
                    '-H "Content-Type: application/json" ', &
                    '-H "Authorization: Bearer ', trim(public_key), '" ', &
                    '-H "X-Timestamp: $TS" ', &
                    '-H "X-Signature: $SIG" ', &
                    '-d "$BODY"); ', &
                    'SVC_ID=$(echo "$RESP" | jq -r ".id // empty"); ', &
                    'if [ -n "$SVC_ID" ]; then ', &
                    'echo -e "\x1b[32m$SVC_ID created\x1b[0m"; ', &
                    'ENV_CONTENT=""; ', &
                    'ENV_LINES="', trim(svc_envs), '"; ', &
                    'if [ -n "$ENV_LINES" ]; then ENV_CONTENT="$ENV_LINES"; fi; ', &
                    'ENV_FILE="', trim(svc_env_file), '"; ', &
                    'if [ -n "$ENV_FILE" ] && [ -f "$ENV_FILE" ]; then ', &
                    'while IFS= read -r line || [ -n "$line" ]; do ', &
                    'case "$line" in "#"*|"") continue ;; esac; ', &
                    'if [ -n "$ENV_CONTENT" ]; then ENV_CONTENT="$ENV_CONTENT', char(10), '"; fi; ', &
                    'ENV_CONTENT="$ENV_CONTENT$line"; ', &
                    'done < "$ENV_FILE"; fi; ', &
                    'if [ -n "$ENV_CONTENT" ]; then ', &
                    'TS2=$(date +%s); ', &
                    'SIG2=$(echo -n "$TS2:PUT:/services/$SVC_ID/env:$ENV_CONTENT" | openssl dgst -sha256 -hmac "', trim(secret_key), '" | cut -d" " -f2); ', &
                    'curl -s -X PUT "https://api.unsandbox.com/services/$SVC_ID/env" ', &
                    '-H "Authorization: Bearer ', trim(public_key), '" ', &
                    '-H "X-Timestamp: $TS2" ', &
                    '-H "X-Signature: $SIG2" ', &
                    '-H "Content-Type: text/plain" ', &
                    '--data-binary "$ENV_CONTENT" >/dev/null && ', &
                    'echo -e "\x1b[32mVault configured\x1b[0m"; fi; ', &
                    'else echo "$RESP" | jq .; fi'
            else
                write(full_cmd, '(60A)') &
                    'BODY=''{"name":"', trim(service_name), '"}''; ', &
                    'TS=$(date +%s); ', &
                    'SIG=$(echo -n "$TS:POST:/services:$BODY" | openssl dgst -sha256 -hmac "', trim(secret_key), '" | cut -d" " -f2); ', &
                    'RESP=$(curl -s -X POST https://api.unsandbox.com/services ', &
                    '-H "Content-Type: application/json" ', &
                    '-H "Authorization: Bearer ', trim(public_key), '" ', &
                    '-H "X-Timestamp: $TS" ', &
                    '-H "X-Signature: $SIG" ', &
                    '-d "$BODY"); ', &
                    'SVC_ID=$(echo "$RESP" | jq -r ".id // empty"); ', &
                    'if [ -n "$SVC_ID" ]; then ', &
                    'echo -e "\x1b[32m$SVC_ID created\x1b[0m"; ', &
                    'ENV_CONTENT=""; ', &
                    'ENV_LINES="', trim(svc_envs), '"; ', &
                    'if [ -n "$ENV_LINES" ]; then ENV_CONTENT="$ENV_LINES"; fi; ', &
                    'ENV_FILE="', trim(svc_env_file), '"; ', &
                    'if [ -n "$ENV_FILE" ] && [ -f "$ENV_FILE" ]; then ', &
                    'while IFS= read -r line || [ -n "$line" ]; do ', &
                    'case "$line" in "#"*|"") continue ;; esac; ', &
                    'if [ -n "$ENV_CONTENT" ]; then ENV_CONTENT="$ENV_CONTENT', char(10), '"; fi; ', &
                    'ENV_CONTENT="$ENV_CONTENT$line"; ', &
                    'done < "$ENV_FILE"; fi; ', &
                    'if [ -n "$ENV_CONTENT" ]; then ', &
                    'TS2=$(date +%s); ', &
                    'SIG2=$(echo -n "$TS2:PUT:/services/$SVC_ID/env:$ENV_CONTENT" | openssl dgst -sha256 -hmac "', trim(secret_key), '" | cut -d" " -f2); ', &
                    'curl -s -X PUT "https://api.unsandbox.com/services/$SVC_ID/env" ', &
                    '-H "Authorization: Bearer ', trim(public_key), '" ', &
                    '-H "X-Timestamp: $TS2" ', &
                    '-H "X-Signature: $SIG2" ', &
                    '-H "Content-Type: text/plain" ', &
                    '--data-binary "$ENV_CONTENT" >/dev/null && ', &
                    'echo -e "\x1b[32mVault configured\x1b[0m"; fi; ', &
                    'else echo "$RESP" | jq .; fi'
            end if
            call execute_command_line(trim(full_cmd), wait=.true.)
        else
            write(0, '(A)') 'Error: Use --list, --info, --logs, --freeze, --unfreeze, --destroy, --dump-bootstrap, --name, or env'
            stop 1
        end if
    end subroutine handle_service

    subroutine handle_key()
        character(len=4096) :: full_cmd
        character(len=256) :: arg
        integer :: i, stat
        logical :: extend_mode
        character(len=32) :: portal_base

        portal_base = 'https://unsandbox.com'
        extend_mode = .false.

        ! Check for --extend flag
        do i = 2, command_argument_count()
            call get_command_argument(i, arg)
            if (trim(arg) == '--extend') then
                extend_mode = .true.
            end if
        end do

        ! Get API key
        call get_environment_variable('UNSANDBOX_API_KEY', api_key, status=stat)
        if (stat /= 0 .or. len_trim(api_key) == 0) then
            write(0, '(A)') 'Error: UNSANDBOX_API_KEY not set'
            stop 1
        end if

        if (extend_mode) then
            ! Validate and extend
            write(full_cmd, '(30A)') &
                'resp=$(curl -s -X POST ', trim(portal_base), '/keys/validate ', &
                '-H "Content-Type: application/json" ', &
                '-H "Authorization: Bearer ', trim(api_key), '" ', &
                '-d "{}"); ', &
                'status=$(echo "$resp" | jq -r ".status // empty"); ', &
                'public_key=$(echo "$resp" | jq -r ".public_key // empty"); ', &
                'tier=$(echo "$resp" | jq -r ".tier // empty"); ', &
                'expires_at=$(echo "$resp" | jq -r ".expires_at // empty"); ', &
                'time_remaining=$(echo "$resp" | jq -r ".time_remaining // empty"); ', &
                'rate_limit=$(echo "$resp" | jq -r ".rate_limit // empty"); ', &
                'burst=$(echo "$resp" | jq -r ".burst // empty"); ', &
                'concurrency=$(echo "$resp" | jq -r ".concurrency // empty"); ', &
                'if [ "$status" = "valid" ]; then ', &
                'echo -e "\x1b[32mValid\x1b[0m"; ', &
                'echo "Public Key: $public_key"; ', &
                'echo "Tier: $tier"; ', &
                'echo "Status: $status"; ', &
                'echo "Expires: $expires_at"; ', &
                '[ -n "$time_remaining" ] && echo "Time Remaining: $time_remaining"; ', &
                '[ -n "$rate_limit" ] && echo "Rate Limit: $rate_limit"; ', &
                '[ -n "$burst" ] && echo "Burst: $burst"; ', &
                '[ -n "$concurrency" ] && echo "Concurrency: $concurrency"; ', &
                'echo -e "\x1b[34mOpening browser to extend key...\x1b[0m"; ', &
                'xdg-open "', trim(portal_base), '/keys/extend?pk=$public_key" 2>/dev/null || ', &
                'sensible-browser "', trim(portal_base), '/keys/extend?pk=$public_key" 2>/dev/null &; ', &
                'elif [ "$status" = "expired" ]; then ', &
                'echo -e "\x1b[31mExpired\x1b[0m"; ', &
                'echo "Public Key: $public_key"; ', &
                'echo "Tier: $tier"; ', &
                'echo "Expired: $expires_at"; ', &
                'echo -e "\x1b[33mTo renew: Visit ', trim(portal_base), '/keys/extend\x1b[0m"; ', &
                'echo -e "\x1b[34mOpening browser to extend key...\x1b[0m"; ', &
                'xdg-open "', trim(portal_base), '/keys/extend?pk=$public_key" 2>/dev/null || ', &
                'sensible-browser "', trim(portal_base), '/keys/extend?pk=$public_key" 2>/dev/null &; ', &
                'else echo -e "\x1b[31mInvalid\x1b[0m"; fi'
        else
            ! Validate only
            write(full_cmd, '(30A)') &
                'resp=$(curl -s -X POST ', trim(portal_base), '/keys/validate ', &
                '-H "Content-Type: application/json" ', &
                '-H "Authorization: Bearer ', trim(api_key), '" ', &
                '-d "{}"); ', &
                'status=$(echo "$resp" | jq -r ".status // empty"); ', &
                'public_key=$(echo "$resp" | jq -r ".public_key // empty"); ', &
                'tier=$(echo "$resp" | jq -r ".tier // empty"); ', &
                'expires_at=$(echo "$resp" | jq -r ".expires_at // empty"); ', &
                'time_remaining=$(echo "$resp" | jq -r ".time_remaining // empty"); ', &
                'rate_limit=$(echo "$resp" | jq -r ".rate_limit // empty"); ', &
                'burst=$(echo "$resp" | jq -r ".burst // empty"); ', &
                'concurrency=$(echo "$resp" | jq -r ".concurrency // empty"); ', &
                'if [ "$status" = "valid" ]; then ', &
                'echo -e "\x1b[32mValid\x1b[0m"; ', &
                'echo "Public Key: $public_key"; ', &
                'echo "Tier: $tier"; ', &
                'echo "Status: $status"; ', &
                'echo "Expires: $expires_at"; ', &
                '[ -n "$time_remaining" ] && echo "Time Remaining: $time_remaining"; ', &
                '[ -n "$rate_limit" ] && echo "Rate Limit: $rate_limit"; ', &
                '[ -n "$burst" ] && echo "Burst: $burst"; ', &
                '[ -n "$concurrency" ] && echo "Concurrency: $concurrency"; ', &
                'elif [ "$status" = "expired" ]; then ', &
                'echo -e "\x1b[31mExpired\x1b[0m"; ', &
                'echo "Public Key: $public_key"; ', &
                'echo "Tier: $tier"; ', &
                'echo "Expired: $expires_at"; ', &
                'echo -e "\x1b[33mTo renew: Visit ', trim(portal_base), '/keys/extend\x1b[0m"; ', &
                'else echo -e "\x1b[31mInvalid\x1b[0m"; fi'
        end if

        call execute_command_line(trim(full_cmd), wait=.true., exitstat=stat)
    end subroutine handle_key

end program unsandbox_cli
