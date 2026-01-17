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


!==============================================================================
! unsandbox SDK for Fortran - Execute code in secure sandboxes
! https://unsandbox.com | https://api.unsandbox.com/openapi
!
! Library Usage:
!   use unsandbox_sdk
!
!   type(unsandbox_client) :: client
!   type(execution_result) :: result
!   integer :: status
!
!   ! Initialize client (loads credentials from environment)
!   call client%init(status)
!
!   ! Execute code synchronously
!   call client%execute("python", 'print("Hello")', result, status)
!   print *, trim(result%stdout)
!
!   ! Execute code asynchronously
!   call client%execute_async("python", code, job_id, status)
!   call client%wait(job_id, result, status)
!
! CLI Usage:
!   ./un script.py
!   ./un session [options]
!   ./un service [options]
!   ./un key [--extend]
!
! Authentication (in priority order):
!   1. Environment variables: UNSANDBOX_PUBLIC_KEY + UNSANDBOX_SECRET_KEY
!   2. Config file: ~/.unsandbox/accounts.csv (public_key,secret_key per line)
!   3. Legacy: UNSANDBOX_API_KEY (deprecated)
!
! Compile:
!   gfortran -o un un.f90
!
!==============================================================================

!------------------------------------------------------------------------------
! Module: unsandbox_sdk
! Description: Unsandbox API client library for Fortran
!
! This module provides a type-safe interface to the unsandbox API for
! executing code in secure sandboxes. Due to Fortran's limited HTTP/JSON
! support, this implementation uses shell commands (curl/jq) for API calls.
!
! Types:
!   unsandbox_client   - Main client class with stored credentials
!   execution_result   - Result from code execution
!   job_info           - Information about an async job
!
! Functions:
!   execute            - Execute code synchronously
!   execute_async      - Execute code asynchronously, returns job_id
!   get_job            - Get status of an async job
!   wait               - Wait for async job completion
!   cancel_job         - Cancel a running job
!   list_jobs          - List all active jobs
!   run                - Execute code with shebang auto-detection
!   run_async          - Execute with auto-detection, returns job_id
!   image              - Generate image from text prompt
!   languages          - Get list of supported languages
!
!------------------------------------------------------------------------------
module unsandbox_sdk
    implicit none
    private

    ! Export public types and procedures
    public :: unsandbox_client
    public :: execution_result
    public :: job_info
    public :: get_credentials
    public :: sign_request
    public :: detect_language

    ! API configuration
    character(len=*), parameter, public :: API_BASE = 'https://api.unsandbox.com'
    character(len=*), parameter, public :: PORTAL_BASE = 'https://unsandbox.com'
    integer, parameter, public :: DEFAULT_TTL = 60
    integer, parameter, public :: DEFAULT_TIMEOUT = 300

    !--------------------------------------------------------------------------
    ! Type: execution_result
    ! Description: Result from code execution
    !
    ! Fields:
    !   success    - Whether execution succeeded
    !   stdout     - Standard output from execution
    !   stderr     - Standard error from execution
    !   exit_code  - Exit code from execution
    !   job_id     - Job ID for async execution
    !   language   - Detected or specified language
    !   time_ms    - Execution time in milliseconds
    !--------------------------------------------------------------------------
    type :: execution_result
        logical :: success = .false.
        character(len=65536) :: stdout = ''
        character(len=65536) :: stderr = ''
        integer :: exit_code = 0
        character(len=256) :: job_id = ''
        character(len=64) :: language = ''
        integer :: time_ms = 0
    end type execution_result

    !--------------------------------------------------------------------------
    ! Type: job_info
    ! Description: Information about an async job
    !
    ! Fields:
    !   job_id     - Unique job identifier
    !   status     - Job status (pending, running, completed, failed, timeout, cancelled)
    !   language   - Programming language
    !   submitted  - Submission timestamp
    !--------------------------------------------------------------------------
    type :: job_info
        character(len=256) :: job_id = ''
        character(len=32) :: status = ''
        character(len=64) :: language = ''
        character(len=64) :: submitted = ''
    end type job_info

    !--------------------------------------------------------------------------
    ! Type: unsandbox_client
    ! Description: API client with stored credentials
    !
    ! Use the client class when making multiple API calls to avoid
    ! repeated credential resolution.
    !
    ! Example:
    !   type(unsandbox_client) :: client
    !   call client%init(status)
    !   call client%execute("python", code, result, status)
    !--------------------------------------------------------------------------
    type :: unsandbox_client
        character(len=256) :: public_key = ''
        character(len=256) :: secret_key = ''
        logical :: initialized = .false.
    contains
        procedure :: init => client_init
        procedure :: execute => client_execute
        procedure :: execute_async => client_execute_async
        procedure :: get_job => client_get_job
        procedure :: wait => client_wait
        procedure :: cancel_job => client_cancel_job
        procedure :: list_jobs => client_list_jobs
        procedure :: run => client_run
        procedure :: run_async => client_run_async
        procedure :: image => client_image
        procedure :: languages => client_languages
    end type unsandbox_client

contains

    !--------------------------------------------------------------------------
    ! Subroutine: get_credentials
    ! Description: Get API credentials from environment or config file
    !
    ! Priority order:
    !   1. Environment variables (UNSANDBOX_PUBLIC_KEY, UNSANDBOX_SECRET_KEY)
    !   2. Config file (~/.unsandbox/accounts.csv)
    !   3. Legacy UNSANDBOX_API_KEY (deprecated)
    !
    ! Arguments:
    !   public_key  - Output: API public key
    !   secret_key  - Output: API secret key
    !   status      - Output: 0 on success, non-zero on error
    !--------------------------------------------------------------------------
    subroutine get_credentials(public_key, secret_key, status)
        character(len=*), intent(out) :: public_key, secret_key
        integer, intent(out) :: status
        character(len=1024) :: home_dir, accounts_path, line, api_key
        integer :: unit_num, ios
        logical :: file_exists

        status = 0
        public_key = ''
        secret_key = ''

        ! Priority 1: Environment variables
        call get_environment_variable('UNSANDBOX_PUBLIC_KEY', public_key, status=ios)
        if (ios == 0 .and. len_trim(public_key) > 0) then
            call get_environment_variable('UNSANDBOX_SECRET_KEY', secret_key, status=ios)
            if (ios == 0 .and. len_trim(secret_key) > 0) then
                return
            end if
        end if

        ! Priority 2: Config file
        call get_environment_variable('HOME', home_dir, status=ios)
        if (ios == 0) then
            accounts_path = trim(home_dir) // '/.unsandbox/accounts.csv'
            inquire(file=trim(accounts_path), exist=file_exists)
            if (file_exists) then
                open(newunit=unit_num, file=trim(accounts_path), status='old', &
                     action='read', iostat=ios)
                if (ios == 0) then
                    do
                        read(unit_num, '(A)', iostat=ios) line
                        if (ios /= 0) exit
                        line = adjustl(line)
                        if (len_trim(line) == 0) cycle
                        if (line(1:1) == '#') cycle
                        ! Parse CSV: public_key,secret_key
                        call parse_csv_line(line, public_key, secret_key)
                        if (len_trim(public_key) > 0 .and. len_trim(secret_key) > 0) then
                            if (public_key(1:8) == 'unsb-pk-' .and. &
                                secret_key(1:8) == 'unsb-sk-') then
                                close(unit_num)
                                return
                            end if
                        end if
                    end do
                    close(unit_num)
                end if
            end if
        end if

        ! Priority 3: Legacy API key
        call get_environment_variable('UNSANDBOX_API_KEY', api_key, status=ios)
        if (ios == 0 .and. len_trim(api_key) > 0) then
            public_key = api_key
            secret_key = api_key
            return
        end if

        ! No credentials found
        status = 1
    end subroutine get_credentials

    !--------------------------------------------------------------------------
    ! Subroutine: parse_csv_line
    ! Description: Parse a CSV line into two fields
    !--------------------------------------------------------------------------
    subroutine parse_csv_line(line, field1, field2)
        character(len=*), intent(in) :: line
        character(len=*), intent(out) :: field1, field2
        integer :: comma_pos

        field1 = ''
        field2 = ''
        comma_pos = index(line, ',')
        if (comma_pos > 0) then
            field1 = line(1:comma_pos-1)
            field2 = line(comma_pos+1:)
        end if
    end subroutine parse_csv_line

    !--------------------------------------------------------------------------
    ! Subroutine: sign_request
    ! Description: Generate HMAC-SHA256 signature for API request
    !
    ! Signature format: HMAC-SHA256(secret_key, "timestamp:METHOD:path:body")
    !
    ! Note: Uses openssl via shell command due to Fortran limitations
    !
    ! Arguments:
    !   secret_key  - API secret key
    !   timestamp   - Unix timestamp as string
    !   method      - HTTP method (GET, POST, etc.)
    !   path        - API endpoint path
    !   body        - Request body (empty string if none)
    !   signature   - Output: Hex-encoded signature
    !--------------------------------------------------------------------------
    subroutine sign_request(secret_key, timestamp, method, path, body, signature)
        character(len=*), intent(in) :: secret_key, timestamp, method, path, body
        character(len=*), intent(out) :: signature
        character(len=4096) :: cmd
        integer :: ios

        ! Use shell to compute HMAC (Fortran lacks native crypto)
        write(cmd, '(10A)') &
            'echo -n "', trim(timestamp), ':', trim(method), ':', trim(path), ':', trim(body), &
            '" | openssl dgst -sha256 -hmac "', trim(secret_key), '" | cut -d" " -f2'

        ! This would need to capture output - simplified for module use
        signature = ''
    end subroutine sign_request

    !--------------------------------------------------------------------------
    ! Subroutine: detect_language
    ! Description: Detect programming language from file extension
    !
    ! Arguments:
    !   filename  - File path
    !   language  - Output: Detected language name
    !   status    - Output: 0 on success, 1 if unknown
    !--------------------------------------------------------------------------
    subroutine detect_language(filename, language, status)
        character(len=*), intent(in) :: filename
        character(len=*), intent(out) :: language
        integer, intent(out) :: status
        integer :: dot_pos
        character(len=16) :: ext

        status = 0
        language = 'unknown'

        dot_pos = index(trim(filename), '.', back=.true.)
        if (dot_pos == 0) then
            status = 1
            return
        end if

        ext = filename(dot_pos:)

        ! Extension mapping
        select case (trim(ext))
            case ('.py')
                language = 'python'
            case ('.js')
                language = 'javascript'
            case ('.ts')
                language = 'typescript'
            case ('.rb')
                language = 'ruby'
            case ('.go')
                language = 'go'
            case ('.rs')
                language = 'rust'
            case ('.c')
                language = 'c'
            case ('.cpp', '.cc', '.cxx')
                language = 'cpp'
            case ('.java')
                language = 'java'
            case ('.kt')
                language = 'kotlin'
            case ('.cs')
                language = 'csharp'
            case ('.fs')
                language = 'fsharp'
            case ('.sh')
                language = 'bash'
            case ('.pl')
                language = 'perl'
            case ('.lua')
                language = 'lua'
            case ('.php')
                language = 'php'
            case ('.hs')
                language = 'haskell'
            case ('.ml')
                language = 'ocaml'
            case ('.clj')
                language = 'clojure'
            case ('.scm')
                language = 'scheme'
            case ('.lisp')
                language = 'commonlisp'
            case ('.erl')
                language = 'erlang'
            case ('.ex', '.exs')
                language = 'elixir'
            case ('.jl')
                language = 'julia'
            case ('.r', '.R')
                language = 'r'
            case ('.cr')
                language = 'crystal'
            case ('.f90', '.f95')
                language = 'fortran'
            case ('.cob')
                language = 'cobol'
            case ('.pro')
                language = 'prolog'
            case ('.forth', '.4th')
                language = 'forth'
            case ('.tcl')
                language = 'tcl'
            case ('.raku')
                language = 'raku'
            case ('.d')
                language = 'd'
            case ('.nim')
                language = 'nim'
            case ('.zig')
                language = 'zig'
            case ('.v')
                language = 'v'
            case ('.groovy')
                language = 'groovy'
            case ('.scala')
                language = 'scala'
            case ('.dart')
                language = 'dart'
            case ('.awk')
                language = 'awk'
            case ('.m')
                language = 'objc'
            case default
                status = 1
        end select
    end subroutine detect_language

    !--------------------------------------------------------------------------
    ! Client methods
    !--------------------------------------------------------------------------

    !--------------------------------------------------------------------------
    ! Subroutine: client_init
    ! Description: Initialize client with credentials
    !
    ! Loads credentials from environment variables or config file.
    !
    ! Arguments:
    !   self    - Client instance
    !   status  - Output: 0 on success, non-zero on error
    !--------------------------------------------------------------------------
    subroutine client_init(self, status)
        class(unsandbox_client), intent(inout) :: self
        integer, intent(out) :: status

        call get_credentials(self%public_key, self%secret_key, status)
        if (status == 0) then
            self%initialized = .true.
        end if
    end subroutine client_init

    !--------------------------------------------------------------------------
    ! Subroutine: client_execute
    ! Description: Execute code synchronously and return results
    !
    ! Arguments:
    !   self        - Client instance
    !   language    - Programming language (python, javascript, etc.)
    !   code        - Source code to execute
    !   result      - Output: Execution result
    !   status      - Output: 0 on success, non-zero on error
    !   network     - Optional: Network mode (zerotrust/semitrusted)
    !   ttl         - Optional: Timeout in seconds
    !   vcpu        - Optional: vCPU count (1-8)
    !--------------------------------------------------------------------------
    subroutine client_execute(self, language, code, result, status, network, ttl, vcpu)
        class(unsandbox_client), intent(in) :: self
        character(len=*), intent(in) :: language, code
        type(execution_result), intent(out) :: result
        integer, intent(out) :: status
        character(len=*), intent(in), optional :: network
        integer, intent(in), optional :: ttl, vcpu
        character(len=16384) :: cmd
        character(len=32) :: net_mode
        integer :: exec_ttl, exec_vcpu

        status = 0
        net_mode = 'zerotrust'
        exec_ttl = DEFAULT_TTL
        exec_vcpu = 1

        if (present(network)) net_mode = network
        if (present(ttl)) exec_ttl = ttl
        if (present(vcpu)) exec_vcpu = vcpu

        if (.not. self%initialized) then
            status = 1
            result%stderr = 'Client not initialized'
            return
        end if

        ! Build and execute shell command with HMAC auth
        write(cmd, '(30A)') &
            'TMPFILE=$(mktemp); ', &
            'cat > "$TMPFILE" << ''CODEEOF''', char(10), trim(code), char(10), 'CODEEOF', char(10), &
            'BODY=$(jq -Rs ''{language: "', trim(language), '", code: ., ', &
            'network_mode: "', trim(net_mode), '", ttl: ', char(48+mod(exec_ttl/10,10)), char(48+mod(exec_ttl,10)), &
            '}'' < "$TMPFILE"); ', &
            'TS=$(date +%s); ', &
            'SIG=$(echo -n "$TS:POST:/execute:$BODY" | openssl dgst -sha256 -hmac "', &
            trim(self%secret_key), '" | cut -d" " -f2); ', &
            'RESP=$(curl -s -X POST ', API_BASE, '/execute ', &
            '-H "Content-Type: application/json" ', &
            '-H "Authorization: Bearer ', trim(self%public_key), '" ', &
            '-H "X-Timestamp: $TS" ', &
            '-H "X-Signature: $SIG" ', &
            '--data-binary "$BODY"); ', &
            'rm -f "$TMPFILE"; ', &
            'echo "$RESP" | jq -r ".stdout // empty"; ', &
            'echo "$RESP" | jq -r ".stderr // empty" >&2; ', &
            'echo "$RESP" | jq -r ".exit_code // 0"'

        call execute_command_line(trim(cmd), wait=.true., exitstat=status)
        result%success = (status == 0)
        result%language = language
    end subroutine client_execute

    !--------------------------------------------------------------------------
    ! Subroutine: client_execute_async
    ! Description: Execute code asynchronously, returns job_id for polling
    !
    ! Arguments:
    !   self     - Client instance
    !   language - Programming language
    !   code     - Source code to execute
    !   job_id   - Output: Job ID for polling
    !   status   - Output: 0 on success, non-zero on error
    !--------------------------------------------------------------------------
    subroutine client_execute_async(self, language, code, job_id, status)
        class(unsandbox_client), intent(in) :: self
        character(len=*), intent(in) :: language, code
        character(len=*), intent(out) :: job_id
        integer, intent(out) :: status
        character(len=8192) :: cmd

        status = 0
        job_id = ''

        if (.not. self%initialized) then
            status = 1
            return
        end if

        write(cmd, '(20A)') &
            'TMPFILE=$(mktemp); ', &
            'cat > "$TMPFILE" << ''CODEEOF''', char(10), trim(code), char(10), 'CODEEOF', char(10), &
            'BODY=$(jq -Rs ''{language: "', trim(language), '", code: .}'' < "$TMPFILE"); ', &
            'TS=$(date +%s); ', &
            'SIG=$(echo -n "$TS:POST:/execute/async:$BODY" | openssl dgst -sha256 -hmac "', &
            trim(self%secret_key), '" | cut -d" " -f2); ', &
            'curl -s -X POST ', API_BASE, '/execute/async ', &
            '-H "Content-Type: application/json" ', &
            '-H "Authorization: Bearer ', trim(self%public_key), '" ', &
            '-H "X-Timestamp: $TS" ', &
            '-H "X-Signature: $SIG" ', &
            '--data-binary "$BODY" | jq -r ".job_id // empty"; ', &
            'rm -f "$TMPFILE"'

        call execute_command_line(trim(cmd), wait=.true., exitstat=status)
    end subroutine client_execute_async

    !--------------------------------------------------------------------------
    ! Subroutine: client_get_job
    ! Description: Get status and results of an async job
    !
    ! Arguments:
    !   self    - Client instance
    !   job_id  - Job ID from execute_async
    !   info    - Output: Job information
    !   status  - Output: 0 on success, non-zero on error
    !--------------------------------------------------------------------------
    subroutine client_get_job(self, job_id, info, status)
        class(unsandbox_client), intent(in) :: self
        character(len=*), intent(in) :: job_id
        type(job_info), intent(out) :: info
        integer, intent(out) :: status
        character(len=4096) :: cmd

        status = 0
        info%job_id = job_id

        write(cmd, '(15A)') &
            'TS=$(date +%s); ', &
            'SIG=$(echo -n "$TS:GET:/jobs/', trim(job_id), ':" | openssl dgst -sha256 -hmac "', &
            trim(self%secret_key), '" | cut -d" " -f2); ', &
            'curl -s -X GET ', API_BASE, '/jobs/', trim(job_id), ' ', &
            '-H "Authorization: Bearer ', trim(self%public_key), '" ', &
            '-H "X-Timestamp: $TS" ', &
            '-H "X-Signature: $SIG" | jq .'

        call execute_command_line(trim(cmd), wait=.true., exitstat=status)
    end subroutine client_get_job

    !--------------------------------------------------------------------------
    ! Subroutine: client_wait
    ! Description: Wait for async job completion with polling
    !
    ! Arguments:
    !   self      - Client instance
    !   job_id    - Job ID from execute_async
    !   result    - Output: Execution result
    !   status    - Output: 0 on success, non-zero on error
    !   max_polls - Optional: Maximum poll attempts (default 100)
    !--------------------------------------------------------------------------
    subroutine client_wait(self, job_id, result, status, max_polls)
        class(unsandbox_client), intent(in) :: self
        character(len=*), intent(in) :: job_id
        type(execution_result), intent(out) :: result
        integer, intent(out) :: status
        integer, intent(in), optional :: max_polls
        character(len=8192) :: cmd
        integer :: polls

        polls = 100
        if (present(max_polls)) polls = max_polls

        status = 0

        ! Use shell loop for polling with exponential backoff
        write(cmd, '(30A,I0,A)') &
            'DELAYS=(300 450 700 900 650 1600 2000); ', &
            'for i in $(seq 1 ', polls, '); do ', &
            'sleep $(echo "scale=3; ${DELAYS[$(( (i-1) % 7 ))]}/1000" | bc); ', &
            'TS=$(date +%s); ', &
            'SIG=$(echo -n "$TS:GET:/jobs/', trim(job_id), ':" | openssl dgst -sha256 -hmac "', &
            trim(self%secret_key), '" | cut -d" " -f2); ', &
            'RESP=$(curl -s -X GET ', API_BASE, '/jobs/', trim(job_id), ' ', &
            '-H "Authorization: Bearer ', trim(self%public_key), '" ', &
            '-H "X-Timestamp: $TS" ', &
            '-H "X-Signature: $SIG"); ', &
            'STATUS=$(echo "$RESP" | jq -r ".status // empty"); ', &
            'case "$STATUS" in ', &
            'completed|failed|timeout|cancelled) ', &
            'echo "$RESP" | jq -r ".stdout // empty"; ', &
            'echo "$RESP" | jq -r ".stderr // empty" >&2; ', &
            'exit 0;; ', &
            'esac; ', &
            'done; ', &
            'echo "Timeout waiting for job" >&2; exit 1'

        call execute_command_line(trim(cmd), wait=.true., exitstat=status)
        result%success = (status == 0)
        result%job_id = job_id
    end subroutine client_wait

    !--------------------------------------------------------------------------
    ! Subroutine: client_cancel_job
    ! Description: Cancel a running job
    !
    ! Arguments:
    !   self    - Client instance
    !   job_id  - Job ID to cancel
    !   status  - Output: 0 on success, non-zero on error
    !--------------------------------------------------------------------------
    subroutine client_cancel_job(self, job_id, status)
        class(unsandbox_client), intent(in) :: self
        character(len=*), intent(in) :: job_id
        integer, intent(out) :: status
        character(len=4096) :: cmd

        write(cmd, '(15A)') &
            'TS=$(date +%s); ', &
            'SIG=$(echo -n "$TS:DELETE:/jobs/', trim(job_id), ':" | openssl dgst -sha256 -hmac "', &
            trim(self%secret_key), '" | cut -d" " -f2); ', &
            'curl -s -X DELETE ', API_BASE, '/jobs/', trim(job_id), ' ', &
            '-H "Authorization: Bearer ', trim(self%public_key), '" ', &
            '-H "X-Timestamp: $TS" ', &
            '-H "X-Signature: $SIG" | jq .'

        call execute_command_line(trim(cmd), wait=.true., exitstat=status)
    end subroutine client_cancel_job

    !--------------------------------------------------------------------------
    ! Subroutine: client_list_jobs
    ! Description: List all active jobs for this API key
    !
    ! Arguments:
    !   self    - Client instance
    !   status  - Output: 0 on success, non-zero on error
    !--------------------------------------------------------------------------
    subroutine client_list_jobs(self, status)
        class(unsandbox_client), intent(in) :: self
        integer, intent(out) :: status
        character(len=4096) :: cmd

        write(cmd, '(15A)') &
            'TS=$(date +%s); ', &
            'SIG=$(echo -n "$TS:GET:/jobs:" | openssl dgst -sha256 -hmac "', &
            trim(self%secret_key), '" | cut -d" " -f2); ', &
            'curl -s -X GET ', API_BASE, '/jobs ', &
            '-H "Authorization: Bearer ', trim(self%public_key), '" ', &
            '-H "X-Timestamp: $TS" ', &
            '-H "X-Signature: $SIG" | jq .'

        call execute_command_line(trim(cmd), wait=.true., exitstat=status)
    end subroutine client_list_jobs

    !--------------------------------------------------------------------------
    ! Subroutine: client_run
    ! Description: Execute code with automatic language detection from shebang
    !
    ! Arguments:
    !   self    - Client instance
    !   code    - Source code with shebang (e.g., #!/usr/bin/env python3)
    !   result  - Output: Execution result
    !   status  - Output: 0 on success, non-zero on error
    !--------------------------------------------------------------------------
    subroutine client_run(self, code, result, status)
        class(unsandbox_client), intent(in) :: self
        character(len=*), intent(in) :: code
        type(execution_result), intent(out) :: result
        integer, intent(out) :: status
        character(len=8192) :: cmd

        status = 0

        write(cmd, '(20A)') &
            'TMPFILE=$(mktemp); ', &
            'cat > "$TMPFILE" << ''CODEEOF''', char(10), trim(code), char(10), 'CODEEOF', char(10), &
            'BODY=$(cat "$TMPFILE"); ', &
            'TS=$(date +%s); ', &
            'SIG=$(echo -n "$TS:POST:/run:$BODY" | openssl dgst -sha256 -hmac "', &
            trim(self%secret_key), '" | cut -d" " -f2); ', &
            'curl -s -X POST ', API_BASE, '/run ', &
            '-H "Content-Type: text/plain" ', &
            '-H "Authorization: Bearer ', trim(self%public_key), '" ', &
            '-H "X-Timestamp: $TS" ', &
            '-H "X-Signature: $SIG" ', &
            '--data-binary "@$TMPFILE" | jq .; ', &
            'rm -f "$TMPFILE"'

        call execute_command_line(trim(cmd), wait=.true., exitstat=status)
        result%success = (status == 0)
    end subroutine client_run

    !--------------------------------------------------------------------------
    ! Subroutine: client_run_async
    ! Description: Execute with auto-detection asynchronously
    !
    ! Arguments:
    !   self    - Client instance
    !   code    - Source code with shebang
    !   job_id  - Output: Job ID for polling
    !   status  - Output: 0 on success, non-zero on error
    !--------------------------------------------------------------------------
    subroutine client_run_async(self, code, job_id, status)
        class(unsandbox_client), intent(in) :: self
        character(len=*), intent(in) :: code
        character(len=*), intent(out) :: job_id
        integer, intent(out) :: status
        character(len=8192) :: cmd

        status = 0
        job_id = ''

        write(cmd, '(20A)') &
            'TMPFILE=$(mktemp); ', &
            'cat > "$TMPFILE" << ''CODEEOF''', char(10), trim(code), char(10), 'CODEEOF', char(10), &
            'BODY=$(cat "$TMPFILE"); ', &
            'TS=$(date +%s); ', &
            'SIG=$(echo -n "$TS:POST:/run/async:$BODY" | openssl dgst -sha256 -hmac "', &
            trim(self%secret_key), '" | cut -d" " -f2); ', &
            'curl -s -X POST ', API_BASE, '/run/async ', &
            '-H "Content-Type: text/plain" ', &
            '-H "Authorization: Bearer ', trim(self%public_key), '" ', &
            '-H "X-Timestamp: $TS" ', &
            '-H "X-Signature: $SIG" ', &
            '--data-binary "@$TMPFILE" | jq -r ".job_id // empty"; ', &
            'rm -f "$TMPFILE"'

        call execute_command_line(trim(cmd), wait=.true., exitstat=status)
    end subroutine client_run_async

    !--------------------------------------------------------------------------
    ! Subroutine: client_image
    ! Description: Generate image from text prompt
    !
    ! Arguments:
    !   self    - Client instance
    !   prompt  - Text description of image to generate
    !   status  - Output: 0 on success, non-zero on error
    !--------------------------------------------------------------------------
    subroutine client_image(self, prompt, status)
        class(unsandbox_client), intent(in) :: self
        character(len=*), intent(in) :: prompt
        integer, intent(out) :: status
        character(len=8192) :: cmd

        write(cmd, '(15A)') &
            'BODY=''{"prompt":"', trim(prompt), '","size":"1024x1024"}''; ', &
            'TS=$(date +%s); ', &
            'SIG=$(echo -n "$TS:POST:/image:$BODY" | openssl dgst -sha256 -hmac "', &
            trim(self%secret_key), '" | cut -d" " -f2); ', &
            'curl -s -X POST ', API_BASE, '/image ', &
            '-H "Content-Type: application/json" ', &
            '-H "Authorization: Bearer ', trim(self%public_key), '" ', &
            '-H "X-Timestamp: $TS" ', &
            '-H "X-Signature: $SIG" ', &
            '-d "$BODY" | jq .'

        call execute_command_line(trim(cmd), wait=.true., exitstat=status)
    end subroutine client_image

    !--------------------------------------------------------------------------
    ! Subroutine: client_languages
    ! Description: Get list of supported programming languages
    !
    ! Arguments:
    !   self    - Client instance
    !   status  - Output: 0 on success, non-zero on error
    !--------------------------------------------------------------------------
    subroutine client_languages(self, status)
        class(unsandbox_client), intent(in) :: self
        integer, intent(out) :: status
        character(len=4096) :: cmd

        write(cmd, '(15A)') &
            'TS=$(date +%s); ', &
            'SIG=$(echo -n "$TS:GET:/languages:" | openssl dgst -sha256 -hmac "', &
            trim(self%secret_key), '" | cut -d" " -f2); ', &
            'curl -s -X GET ', API_BASE, '/languages ', &
            '-H "Authorization: Bearer ', trim(self%public_key), '" ', &
            '-H "X-Timestamp: $TS" ', &
            '-H "X-Signature: $SIG" | jq .'

        call execute_command_line(trim(cmd), wait=.true., exitstat=status)
    end subroutine client_languages

end module unsandbox_sdk


!==============================================================================
! Main Program: unsandbox_cli
! Description: CLI interface for unsandbox API
!
! This is the command-line interface that uses the unsandbox_sdk module.
! Run without arguments for usage information.
!==============================================================================
program unsandbox_cli
    use unsandbox_sdk
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
        call print_help()
        stop 1
    end if

    ! Check for subcommands
    call get_command_argument(1, arg, status=stat)
    if (trim(arg) == '-h' .or. trim(arg) == '--help') then
        call print_help()
        stop 0
    else if (trim(arg) == 'session') then
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

    subroutine print_help()
        write(*, '(A)') 'unsandbox SDK for Fortran - Execute code in secure sandboxes'
        write(*, '(A)') 'https://unsandbox.com | https://api.unsandbox.com/openapi'
        write(*, '(A)') ''
        write(*, '(A)') 'Usage: ./un [options] <source_file>'
        write(*, '(A)') '       ./un session [options]'
        write(*, '(A)') '       ./un service [options]'
        write(*, '(A)') '       ./un key [--extend]'
        write(*, '(A)') ''
        write(*, '(A)') 'Execute options:'
        write(*, '(A)') '  -e KEY=VALUE    Set environment variable'
        write(*, '(A)') '  -f FILE         Add input file'
        write(*, '(A)') '  -n MODE         Network mode (zerotrust/semitrusted)'
        write(*, '(A)') '  -v N            vCPU count (1-8)'
        write(*, '(A)') ''
        write(*, '(A)') 'Session options:'
        write(*, '(A)') '  -l, --list      List active sessions'
        write(*, '(A)') '  --kill ID       Terminate session'
        write(*, '(A)') ''
        write(*, '(A)') 'Service options:'
        write(*, '(A)') '  -l, --list      List services'
        write(*, '(A)') '  --name NAME     Service name (creates service)'
        write(*, '(A)') '  --info ID       Get service details'
        write(*, '(A)') '  --logs ID       Get service logs'
        write(*, '(A)') '  --freeze ID     Freeze service'
        write(*, '(A)') '  --unfreeze ID   Unfreeze service'
        write(*, '(A)') '  --destroy ID    Destroy service'
        write(*, '(A)') '  --resize ID     Resize service (with -v N)'
        write(*, '(A)') ''
        write(*, '(A)') 'Vault commands:'
        write(*, '(A)') '  service env status <id>   Check vault status'
        write(*, '(A)') '  service env set <id>      Set vault (-e KEY=VAL)'
        write(*, '(A)') '  service env export <id>   Export vault'
        write(*, '(A)') '  service env delete <id>   Delete vault'
        write(*, '(A)') ''
        write(*, '(A)') 'Key options:'
        write(*, '(A)') '  --extend        Open browser to extend key'
        write(*, '(A)') ''
        write(*, '(A)') 'Library Usage:'
        write(*, '(A)') '  use unsandbox_sdk'
        write(*, '(A)') '  type(unsandbox_client) :: client'
        write(*, '(A)') '  call client%init(status)'
        write(*, '(A)') '  call client%execute("python", code, result, status)'
    end subroutine print_help

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
        call detect_language(fname, language, stat)
        if (stat /= 0) then
            write(0, '(A,A)') 'Error: Unknown language for file: ', trim(fname)
            stop 1
        end if

        ! Get API keys
        call get_credentials(public_key, secret_key, stat)
        if (stat /= 0) then
            write(0, '(A)') 'Error: No credentials found. Set UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY'
            stop 1
        end if

        ! Build curl command with HMAC auth
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
                        write(0, '(A)') 'Usage: ./un session [options]'
                        stop 1
                    end if
                end if
            end if
        end do

        ! Get API keys
        call get_credentials(public_key, secret_key, stat)
        if (stat /= 0) then
            write(0, '(A)') 'Error: No credentials found'
            stop 1
        end if

        if (list_mode) then
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
            else if (trim(arg) == '--vcpu' .or. trim(arg) == '-v') then
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

        ! Get API keys
        call get_credentials(public_key, secret_key, stat)
        if (stat /= 0) then
            write(0, '(A)') 'Error: No credentials found'
            stop 1
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
                write(0, '(A)') 'Usage: ./un service env <status|set|export|delete> <service_id>'
                stop 1
            end if
        end if

        if (list_mode) then
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
            call execute_command_line(trim(full_cmd), wait=.true.)
        else
            write(0, '(A)') 'Error: Use --list, --info, --logs, --freeze, --unfreeze, --destroy, --dump-bootstrap, --name, or env'
            stop 1
        end if
    end subroutine handle_service

    subroutine handle_key()
        character(len=4096) :: full_cmd
        character(len=256) :: arg
        character(len=1024) :: public_key, secret_key
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
        call get_credentials(public_key, secret_key, stat)
        if (stat /= 0) then
            write(0, '(A)') 'Error: No credentials found'
            stop 1
        end if

        if (extend_mode) then
            write(full_cmd, '(30A)') &
                'resp=$(curl -s -X POST ', trim(portal_base), '/keys/validate ', &
                '-H "Content-Type: application/json" ', &
                '-H "Authorization: Bearer ', trim(public_key), '" ', &
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
            write(full_cmd, '(30A)') &
                'resp=$(curl -s -X POST ', trim(portal_base), '/keys/validate ', &
                '-H "Content-Type: application/json" ', &
                '-H "Authorization: Bearer ', trim(public_key), '" ', &
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
