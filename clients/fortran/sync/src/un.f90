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
!   1. --account N flag -> accounts.csv row N (bypasses env vars)
!   2. Environment variables: UNSANDBOX_PUBLIC_KEY + UNSANDBOX_SECRET_KEY
!   3. Config file: ~/.unsandbox/accounts.csv row 0 (or UNSANDBOX_ACCOUNT)
!   4. ./accounts.csv row 0
!   5. Legacy: UNSANDBOX_API_KEY (deprecated)
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
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char
    implicit none
    private

    ! Export public types and procedures
    public :: unsandbox_client
    public :: execution_result
    public :: job_info
    public :: get_credentials
    public :: sign_request
    public :: detect_language
    public :: api_base_url
    public :: set_env
    public :: normalize_os

    ! API configuration
    ! Default API base; override with UNSANDBOX_API_URL (see api_base_url())
    character(len=*), parameter, public :: API_BASE = 'https://api.unsandbox.com'

    ! Request signing helper (POSIX sh). Every request command pipes
    ! "ts:METHOD:path:body" into `sh -c "$UNSB_HMAC"`, which prints the hex
    ! HMAC-SHA256 keyed by $UNSB_SK. Key and helper reach the shell through the
    ! environment (export_signing_env), never a command line: ps shows argv to
    ! every user, the environment only to the owner. HMAC from plain SHA-256:
    ! sha256((K^opad) || sha256((K^ipad) || msg)), K zero-padded to 64 bytes.
    character(len=*), parameter :: HMAC_SH = &
        'S=sha256sum; command -v sha256sum >/dev/null 2>&1 || S="openssl dgst -sha256 -r"; ' // &
        'if [ ${#UNSB_SK} -gt 64 ]; then kb=$(printf %s "$UNSB_SK" | $S | cut -c1-64 | sed "s/../0x& /g"); ' // &
        'else kb=$(printf %s "$UNSB_SK" | od -An -v -tu1); fi; ip=; op=; n=0; ' // &
        'for c in $kb; do v=$((c^54)); ip="$ip\\$((v>>6))$((v>>3&7))$((v&7))"; ' // &
        'v=$((c^92)); op="$op\\$((v>>6))$((v>>3&7))$((v&7))"; n=$((n+1)); done; ' // &
        'while [ $n -lt 64 ]; do ip="$ip\\066"; op="$op\\134"; n=$((n+1)); done; ' // &
        'ih=$( { printf "$ip"; cat; } | $S | cut -c1-64 | sed "s/../0x& /g"); ' // &
        'x=; for c in $ih; do x="$x\\$((c>>6))$((c>>3&7))$((c&7))"; done; ' // &
        '{ printf "$op"; printf "$x"; } | $S | cut -c1-64'
    character(len=*), parameter, public :: PORTAL_BASE = 'https://unsandbox.com'
    integer, parameter, public :: DEFAULT_TTL = 60
    integer, parameter, public :: DEFAULT_TIMEOUT = 300
    integer, parameter, public :: LANGUAGES_CACHE_TTL = 3600  ! 1 hour cache TTL

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

    interface
        function c_setenv(name, value, overwrite) bind(C, name='setenv') result(rc)
            import :: c_char, c_int
            character(kind=c_char), dimension(*), intent(in) :: name, value
            integer(c_int), value :: overwrite
            integer(c_int) :: rc
        end function c_setenv
    end interface

contains

    !--------------------------------------------------------------------------
    ! Function: api_base_url
    ! Description: API base URL from UNSANDBOX_API_URL (default API_BASE),
    !              trailing slashes stripped. Used for every API request.
    !--------------------------------------------------------------------------
    function api_base_url() result(base)
        character(len=1024) :: base
        integer :: ios, n

        call get_environment_variable('UNSANDBOX_API_URL', base, status=ios)
        if (ios /= 0 .or. len_trim(base) == 0) base = API_BASE
        n = len_trim(base)
        do while (n > 1 .and. base(n:n) == '/')
            base(n:n) = ' '
            n = n - 1
        end do
    end function api_base_url

    !--------------------------------------------------------------------------
    ! Subroutine: set_env
    ! Description: Export a variable to child shells. Secrets travel to helper
    !              shells this way, never on a command line (ps sees those).
    !--------------------------------------------------------------------------
    subroutine set_env(name, value)
        character(len=*), intent(in) :: name, value
        integer(c_int) :: rc

        rc = c_setenv(trim(name) // c_null_char, value // c_null_char, 1_c_int)
    end subroutine set_env

    !--------------------------------------------------------------------------
    ! Subroutine: normalize_os
    ! Description: Validate a target OS (linux or freebsd, any case) and
    !              return it lowercased. status = 1 for any other value,
    !              including an empty string. Sent as "os" only when given.
    !--------------------------------------------------------------------------
    subroutine normalize_os(value, normalized, status)
        character(len=*), intent(in) :: value
        character(len=*), intent(out) :: normalized
        integer, intent(out) :: status
        character(len=len(value)) :: low
        integer :: k, c

        low = value
        do k = 1, len(low)
            c = iachar(low(k:k))
            if (c >= 65 .and. c <= 90) low(k:k) = achar(c + 32)
        end do
        normalized = ''
        status = 1
        if (low == 'linux' .or. low == 'freebsd') then
            normalized = trim(low)
            status = 0
        end if
    end subroutine normalize_os

    !--------------------------------------------------------------------------
    ! Subroutine: load_csv_row
    ! Description: Load public_key,secret_key from a CSV file at row_index
    !              (0-based, skipping blank lines and '#' comments).
    !
    ! Arguments:
    !   csv_path   - Path to CSV file
    !   row_index  - Zero-based data row to read
    !   public_key - Output: public key (empty if not found)
    !   secret_key - Output: secret key (empty if not found)
    !--------------------------------------------------------------------------
    subroutine load_csv_row(csv_path, row_index, public_key, secret_key)
        character(len=*), intent(in)  :: csv_path
        integer,          intent(in)  :: row_index
        character(len=*), intent(out) :: public_key, secret_key
        character(len=1024) :: line
        integer :: unit_num, ios, data_index
        logical :: file_exists

        public_key = ''
        secret_key = ''
        data_index = 0

        inquire(file=trim(csv_path), exist=file_exists)
        if (.not. file_exists) return

        open(newunit=unit_num, file=trim(csv_path), status='old', action='read', iostat=ios)
        if (ios /= 0) return

        do
            read(unit_num, '(A)', iostat=ios) line
            if (ios /= 0) exit
            line = adjustl(line)
            if (len_trim(line) == 0) cycle
            if (line(1:1) == '#') cycle
            if (data_index == row_index) then
                call parse_csv_line(line, public_key, secret_key)
                close(unit_num)
                return
            end if
            data_index = data_index + 1
        end do
        close(unit_num)
    end subroutine load_csv_row

    !--------------------------------------------------------------------------
    ! Subroutine: get_credentials
    ! Description: Get API credentials from environment or config file
    !
    ! Priority order:
    !   1. account_index >= 0 -> accounts.csv row N (bypasses env vars)
    !   2. Environment variables (UNSANDBOX_PUBLIC_KEY, UNSANDBOX_SECRET_KEY)
    !   3. Config file (~/.unsandbox/accounts.csv row 0 or UNSANDBOX_ACCOUNT)
    !   4. ./accounts.csv row 0
    !   5. Legacy UNSANDBOX_API_KEY (deprecated)
    !
    ! Arguments:
    !   public_key    - Output: API public key
    !   secret_key    - Output: API secret key
    !   status        - Output: 0 on success, non-zero on error
    !   account_index - Optional input: if >= 0, load that CSV row directly
    !--------------------------------------------------------------------------
    subroutine get_credentials(public_key, secret_key, status, account_index, source_csv)
        character(len=*), intent(out) :: public_key, secret_key
        integer, intent(out) :: status
        integer, intent(in), optional :: account_index
        character(len=*), intent(out), optional :: source_csv

        call resolve_credentials(public_key, secret_key, status, account_index, source_csv)
        if (status == 0) call export_signing_env(secret_key)
    end subroutine get_credentials

    !--------------------------------------------------------------------------
    ! Subroutine: export_signing_env
    ! Description: Hand the secret and the signing helper to child shells via
    !              the environment (UNSB_SK, UNSB_HMAC). See HMAC_SH.
    !--------------------------------------------------------------------------
    subroutine export_signing_env(secret_key)
        character(len=*), intent(in) :: secret_key

        call set_env('UNSB_SK', trim(secret_key))
        call set_env('UNSB_HMAC', HMAC_SH)
    end subroutine export_signing_env

    !--------------------------------------------------------------------------
    ! Subroutine: resolve_credentials
    ! Description: Credential lookup behind get_credentials (priority above)
    !--------------------------------------------------------------------------
    subroutine resolve_credentials(public_key, secret_key, status, account_index, source_csv)
        character(len=*), intent(out) :: public_key, secret_key
        integer, intent(out) :: status
        integer, intent(in), optional :: account_index
        character(len=*), intent(out), optional :: source_csv   ! accounts.csv used, or ''
        character(len=1024) :: home_dir, accounts_path, api_key, acct_env
        integer :: ios, acct_idx, default_index

        status = 0
        public_key = ''
        secret_key = ''
        if (present(source_csv)) source_csv = ''

        ! Priority 1: account_index >= 0 -> load that CSV row (bypasses env vars)
        if (present(account_index)) then
            if (account_index >= 0) then
                acct_idx = account_index
                call get_environment_variable('HOME', home_dir, status=ios)
                if (ios == 0) then
                    accounts_path = trim(home_dir) // '/.unsandbox/accounts.csv'
                    call load_csv_row(accounts_path, acct_idx, public_key, secret_key)
                    if (len_trim(public_key) > 0) then
                        if (present(source_csv)) source_csv = accounts_path
                        return
                    end if
                end if
                call load_csv_row('accounts.csv', acct_idx, public_key, secret_key)
                if (len_trim(public_key) > 0) then
                    if (present(source_csv)) source_csv = 'accounts.csv'
                    return
                end if
                status = 1
                return
            end if
        end if

        ! Priority 2: Environment variables
        call get_environment_variable('UNSANDBOX_PUBLIC_KEY', public_key, status=ios)
        if (ios == 0 .and. len_trim(public_key) > 0) then
            call get_environment_variable('UNSANDBOX_SECRET_KEY', secret_key, status=ios)
            if (ios == 0 .and. len_trim(secret_key) > 0) then
                return
            end if
        end if

        ! Priority 3: ~/.unsandbox/accounts.csv (default row)
        call get_environment_variable('UNSANDBOX_ACCOUNT', acct_env, status=ios)
        if (ios == 0 .and. len_trim(acct_env) > 0) then
            read(acct_env, *, iostat=ios) default_index
            if (ios /= 0) default_index = 0
        else
            default_index = 0
        end if

        call get_environment_variable('HOME', home_dir, status=ios)
        if (ios == 0) then
            accounts_path = trim(home_dir) // '/.unsandbox/accounts.csv'
            call load_csv_row(accounts_path, default_index, public_key, secret_key)
            if (len_trim(public_key) > 0) then
                if (present(source_csv)) source_csv = accounts_path
                return
            end if
        end if

        ! Priority 4: ./accounts.csv
        call load_csv_row('accounts.csv', default_index, public_key, secret_key)
        if (len_trim(public_key) > 0) then
            if (present(source_csv)) source_csv = 'accounts.csv'
            return
        end if

        ! Priority 5: Legacy API key
        call get_environment_variable('UNSANDBOX_API_KEY', api_key, status=ios)
        if (ios == 0 .and. len_trim(api_key) > 0) then
            public_key = api_key
            secret_key = api_key
            return
        end if

        ! No credentials found
        status = 1
    end subroutine resolve_credentials

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
            ! Only the 2nd field: later fields (labels) are not part of the secret
            comma_pos = index(field2, ',')
            if (comma_pos > 0) field2 = field2(1:comma_pos-1)
        end if
    end subroutine parse_csv_line

    !--------------------------------------------------------------------------
    ! Subroutine: sign_request
    ! Description: Generate HMAC-SHA256 signature for API request
    !
    ! Signature format: HMAC-SHA256(secret_key, "timestamp:METHOD:path:body")
    !
    ! Note: Signs in a child shell (HMAC_SH) due to Fortran limitations
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
            'printf %s "', trim(timestamp), ':', trim(method), ':', trim(path), ':', trim(body), &
            '" | sh -c "$UNSB_HMAC"'

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
    !   target_os   - Optional: Target OS (linux or freebsd); omitted = API default
    !--------------------------------------------------------------------------
    subroutine client_execute(self, language, code, result, status, network, ttl, vcpu, target_os)
        class(unsandbox_client), intent(in) :: self
        character(len=*), intent(in) :: language, code
        type(execution_result), intent(out) :: result
        integer, intent(out) :: status
        character(len=*), intent(in), optional :: network
        integer, intent(in), optional :: ttl, vcpu
        character(len=*), intent(in), optional :: target_os
        character(len=16384) :: cmd
        character(len=32) :: net_mode, os_jq
        integer :: exec_ttl, exec_vcpu

        status = 0
        net_mode = 'zerotrust'
        exec_ttl = DEFAULT_TTL
        exec_vcpu = 1

        if (present(network)) net_mode = network
        if (present(ttl)) exec_ttl = ttl
        if (present(vcpu)) exec_vcpu = vcpu

        os_jq = ''
        if (present(target_os)) then
            call normalize_os(target_os, os_jq, status)
            if (status /= 0) then
                result%stderr = 'Invalid target_os (expected linux or freebsd)'
                return
            end if
            os_jq = ', os: "' // trim(os_jq) // '"'
        end if

        if (.not. self%initialized) then
            status = 1
            result%stderr = 'Client not initialized'
            return
        end if

        ! Build and execute shell command with HMAC auth
        write(cmd, '(40A)') &
            'TMPFILE=$(mktemp); ', &
            'cat > "$TMPFILE" << ''CODEEOF''', char(10), trim(code), char(10), 'CODEEOF', char(10), &
            'BODY=$(jq -Rs ''{language: "', trim(language), '", code: ., ', &
            'network_mode: "', trim(net_mode), '", ttl: ', char(48+mod(exec_ttl/10,10)), char(48+mod(exec_ttl,10)), &
            trim(os_jq), '}'' < "$TMPFILE"); ', &
            'TS=$(date +%s); ', &
            'SIG=$(printf %s "$TS:POST:/execute:$BODY" | sh -c "$UNSB_HMAC"); ', &
            'RESP=$(curl -s -X POST ', trim(api_base_url()), '/execute ', &
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
    !   target_os - Optional: Target OS (linux or freebsd); omitted = API default
    !--------------------------------------------------------------------------
    subroutine client_execute_async(self, language, code, job_id, status, target_os)
        class(unsandbox_client), intent(in) :: self
        character(len=*), intent(in) :: language, code
        character(len=*), intent(out) :: job_id
        integer, intent(out) :: status
        character(len=*), intent(in), optional :: target_os
        character(len=8192) :: cmd
        character(len=32) :: os_jq

        status = 0
        job_id = ''

        os_jq = ''
        if (present(target_os)) then
            call normalize_os(target_os, os_jq, status)
            if (status /= 0) return
            os_jq = ', os: "' // trim(os_jq) // '"'
        end if

        if (.not. self%initialized) then
            status = 1
            return
        end if

        write(cmd, '(20A)') &
            'TMPFILE=$(mktemp); ', &
            'cat > "$TMPFILE" << ''CODEEOF''', char(10), trim(code), char(10), 'CODEEOF', char(10), &
            'BODY=$(jq -Rs ''{language: "', trim(language), '", code: .', trim(os_jq), '}'' < "$TMPFILE"); ', &
            'TS=$(date +%s); ', &
            'SIG=$(printf %s "$TS:POST:/execute/async:$BODY" | sh -c "$UNSB_HMAC"); ', &
            'curl -s -X POST ', trim(api_base_url()), '/execute/async ', &
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
            'SIG=$(printf %s "$TS:GET:/jobs/', trim(job_id), ':" | sh -c "$UNSB_HMAC"); ', &
            'curl -s -X GET ', trim(api_base_url()), '/jobs/', trim(job_id), ' ', &
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
            'SIG=$(printf %s "$TS:GET:/jobs/', trim(job_id), ':" | sh -c "$UNSB_HMAC"); ', &
            'RESP=$(curl -s -X GET ', trim(api_base_url()), '/jobs/', trim(job_id), ' ', &
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
            'SIG=$(printf %s "$TS:DELETE:/jobs/', trim(job_id), ':" | sh -c "$UNSB_HMAC"); ', &
            'curl -s -X DELETE ', trim(api_base_url()), '/jobs/', trim(job_id), ' ', &
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
            'SIG=$(printf %s "$TS:GET:/jobs:" | sh -c "$UNSB_HMAC"); ', &
            'curl -s -X GET ', trim(api_base_url()), '/jobs ', &
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

        write(cmd, '(30A)') &
            'TMPFILE=$(mktemp); ', &
            'cat > "$TMPFILE" << ''CODEEOF''', char(10), trim(code), char(10), 'CODEEOF', char(10), &
            'BODY=$(cat "$TMPFILE"); ', &
            'TS=$(date +%s); ', &
            'SIG=$(printf %s "$TS:POST:/run:$BODY" | sh -c "$UNSB_HMAC"); ', &
            'printf %s "$BODY" | curl -s -X POST ', trim(api_base_url()), '/run ', &
            '-H "Content-Type: text/plain" ', &
            '-H "Authorization: Bearer ', trim(self%public_key), '" ', &
            '-H "X-Timestamp: $TS" ', &
            '-H "X-Signature: $SIG" ', &
            '--data-binary @- | jq .; ', &
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

        write(cmd, '(30A)') &
            'TMPFILE=$(mktemp); ', &
            'cat > "$TMPFILE" << ''CODEEOF''', char(10), trim(code), char(10), 'CODEEOF', char(10), &
            'BODY=$(cat "$TMPFILE"); ', &
            'TS=$(date +%s); ', &
            'SIG=$(printf %s "$TS:POST:/run/async:$BODY" | sh -c "$UNSB_HMAC"); ', &
            'printf %s "$BODY" | curl -s -X POST ', trim(api_base_url()), '/run/async ', &
            '-H "Content-Type: text/plain" ', &
            '-H "Authorization: Bearer ', trim(self%public_key), '" ', &
            '-H "X-Timestamp: $TS" ', &
            '-H "X-Signature: $SIG" ', &
            '--data-binary @- | jq -r ".job_id // empty"; ', &
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
            'SIG=$(printf %s "$TS:POST:/image:$BODY" | sh -c "$UNSB_HMAC"); ', &
            'curl -s -X POST ', trim(api_base_url()), '/image ', &
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
            'SIG=$(printf %s "$TS:GET:/languages:" | sh -c "$UNSB_HMAC"); ', &
            'curl -s -X GET ', trim(api_base_url()), '/languages ', &
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
    integer :: account_index   ! -1 = not set; >= 0 means use that CSV row
    logical :: list_flag, is_session, is_service, is_key
    ! Arguments with the global --os flag removed; handlers read these via
    ! get_arg/arg_count. target_os is '' (field omitted) or linux/freebsd.
    character(len=4096), allocatable :: cli_args(:)
    integer :: cli_argc
    character(len=16) :: target_os

    ! Initialize
    subcommand = ''
    list_flag = .false.
    is_session = .false.
    is_service = .false.
    is_key = .false.
    session_id = ''
    service_id = ''
    account_index = -1

    ! Every API request below targets $UN_API (UNSANDBOX_API_URL or default)
    call set_env('UN_API', trim(api_base_url()))

    ! Strip and validate the global --os flag before any request or credentials
    call extract_os_flag()

    ! Get command line arguments count
    nargs = arg_count()
    if (nargs < 1) then
        call print_help()
        stop 1
    end if

    ! Pre-scan all arguments for --account N
    do i = 1, nargs - 1
        call get_arg(i, arg)
        if (trim(arg) == '--account') then
            call get_arg(i + 1, arg)
            read(arg, *, iostat=stat) account_index
            if (stat /= 0) account_index = -1
            exit
        end if
    end do

    ! Check for subcommands
    call get_arg(1, arg, status=stat)
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
    else if (trim(arg) == 'languages') then
        call handle_languages()
        stop 0
    else if (trim(arg) == 'image') then
        call handle_image()
        stop 0
    else if (trim(arg) == 'snapshot') then
        call handle_snapshot()
        stop 0
    else
        ! Default execute command
        filename = trim(arg)
        call handle_execute(filename)
        stop 0
    end if

contains

    ! Remove --os OS / --os=OS from the arguments (any position) and validate
    ! it: linux or freebsd, case-insensitive. Anything else exits non-zero.
    subroutine extract_os_flag()
        character(len=4096) :: a, val
        integer :: k, n, ios

        target_os = ''
        n = command_argument_count()
        allocate(cli_args(max(n, 1)))
        cli_argc = 0
        k = 1
        do while (k <= n)
            call get_command_argument(k, a)
            if (trim(a) == '--os') then
                if (k + 1 > n) then
                    write(0, '(A)') 'Error: --os requires a value: linux or freebsd'
                    stop 1
                end if
                call get_command_argument(k + 1, val)
                k = k + 1
            else if (len_trim(a) >= 5 .and. a(1:5) == '--os=') then
                val = a(6:)
            else
                cli_argc = cli_argc + 1
                cli_args(cli_argc) = a
                k = k + 1
                cycle
            end if
            call normalize_os(trim(val), target_os, ios)
            if (ios /= 0) then
                write(0, '(A,A,A)') 'Error: invalid --os ''', trim(val), ''' (expected linux or freebsd)'
                stop 1
            end if
            k = k + 1
        end do
    end subroutine extract_os_flag

    integer function arg_count()
        arg_count = cli_argc
    end function arg_count

    subroutine get_arg(k, val, status)
        integer, intent(in) :: k
        character(len=*), intent(out) :: val
        integer, intent(out), optional :: status

        val = ''
        if (present(status)) status = 0
        if (k >= 1 .and. k <= cli_argc) then
            val = cli_args(k)
        else if (present(status)) then
            status = 1
        end if
    end subroutine get_arg

    ! JSON fragment for request bodies: ,"os":"freebsd" when --os was given
    function os_json() result(frag)
        character(len=:), allocatable :: frag
        frag = ''
        if (len_trim(target_os) > 0) frag = ',"os":"' // trim(target_os) // '"'
    end function os_json

    ! jq object fragment: , os: "freebsd" when --os was given
    function os_jq() result(frag)
        character(len=:), allocatable :: frag
        frag = ''
        if (len_trim(target_os) > 0) frag = ', os: "' // trim(target_os) // '"'
    end function os_jq

    subroutine print_help()
        write(*, '(A)') 'unsandbox SDK for Fortran - Execute code in secure sandboxes'
        write(*, '(A)') 'https://unsandbox.com | https://api.unsandbox.com/openapi'
        write(*, '(A)') ''
        write(*, '(A)') 'Usage: ./un [options] <source_file>'
        write(*, '(A)') '       ./un session [options]'
        write(*, '(A)') '       ./un service [options]'
        write(*, '(A)') '       ./un snapshot [options]'
        write(*, '(A)') '       ./un image [options]'
        write(*, '(A)') '       ./un key [--extend]'
        write(*, '(A)') '       ./un key --rotate [--otp CODE]'
        write(*, '(A)') '       ./un languages [--json]'
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
        write(*, '(A)') 'Snapshot options:'
        write(*, '(A)') '  -l, --list          List all snapshots'
        write(*, '(A)') '  --info ID           Get snapshot details'
        write(*, '(A)') '  --delete ID         Delete a snapshot'
        write(*, '(A)') '  --lock ID           Lock snapshot'
        write(*, '(A)') '  --unlock ID         Unlock snapshot'
        write(*, '(A)') '  --restore ID        Restore from snapshot'
        write(*, '(A)') '  --clone ID          Clone snapshot (requires --type)'
        write(*, '(A)') '  --type TYPE         Clone type: session or service'
        write(*, '(A)') '  --name NAME         Name for cloned resource'
        write(*, '(A)') '  --shell SHELL       Shell for cloned session'
        write(*, '(A)') '  --ports PORTS       Ports for cloned service'
        write(*, '(A)') ''
        write(*, '(A)') 'Image options:'
        write(*, '(A)') '  -l, --list          List all images'
        write(*, '(A)') '  --info ID           Get image details'
        write(*, '(A)') '  --delete ID         Delete an image'
        write(*, '(A)') '  --lock ID           Lock image'
        write(*, '(A)') '  --unlock ID         Unlock image'
        write(*, '(A)') '  --publish ID        Publish (requires --source-type)'
        write(*, '(A)') '  --source-type TYPE  Source: service or snapshot'
        write(*, '(A)') '  --visibility ID MODE  Set: private/unlisted/public'
        write(*, '(A)') '  --spawn ID          Spawn service from image'
        write(*, '(A)') '  --clone ID          Clone an image'
        write(*, '(A)') '  --name NAME         Name for spawn/clone'
        write(*, '(A)') '  --ports PORTS       Ports for spawn'
        write(*, '(A)') ''
        write(*, '(A)') 'Key options:'
        write(*, '(A)') '  --extend        Open browser to extend key'
        write(*, '(A)') ''
        write(*, '(A)') 'Languages options:'
        write(*, '(A)') '  --json          Output as JSON array'
        write(*, '(A)') ''
        write(*, '(A)') 'Credential options (global):'
        write(*, '(A)') '  --account N     Use row N from accounts.csv (bypasses env vars)'
        write(*, '(A)') ''
        write(*, '(A)') 'Global options:'
        write(*, '(A)') '  --os OS         Target OS: linux (default) or freebsd (execute, session, service)'
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
        logical :: artifacts, has_env, has_files, file_exists

        ! Check if file exists
        inquire(file=trim(fname), exist=file_exists)
        if (.not. file_exists) then
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
        call get_credentials(public_key, secret_key, stat, account_index)
        if (stat /= 0) then
            write(0, '(A)') 'Error: No credentials found. Set UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY'
            stop 1
        end if

        ! Build curl command with HMAC auth
        write(full_cmd, '(40A)') &
            'TS=$(date +%s); ', &
            'BODY=$(jq -Rs ''{language: "', trim(language), '", code: .', os_jq(), '}'' < "', trim(fname), '"); ', &
            'SIG=$(printf %s "$TS:POST:/execute:$BODY" | sh -c "$UNSB_HMAC"); ', &
            'curl -s -X POST $UN_API/execute ', &
            '-H "Content-Type: application/json" ', &
            '-H "Authorization: Bearer ', trim(public_key), '" ', &
            '-H "X-Timestamp: $TS" ', &
            '-H "X-Signature: $SIG" ', &
            '--data-binary "$BODY" -o /tmp/unsandbox_resp.json; ', &
            'RESP=$(cat /tmp/unsandbox_resp.json); ', &
            'if echo "$RESP" | grep -q "timestamp" && ', &
            '(echo "$RESP" | grep -Eq "(401|expired|invalid)"); then ', &
            'echo -e "\x1b[31mError: Request timestamp expired (must be within 5 minutes of server time)\x1b[0m" >&2; ', &
            'echo -e "\x1b[33mYour computer''s clock may have drifted.\x1b[0m" >&2; ', &
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
        do i = 2, arg_count()
            call get_arg(i, arg)
            if (trim(arg) == '-l' .or. trim(arg) == '--list') then
                list_mode = .true.
            else if (trim(arg) == '--kill') then
                kill_mode = .true.
                if (i+1 <= arg_count()) then
                    call get_arg(i+1, session_id)
                end if
            else if (trim(arg) == '-f') then
                if (i+1 <= arg_count()) then
                    call get_arg(i+1, arg)
                    if (len_trim(input_files) > 0) then
                        input_files = trim(input_files) // ',' // trim(arg)
                    else
                        input_files = trim(arg)
                    end if
                end if
            else if (trim(arg) == '--account') then
                ! already processed in main pre-scan; skip this token and its value
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
        call get_credentials(public_key, secret_key, stat, account_index)
        if (stat /= 0) then
            write(0, '(A)') 'Error: No credentials found'
            stop 1
        end if

        if (list_mode) then
            write(full_cmd, '(20A)') &
                'TS=$(date +%s); ', &
                'SIG=$(printf %s "$TS:GET:/sessions:" | sh -c "$UNSB_HMAC"); ', &
                'curl -s -X GET $UN_API/sessions ', &
                '-H "Authorization: Bearer ', trim(public_key), '" ', &
                '-H "X-Timestamp: $TS" ', &
                '-H "X-Signature: $SIG" | ', &
                'jq -r ''.sessions[] | "\(.id) \(.shell) \(.status) \(.created_at)"'' ', &
                '2>/dev/null || echo "No active sessions"'
            call execute_command_line(trim(full_cmd), wait=.true.)
        else if (kill_mode .and. len_trim(session_id) > 0) then
            write(full_cmd, '(20A)') &
                'TS=$(date +%s); ', &
                'SIG=$(printf %s "$TS:DELETE:/sessions/', trim(session_id), ':" | sh -c "$UNSB_HMAC"); ', &
                'curl -s -X DELETE $UN_API/sessions/', &
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
                    'BODY=''{"shell":"bash"', os_json(), ',"input_files":[''"$INPUT_FILES"'']}''; ', &
                    'TS=$(date +%s); ', &
                    'SIG=$(printf %s "$TS:POST:/sessions:$BODY" | sh -c "$UNSB_HMAC"); ', &
                    'curl -s -X POST $UN_API/sessions ', &
                    '-H "Content-Type: application/json" ', &
                    '-H "Authorization: Bearer ', trim(public_key), '" ', &
                    '-H "X-Timestamp: $TS" ', &
                    '-H "X-Signature: $SIG" ', &
                    '-d "$BODY" && ', &
                    'echo -e "\x1b[33mSession created (WebSocket required)\x1b[0m"'
            else
                write(full_cmd, '(20A)') &
                    'BODY=''{"shell":"bash"', os_json(), '}''; ', &
                    'TS=$(date +%s); ', &
                    'SIG=$(printf %s "$TS:POST:/sessions:$BODY" | sh -c "$UNSB_HMAC"); ', &
                    'curl -s -X POST $UN_API/sessions ', &
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
        do while (i <= arg_count())
            call get_arg(i, arg)
            if (trim(arg) == '-l' .or. trim(arg) == '--list') then
                list_mode = .true.
            else if (trim(arg) == 'env') then
                if (i+2 <= arg_count()) then
                    call get_arg(i+1, env_action)
                    call get_arg(i+2, env_target)
                    i = i + 2
                end if
            else if (trim(arg) == '--name') then
                operation = 'create'
                if (i+1 <= arg_count()) then
                    call get_arg(i+1, service_name)
                    i = i + 1
                end if
            else if (trim(arg) == '--type') then
                if (i+1 <= arg_count()) then
                    call get_arg(i+1, service_type)
                    i = i + 1
                end if
            else if (trim(arg) == '-e') then
                if (i+1 <= arg_count()) then
                    call get_arg(i+1, arg)
                    if (len_trim(svc_envs) > 0) then
                        svc_envs = trim(svc_envs) // char(10) // trim(arg)
                    else
                        svc_envs = trim(arg)
                    end if
                    i = i + 1
                end if
            else if (trim(arg) == '--env-file') then
                if (i+1 <= arg_count()) then
                    call get_arg(i+1, svc_env_file)
                    i = i + 1
                end if
            else if (trim(arg) == '-f') then
                if (i+1 <= arg_count()) then
                    call get_arg(i+1, arg)
                    if (len_trim(input_files) > 0) then
                        input_files = trim(input_files) // ',' // trim(arg)
                    else
                        input_files = trim(arg)
                    end if
                    i = i + 1
                end if
            else if (trim(arg) == '--info') then
                operation = 'info'
                if (i+1 <= arg_count()) then
                    call get_arg(i+1, service_id)
                    i = i + 1
                end if
            else if (trim(arg) == '--logs') then
                operation = 'logs'
                if (i+1 <= arg_count()) then
                    call get_arg(i+1, service_id)
                    i = i + 1
                end if
            else if (trim(arg) == '--freeze') then
                operation = 'sleep'
                if (i+1 <= arg_count()) then
                    call get_arg(i+1, service_id)
                    i = i + 1
                end if
            else if (trim(arg) == '--unfreeze') then
                operation = 'wake'
                if (i+1 <= arg_count()) then
                    call get_arg(i+1, service_id)
                    i = i + 1
                end if
            else if (trim(arg) == '--destroy') then
                operation = 'destroy'
                if (i+1 <= arg_count()) then
                    call get_arg(i+1, service_id)
                    i = i + 1
                end if
            else if (trim(arg) == '--resize') then
                operation = 'resize'
                if (i+1 <= arg_count()) then
                    call get_arg(i+1, service_id)
                    i = i + 1
                end if
            else if (trim(arg) == '--vcpu' .or. trim(arg) == '-v') then
                if (i+1 <= arg_count()) then
                    call get_arg(i+1, arg)
                    read(arg, *) resize_vcpu
                    i = i + 1
                end if
            else if (trim(arg) == '--dump-bootstrap') then
                operation = 'dump-bootstrap'
                if (i+1 <= arg_count()) then
                    call get_arg(i+1, service_id)
                    i = i + 1
                end if
            else if (trim(arg) == '--dump-file') then
                operation = 'dump-file'
                if (i+1 <= arg_count()) then
                    call get_arg(i+1, service_type)
                    i = i + 1
                end if
            else if (trim(arg) == '--set-unfreeze-on-demand') then
                operation = 'set-unfreeze-on-demand'
                if (i+1 <= arg_count()) then
                    call get_arg(i+1, service_id)
                    i = i + 1
                end if
                if (i+1 <= arg_count()) then
                    call get_arg(i+1, service_type)
                    i = i + 1
                end if
            end if
            i = i + 1
        end do

        ! Get API keys
        call get_credentials(public_key, secret_key, stat, account_index)
        if (stat /= 0) then
            write(0, '(A)') 'Error: No credentials found'
            stop 1
        end if

        ! Handle env subcommand
        if (len_trim(env_action) > 0) then
            if (trim(env_action) == 'status') then
                write(full_cmd, '(20A)') &
                    'TS=$(date +%s); ', &
                    'SIG=$(printf %s "$TS:GET:/services/', trim(env_target), '/env:" | ', &
                    'sh -c "$UNSB_HMAC"); ', &
                    'curl -s -X GET "$UN_API/services/', trim(env_target), '/env" ', &
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
                    'SIG=$(printf %s "$TS:PUT:/services/', trim(env_target), '/env:$ENV_CONTENT" | ', &
                    'sh -c "$UNSB_HMAC"); ', &
                    'curl -s -X PUT "$UN_API/services/', trim(env_target), '/env" ', &
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
                    'SIG=$(printf %s "$TS:POST:/services/', trim(env_target), '/env/export:" | ', &
                    'sh -c "$UNSB_HMAC"); ', &
                    'curl -s -X POST "$UN_API/services/', trim(env_target), '/env/export" ', &
                    '-H "Authorization: Bearer ', trim(public_key), '" ', &
                    '-H "X-Timestamp: $TS" ', &
                    '-H "X-Signature: $SIG" | jq -r ".content // empty"'
                call execute_command_line(trim(full_cmd), wait=.true.)
                return
            else if (trim(env_action) == 'delete') then
                write(full_cmd, '(20A)') &
                    'TS=$(date +%s); ', &
                    'SIG=$(printf %s "$TS:DELETE:/services/', trim(env_target), '/env:" | ', &
                    'sh -c "$UNSB_HMAC"); ', &
                    'curl -s -X DELETE "$UN_API/services/', trim(env_target), '/env" ', &
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
                'SIG=$(printf %s "$TS:GET:/services:" | sh -c "$UNSB_HMAC"); ', &
                'curl -s -X GET $UN_API/services ', &
                '-H "Authorization: Bearer ', trim(public_key), '" ', &
                '-H "X-Timestamp: $TS" ', &
                '-H "X-Signature: $SIG" | ', &
                'jq -r ''.services[] | "\(.id) \(.name) \(.status)"'' ', &
                '2>/dev/null || echo "No services"'
            call execute_command_line(trim(full_cmd), wait=.true.)
        else if (trim(operation) == 'info' .and. len_trim(service_id) > 0) then
            write(full_cmd, '(20A)') &
                'TS=$(date +%s); ', &
                'SIG=$(printf %s "$TS:GET:/services/', trim(service_id), ':" | sh -c "$UNSB_HMAC"); ', &
                'curl -s -X GET $UN_API/services/', &
                trim(service_id), ' ', &
                '-H "Authorization: Bearer ', trim(public_key), '" ', &
                '-H "X-Timestamp: $TS" ', &
                '-H "X-Signature: $SIG" | jq .'
            call execute_command_line(trim(full_cmd), wait=.true.)
        else if (trim(operation) == 'logs' .and. len_trim(service_id) > 0) then
            write(full_cmd, '(20A)') &
                'TS=$(date +%s); ', &
                'SIG=$(printf %s "$TS:GET:/services/', trim(service_id), '/logs:" | sh -c "$UNSB_HMAC"); ', &
                'curl -s -X GET $UN_API/services/', &
                trim(service_id), '/logs ', &
                '-H "Authorization: Bearer ', trim(public_key), '" ', &
                '-H "X-Timestamp: $TS" ', &
                '-H "X-Signature: $SIG" | jq -r ".logs"'
            call execute_command_line(trim(full_cmd), wait=.true.)
        else if (trim(operation) == 'sleep' .and. len_trim(service_id) > 0) then
            write(full_cmd, '(20A)') &
                'TS=$(date +%s); ', &
                'SIG=$(printf %s "$TS:POST:/services/', trim(service_id), '/freeze:" | sh -c "$UNSB_HMAC"); ', &
                'curl -s -X POST $UN_API/services/', &
                trim(service_id), '/freeze ', &
                '-H "Authorization: Bearer ', trim(public_key), '" ', &
                '-H "X-Timestamp: $TS" ', &
                '-H "X-Signature: $SIG" >/dev/null && ', &
                'echo -e "\x1b[32mService frozen: ', trim(service_id), '\x1b[0m"'
            call execute_command_line(trim(full_cmd), wait=.true.)
        else if (trim(operation) == 'wake' .and. len_trim(service_id) > 0) then
            write(full_cmd, '(20A)') &
                'TS=$(date +%s); ', &
                'SIG=$(printf %s "$TS:POST:/services/', trim(service_id), '/unfreeze:" | sh -c "$UNSB_HMAC"); ', &
                'curl -s -X POST $UN_API/services/', &
                trim(service_id), '/unfreeze ', &
                '-H "Authorization: Bearer ', trim(public_key), '" ', &
                '-H "X-Timestamp: $TS" ', &
                '-H "X-Signature: $SIG" >/dev/null && ', &
                'echo -e "\x1b[32mService unfreezing: ', trim(service_id), '\x1b[0m"'
            call execute_command_line(trim(full_cmd), wait=.true.)
        else if (trim(operation) == 'destroy' .and. len_trim(service_id) > 0) then
            write(full_cmd, '(50A)') &
                'TS=$(date +%s); ', &
                'SIG=$(printf %s "$TS:DELETE:/services/', trim(service_id), ':" | sh -c "$UNSB_HMAC"); ', &
                'RESP=$(curl -s -w "\n%{http_code}" -X DELETE $UN_API/services/', &
                trim(service_id), ' ', &
                '-H "Authorization: Bearer ', trim(public_key), '" ', &
                '-H "X-Timestamp: $TS" ', &
                '-H "X-Signature: $SIG"); ', &
                'HTTP_CODE=$(echo "$RESP" | tail -n1); ', &
                'BODY=$(echo "$RESP" | sed ''$d''); ', &
                'if [ "$HTTP_CODE" = "428" ]; then ', &
                'CHALLENGE_ID=$(echo "$BODY" | jq -r ".challenge_id // empty"); ', &
                'echo -e "\x1b[33mConfirmation required. Check your email for a one-time code.\x1b[0m" >&2; ', &
                'echo -n "Enter OTP: " >&2; read OTP; ', &
                'if [ -z "$OTP" ]; then echo -e "\x1b[31mError: Operation cancelled\x1b[0m" >&2; exit 1; fi; ', &
                'TS2=$(date +%s); ', &
                'SIG2=$(printf %s "$TS2:DELETE:/services/', trim(service_id), ':" | sh -c "$UNSB_HMAC"); ', &
                'curl -s -X DELETE $UN_API/services/', trim(service_id), ' ', &
                '-H "Authorization: Bearer ', trim(public_key), '" ', &
                '-H "X-Timestamp: $TS2" ', &
                '-H "X-Signature: $SIG2" ', &
                '-H "X-Sudo-OTP: $OTP" ', &
                '-H "X-Sudo-Challenge: $CHALLENGE_ID" >/dev/null && ', &
                'echo -e "\x1b[32mService destroyed: ', trim(service_id), '\x1b[0m"; ', &
                'elif [ "$HTTP_CODE" = "200" ] || [ "$HTTP_CODE" = "204" ]; then ', &
                'echo -e "\x1b[32mService destroyed: ', trim(service_id), '\x1b[0m"; ', &
                'else echo -e "\x1b[31mError: HTTP $HTTP_CODE\x1b[0m" >&2; echo "$BODY" >&2; exit 1; fi'
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
                'SIG=$(printf %s "$TS:PATCH:/services/', trim(service_id), ':$BODY" | sh -c "$UNSB_HMAC"); ', &
                'curl -s -X PATCH $UN_API/services/', &
                trim(service_id), ' ', &
                '-H "Content-Type: application/json" ', &
                '-H "Authorization: Bearer ', trim(public_key), '" ', &
                '-H "X-Timestamp: $TS" ', &
                '-H "X-Signature: $SIG" ', &
                '-d "$BODY" >/dev/null && ', &
                'echo -e "\x1b[32mService resized to ', resize_vcpu, ' vCPU, ', resize_vcpu * 2, ' GB RAM\x1b[0m"'
            call execute_command_line(trim(full_cmd), wait=.true.)
        else if (trim(operation) == 'set-unfreeze-on-demand' .and. len_trim(service_id) > 0) then
            ! service_type holds the enabled value (true/false/1/0/yes/no/on/off)
            write(full_cmd, '(30A)') &
                'ENABLED_STR=$(echo "', trim(service_type), '" | tr "[:upper:]" "[:lower:]"); ', &
                'case "$ENABLED_STR" in true|1|yes|on) ENABLED=true;; *) ENABLED=false;; esac; ', &
                'BODY="{\"unfreeze_on_demand\":$ENABLED}"; ', &
                'TS=$(date +%s); ', &
                'SIG=$(printf %s "$TS:PATCH:/services/', trim(service_id), ':$BODY" | sh -c "$UNSB_HMAC"); ', &
                'curl -s -X PATCH $UN_API/services/', &
                trim(service_id), ' ', &
                '-H "Content-Type: application/json" ', &
                '-H "Authorization: Bearer ', trim(public_key), '" ', &
                '-H "X-Timestamp: $TS" ', &
                '-H "X-Signature: $SIG" ', &
                '-d "$BODY" >/dev/null && ', &
                'echo -e "\x1b[32mService unfreeze_on_demand set to $ENABLED: ', trim(service_id), '\x1b[0m"'
            call execute_command_line(trim(full_cmd), wait=.true.)
        else if (trim(operation) == 'dump-bootstrap' .and. len_trim(service_id) > 0) then
            write(full_cmd, '(30A)') &
                'echo "Fetching bootstrap script from ', trim(service_id), '..." >&2; ', &
                'BODY=''{"command":"cat /tmp/bootstrap.sh"}''; ', &
                'TS=$(date +%s); ', &
                'SIG=$(printf %s "$TS:POST:/services/', trim(service_id), '/execute:$BODY" | sh -c "$UNSB_HMAC"); ', &
                'RESP=$(curl -s -X POST $UN_API/services/', &
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
                'BODY=''{"name":"', trim(service_name), '"', os_json(), '}''; ', &
                'TS=$(date +%s); ', &
                'SIG=$(printf %s "$TS:POST:/services:$BODY" | sh -c "$UNSB_HMAC"); ', &
                'RESP=$(curl -s -X POST $UN_API/services ', &
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
                'SIG2=$(printf %s "$TS2:PUT:/services/$SVC_ID/env:$ENV_CONTENT" | sh -c "$UNSB_HMAC"); ', &
                'curl -s -X PUT "$UN_API/services/$SVC_ID/env" ', &
                '-H "Authorization: Bearer ', trim(public_key), '" ', &
                '-H "X-Timestamp: $TS2" ', &
                '-H "X-Signature: $SIG2" ', &
                '-H "Content-Type: text/plain" ', &
                '--data-binary "$ENV_CONTENT" >/dev/null && ', &
                'echo -e "\x1b[32mVault configured\x1b[0m"; fi; ', &
                'else echo "$RESP" | jq .; fi'
            call execute_command_line(trim(full_cmd), wait=.true.)
        else
            write(0, '(A)') 'Error: Use --list, --info, --logs, --freeze, --unfreeze, --destroy, --set-unfreeze-on-demand, --dump-bootstrap, --name, or env'
            stop 1
        end if
    end subroutine handle_service

    subroutine handle_snapshot()
        character(len=8192) :: full_cmd
        character(len=256) :: arg, snapshot_id, operation, clone_type, name, ports, shell
        character(len=1024) :: public_key, secret_key
        integer :: i, stat
        logical :: list_mode

        snapshot_id = ''
        operation = ''
        clone_type = ''
        name = ''
        ports = ''
        shell = ''
        list_mode = .false.

        ! Parse arguments
        do i = 2, arg_count()
            call get_arg(i, arg)
            if (trim(arg) == '-l' .or. trim(arg) == '--list') then
                list_mode = .true.
            else if (trim(arg) == '--info') then
                operation = 'info'
                if (i < arg_count()) then
                    call get_arg(i + 1, snapshot_id)
                end if
            else if (trim(arg) == '--delete') then
                operation = 'delete'
                if (i < arg_count()) then
                    call get_arg(i + 1, snapshot_id)
                end if
            else if (trim(arg) == '--lock') then
                operation = 'lock'
                if (i < arg_count()) then
                    call get_arg(i + 1, snapshot_id)
                end if
            else if (trim(arg) == '--unlock') then
                operation = 'unlock'
                if (i < arg_count()) then
                    call get_arg(i + 1, snapshot_id)
                end if
            else if (trim(arg) == '--restore') then
                operation = 'restore'
                if (i < arg_count()) then
                    call get_arg(i + 1, snapshot_id)
                end if
            else if (trim(arg) == '--clone') then
                operation = 'clone'
                if (i < arg_count()) then
                    call get_arg(i + 1, snapshot_id)
                end if
            else if (trim(arg) == '--type') then
                if (i < arg_count()) then
                    call get_arg(i + 1, clone_type)
                end if
            else if (trim(arg) == '--name') then
                if (i < arg_count()) then
                    call get_arg(i + 1, name)
                end if
            else if (trim(arg) == '--ports') then
                if (i < arg_count()) then
                    call get_arg(i + 1, ports)
                end if
            else if (trim(arg) == '--shell') then
                if (i < arg_count()) then
                    call get_arg(i + 1, shell)
                end if
            end if
        end do

        ! Get credentials
        call get_credentials(public_key, secret_key, stat, account_index)
        if (stat /= 0) then
            write(0, '(A)') 'Error: No credentials found'
            stop 1
        end if

        if (list_mode) then
            ! List snapshots
            write(full_cmd, '(20A)') &
                'TS=$(date +%s); ', &
                'SIG=$(printf %s "$TS:GET:/snapshots:" | sh -c "$UNSB_HMAC"); ', &
                'curl -s -X GET $UN_API/snapshots ', &
                '-H "Authorization: Bearer ', trim(public_key), '" ', &
                '-H "X-Timestamp: $TS" ', &
                '-H "X-Signature: $SIG" | jq .'
            call execute_command_line(trim(full_cmd), wait=.true.)
        else if (trim(operation) == 'info' .and. len_trim(snapshot_id) > 0) then
            ! Get snapshot info
            write(full_cmd, '(20A)') &
                'TS=$(date +%s); ', &
                'SIG=$(printf %s "$TS:GET:/snapshots/', trim(snapshot_id), ':" | sh -c "$UNSB_HMAC"); ', &
                'curl -s -X GET $UN_API/snapshots/', trim(snapshot_id), ' ', &
                '-H "Authorization: Bearer ', trim(public_key), '" ', &
                '-H "X-Timestamp: $TS" ', &
                '-H "X-Signature: $SIG" | jq .'
            call execute_command_line(trim(full_cmd), wait=.true.)
        else if (trim(operation) == 'delete' .and. len_trim(snapshot_id) > 0) then
            ! Delete snapshot (with sudo)
            write(full_cmd, '(30A)') &
                'TS=$(date +%s); ', &
                'SIG=$(printf %s "$TS:DELETE:/snapshots/', trim(snapshot_id), ':" | sh -c "$UNSB_HMAC"); ', &
                'RESP=$(curl -s -w "\n%{http_code}" -X DELETE $UN_API/snapshots/', trim(snapshot_id), ' ', &
                '-H "Authorization: Bearer ', trim(public_key), '" ', &
                '-H "X-Timestamp: $TS" ', &
                '-H "X-Signature: $SIG"); ', &
                'HTTP_CODE=$(echo "$RESP" | tail -1); ', &
                'BODY=$(echo "$RESP" | head -n -1); ', &
                'if [ "$HTTP_CODE" = "428" ]; then ', &
                'OTP=$(echo "$BODY" | jq -r ".otp // empty"); ', &
                'if [ -n "$OTP" ]; then ', &
                'TS2=$(date +%s); ', &
                'SIG2=$(printf %s "$TS2:DELETE:/snapshots/', trim(snapshot_id), ':" | sh -c "$UNSB_HMAC"); ', &
                'curl -s -X DELETE $UN_API/snapshots/', trim(snapshot_id), ' ', &
                '-H "Authorization: Bearer ', trim(public_key), '" ', &
                '-H "X-Timestamp: $TS2" ', &
                '-H "X-Signature: $SIG2" ', &
                '-H "X-Sudo-OTP: $OTP" | jq .; ', &
                'echo -e "\x1b[32mSnapshot deleted\x1b[0m"; fi; ', &
                'else echo "$BODY" | jq .; fi'
            call execute_command_line(trim(full_cmd), wait=.true.)
        else if (trim(operation) == 'lock' .and. len_trim(snapshot_id) > 0) then
            ! Lock snapshot
            write(full_cmd, '(20A)') &
                'TS=$(date +%s); ', &
                'SIG=$(printf %s "$TS:POST:/snapshots/', trim(snapshot_id), '/lock:" | sh -c "$UNSB_HMAC"); ', &
                'curl -s -X POST $UN_API/snapshots/', trim(snapshot_id), '/lock ', &
                '-H "Authorization: Bearer ', trim(public_key), '" ', &
                '-H "X-Timestamp: $TS" ', &
                '-H "X-Signature: $SIG" | jq . && ', &
                'echo -e "\x1b[32mSnapshot locked\x1b[0m"'
            call execute_command_line(trim(full_cmd), wait=.true.)
        else if (trim(operation) == 'unlock' .and. len_trim(snapshot_id) > 0) then
            ! Unlock snapshot (with sudo)
            write(full_cmd, '(30A)') &
                'TS=$(date +%s); ', &
                'BODY="{}"; ', &
                'SIG=$(printf %s "$TS:POST:/snapshots/', trim(snapshot_id), '/unlock:$BODY" | sh -c "$UNSB_HMAC"); ', &
                'RESP=$(curl -s -w "\n%{http_code}" -X POST $UN_API/snapshots/', trim(snapshot_id), '/unlock ', &
                '-H "Authorization: Bearer ', trim(public_key), '" ', &
                '-H "X-Timestamp: $TS" ', &
                '-H "X-Signature: $SIG" ', &
                '-H "Content-Type: application/json" ', &
                '-d "$BODY"); ', &
                'HTTP_CODE=$(echo "$RESP" | tail -1); ', &
                'BODY_RESP=$(echo "$RESP" | head -n -1); ', &
                'if [ "$HTTP_CODE" = "428" ]; then ', &
                'OTP=$(echo "$BODY_RESP" | jq -r ".otp // empty"); ', &
                'if [ -n "$OTP" ]; then ', &
                'TS2=$(date +%s); ', &
                'SIG2=$(printf %s "$TS2:POST:/snapshots/', trim(snapshot_id), '/unlock:$BODY" | sh -c "$UNSB_HMAC"); ', &
                'curl -s -X POST $UN_API/snapshots/', trim(snapshot_id), '/unlock ', &
                '-H "Authorization: Bearer ', trim(public_key), '" ', &
                '-H "X-Timestamp: $TS2" ', &
                '-H "X-Signature: $SIG2" ', &
                '-H "X-Sudo-OTP: $OTP" ', &
                '-H "Content-Type: application/json" ', &
                '-d "$BODY" | jq .; ', &
                'echo -e "\x1b[32mSnapshot unlocked\x1b[0m"; fi; ', &
                'else echo "$BODY_RESP" | jq .; fi'
            call execute_command_line(trim(full_cmd), wait=.true.)
        else if (trim(operation) == 'restore' .and. len_trim(snapshot_id) > 0) then
            ! Restore snapshot
            write(full_cmd, '(20A)') &
                'TS=$(date +%s); ', &
                'BODY="{}"; ', &
                'SIG=$(printf %s "$TS:POST:/snapshots/', trim(snapshot_id), '/restore:$BODY" | sh -c "$UNSB_HMAC"); ', &
                'curl -s -X POST $UN_API/snapshots/', trim(snapshot_id), '/restore ', &
                '-H "Authorization: Bearer ', trim(public_key), '" ', &
                '-H "X-Timestamp: $TS" ', &
                '-H "X-Signature: $SIG" ', &
                '-H "Content-Type: application/json" ', &
                '-d "$BODY" | jq . && ', &
                'echo -e "\x1b[32mSnapshot restored\x1b[0m"'
            call execute_command_line(trim(full_cmd), wait=.true.)
        else if (trim(operation) == 'clone' .and. len_trim(snapshot_id) > 0) then
            ! Clone snapshot
            if (len_trim(clone_type) == 0) then
                write(0, '(A)') 'Error: --type required for --clone (session or service)'
                stop 1
            end if
            write(full_cmd, '(30A)') &
                'TS=$(date +%s); ', &
                'BODY=''{"type":"', trim(clone_type), '"'
            if (len_trim(name) > 0) then
                write(full_cmd, '(A,A)') trim(full_cmd), ',"name":"' // trim(name) // '"'
            end if
            if (len_trim(ports) > 0) then
                write(full_cmd, '(A,A)') trim(full_cmd), ',"ports":[' // trim(ports) // ']'
            end if
            if (len_trim(shell) > 0) then
                write(full_cmd, '(A,A)') trim(full_cmd), ',"shell":"' // trim(shell) // '"'
            end if
            write(full_cmd, '(A,20A)') trim(full_cmd), '}''; ', &
                'SIG=$(printf %s "$TS:POST:/snapshots/', trim(snapshot_id), '/clone:$BODY" | sh -c "$UNSB_HMAC"); ', &
                'curl -s -X POST $UN_API/snapshots/', trim(snapshot_id), '/clone ', &
                '-H "Authorization: Bearer ', trim(public_key), '" ', &
                '-H "X-Timestamp: $TS" ', &
                '-H "X-Signature: $SIG" ', &
                '-H "Content-Type: application/json" ', &
                '-d "$BODY" | jq . && ', &
                'echo -e "\x1b[32mSnapshot cloned\x1b[0m"'
            call execute_command_line(trim(full_cmd), wait=.true.)
        else
            write(0, '(A)') 'Error: Use --list, --info, --delete, --lock, --unlock, --restore, or --clone'
            stop 1
        end if
    end subroutine handle_snapshot

    subroutine handle_image()
        character(len=8192) :: full_cmd
        character(len=256) :: arg, image_id, operation, source_type, name, ports, visibility_mode
        character(len=1024) :: public_key, secret_key
        integer :: i, stat
        logical :: list_mode

        image_id = ''
        operation = ''
        source_type = ''
        name = ''
        ports = ''
        visibility_mode = ''
        list_mode = .false.

        ! Get credentials
        call get_credentials(public_key, secret_key, stat, account_index)
        if (stat /= 0) then
            write(0, '(A)') 'Error: No credentials found'
            stop 1
        end if

        ! Parse image arguments
        do i = 2, arg_count()
            call get_arg(i, arg)
            if (trim(arg) == '-l' .or. trim(arg) == '--list') then
                list_mode = .true.
                operation = 'list'
            else if (trim(arg) == '--info') then
                operation = 'info'
                if (i < arg_count()) then
                    call get_arg(i+1, image_id)
                end if
            else if (trim(arg) == '--delete') then
                operation = 'delete'
                if (i < arg_count()) then
                    call get_arg(i+1, image_id)
                end if
            else if (trim(arg) == '--lock') then
                operation = 'lock'
                if (i < arg_count()) then
                    call get_arg(i+1, image_id)
                end if
            else if (trim(arg) == '--unlock') then
                operation = 'unlock'
                if (i < arg_count()) then
                    call get_arg(i+1, image_id)
                end if
            else if (trim(arg) == '--publish') then
                operation = 'publish'
                if (i < arg_count()) then
                    call get_arg(i+1, image_id)
                end if
            else if (trim(arg) == '--source-type') then
                if (i < arg_count()) then
                    call get_arg(i+1, source_type)
                end if
            else if (trim(arg) == '--visibility') then
                operation = 'visibility'
                if (i < arg_count()) then
                    call get_arg(i+1, image_id)
                end if
                if (i+1 < arg_count()) then
                    call get_arg(i+2, visibility_mode)
                end if
            else if (trim(arg) == '--spawn') then
                operation = 'spawn'
                if (i < arg_count()) then
                    call get_arg(i+1, image_id)
                end if
            else if (trim(arg) == '--clone') then
                operation = 'clone'
                if (i < arg_count()) then
                    call get_arg(i+1, image_id)
                end if
            else if (trim(arg) == '--name') then
                if (i < arg_count()) then
                    call get_arg(i+1, name)
                end if
            else if (trim(arg) == '--ports') then
                if (i < arg_count()) then
                    call get_arg(i+1, ports)
                end if
            end if
        end do

        if (trim(operation) == 'list') then
            write(full_cmd, '(20A)') &
                'TS=$(date +%s); ', &
                'SIG=$(printf %s "$TS:GET:/images:" | sh -c "$UNSB_HMAC"); ', &
                'curl -s -X GET $UN_API/images ', &
                '-H "Authorization: Bearer ', trim(public_key), '" ', &
                '-H "X-Timestamp: $TS" -H "X-Signature: $SIG" | jq .'
            call execute_command_line(trim(full_cmd), wait=.true.)
        else if (trim(operation) == 'info' .and. len_trim(image_id) > 0) then
            write(full_cmd, '(20A)') &
                'TS=$(date +%s); ', &
                'SIG=$(printf %s "$TS:GET:/images/', trim(image_id), ':" | sh -c "$UNSB_HMAC"); ', &
                'curl -s -X GET $UN_API/images/', trim(image_id), ' ', &
                '-H "Authorization: Bearer ', trim(public_key), '" ', &
                '-H "X-Timestamp: $TS" -H "X-Signature: $SIG" | jq .'
            call execute_command_line(trim(full_cmd), wait=.true.)
        else if (trim(operation) == 'delete' .and. len_trim(image_id) > 0) then
            write(full_cmd, '(50A)') &
                'TS=$(date +%s); ', &
                'SIG=$(printf %s "$TS:DELETE:/images/', trim(image_id), ':" | sh -c "$UNSB_HMAC"); ', &
                'RESP=$(curl -s -w "\n%{http_code}" -X DELETE $UN_API/images/', trim(image_id), ' ', &
                '-H "Authorization: Bearer ', trim(public_key), '" ', &
                '-H "X-Timestamp: $TS" -H "X-Signature: $SIG"); ', &
                'HTTP_CODE=$(echo "$RESP" | tail -n1); ', &
                'BODY=$(echo "$RESP" | sed ''$d''); ', &
                'if [ "$HTTP_CODE" = "428" ]; then ', &
                'CHALLENGE_ID=$(echo "$BODY" | jq -r ".challenge_id // empty"); ', &
                'echo -e "\x1b[33mConfirmation required. Check your email for a one-time code.\x1b[0m" >&2; ', &
                'echo -n "Enter OTP: " >&2; read OTP; ', &
                'if [ -z "$OTP" ]; then echo -e "\x1b[31mError: Operation cancelled\x1b[0m" >&2; exit 1; fi; ', &
                'TS2=$(date +%s); ', &
                'SIG2=$(printf %s "$TS2:DELETE:/images/', trim(image_id), ':" | sh -c "$UNSB_HMAC"); ', &
                'curl -s -X DELETE $UN_API/images/', trim(image_id), ' ', &
                '-H "Authorization: Bearer ', trim(public_key), '" ', &
                '-H "X-Timestamp: $TS2" -H "X-Signature: $SIG2" ', &
                '-H "X-Sudo-OTP: $OTP" -H "X-Sudo-Challenge: $CHALLENGE_ID" >/dev/null && ', &
                'echo -e "\x1b[32mImage deleted: ', trim(image_id), '\x1b[0m"; ', &
                'elif [ "$HTTP_CODE" = "200" ] || [ "$HTTP_CODE" = "204" ]; then ', &
                'echo -e "\x1b[32mImage deleted: ', trim(image_id), '\x1b[0m"; ', &
                'else echo -e "\x1b[31mError: HTTP $HTTP_CODE\x1b[0m" >&2; echo "$BODY" >&2; exit 1; fi'
            call execute_command_line(trim(full_cmd), wait=.true.)
        else if (trim(operation) == 'lock' .and. len_trim(image_id) > 0) then
            write(full_cmd, '(20A)') &
                'TS=$(date +%s); ', &
                'SIG=$(printf %s "$TS:POST:/images/', trim(image_id), '/lock:" | sh -c "$UNSB_HMAC"); ', &
                'curl -s -X POST $UN_API/images/', trim(image_id), '/lock ', &
                '-H "Authorization: Bearer ', trim(public_key), '" ', &
                '-H "X-Timestamp: $TS" -H "X-Signature: $SIG" >/dev/null; ', &
                'echo -e "\x1b[32mImage locked: ', trim(image_id), '\x1b[0m"'
            call execute_command_line(trim(full_cmd), wait=.true.)
        else if (trim(operation) == 'unlock' .and. len_trim(image_id) > 0) then
            write(full_cmd, '(50A)') &
                'TS=$(date +%s); BODY="{}"; ', &
                'SIG=$(printf %s "$TS:POST:/images/', trim(image_id), '/unlock:$BODY" | sh -c "$UNSB_HMAC"); ', &
                'RESP=$(curl -s -w "\n%{http_code}" -X POST $UN_API/images/', trim(image_id), '/unlock ', &
                '-H "Content-Type: application/json" ', &
                '-H "Authorization: Bearer ', trim(public_key), '" ', &
                '-H "X-Timestamp: $TS" -H "X-Signature: $SIG" -d "$BODY"); ', &
                'HTTP_CODE=$(echo "$RESP" | tail -n1); ', &
                'RESPBODY=$(echo "$RESP" | sed ''$d''); ', &
                'if [ "$HTTP_CODE" = "428" ]; then ', &
                'CHALLENGE_ID=$(echo "$RESPBODY" | jq -r ".challenge_id // empty"); ', &
                'echo -e "\x1b[33mConfirmation required. Check your email for a one-time code.\x1b[0m" >&2; ', &
                'echo -n "Enter OTP: " >&2; read OTP; ', &
                'if [ -z "$OTP" ]; then echo -e "\x1b[31mError: Operation cancelled\x1b[0m" >&2; exit 1; fi; ', &
                'TS2=$(date +%s); ', &
                'SIG2=$(printf %s "$TS2:POST:/images/', trim(image_id), '/unlock:$BODY" | sh -c "$UNSB_HMAC"); ', &
                'curl -s -X POST $UN_API/images/', trim(image_id), '/unlock ', &
                '-H "Content-Type: application/json" ', &
                '-H "Authorization: Bearer ', trim(public_key), '" ', &
                '-H "X-Timestamp: $TS2" -H "X-Signature: $SIG2" ', &
                '-H "X-Sudo-OTP: $OTP" -H "X-Sudo-Challenge: $CHALLENGE_ID" -d "$BODY" >/dev/null && ', &
                'echo -e "\x1b[32mImage unlocked: ', trim(image_id), '\x1b[0m"; ', &
                'elif [ "$HTTP_CODE" = "200" ] || [ "$HTTP_CODE" = "204" ]; then ', &
                'echo -e "\x1b[32mImage unlocked: ', trim(image_id), '\x1b[0m"; ', &
                'else echo -e "\x1b[31mError: HTTP $HTTP_CODE\x1b[0m" >&2; echo "$RESPBODY" >&2; exit 1; fi'
            call execute_command_line(trim(full_cmd), wait=.true.)
        else if (trim(operation) == 'publish' .and. len_trim(image_id) > 0) then
            if (len_trim(source_type) == 0) then
                write(0, '(A)') 'Error: --source-type required (service or snapshot)'
                stop 1
            end if
            if (len_trim(name) > 0) then
                write(full_cmd, '(25A)') &
                    'BODY=''{"source_type":"', trim(source_type), '","source_id":"', trim(image_id), '","name":"', trim(name), '"}''; ', &
                    'TS=$(date +%s); ', &
                    'SIG=$(printf %s "$TS:POST:/images/publish:$BODY" | sh -c "$UNSB_HMAC"); ', &
                    'curl -s -X POST $UN_API/images/publish ', &
                    '-H "Content-Type: application/json" ', &
                    '-H "Authorization: Bearer ', trim(public_key), '" ', &
                    '-H "X-Timestamp: $TS" -H "X-Signature: $SIG" ', &
                    '-d "$BODY" | jq .; ', &
                    'echo -e "\x1b[32mImage published\x1b[0m"'
            else
                write(full_cmd, '(25A)') &
                    'BODY=''{"source_type":"', trim(source_type), '","source_id":"', trim(image_id), '"}''; ', &
                    'TS=$(date +%s); ', &
                    'SIG=$(printf %s "$TS:POST:/images/publish:$BODY" | sh -c "$UNSB_HMAC"); ', &
                    'curl -s -X POST $UN_API/images/publish ', &
                    '-H "Content-Type: application/json" ', &
                    '-H "Authorization: Bearer ', trim(public_key), '" ', &
                    '-H "X-Timestamp: $TS" -H "X-Signature: $SIG" ', &
                    '-d "$BODY" | jq .; ', &
                    'echo -e "\x1b[32mImage published\x1b[0m"'
            end if
            call execute_command_line(trim(full_cmd), wait=.true.)
        else if (trim(operation) == 'visibility' .and. len_trim(image_id) > 0 .and. len_trim(visibility_mode) > 0) then
            write(full_cmd, '(25A)') &
                'BODY=''{"visibility":"', trim(visibility_mode), '"}''; ', &
                'TS=$(date +%s); ', &
                'SIG=$(printf %s "$TS:POST:/images/', trim(image_id), '/visibility:$BODY" | sh -c "$UNSB_HMAC"); ', &
                'curl -s -X POST $UN_API/images/', trim(image_id), '/visibility ', &
                '-H "Content-Type: application/json" ', &
                '-H "Authorization: Bearer ', trim(public_key), '" ', &
                '-H "X-Timestamp: $TS" -H "X-Signature: $SIG" ', &
                '-d "$BODY" >/dev/null; ', &
                'echo -e "\x1b[32mImage visibility set to ', trim(visibility_mode), '\x1b[0m"'
            call execute_command_line(trim(full_cmd), wait=.true.)
        else if (trim(operation) == 'spawn' .and. len_trim(image_id) > 0) then
            if (len_trim(name) > 0 .and. len_trim(ports) > 0) then
                write(full_cmd, '(25A)') &
                    'BODY=''{"name":"', trim(name), '","ports":[', trim(ports), ']}''; ', &
                    'TS=$(date +%s); ', &
                    'SIG=$(printf %s "$TS:POST:/images/', trim(image_id), '/spawn:$BODY" | sh -c "$UNSB_HMAC"); ', &
                    'curl -s -X POST $UN_API/images/', trim(image_id), '/spawn ', &
                    '-H "Content-Type: application/json" ', &
                    '-H "Authorization: Bearer ', trim(public_key), '" ', &
                    '-H "X-Timestamp: $TS" -H "X-Signature: $SIG" ', &
                    '-d "$BODY" | jq .; ', &
                    'echo -e "\x1b[32mService spawned from image\x1b[0m"'
            else if (len_trim(name) > 0) then
                write(full_cmd, '(25A)') &
                    'BODY=''{"name":"', trim(name), '"}''; ', &
                    'TS=$(date +%s); ', &
                    'SIG=$(printf %s "$TS:POST:/images/', trim(image_id), '/spawn:$BODY" | sh -c "$UNSB_HMAC"); ', &
                    'curl -s -X POST $UN_API/images/', trim(image_id), '/spawn ', &
                    '-H "Content-Type: application/json" ', &
                    '-H "Authorization: Bearer ', trim(public_key), '" ', &
                    '-H "X-Timestamp: $TS" -H "X-Signature: $SIG" ', &
                    '-d "$BODY" | jq .; ', &
                    'echo -e "\x1b[32mService spawned from image\x1b[0m"'
            else
                write(full_cmd, '(25A)') &
                    'BODY=''{}''; ', &
                    'TS=$(date +%s); ', &
                    'SIG=$(printf %s "$TS:POST:/images/', trim(image_id), '/spawn:$BODY" | sh -c "$UNSB_HMAC"); ', &
                    'curl -s -X POST $UN_API/images/', trim(image_id), '/spawn ', &
                    '-H "Content-Type: application/json" ', &
                    '-H "Authorization: Bearer ', trim(public_key), '" ', &
                    '-H "X-Timestamp: $TS" -H "X-Signature: $SIG" ', &
                    '-d "$BODY" | jq .; ', &
                    'echo -e "\x1b[32mService spawned from image\x1b[0m"'
            end if
            call execute_command_line(trim(full_cmd), wait=.true.)
        else if (trim(operation) == 'clone' .and. len_trim(image_id) > 0) then
            if (len_trim(name) > 0) then
                write(full_cmd, '(25A)') &
                    'BODY=''{"name":"', trim(name), '"}''; ', &
                    'TS=$(date +%s); ', &
                    'SIG=$(printf %s "$TS:POST:/images/', trim(image_id), '/clone:$BODY" | sh -c "$UNSB_HMAC"); ', &
                    'curl -s -X POST $UN_API/images/', trim(image_id), '/clone ', &
                    '-H "Content-Type: application/json" ', &
                    '-H "Authorization: Bearer ', trim(public_key), '" ', &
                    '-H "X-Timestamp: $TS" -H "X-Signature: $SIG" ', &
                    '-d "$BODY" | jq .; ', &
                    'echo -e "\x1b[32mImage cloned\x1b[0m"'
            else
                write(full_cmd, '(25A)') &
                    'BODY=''{}''; ', &
                    'TS=$(date +%s); ', &
                    'SIG=$(printf %s "$TS:POST:/images/', trim(image_id), '/clone:$BODY" | sh -c "$UNSB_HMAC"); ', &
                    'curl -s -X POST $UN_API/images/', trim(image_id), '/clone ', &
                    '-H "Content-Type: application/json" ', &
                    '-H "Authorization: Bearer ', trim(public_key), '" ', &
                    '-H "X-Timestamp: $TS" -H "X-Signature: $SIG" ', &
                    '-d "$BODY" | jq .; ', &
                    'echo -e "\x1b[32mImage cloned\x1b[0m"'
            end if
            call execute_command_line(trim(full_cmd), wait=.true.)
        else
            write(0, '(A)') 'Error: Use --list, --info ID, --delete ID, --lock ID, --unlock ID, --publish ID, --visibility ID MODE, --spawn ID, or --clone ID'
            stop 1
        end if
    end subroutine handle_image

    subroutine handle_key()
        character(len=4096) :: full_cmd
        character(len=256) :: arg, otp_code
        character(len=1024) :: public_key, secret_key, source_csv
        integer :: i, stat
        logical :: extend_mode, rotate_mode
        character(len=32) :: portal_base
        character(len=:), allocatable :: script
        character(len=1), parameter :: nl = achar(10)

        portal_base = 'https://unsandbox.com'
        extend_mode = .false.
        rotate_mode = .false.
        otp_code = ''

        ! Check for --extend, --rotate, --otp CODE
        do i = 2, arg_count()
            call get_arg(i, arg)
            if (trim(arg) == '--extend') then
                extend_mode = .true.
            else if (trim(arg) == '--rotate') then
                rotate_mode = .true.
            else if (trim(arg) == '--otp' .and. i < arg_count()) then
                call get_arg(i + 1, otp_code)
            end if
        end do

        ! Get API key
        call get_credentials(public_key, secret_key, stat, account_index, source_csv)
        if (stat /= 0) then
            write(0, '(A)') 'Error: No credentials found'
            stop 1
        end if

        if (rotate_mode) then
            call handle_key_rotate(public_key, secret_key, source_csv, otp_code)
            return
        end if

        ! Key validation: signed POST $UN_API/keys/validate, empty body (get_credentials
        ! already exported UNSB_SK/UNSB_HMAC for the shell). Display per CLI_SPEC.md
        ! "`un key` output": the API returns no public key, so we print ours.
        if (extend_mode) then
            write(full_cmd, '(*(A))') 'echo "Opening browser to extend key..."; xdg-open "', &
                trim(portal_base), '/keys/extend?pk=', trim(public_key), '" 2>/dev/null &'
            call execute_command_line(trim(full_cmd), wait=.true., exitstat=stat)
            return
        end if

        script = 'PK=''' // trim(public_key) // '''' // nl // &
            'TS=$(date +%s); SIG=$(printf %s "$TS:POST:/keys/validate:" | sh -c "$UNSB_HMAC")' // nl // &
            'raw=$(curl -s -w ''\n%{http_code}'' -X POST "$UN_API/keys/validate" -H "Authorization: Bea' // &
            'rer $PK" -H "X-Timestamp: $TS" -H "X-Signature: $SIG") || { echo ''Error: request to /keys' // &
            '/validate failed'' >&2; exit 1; }' // nl // &
            'code=$(printf ''%s\n'' "$raw" | tail -n 1); resp=$(printf ''%s\n'' "$raw" | sed ''$d'')' // nl // &
            'case "$code" in 200) st=$(printf ''%s'' "$resp" | jq -r ''.status // (if .valid then "acti' // &
            've" else "invalid" end)''); reason="status $st";; 401) reason=$(printf ''%s'' "$resp" | jq' // &
            ' -r ''.error // "unauthorized"''); case "$reason" in :expired|expired) st=expired;; :suspe' // &
            'nded|suspended) st=suspended;; *) st=invalid;; esac;; *) echo "Error: HTTP $code" >&2; exi' // &
            't 4;; esac' // nl // &
            'case "$st" in active) ;; expired) printf ''\033[31mExpired\033[0m\nPublic Key: %s\n\033[33' // &
            'mTo renew: Visit https://unsandbox.com/keys/extend\033[0m\n'' "$PK"; exit 3;; suspended) p' // &
            'rintf ''\033[31mSuspended\033[0m\nPublic Key: %s\n'' "$PK"; exit 3;; *) printf ''\033[31mI' // &
            'nvalid\033[0m\nPublic Key: %s\nReason: %s\n'' "$PK" "$reason"; exit 3;; esac' // nl // &
            'tier=$(printf ''%s'' "$resp" | jq -r ''.tier // "N/A"''); status=$(printf ''%s'' "$resp" |' // &
            ' jq -r ''.status // "N/A"''); vt=$(printf ''%s'' "$resp" | jq -r ''.valid_through // empty' // &
            ' | floor''); exp=N/A; rem=N/A' // nl // &
            'if [ -n "$vt" ]; then s=$((vt / 1000)); exp=$(date -u -d "@$s" +%Y-%m-%dT%H:%M:%SZ 2>/dev/' // &
            'null || date -u -r "$s" +%Y-%m-%dT%H:%M:%SZ); l=$((s - $(date +%s))); if [ "$l" -le 0 ]; t' // &
            'hen rem=expired; else rem="$((l / 86400))d $((l % 86400 / 3600))h $((l % 3600 / 60))m"; fi' // &
            '; fi' // nl // &
            'printf ''\033[32mValid\033[0m\nPublic Key: %s\nTier: %s\nStatus: %s\nExpires: %s\nTime Rem' // &
            'aining: %s\n'' "$PK" "$tier" "$status" "$exp" "$rem"' // nl
        call execute_command_line(script, wait=.true., exitstat=stat)
        if (stat /= 0) call exit(stat)
    end subroutine handle_key

    !--------------------------------------------------------------------------
    ! Subroutine: handle_key_rotate
    ! Description: un key --rotate [--otp CODE] -- rotate this key's secret
    !   (sudo OTP), then update the accounts.csv the keys came from. Flow per
    !   CLI_SPEC.md. Secrets reach the helper shell through the environment
    !   only; the script text holds none (HMAC computed with bash builtins).
    !--------------------------------------------------------------------------
    subroutine handle_key_rotate(public_key, secret_key, source_csv, otp_code)
        character(len=*), intent(in) :: public_key, secret_key, source_csv, otp_code
        character(len=:), allocatable :: script
        character(len=1), parameter :: nl = achar(10)
        integer :: stat

        script = &
            'A=${UNSANDBOX_API_URL:-https://api.unsandbox.com}; while [ "${A%/}" != "$A" ]; do A=${A%' // &
            '/}; done' // nl // &
            'P=/keys/self/rotate-secret; D="$HOME/.unsandbox"' // nl // &
            'if command -v sha256sum >/dev/null 2>&1; then S=sha256sum; else S="shasum -a 256"; fi' // nl // &
            'CF="$D/rotate-$(printf %s "$UN_PK" | $S | cut -c1-16).challenge"' // nl // &
            'RB=$(mktemp) || exit 1; trap ''rm -f "$RB"'' EXIT' // nl // &
            'hmac() { local k=$1 i c ip= op= ih= x' // nl // &
            '  for ((i=0;i<64;i++)); do if ((i<${#k})); then printf -v c %d "''${k:i:1}"; else c=0; f' // &
            'i' // nl // &
            '    printf -v ip ''%s\\x%02x'' "$ip" $((c^54)); printf -v op ''%s\\x%02x'' "$op" $((c^92' // &
            ')); done' // nl // &
            '  ih=$( { printf "$ip"; printf %s "$2"; } | $S | cut -c1-64)' // nl // &
            '  for ((i=0;i<64;i+=2)); do x="$x\\x${ih:i:2}"; done' // nl // &
            '  { printf "$op"; printf "$x"; } | $S | cut -c1-64; }' // nl // &
            'post() { local ts sig; ts=$(date +%s); sig=$(hmac "$UN_SK" "$ts:POST:$P:{}")' // nl // &
            '  { printf ''url = "%s"\nrequest = "POST"\n'' "$A$P"' // nl // &
            '    printf ''header = "Content-Type: application/json"\nheader = "Authorization: Bearer ' // &
            '%s"\n'' "$UN_PK"' // nl // &
            '    printf ''header = "X-Timestamp: %s"\nheader = "X-Signature: %s"\n'' "$ts" "$sig"' // nl // &
            '    if [ -n "$1" ]; then printf ''header = "X-Sudo-OTP: %s"\nheader = "X-Sudo-Challenge:' // &
            ' %s"\n'' "$1" "$2"; fi' // nl // &
            '    printf ''data = "{}"\n''; } | curl -s -K - -o "$RB" -w ''%{http_code}''; }' // nl // &
            'if [ -n "${UN_OTP:-}" ]; then' // nl // &
            '  CH=$(cat "$CF" 2>/dev/null) || { echo "Error: no rotation in progress; run: un key --r' // &
            'otate" >&2; exit 2; }' // nl // &
            '  CODE=$(post "$UN_OTP" "$CH"); rm -f "$CF"' // nl // &
            'else' // nl // &
            '  CODE=$(post)' // nl // &
            '  if [ "$CODE" = 428 ]; then CH=$(jq -r ''.challenge_id // empty'' "$RB")' // nl // &
            '    if [ -t 0 ]; then echo "One-time code emailed to the key owner." >&2; printf ''Code:' // &
            ' '' >&2; read -r OTP' // nl // &
            '      [ -n "$OTP" ] || { echo "Error: cancelled" >&2; exit 1; }; CODE=$(post "$OTP" "$CH' // &
            '")' // nl // &
            '    else (umask 077; mkdir -p "$D" && printf %s "$CH" > "$CF") && chmod 600 "$CF" || exi' // &
            't 1' // nl // &
            '      echo "One-time code emailed to the key owner. Finish with: un key --rotate --otp C' // &
            'ODE"; exit 0; fi; fi' // nl // &
            'fi' // nl // &
            'if [ "$CODE" != 200 ]; then' // nl // &
            '  if [ "$CODE" = 403 ] && [ "$(jq -r ''.error // empty'' "$RB" 2>/dev/null)" = email_req' // &
            'uired ]; then' // nl // &
            '    echo "Error: this key has no email; add one or use POST /keys/recover" >&2; exit 3; ' // &
            'fi' // nl // &
            '  [ "$CODE" = 401 ] && { echo "Error: authentication failed" >&2; exit 3; }' // nl // &
            '  M=$(jq -r ''.message // .error // empty'' "$RB" 2>/dev/null); echo "Error: ${M:-HTTP $' // &
            'CODE}" >&2; exit 4; fi' // nl // &
            'NS=$(jq -r ''.secret // empty'' "$RB"); [ -n "$NS" ] || { echo "Error: API returned no s' // &
            'ecret" >&2; exit 4; }' // nl // &
            'if [ -n "${UN_CSV:-}" ] && [ -f "$UN_CSV" ] && T=$(mktemp "$(dirname "$UN_CSV")/.account' // &
            's.csv.XXXXXX"); then' // nl // &
            '  chmod 600 "$T"; NS="$NS" awk -F, -v OFS=, -v pk="$UN_PK" ''BEGIN{d=0} { t=$1; gsub(/^[' // &
            ' \t]+|[ \t\r]+$/,"",t)' // nl // &
            '    if (!d && $0 !~ /^[ \t]*(#|$)/ && NF>=2 && t==pk) { $2=ENVIRON["NS"]; d=1 } print } ' // &
            'END{exit d?0:1}'' "$UN_CSV" > "$T" \' // nl // &
            '    && mv -f "$T" "$UN_CSV" && { echo "Secret rotated. Updated $UN_CSV (public key $UN_P' // &
            'K)."; exit 0; }; rm -f "$T"; fi' // nl // &
            'echo "New secret (shown once, store it now): $NS"' // nl // &
            'echo "Warning: UNSANDBOX_SECRET_KEY / -k still hold the old, now invalid, secret."' // nl

        call set_env('UN_PK', trim(public_key))
        call set_env('UN_SK', trim(secret_key))
        call set_env('UN_CSV', trim(source_csv))
        call set_env('UN_OTP', trim(otp_code))
        call set_env('UN_ROTATE_SH', script)
        stat = 1
        call execute_command_line('bash -c "$UN_ROTATE_SH"', wait=.true., exitstat=stat)
        call set_env('UN_SK', '')
        if (stat /= 0) stop stat, quiet=.true.
    end subroutine handle_key_rotate

    subroutine handle_languages()
        character(len=8192) :: full_cmd
        character(len=256) :: arg
        character(len=1024) :: public_key, secret_key
        integer :: i, stat
        logical :: json_mode

        json_mode = .false.

        ! Check for --json flag
        do i = 2, arg_count()
            call get_arg(i, arg)
            if (trim(arg) == '--json') then
                json_mode = .true.
            end if
        end do

        ! Get API keys
        call get_credentials(public_key, secret_key, stat, account_index)
        if (stat /= 0) then
            write(0, '(A)') 'Error: No credentials found'
            stop 1
        end if

        if (json_mode) then
            ! Output as JSON array with caching
            write(full_cmd, '(40A)') &
                'CACHE_TTL=3600; ', &
                'CACHE_FILE="$HOME/.unsandbox/languages.json"; ', &
                'if [ -f "$CACHE_FILE" ]; then ', &
                'CACHE_TS=$(jq -r ".timestamp // 0" "$CACHE_FILE" 2>/dev/null); ', &
                'CURRENT_TS=$(date +%s); ', &
                'AGE=$((CURRENT_TS - CACHE_TS)); ', &
                'if [ $AGE -lt $CACHE_TTL ]; then ', &
                'jq -c ".languages" "$CACHE_FILE"; exit 0; fi; fi; ', &
                'TS=$(date +%s); ', &
                'SIG=$(printf %s "$TS:GET:/languages:" | sh -c "$UNSB_HMAC"); ', &
                'RESP=$(curl -s -X GET $UN_API/languages ', &
                '-H "Authorization: Bearer ', trim(public_key), '" ', &
                '-H "X-Timestamp: $TS" ', &
                '-H "X-Signature: $SIG"); ', &
                'LANGS=$(echo "$RESP" | jq -c ".languages // []"); ', &
                'mkdir -p "$HOME/.unsandbox"; ', &
                'echo "{\"languages\":$LANGS,\"timestamp\":$(date +%s)}" > "$CACHE_FILE"; ', &
                'echo "$LANGS"'
        else
            ! Output one language per line with caching
            write(full_cmd, '(40A)') &
                'CACHE_TTL=3600; ', &
                'CACHE_FILE="$HOME/.unsandbox/languages.json"; ', &
                'if [ -f "$CACHE_FILE" ]; then ', &
                'CACHE_TS=$(jq -r ".timestamp // 0" "$CACHE_FILE" 2>/dev/null); ', &
                'CURRENT_TS=$(date +%s); ', &
                'AGE=$((CURRENT_TS - CACHE_TS)); ', &
                'if [ $AGE -lt $CACHE_TTL ]; then ', &
                'jq -r ".languages[]" "$CACHE_FILE"; exit 0; fi; fi; ', &
                'TS=$(date +%s); ', &
                'SIG=$(printf %s "$TS:GET:/languages:" | sh -c "$UNSB_HMAC"); ', &
                'RESP=$(curl -s -X GET $UN_API/languages ', &
                '-H "Authorization: Bearer ', trim(public_key), '" ', &
                '-H "X-Timestamp: $TS" ', &
                '-H "X-Signature: $SIG"); ', &
                'LANGS=$(echo "$RESP" | jq -c ".languages // []"); ', &
                'mkdir -p "$HOME/.unsandbox"; ', &
                'echo "{\"languages\":$LANGS,\"timestamp\":$(date +%s)}" > "$CACHE_FILE"; ', &
                'echo "$RESP" | jq -r ".languages[]"'
        end if

        call execute_command_line(trim(full_cmd), wait=.true., exitstat=stat)
    end subroutine handle_languages

end program unsandbox_cli
