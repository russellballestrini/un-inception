! PUBLIC DOMAIN - NO LICENSE, NO WARRANTY
!
! This is free public domain software for the public good of a permacomputer hosted
! at permacomputer.com - an always-on computer by the people, for the people. One
! which is durable, easy to repair, and distributed like tap water for machine
! learning intelligence.
!
! The permacomputer is community-owned infrastructure optimized around four values:
!
!   TRUTH    - Source code must be open source & freely distributed
!   FREEDOM  - Voluntary participation without corporate control
!   HARMONY  - Systems operating with minimal waste that self-renew
!   LOVE     - Individual rights protected while fostering cooperation
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
    logical :: list_flag, is_session, is_service

    ! Initialize
    subcommand = ''
    list_flag = .false.
    is_session = .false.
    is_service = .false.
    session_id = ''
    service_id = ''

    ! Get command line arguments count
    nargs = command_argument_count()
    if (nargs < 1) then
        write(0, '(A)') 'Usage: un.f90 [options] <source_file>'
        write(0, '(A)') '       un.f90 session [options]'
        write(0, '(A)') '       un.f90 service [options]'
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
    else
        ! Default execute command
        filename = trim(arg)
        call handle_execute(filename)
        stop 0
    end if

contains

    subroutine handle_execute(fname)
        character(len=*), intent(in) :: fname
        character(len=2048) :: full_cmd
        character(len=1024) :: env_opts, file_opts, net_opt
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

        ! Get API key
        call get_environment_variable('UNSANDBOX_API_KEY', api_key, status=stat)
        if (stat /= 0 .or. len_trim(api_key) == 0) then
            write(0, '(A)') 'Error: UNSANDBOX_API_KEY environment variable not set'
            stop 1
        end if

        ! Parse additional arguments (simple version - only support basic flags)
        env_opts = ''
        file_opts = ''
        net_opt = ''
        artifacts = .false.

        ! Build curl command
        write(full_cmd, '(20A)') &
            'curl -s -X POST https://api.unsandbox.com/execute ', &
            '-H "Content-Type: application/json" ', &
            '-H "Authorization: Bearer ', trim(api_key), '" ', &
            '--data-binary @- -o /tmp/unsandbox_resp.json ', &
            '< <(jq -Rs ''{language: "', trim(language), '", code: .}'' ', &
            '< "', trim(fname), '"); ', &
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
        character(len=2048) :: full_cmd
        character(len=256) :: arg, session_id
        integer :: i, stat
        logical :: list_mode, kill_mode

        list_mode = .false.
        kill_mode = .false.
        session_id = ''

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
            end if
        end do

        ! Get API key
        call get_environment_variable('UNSANDBOX_API_KEY', api_key, status=stat)
        if (stat /= 0 .or. len_trim(api_key) == 0) then
            write(0, '(A)') 'Error: UNSANDBOX_API_KEY not set'
            stop 1
        end if

        if (list_mode) then
            ! List sessions
            write(full_cmd, '(10A)') &
                'curl -s -X GET https://api.unsandbox.com/sessions ', &
                '-H "Authorization: Bearer ', trim(api_key), '" | ', &
                'jq -r ''.sessions[] | "\(.id) \(.shell) \(.status) \(.created_at)"'' ', &
                '2>/dev/null || echo "No active sessions"'
            call execute_command_line(trim(full_cmd), wait=.true.)
        else if (kill_mode .and. len_trim(session_id) > 0) then
            ! Kill session
            write(full_cmd, '(10A)') &
                'curl -s -X DELETE https://api.unsandbox.com/sessions/', &
                trim(session_id), ' ', &
                '-H "Authorization: Bearer ', trim(api_key), '" >/dev/null && ', &
                'echo -e "\x1b[32mSession terminated: ', trim(session_id), '\x1b[0m"'
            call execute_command_line(trim(full_cmd), wait=.true.)
        else
            write(0, '(A)') 'Error: Use --list or --kill ID'
            stop 1
        end if
    end subroutine handle_session

    subroutine handle_service()
        character(len=2048) :: full_cmd
        character(len=256) :: arg, service_id, operation
        integer :: i, stat
        logical :: list_mode

        list_mode = .false.
        operation = ''
        service_id = ''

        ! Parse service arguments
        do i = 2, command_argument_count()
            call get_command_argument(i, arg)
            if (trim(arg) == '-l' .or. trim(arg) == '--list') then
                list_mode = .true.
            else if (trim(arg) == '--info') then
                operation = 'info'
                if (i+1 <= command_argument_count()) then
                    call get_command_argument(i+1, service_id)
                end if
            else if (trim(arg) == '--logs') then
                operation = 'logs'
                if (i+1 <= command_argument_count()) then
                    call get_command_argument(i+1, service_id)
                end if
            else if (trim(arg) == '--sleep') then
                operation = 'sleep'
                if (i+1 <= command_argument_count()) then
                    call get_command_argument(i+1, service_id)
                end if
            else if (trim(arg) == '--wake') then
                operation = 'wake'
                if (i+1 <= command_argument_count()) then
                    call get_command_argument(i+1, service_id)
                end if
            else if (trim(arg) == '--destroy') then
                operation = 'destroy'
                if (i+1 <= command_argument_count()) then
                    call get_command_argument(i+1, service_id)
                end if
            end if
        end do

        ! Get API key
        call get_environment_variable('UNSANDBOX_API_KEY', api_key, status=stat)
        if (stat /= 0 .or. len_trim(api_key) == 0) then
            write(0, '(A)') 'Error: UNSANDBOX_API_KEY not set'
            stop 1
        end if

        if (list_mode) then
            ! List services
            write(full_cmd, '(10A)') &
                'curl -s -X GET https://api.unsandbox.com/services ', &
                '-H "Authorization: Bearer ', trim(api_key), '" | ', &
                'jq -r ''.services[] | "\(.id) \(.name) \(.status)"'' ', &
                '2>/dev/null || echo "No services"'
            call execute_command_line(trim(full_cmd), wait=.true.)
        else if (trim(operation) == 'info' .and. len_trim(service_id) > 0) then
            write(full_cmd, '(10A)') &
                'curl -s -X GET https://api.unsandbox.com/services/', &
                trim(service_id), ' ', &
                '-H "Authorization: Bearer ', trim(api_key), '" | jq .'
            call execute_command_line(trim(full_cmd), wait=.true.)
        else if (trim(operation) == 'logs' .and. len_trim(service_id) > 0) then
            write(full_cmd, '(10A)') &
                'curl -s -X GET https://api.unsandbox.com/services/', &
                trim(service_id), '/logs ', &
                '-H "Authorization: Bearer ', trim(api_key), '" | jq -r ".logs"'
            call execute_command_line(trim(full_cmd), wait=.true.)
        else if (trim(operation) == 'sleep' .and. len_trim(service_id) > 0) then
            write(full_cmd, '(10A)') &
                'curl -s -X POST https://api.unsandbox.com/services/', &
                trim(service_id), '/sleep ', &
                '-H "Authorization: Bearer ', trim(api_key), '" >/dev/null && ', &
                'echo -e "\x1b[32mService sleeping: ', trim(service_id), '\x1b[0m"'
            call execute_command_line(trim(full_cmd), wait=.true.)
        else if (trim(operation) == 'wake' .and. len_trim(service_id) > 0) then
            write(full_cmd, '(10A)') &
                'curl -s -X POST https://api.unsandbox.com/services/', &
                trim(service_id), '/wake ', &
                '-H "Authorization: Bearer ', trim(api_key), '" >/dev/null && ', &
                'echo -e "\x1b[32mService waking: ', trim(service_id), '\x1b[0m"'
            call execute_command_line(trim(full_cmd), wait=.true.)
        else if (trim(operation) == 'destroy' .and. len_trim(service_id) > 0) then
            write(full_cmd, '(10A)') &
                'curl -s -X DELETE https://api.unsandbox.com/services/', &
                trim(service_id), ' ', &
                '-H "Authorization: Bearer ', trim(api_key), '" >/dev/null && ', &
                'echo -e "\x1b[32mService destroyed: ', trim(service_id), '\x1b[0m"'
            call execute_command_line(trim(full_cmd), wait=.true.)
        else
            write(0, '(A)') 'Error: Use --list, --info, --logs, --sleep, --wake, or --destroy'
            stop 1
        end if
    end subroutine handle_service

end program unsandbox_cli
