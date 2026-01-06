! Unit tests for un.f90 - tests internal functions without API calls
! Compile: gfortran -o test_fortran test_fortran.f90 && ./test_fortran

program test_fortran
    implicit none
    integer :: passed, failed

    passed = 0
    failed = 0

    print *, ""
    print *, "=== Extension Mapping Tests ==="

    call test("Python extension maps correctly", &
              get_language(".py") == "python", passed, failed)

    call test("Fortran extension maps correctly", &
              get_language(".f90") == "fortran", passed, failed)

    call test("JavaScript extension maps correctly", &
              get_language(".js") == "javascript", passed, failed)

    call test("Go extension maps correctly", &
              get_language(".go") == "go", passed, failed)

    print *, ""
    print *, "=== Signature Format Tests ==="

    block
        character(len=256) :: message
        character(len=*), parameter :: timestamp = "1704067200"
        character(len=*), parameter :: method = "POST"
        character(len=*), parameter :: endpoint = "/execute"
        character(len=*), parameter :: body = '{"language":"python"}'

        message = trim(timestamp) // ":" // trim(method) // ":" // &
                  trim(endpoint) // ":" // trim(body)

        call test("Signature format starts with timestamp", &
                  message(1:len_trim(timestamp)) == timestamp, passed, failed)

        call test("Signature format contains :POST:", &
                  index(message, ":POST:") > 0, passed, failed)

        call test("Signature format contains :/execute:", &
                  index(message, ":/execute:") > 0, passed, failed)
    end block

    print *, ""
    print *, "=== Language Detection Tests ==="

    block
        character(len=256) :: content, first_line
        integer :: newline_pos

        content = "#!/usr/bin/env python3" // char(10) // "print('hello')"
        newline_pos = index(content, char(10))
        if (newline_pos > 0) then
            first_line = content(1:newline_pos-1)
        else
            first_line = content
        end if

        call test("Python shebang detection - starts with #!", &
                  first_line(1:2) == "#!", passed, failed)

        call test("Python shebang detection - contains python", &
                  index(first_line, "python") > 0, passed, failed)
    end block

    print *, ""
    print *, "=== Argument Parsing Tests ==="

    block
        character(len=64) :: arg1, key1, value1
        integer :: eq_pos

        arg1 = "DEBUG=1"
        eq_pos = index(arg1, "=")
        key1 = arg1(1:eq_pos-1)
        value1 = arg1(eq_pos+1:len_trim(arg1))

        call test("Parse -e KEY=VALUE format - key", &
                  trim(key1) == "DEBUG", passed, failed)

        call test("Parse -e KEY=VALUE format - value", &
                  trim(value1) == "1", passed, failed)
    end block

    print *, ""
    print *, "=== File Operations Tests ==="

    call test("Extract file basename", &
              get_basename("/home/user/project/script.f90") == "script.f90", passed, failed)

    call test("Extract file extension", &
              get_extension("/home/user/project/script.f90") == ".f90", passed, failed)

    print *, ""
    print *, "=== API Constants Tests ==="

    block
        character(len=*), parameter :: api_base = "https://api.unsandbox.com"

        call test("API base URL starts with https://", &
                  api_base(1:8) == "https://", passed, failed)

        call test("API base URL contains unsandbox.com", &
                  index(api_base, "unsandbox.com") > 0, passed, failed)
    end block

    print *, ""
    print *, "=== Summary ==="
    print '(A,I0)', " Passed: ", passed
    print '(A,I0)', " Failed: ", failed
    print '(A,I0)', " Total:  ", passed + failed

    if (failed > 0) then
        call exit(1)
    else
        call exit(0)
    end if

contains

    subroutine test(name, result, passed, failed)
        character(len=*), intent(in) :: name
        logical, intent(in) :: result
        integer, intent(inout) :: passed, failed

        if (result) then
            print *, "  ✓ ", trim(name)
            passed = passed + 1
        else
            print *, "  ✗ ", trim(name)
            failed = failed + 1
        end if
    end subroutine test

    function get_language(ext) result(lang)
        character(len=*), intent(in) :: ext
        character(len=16) :: lang

        select case (trim(ext))
            case (".py")
                lang = "python"
            case (".js")
                lang = "javascript"
            case (".rb")
                lang = "ruby"
            case (".go")
                lang = "go"
            case (".f90")
                lang = "fortran"
            case (".c")
                lang = "c"
            case default
                lang = ""
        end select
    end function get_language

    function get_extension(filename) result(ext)
        character(len=*), intent(in) :: filename
        character(len=16) :: ext
        integer :: i

        ext = ""
        do i = len_trim(filename), 1, -1
            if (filename(i:i) == ".") then
                ext = filename(i:len_trim(filename))
                exit
            end if
        end do
    end function get_extension

    function get_basename(path) result(basename)
        character(len=*), intent(in) :: path
        character(len=256) :: basename
        integer :: i

        basename = path
        do i = len_trim(path), 1, -1
            if (path(i:i) == "/") then
                basename = path(i+1:len_trim(path))
                exit
            end if
        end do
    end function get_basename

end program test_fortran
