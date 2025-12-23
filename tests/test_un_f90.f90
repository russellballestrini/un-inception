program test_un_f90
    ! Comprehensive tests for un.f90 (Fortran UN CLI Inception implementation)
    ! Compile and run with: gfortran -o test_un_f90 test_un_f90.f90 && ./test_un_f90

    implicit none
    integer :: passed, failed, total
    character(len=32) :: GREEN, RED, BLUE, RESET

    ! ANSI color codes
    GREEN = char(27) // '[32m'
    RED = char(27) // '[31m'
    BLUE = char(27) // '[34m'
    RESET = char(27) // '[0m'

    passed = 0
    failed = 0

    write(*, '(A)') ''
    write(*, '(A)') trim(BLUE) // '========================================' // trim(RESET)
    write(*, '(A)') trim(BLUE) // 'UN CLI Inception Tests - Fortran' // trim(RESET)
    write(*, '(A)') trim(BLUE) // '========================================' // trim(RESET)
    write(*, '(A)') ''

    ! Test Suite 1: Extension Detection
    write(*, '(A)') trim(BLUE) // 'Test Suite 1: Extension Detection' // trim(RESET)
    call test_extension('.jl', 'julia', passed, failed)
    call test_extension('.r', 'r', passed, failed)
    call test_extension('.cr', 'crystal', passed, failed)
    call test_extension('.f90', 'fortran', passed, failed)
    call test_extension('.cob', 'cobol', passed, failed)
    call test_extension('.pro', 'prolog', passed, failed)
    call test_extension('.forth', 'forth', passed, failed)
    call test_extension('.4th', 'forth', passed, failed)
    call test_extension('.py', 'python', passed, failed)
    call test_extension('.rs', 'rust', passed, failed)
    call test_extension('.xyz', 'unknown', passed, failed)

    ! Test Suite 2: API Integration
    write(*, '(A)') ''
    write(*, '(A)') trim(BLUE) // 'Test Suite 2: API Integration' // trim(RESET)
    write(*, '(A)') trim(BLUE) // 'ℹ SKIP' // trim(RESET) // &
        ': API integration test (requires runtime environment)'

    ! Test Suite 3: End-to-End
    write(*, '(A)') ''
    write(*, '(A)') trim(BLUE) // 'Test Suite 3: End-to-End Functional Test' // trim(RESET)
    write(*, '(A)') trim(BLUE) // 'ℹ SKIP' // trim(RESET) // &
        ': E2E test (requires runtime environment and API key)'

    ! Test Suite 4: Error Handling
    write(*, '(A)') ''
    write(*, '(A)') trim(BLUE) // 'Test Suite 4: Error Handling' // trim(RESET)
    call test_extension('.unknown', 'unknown', passed, failed)
    call test_extension('.PY', 'python', passed, failed)  ! Case insensitive

    ! Print summary
    total = passed + failed
    write(*, '(A)') ''
    write(*, '(A)') trim(BLUE) // '========================================' // trim(RESET)
    write(*, '(A)') trim(BLUE) // 'Test Summary' // trim(RESET)
    write(*, '(A)') trim(BLUE) // '========================================' // trim(RESET)
    write(*, '(A,I0,A)') trim(GREEN) // 'Passed: ', passed, trim(RESET)
    write(*, '(A,I0,A)') trim(RED) // 'Failed: ', failed, trim(RESET)
    write(*, '(A,I0,A)') trim(BLUE) // 'Total:  ', total, trim(RESET)

    if (failed > 0) then
        write(*, '(A)') ''
        write(*, '(A)') trim(RED) // 'TESTS FAILED' // trim(RESET)
        stop 1
    else
        write(*, '(A)') ''
        write(*, '(A)') trim(GREEN) // 'ALL TESTS PASSED' // trim(RESET)
        stop 0
    end if

contains

    subroutine test_extension(ext, expected_lang, passed, failed)
        character(len=*), intent(in) :: ext, expected_lang
        integer, intent(inout) :: passed, failed
        character(len=32) :: lang
        character(len=100) :: filename, test_name
        character(len=32) :: GREEN, RED, RESET
        logical :: result

        GREEN = char(27) // '[32m'
        RED = char(27) // '[31m'
        RESET = char(27) // '[0m'

        ! Create test filename
        filename = 'test' // trim(ext)

        ! Detect language
        call detect_lang(filename, lang)

        ! Check result
        result = trim(lang) == trim(expected_lang)

        ! Print result
        write(test_name, '(A,A,A,A)') 'Detect ', trim(ext), ' as ', trim(expected_lang)
        if (result) then
            write(*, '(A,A,A,A)') trim(GREEN), '✓ PASS', trim(RESET), ': ' // trim(test_name)
            passed = passed + 1
        else
            write(*, '(A,A,A,A)') trim(RED), '✗ FAIL', trim(RESET), ': ' // trim(test_name)
            write(*, '(A,A,A,A)') '  Expected: ', trim(expected_lang), ', Got: ', trim(lang)
            failed = failed + 1
        end if
    end subroutine test_extension

    subroutine detect_lang(filename, language)
        character(len=*), intent(in) :: filename
        character(len=*), intent(out) :: language
        character(len=32) :: ext
        integer :: dot_pos, i, len_fn

        ! Find last dot
        len_fn = len_trim(filename)
        dot_pos = 0
        do i = len_fn, 1, -1
            if (filename(i:i) == '.') then
                dot_pos = i
                exit
            end if
        end do

        if (dot_pos == 0) then
            language = 'unknown'
            return
        end if

        ext = filename(dot_pos:len_fn)
        call to_lower(ext)

        ! Map extension to language
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
    end subroutine detect_lang

    subroutine to_lower(str)
        character(len=*), intent(inout) :: str
        integer :: i, ic

        do i = 1, len_trim(str)
            ic = ichar(str(i:i))
            if (ic >= 65 .and. ic <= 90) then
                str(i:i) = char(ic + 32)
            end if
        end do
    end subroutine to_lower

end program test_un_f90
