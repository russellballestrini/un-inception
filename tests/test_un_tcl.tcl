#!/usr/bin/env tclsh
# Test suite for un.tcl (TCL implementation)

set SCRIPT_DIR [file dirname [file normalize [info script]]]
set UN_TCL [file join $SCRIPT_DIR .. un.tcl]
set TEST_DIR [file join $SCRIPT_DIR .. .. test]

# Colors
proc color_red {text} { return "\033\[0;31m${text}\033\[0m" }
proc color_green {text} { return "\033\[0;32m${text}\033\[0m" }
proc color_yellow {text} { return "\033\[1;33m${text}\033\[0m" }
proc color_blue {text} { return "\033\[0;34m${text}\033\[0m" }

# Test counters
set TESTS_RUN 0
set TESTS_PASSED 0
set TESTS_FAILED 0

# Test result tracking
proc test_passed {name} {
    global TESTS_PASSED TESTS_RUN
    incr TESTS_PASSED
    incr TESTS_RUN
    puts "[color_green "✓ PASS"]: $name"
}

proc test_failed {name {error ""}} {
    global TESTS_FAILED TESTS_RUN
    incr TESTS_FAILED
    incr TESTS_RUN
    puts "[color_red "✗ FAIL"]: $name"
    if {$error ne ""} {
        puts "[color_red "  Error: $error"]"
    }
}

proc test_skipped {name} {
    puts "[color_yellow "⊘ SKIP"]: $name"
}

# Helper to run command and capture output
proc run_command {cmd} {
    if {[catch {exec {*}$cmd 2>@1} result]} {
        return [list 1 $result]
    } else {
        return [list 0 $result]
    }
}

# Unit Tests
puts "[color_blue "=== Unit Tests for un.tcl ==="]"

# Test: Script exists and is executable
if {[file exists $UN_TCL] && [file executable $UN_TCL]} {
    test_passed "Script exists and is executable"
} else {
    test_failed "Script exists and is executable" "File not found or not executable"
}

# Test: Usage message when no arguments
set result [run_command [list $UN_TCL]]
set exit_code [lindex $result 0]
set output [lindex $result 1]

if {$exit_code != 0 && [string match "*Usage:*" $output]} {
    test_passed "Shows usage message with no arguments"
} else {
    test_failed "Shows usage message with no arguments" "Expected usage message"
}

# Test: Error on non-existent file
set result [run_command [list $UN_TCL /tmp/nonexistent_file_12345.xyz]]
set exit_code [lindex $result 0]
set output [lindex $result 1]

if {$exit_code != 0 && [string match "*not found*" $output]} {
    test_passed "Handles non-existent file"
} else {
    test_failed "Handles non-existent file" "Expected 'not found' message"
}

# Test: Error on unknown extension
set unknown_file "/tmp/test_unknown_ext_[pid].unknownext"
set fp [open $unknown_file w]
puts $fp "test"
close $fp

set result [run_command [list $UN_TCL $unknown_file]]
set exit_code [lindex $result 0]
set output [lindex $result 1]

file delete $unknown_file

if {$exit_code != 0 && [string match "*Unknown file extension*" $output]} {
    test_passed "Handles unknown file extension"
} else {
    test_failed "Handles unknown file extension" "Expected 'Unknown file extension' message"
}

# Test: Error when API key not set
if {[info exists ::env(UNSANDBOX_API_KEY)] && $::env(UNSANDBOX_API_KEY) ne ""} {
    set test_file [file join $TEST_DIR fib.py]
    if {[file exists $test_file]} {
        # Temporarily unset API key
        set old_key $::env(UNSANDBOX_API_KEY)
        unset ::env(UNSANDBOX_API_KEY)

        set result [run_command [list $UN_TCL $test_file]]
        set exit_code [lindex $result 0]
        set output [lindex $result 1]

        set ::env(UNSANDBOX_API_KEY) $old_key

        if {$exit_code != 0 && [string match "*UNSANDBOX_API_KEY*" $output]} {
            test_passed "Requires API key"
        } else {
            test_failed "Requires API key" "Expected API key error message"
        }
    } else {
        test_skipped "Requires API key (test file not found)"
    }
} else {
    test_skipped "Requires API key (API key already not set)"
}

# Integration Tests (require API key)
if {[info exists ::env(UNSANDBOX_API_KEY)] && $::env(UNSANDBOX_API_KEY) ne ""} {
    puts "\n[color_blue "=== Integration Tests for un.tcl ==="]"

    # Test: Can execute Python file
    set fib_py [file join $TEST_DIR fib.py]
    if {[file exists $fib_py]} {
        set result [run_command [list $UN_TCL $fib_py]]
        set exit_code [lindex $result 0]
        set output [lindex $result 1]

        if {$exit_code == 0 && [string match "*fib(10)*" $output]} {
            test_passed "Executes Python file successfully"
        } else {
            test_failed "Executes Python file successfully" "Expected fibonacci output"
        }
    } else {
        test_skipped "Executes Python file successfully (fib.py not found)"
    }

    # Test: Can execute Bash file
    set fib_sh [file join $TEST_DIR fib.sh]
    if {[file exists $fib_sh]} {
        set result [run_command [list $UN_TCL $fib_sh]]
        set exit_code [lindex $result 0]
        set output [lindex $result 1]

        if {$exit_code == 0 && [string match "*fib(10)*" $output]} {
            test_passed "Executes Bash file successfully"
        } else {
            test_failed "Executes Bash file successfully" "Expected fibonacci output"
        }
    } else {
        test_skipped "Executes Bash file successfully (fib.sh not found)"
    }
} else {
    puts "\n[color_yellow "Skipping integration tests (UNSANDBOX_API_KEY not set)"]"
}

# Summary
puts "\n[color_blue "=== Test Summary ==="]"
puts "Total: $TESTS_RUN | Passed: $TESTS_PASSED | Failed: $TESTS_FAILED"

if {$TESTS_FAILED == 0} {
    puts "[color_green "All tests passed!"]"
    exit 0
} else {
    puts "[color_red "Some tests failed!"]"
    exit 1
}
