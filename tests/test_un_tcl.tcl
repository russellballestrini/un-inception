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

# Check if required TCL packages are available
set has_required_packages 1
foreach pkg {http json tls base64 sha256} {
    if {[catch {package require $pkg}]} {
        set has_required_packages 0
        break
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

if {!$has_required_packages} {
    test_skipped "Shows usage message with no arguments (missing TCL packages)"
    test_skipped "Handles non-existent file (missing TCL packages)"
    test_skipped "Handles unknown file extension (missing TCL packages)"
} else {
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

    if {$exit_code != 0 && ([string match "*not found*" $output] || [string match "*does not exist*" $output] || [string match "*Error:*" $output])} {
        test_passed "Handles non-existent file"
    } else {
        test_failed "Handles non-existent file" "Expected error message"
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

    if {$exit_code != 0 && ([string match "*unknown*" $output] || [string match "*extension*" $output] || [string match "*Error:*" $output] || [string match "*cannot detect*" $output])} {
        test_passed "Handles unknown file extension"
    } else {
        test_failed "Handles unknown file extension" "Expected extension error message"
    }
}

# Test: Error when authentication not set
set has_hmac [expr {[info exists ::env(UNSANDBOX_PUBLIC_KEY)] && $::env(UNSANDBOX_PUBLIC_KEY) ne "" && [info exists ::env(UNSANDBOX_SECRET_KEY)] && $::env(UNSANDBOX_SECRET_KEY) ne ""}]
set has_legacy [expr {[info exists ::env(UNSANDBOX_API_KEY)] && $::env(UNSANDBOX_API_KEY) ne ""}]

if {!$has_required_packages} {
    test_skipped "Requires authentication (missing TCL packages)"
} elseif {$has_hmac || $has_legacy} {
    set test_file [file join $TEST_DIR fib.py]
    if {[file exists $test_file]} {
        # Temporarily unset auth keys
        if {$has_hmac} {
            set old_pub $::env(UNSANDBOX_PUBLIC_KEY)
            set old_sec $::env(UNSANDBOX_SECRET_KEY)
            unset ::env(UNSANDBOX_PUBLIC_KEY)
            unset ::env(UNSANDBOX_SECRET_KEY)
        }
        if {$has_legacy} {
            set old_key $::env(UNSANDBOX_API_KEY)
            unset ::env(UNSANDBOX_API_KEY)
        }

        set result [run_command [list $UN_TCL $test_file]]
        set exit_code [lindex $result 0]
        set output [lindex $result 1]

        # Restore keys
        if {$has_hmac} {
            set ::env(UNSANDBOX_PUBLIC_KEY) $old_pub
            set ::env(UNSANDBOX_SECRET_KEY) $old_sec
        }
        if {$has_legacy} {
            set ::env(UNSANDBOX_API_KEY) $old_key
        }

        if {$exit_code != 0 && ([string match "*UNSANDBOX_PUBLIC_KEY*" $output] || [string match "*UNSANDBOX_API_KEY*" $output])} {
            test_passed "Requires authentication"
        } else {
            test_failed "Requires authentication" "Expected auth error message"
        }
    } else {
        test_skipped "Requires authentication (test file not found)"
    }
} else {
    test_skipped "Requires authentication (auth already not set)"
}

# Integration Tests (require authentication and packages)
if {!$has_required_packages} {
    puts "\n[color_yellow "Skipping integration tests (missing TCL packages)"]"
} elseif {$has_hmac || $has_legacy} {
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
    puts "\n[color_yellow "Skipping integration tests (UNSANDBOX authentication not configured)"]"
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
