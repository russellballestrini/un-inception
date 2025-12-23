#!/usr/bin/env raku
# Test suite for un.raku (Raku implementation)

use Test;

my $SCRIPT_DIR = $*PROGRAM.IO.parent;
my $UN_RAKU = $SCRIPT_DIR.add('../un.raku');
my $TEST_DIR = $SCRIPT_DIR.add('../../test');

# Colors
sub color-red($text) { "\e[0;31m{$text}\e[0m" }
sub color-green($text) { "\e[0;32m{$text}\e[0m" }
sub color-yellow($text) { "\e[1;33m{$text}\e[0m" }
sub color-blue($text) { "\e[0;34m{$text}\e[0m" }

# Test counters
my $tests-run = 0;
my $tests-passed = 0;
my $tests-failed = 0;

# Test result tracking
sub test-passed($name) {
    $tests-passed++;
    $tests-run++;
    say color-green("✓ PASS") ~ ": $name";
}

sub test-failed($name, $error = '') {
    $tests-failed++;
    $tests-run++;
    say color-red("✗ FAIL") ~ ": $name";
    say color-red("  Error: $error") if $error;
}

sub test-skipped($name) {
    say color-yellow("⊘ SKIP") ~ ": $name";
}

# Helper to run command and capture output
sub run-command(@cmd) {
    my $proc = run @cmd, :out, :err;
    my $stdout = $proc.out.slurp;
    my $stderr = $proc.err.slurp;
    my $output = $stdout ~ $stderr;
    return ($proc.exitcode, $output);
}

# Unit Tests
say color-blue("=== Unit Tests for un.raku ===");

# Test: Script exists and is executable
if $UN_RAKU.IO.e && $UN_RAKU.IO.x {
    test-passed("Script exists and is executable");
} else {
    test-failed("Script exists and is executable", "File not found or not executable");
}

# Test: Usage message when no arguments
my ($exit-code, $output) = run-command([$UN_RAKU]);
if $exit-code != 0 && $output ~~ /Usage/ {
    test-passed("Shows usage message with no arguments");
} else {
    test-failed("Shows usage message with no arguments", "Expected usage message");
}

# Test: Error on non-existent file
($exit-code, $output) = run-command([$UN_RAKU, '/tmp/nonexistent_file_12345.xyz']);
if $exit-code != 0 && $output ~~ /'not found'/ {
    test-passed("Handles non-existent file");
} else {
    test-failed("Handles non-existent file", "Expected 'not found' message");
}

# Test: Error on unknown extension
my $unknown-file = "/tmp/test_unknown_ext_{$*PID}.unknownext";
spurt $unknown-file, "test";

($exit-code, $output) = run-command([$UN_RAKU, $unknown-file]);
unlink $unknown-file;

if $exit-code != 0 && $output ~~ /'Unknown file extension'/ {
    test-passed("Handles unknown file extension");
} else {
    test-failed("Handles unknown file extension", "Expected 'Unknown file extension' message");
}

# Test: Error when API key not set
if %*ENV<UNSANDBOX_API_KEY>:exists && %*ENV<UNSANDBOX_API_KEY> {
    my $test-file = $TEST_DIR.add('fib.py');
    if $test-file.IO.e {
        # Temporarily unset API key
        my $old-key = %*ENV<UNSANDBOX_API_KEY>;
        %*ENV<UNSANDBOX_API_KEY>:delete;

        ($exit-code, $output) = run-command([$UN_RAKU, ~$test-file]);

        %*ENV<UNSANDBOX_API_KEY> = $old-key;

        if $exit-code != 0 && $output ~~ /UNSANDBOX_API_KEY/ {
            test-passed("Requires API key");
        } else {
            test-failed("Requires API key", "Expected API key error message");
        }
    } else {
        test-skipped("Requires API key (test file not found)");
    }
} else {
    test-skipped("Requires API key (API key already not set)");
}

# Integration Tests (require API key)
if %*ENV<UNSANDBOX_API_KEY>:exists && %*ENV<UNSANDBOX_API_KEY> {
    say "";
    say color-blue("=== Integration Tests for un.raku ===");

    # Test: Can execute Python file
    my $fib-py = $TEST_DIR.add('fib.py');
    if $fib-py.IO.e {
        ($exit-code, $output) = run-command([$UN_RAKU, ~$fib-py]);
        if $exit-code == 0 && $output ~~ /'fib(10)'/ {
            test-passed("Executes Python file successfully");
        } else {
            test-failed("Executes Python file successfully", "Expected fibonacci output");
        }
    } else {
        test-skipped("Executes Python file successfully (fib.py not found)");
    }

    # Test: Can execute Bash file
    my $fib-sh = $TEST_DIR.add('fib.sh');
    if $fib-sh.IO.e {
        ($exit-code, $output) = run-command([$UN_RAKU, ~$fib-sh]);
        if $exit-code == 0 && $output ~~ /'fib(10)'/ {
            test-passed("Executes Bash file successfully");
        } else {
            test-failed("Executes Bash file successfully", "Expected fibonacci output");
        }
    } else {
        test-skipped("Executes Bash file successfully (fib.sh not found)");
    }
} else {
    say "";
    say color-yellow("Skipping integration tests (UNSANDBOX_API_KEY not set)");
}

# Summary
say "";
say color-blue("=== Test Summary ===");
say "Total: $tests-run | Passed: $tests-passed | Failed: $tests-failed";

if $tests-failed == 0 {
    say color-green("All tests passed!");
    exit 0;
} else {
    say color-red("Some tests failed!");
    exit 1;
}
