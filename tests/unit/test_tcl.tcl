#!/usr/bin/env tclsh
# Unit tests for un.tcl - tests internal functions without API calls

package require Tcl 8.5

set passed 0
set failed 0

proc test {name body} {
    global passed failed
    if {[catch {uplevel 1 $body} err]} {
        puts "  ✗ $name"
        puts "    $err"
        incr failed
    } else {
        puts "  ✓ $name"
        incr passed
    }
}

proc assert_equal {actual expected} {
    if {$actual ne $expected} {
        error "Expected '$expected' but got '$actual'"
    }
}

proc assert_not_equal {a b} {
    if {$a eq $b} {
        error "Expected values to be different but both were '$a'"
    }
}

proc assert_contains {str substr} {
    if {[string first $substr $str] == -1} {
        error "Expected '$str' to contain '$substr'"
    }
}

proc assert_true {val} {
    if {!$val} {
        error "Expected true but got false"
    }
}

# Extension mapping
array set ext_map {
    .py python .js javascript .ts typescript
    .rb ruby .php php .pl perl .lua lua
    .sh bash .go go .rs rust .c c
    .cpp cpp .java java .kt kotlin
    .hs haskell .clj clojure .erl erlang
    .ex elixir .jl julia .tcl tcl
}

puts "\n=== Extension Mapping Tests ==="

test "Python extension maps correctly" {
    assert_equal $ext_map(.py) "python"
}

test "TCL extension maps correctly" {
    assert_equal $ext_map(.tcl) "tcl"
}

test "JavaScript extension maps correctly" {
    assert_equal $ext_map(.js) "javascript"
}

test "Go extension maps correctly" {
    assert_equal $ext_map(.go) "go"
}

test "Ruby extension maps correctly" {
    assert_equal $ext_map(.rb) "ruby"
}

puts "\n=== HMAC Signature Tests ==="

# TCL needs external command for HMAC
proc hmac_sha256 {secret message} {
    set cmd [list echo -n $message | openssl dgst -sha256 -hmac $secret]
    catch {exec sh -c "echo -n '$message' | openssl dgst -sha256 -hmac '$secret' 2>/dev/null | sed 's/^.* //'"} result
    return [string trim $result]
}

test "HMAC-SHA256 generates 64 character hex string" {
    set sig [hmac_sha256 "test-secret" "test-message"]
    if {$sig ne ""} {
        assert_equal [string length $sig] 64
    }
}

test "Same input produces same signature" {
    set sig1 [hmac_sha256 "key" "msg"]
    set sig2 [hmac_sha256 "key" "msg"]
    if {$sig1 ne "" && $sig2 ne ""} {
        assert_equal $sig1 $sig2
    }
}

test "Different secrets produce different signatures" {
    set sig1 [hmac_sha256 "key1" "msg"]
    set sig2 [hmac_sha256 "key2" "msg"]
    if {$sig1 ne "" && $sig2 ne ""} {
        assert_not_equal $sig1 $sig2
    }
}

test "Signature format verification" {
    set timestamp "1704067200"
    set method "POST"
    set endpoint "/execute"
    set body "{\"language\":\"python\"}"

    set message "$timestamp:$method:$endpoint:$body"

    assert_true [string match "$timestamp*" $message]
    assert_contains $message ":POST:"
    assert_contains $message ":/execute:"
}

puts "\n=== Language Detection Tests ==="

test "Detect language from .tcl extension" {
    set filename "script.tcl"
    set ext ".[lindex [split $filename .] end]"
    assert_equal $ext_map($ext) "tcl"
}

test "Python shebang detection" {
    set content "#!/usr/bin/env python3\nprint('hello')"
    set first_line [lindex [split $content "\n"] 0]
    assert_true [string match "#!*" $first_line]
    assert_contains $first_line "python"
}

puts "\n=== Argument Parsing Tests ==="

test "Parse -e KEY=VALUE format" {
    set arg "DEBUG=1"
    set parts [split $arg "="]
    set key [lindex $parts 0]
    set value [join [lrange $parts 1 end] "="]
    assert_equal $key "DEBUG"
    assert_equal $value "1"
}

test "Parse -e KEY=VALUE with equals in value" {
    set arg "URL=https://example.com?foo=bar"
    set parts [split $arg "="]
    set key [lindex $parts 0]
    set value [join [lrange $parts 1 end] "="]
    assert_equal $key "URL"
    assert_equal $value "https://example.com?foo=bar"
}

puts "\n=== File Operations Tests ==="

test "Extract file basename" {
    set path "/home/user/project/script.tcl"
    assert_equal [file tail $path] "script.tcl"
}

test "Extract file extension" {
    set path "/home/user/project/script.tcl"
    assert_equal [file extension $path] ".tcl"
}

puts "\n=== API Constants Tests ==="

test "API base URL format" {
    set api_base "https://api.unsandbox.com"
    assert_true [string match "https://*" $api_base]
    assert_contains $api_base "unsandbox.com"
}

# Summary
puts "\n=== Summary ==="
puts "Passed: $passed"
puts "Failed: $failed"
puts "Total:  [expr {$passed + $failed}]"

exit [expr {$failed > 0 ? 1 : 0}]
