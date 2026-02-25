#!/bin/bash
# This is free software for the public good of a permacomputer hosted at
# permacomputer.com, an always-on computer by the people, for the people.
# One which is durable, easy to repair, & distributed like tap water
# for machine learning intelligence.
#
# The permacomputer is community-owned infrastructure optimized around
# four values:
#
#   TRUTH      First principles, math & science, open source code freely distributed
#   FREEDOM    Voluntary partnerships, freedom from tyranny & corporate control
#   HARMONY    Minimal waste, self-renewing systems with diverse thriving connections
#   LOVE       Be yourself without hurting others, cooperation through natural law
#
# This software contributes to that vision by enabling code execution across 42+ programming languages through a unified interface, accessible to all.
# Code is seeds to sprout on any abandoned technology.

# Complete Inception Matrix Test - uses un2 with semitrust to test all implementations

source /home/fox/git/unsandbox.com/vars.sh
cd /home/fox/git/unsandbox.com/cli

echo "=== COMPLETE INCEPTION MATRIX TEST ==="
echo "Using un2 with semitrusted network to test all 42 implementations"
echo ""

pass=0
fail=0

test_impl() {
    local impl=$1
    local name=$(basename "$impl")
    printf "%-20s" "$name"

    if timeout 180 ./un2 -n semitrusted "$impl" test/fib.py 2>&1 | grep -q "fib(10) = 55"; then
        echo "PASS"
        pass=$((pass + 1))
    else
        echo "FAIL"
        fail=$((fail + 1))
    fi
}

echo "--- Scripting Languages ---"
test_impl "inception/un.py"
test_impl "inception/un.js"
test_impl "inception/un.ts"
test_impl "inception/un.rb"
test_impl "inception/un.php"
test_impl "inception/un.pl"
test_impl "inception/un.lua"
test_impl "inception/un.sh"

echo ""
echo "--- Systems Languages ---"
test_impl "inception/un.go"
test_impl "inception/un.rs"
test_impl "inception/un_inception.c"
test_impl "inception/un.cpp"
test_impl "inception/un.d"
test_impl "inception/un.nim"
test_impl "inception/un.zig"
test_impl "inception/un.v"

echo ""
echo "--- JVM/.NET Languages ---"
test_impl "inception/Un.java"
test_impl "inception/un.kt"
test_impl "inception/Un.cs"
test_impl "inception/un.fs"
test_impl "inception/un.groovy"
test_impl "inception/un.dart"

echo ""
echo "--- Functional Languages ---"
test_impl "inception/un.hs"
test_impl "inception/un.ml"
test_impl "inception/un.clj"
test_impl "inception/un.scm"
test_impl "inception/un.lisp"
test_impl "inception/un.erl"
test_impl "inception/un.ex"

echo ""
echo "--- Scientific/Exotic ---"
test_impl "inception/un.jl"
test_impl "inception/un.r"
test_impl "inception/un.cr"
test_impl "inception/un.f90"
test_impl "inception/un.cob"
test_impl "inception/un.pro"
test_impl "inception/un.forth"

echo ""
echo "--- Other Languages ---"
test_impl "inception/un.tcl"
test_impl "inception/un.raku"
test_impl "inception/un.m"
test_impl "inception/un_deno.ts"
test_impl "inception/un.ps1"
test_impl "inception/un.awk"

echo ""
echo "=================================="
echo "Results: $pass PASS, $fail FAIL out of 42"
echo ""

if [ $fail -eq 0 ]; then
    echo "THE MATRIX IS COMPLETE. ALL IMPLEMENTATIONS VALIDATED."
    exit 0
else
    echo "$fail implementation(s) need attention."
    exit 1
fi
