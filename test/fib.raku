#!/usr/bin/env raku

sub fib(Int $n) returns Int {
    return $n if $n <= 1;
    return fib($n - 1) + fib($n - 2);
}

for 0..10 -> $i {
    say "fib($i) = {fib($i)}";
}
