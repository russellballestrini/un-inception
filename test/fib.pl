#!/usr/bin/env perl
sub fib {
    my $n = shift;
    return $n if $n <= 1;
    return fib($n-1) + fib($n-2);
}

for my $i (0..10) {
    print "fib($i) = " . fib($i) . "\n";
}
