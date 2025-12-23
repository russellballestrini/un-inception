#!/usr/bin/env tclsh

proc fib {n} {
    if {$n <= 1} {
        return $n
    }
    return [expr {[fib [expr {$n - 1}]] + [fib [expr {$n - 2}]]}]
}

for {set i 0} {$i <= 10} {incr i} {
    puts "fib($i) = [fib $i]"
}
