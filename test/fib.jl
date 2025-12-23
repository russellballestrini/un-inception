#!/usr/bin/env julia
function fib(n)
    if n <= 1
        return n
    end
    return fib(n-1) + fib(n-2)
end

for i in 0:10
    println("fib($i) = $(fib(i))")
end
