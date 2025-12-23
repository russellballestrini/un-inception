#!/usr/bin/env groovy
def fib(n) {
    if (n <= 1) return n
    return fib(n-1) + fib(n-2)
}

(0..10).each { i ->
    println "fib($i) = ${fib(i)}"
}
