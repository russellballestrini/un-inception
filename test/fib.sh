#!/bin/bash
fib() {
    local n=$1
    if [ $n -le 1 ]; then
        echo $n
        return
    fi
    local a=$(fib $((n-1)))
    local b=$(fib $((n-2)))
    echo $((a + b))
}

for i in {0..10}; do
    echo "fib($i) = $(fib $i)"
done
