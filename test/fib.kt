fun fib(n: Int): Int {
    if (n <= 1) return n
    return fib(n-1) + fib(n-2)
}

fun main() {
    for (i in 0..10) {
        println("fib($i) = ${fib(i)}")
    }
}
