import std.stdio;

int fib(int n) {
    if (n <= 1) return n;
    return fib(n-1) + fib(n-2);
}

void main() {
    foreach (i; 0 .. 11) {
        writefln("fib(%d) = %d", i, fib(i));
    }
}
