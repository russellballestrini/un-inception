// Fibonacci example for unsandbox Java SDK
// Expected output: fib(10) = 55

public class Fibonacci {
    public static int fib(int n) {
        if (n <= 1) return n;
        return fib(n - 1) + fib(n - 2);
    }

    public static void main(String[] args) {
        System.out.println("fib(10) = " + fib(10));
    }
}
