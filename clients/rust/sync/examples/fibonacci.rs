// PUBLIC DOMAIN - NO LICENSE, NO WARRANTY
//
// Fibonacci example - standalone version
//
// This example demonstrates executing a recursive algorithm.
// Shows how to run complex computations (simulated).
//
// To run:
//     rustc fibonacci.rs && ./fibonacci
//
// Expected output:
//     Executing Fibonacci code...
//     Result status: completed
//     Output:
//     fib(0) = 0
//     fib(1) = 1
//     fib(2) = 1
//     fib(3) = 2
//     fib(4) = 3
//     fib(5) = 5
//     fib(6) = 8
//     fib(7) = 13
//     fib(8) = 21
//     fib(9) = 34
//     fib(10) = 55
//     Execution time: 150ms

fn fib(n: u32) -> u32 {
    if n <= 1 {
        n
    } else {
        fib(n - 1) + fib(n - 2)
    }
}

fn main() {
    // Execute the fibonacci calculation (simulated API call)
    println!("Executing Fibonacci code...");
    println!("Result status: completed");
    println!("Output:");

    for i in 0..=10 {
        println!("fib({}) = {}", i, fib(i));
    }

    println!("Execution time: 150ms");
}
