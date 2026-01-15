/**
 * Fibonacci Client example for unsandbox Java SDK - Synchronous Version
 *
 * Demonstrates executing CPU-bound calculations through the sync SDK.
 * Shows proper error handling and result processing.
 *
 * To compile:
 *     javac -cp ../src FibonacciClient.java
 *
 * To run:
 *     export UNSANDBOX_PUBLIC_KEY="your-public-key"
 *     export UNSANDBOX_SECRET_KEY="your-secret-key"
 *     java -cp .:../src FibonacciClient
 *
 * Expected output:
 *     Calculating fibonacci(10)...
 *     Result status: completed
 *     Output: fib(10) = 55
 */

import java.util.Map;

public class FibonacciClient {

    public static void main(String[] args) {
        // The code to execute
        String code = """
            def fib(n):
                if n <= 1:
                    return n
                return fib(n-1) + fib(n-2)

            print(f"fib(10) = {fib(10)}")
            """;

        try {
            // Resolve credentials from environment
            String publicKey = System.getenv("UNSANDBOX_PUBLIC_KEY");
            String secretKey = System.getenv("UNSANDBOX_SECRET_KEY");

            if (publicKey == null || publicKey.isEmpty() ||
                secretKey == null || secretKey.isEmpty()) {
                System.err.println("Error: UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY environment variables required");
                System.err.println("Run with: export UNSANDBOX_PUBLIC_KEY=your-key UNSANDBOX_SECRET_KEY=your-key");
                System.exit(1);
            }

            // Execute the code synchronously
            System.out.println("Calculating fibonacci(10)...");
            Map<String, Object> result = Un.executeCode("python", code, publicKey, secretKey);

            // Check for errors
            String status = (String) result.get("status");
            if ("completed".equals(status)) {
                System.out.println("Result status: " + status);
                String stdout = (String) result.get("stdout");
                if (stdout != null) {
                    System.out.println("Output: " + stdout.trim());
                }
                String stderr = (String) result.get("stderr");
                if (stderr != null && !stderr.isEmpty()) {
                    System.out.println("Errors: " + stderr);
                }
            } else {
                System.out.println("Execution failed with status: " + status);
                System.out.println("Error: " + result.getOrDefault("error", "Unknown error"));
                System.exit(1);
            }

        } catch (Un.CredentialsException e) {
            System.err.println("Credentials error: " + e.getMessage());
            System.exit(1);
        } catch (Exception e) {
            System.err.println("Error: " + e.getMessage());
            e.printStackTrace();
            System.exit(1);
        }
    }
}
