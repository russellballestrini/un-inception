/**
 * Hello World Client example - standalone version
 *
 * This example demonstrates basic synchronous execution patterns.
 * Shows how to execute code from a Java program (simulated).
 *
 * To compile and run:
 *     javac HelloWorldClient.java && java HelloWorldClient
 *
 * Expected output:
 *     Executing code synchronously...
 *     Result status: completed
 *     Output: Hello from unsandbox!
 */

public class HelloWorldClient {

    public static void main(String[] args) {
        // The code to execute
        String code = "print(\"Hello from unsandbox!\")";

        // Execute the code synchronously (simulated)
        System.out.println("Executing code synchronously...");

        // Simulated result
        String status = "completed";
        String stdout = "Hello from unsandbox!\n";

        // Print result
        System.out.println("Result status: " + status);
        System.out.println("Output: " + stdout.trim());
    }
}
