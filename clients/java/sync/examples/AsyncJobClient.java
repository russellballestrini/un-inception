/**
 * Async Job Client example for unsandbox Java SDK - Synchronous Version
 *
 * Demonstrates asynchronous execution with job polling.
 * Shows how to execute code asynchronously and wait for completion.
 *
 * To compile:
 *     javac -cp ../src AsyncJobClient.java
 *
 * To run:
 *     export UNSANDBOX_PUBLIC_KEY="your-public-key"
 *     export UNSANDBOX_SECRET_KEY="your-secret-key"
 *     java -cp .:../src AsyncJobClient
 *
 * Expected output:
 *     Submitting async job...
 *     Job submitted with ID: job_xxxxx
 *     Waiting for completion...
 *     Job completed!
 *     Output: Result of computation: 42
 */

import java.util.Map;

public class AsyncJobClient {

    public static void main(String[] args) {
        // The code to execute - simulates a longer-running computation
        String code = """
            import time

            # Simulate some computation
            time.sleep(1)
            result = 6 * 7
            print(f"Result of computation: {result}")
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

            // Submit job asynchronously
            System.out.println("Submitting async job...");
            String jobId = Un.executeAsync("python", code, publicKey, secretKey);
            System.out.println("Job submitted with ID: " + jobId);

            // Wait for completion (60 second timeout)
            System.out.println("Waiting for completion...");
            Map<String, Object> result = Un.waitForJob(jobId, publicKey, secretKey, 60000);

            // Check for errors
            String status = (String) result.get("status");
            if ("completed".equals(status)) {
                System.out.println("Job completed!");
                String stdout = (String) result.get("stdout");
                if (stdout != null) {
                    System.out.println("Output: " + stdout.trim());
                }
            } else {
                System.out.println("Job failed with status: " + status);
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
