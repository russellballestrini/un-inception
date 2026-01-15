/**
 * HTTP Request Client example for unsandbox Java SDK - Synchronous Version
 *
 * This example demonstrates making HTTP requests from within a sandboxed environment.
 * Uses semitrusted mode which provides internet access through an egress proxy.
 *
 * To compile:
 *     javac -cp ../src HttpRequestClient.java
 *
 * To run:
 *     export UNSANDBOX_PUBLIC_KEY="your-public-key"
 *     export UNSANDBOX_SECRET_KEY="your-secret-key"
 *     java -cp .:../src HttpRequestClient
 *
 * Expected output:
 *     Executing HTTP request in sandbox...
 *
 *     === STDOUT ===
 *     Status Code: 200
 *     Response: {"origin": "..."}
 */

import java.util.Map;

public class HttpRequestClient {

    public static void main(String[] args) {
        // The code to execute - uses requests library (pre-installed in sandbox)
        String code = """
            import requests
            import json

            try:
                # Make HTTP request to httpbin.org
                response = requests.get('https://httpbin.org/ip', timeout=10)
                print(f"Status Code: {response.status_code}")

                # Parse and display response
                data = response.json()
                print(f"Response: {json.dumps(data)}")

            except requests.RequestException as e:
                print(f"Request failed: {e}")
            except Exception as e:
                print(f"Error: {e}")
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

            // Execute the code
            System.out.println("Executing HTTP request in sandbox...");
            Map<String, Object> result = Un.executeCode("python", code, publicKey, secretKey);

            // Check for errors
            String status = (String) result.get("status");
            if ("completed".equals(status)) {
                System.out.println("\n=== STDOUT ===");
                String stdout = (String) result.get("stdout");
                if (stdout != null) {
                    System.out.println(stdout);
                }
                String stderr = (String) result.get("stderr");
                if (stderr != null && !stderr.isEmpty()) {
                    System.out.println("\n=== STDERR ===");
                    System.out.println(stderr);
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
