/*
Hello World example for unsandbox Go SDK - Asynchronous Version

This example demonstrates basic async execution with the unsandbox SDK.
Shows how to use goroutines and channels for non-blocking code execution.

To run:
    export UNSANDBOX_PUBLIC_KEY="your-public-key"
    export UNSANDBOX_SECRET_KEY="your-secret-key"
    go run hello_world.go

Expected output:
    Executing code asynchronously...
    Result status: completed
    Output: Hello from async unsandbox!
*/
package main

import (
	"fmt"
	"log"
	"os"

	un_async "github.com/unsandbox/un-go-async/src"
)

func main() {
	// The code to execute
	code := `print("Hello from async unsandbox!")`

	// Resolve credentials from environment
	creds, err := un_async.ResolveCredentials("", "")
	if err != nil {
		log.Printf("Error: UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY environment variables required")
		log.Printf("Run with: export UNSANDBOX_PUBLIC_KEY=your-key UNSANDBOX_SECRET_KEY=your-key")
		os.Exit(1)
	}

	// Execute the code asynchronously (returns channel)
	fmt.Println("Executing code asynchronously...")
	resultChan := un_async.ExecuteCode(creds, "python", code)

	// Wait for result from channel
	result := <-resultChan

	// Check for errors
	if result.Err != nil {
		log.Fatalf("Execution error: %v", result.Err)
	}

	// Check status
	if status, ok := result.Data["status"].(string); ok && status == "completed" {
		fmt.Printf("Result status: %s\n", status)
		if stdout, ok := result.Data["stdout"].(string); ok {
			fmt.Printf("Output: %s", stdout)
		}
		if stderr, ok := result.Data["stderr"].(string); ok && stderr != "" {
			fmt.Printf("Errors: %s", stderr)
		}
	} else {
		status := result.Data["status"]
		errMsg := result.Data["error"]
		log.Fatalf("Execution failed with status: %v, error: %v", status, errMsg)
	}
}
