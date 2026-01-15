/*
Concurrent Execution example for unsandbox Go SDK - Asynchronous Version

This example demonstrates running multiple code executions concurrently.
Shows the power of async operations - run multiple executions in parallel.

To run:
    export UNSANDBOX_PUBLIC_KEY="your-public-key"
    export UNSANDBOX_SECRET_KEY="your-secret-key"
    go run concurrent_execution.go

Expected output:
    Starting 3 concurrent executions...
    [Python] Status: completed, Output: Python says hello!
    [JavaScript] Status: completed, Output: JavaScript says hello!
    [Ruby] Status: completed, Output: Ruby says hello!
    All 3 executions completed successfully!
*/
package main

import (
	"fmt"
	"log"
	"os"
	"sync"

	un_async "github.com/unsandbox/un-go-async/src"
)

type execution struct {
	name     string
	language string
	code     string
}

func main() {
	// Define multiple executions
	executions := []execution{
		{"Python", "python", `print("Python says hello!")`},
		{"JavaScript", "javascript", `console.log("JavaScript says hello!");`},
		{"Ruby", "ruby", `puts "Ruby says hello!"`},
	}

	// Resolve credentials
	creds, err := un_async.ResolveCredentials("", "")
	if err != nil {
		log.Printf("Error: UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY environment variables required")
		os.Exit(1)
	}

	fmt.Printf("Starting %d concurrent executions...\n", len(executions))

	// Use WaitGroup to wait for all executions
	var wg sync.WaitGroup
	var mu sync.Mutex
	successCount := 0

	for _, exec := range executions {
		wg.Add(1)
		go func(e execution) {
			defer wg.Done()

			// Execute asynchronously
			resultChan := un_async.ExecuteCode(creds, e.language, e.code)
			result := <-resultChan

			mu.Lock()
			defer mu.Unlock()

			if result.Err != nil {
				fmt.Printf("[%s] Error: %v\n", e.name, result.Err)
				return
			}

			status := result.Data["status"]
			stdout := result.Data["stdout"]
			fmt.Printf("[%s] Status: %v, Output: %v", e.name, status, stdout)

			if status == "completed" {
				successCount++
			}
		}(exec)
	}

	// Wait for all executions to complete
	wg.Wait()

	fmt.Printf("All %d executions completed successfully!\n", successCount)
}
