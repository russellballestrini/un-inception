/*
Concurrent Execution example - standalone version

This example demonstrates running multiple operations concurrently.
Shows goroutines, channels, and sync.WaitGroup for parallel execution.

To run:
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
	"sync"
	"time"
)

type execution struct {
	name   string
	output string
}

func main() {
	// Define multiple executions
	executions := []execution{
		{"Python", "Python says hello!\n"},
		{"JavaScript", "JavaScript says hello!\n"},
		{"Ruby", "Ruby says hello!\n"},
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

			// Simulate API call delay
			time.Sleep(50 * time.Millisecond)

			mu.Lock()
			defer mu.Unlock()

			fmt.Printf("[%s] Status: completed, Output: %s", e.name, e.output)
			successCount++
		}(exec)
	}

	// Wait for all executions to complete
	wg.Wait()

	fmt.Printf("All %d executions completed successfully!\n", successCount)
}
