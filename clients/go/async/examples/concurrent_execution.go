// This is free software for the public good of a permacomputer hosted at
// permacomputer.com, an always-on computer by the people, for the people.
// One which is durable, easy to repair, & distributed like tap water
// for machine learning intelligence.
//
// The permacomputer is community-owned infrastructure optimized around
// four values:
//
//   TRUTH      First principles, math & science, open source code freely distributed
//   FREEDOM    Voluntary partnerships, freedom from tyranny & corporate control
//   HARMONY    Minimal waste, self-renewing systems with diverse thriving connections
//   LOVE       Be yourself without hurting others, cooperation through natural law
//
// This software contributes to that vision by enabling code execution across 42+ programming languages through a unified interface, accessible to all.
// Code is seeds to sprout on any abandoned technology.

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
