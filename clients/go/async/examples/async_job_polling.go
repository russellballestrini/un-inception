/*
Async Job Polling example - standalone version

This example demonstrates the async job polling pattern:
1. Submit a job (returns immediately with job ID)
2. Poll for completion
3. Retrieve results

To run:
    go run async_job_polling.go

Expected output:
    Submitting async job...
    Job submitted with ID: job-example-123
    Polling for completion...
    Poll 1: status=queued
    Poll 2: status=running
    Poll 3: status=completed
    Job completed!
    Status: completed
    Output: Calculation result: 55
*/
package main

import (
	"fmt"
	"time"
)

func main() {
	fmt.Println("Submitting async job...")

	// Simulate job submission
	jobID := "job-example-123"
	fmt.Printf("Job submitted with ID: %s\n", jobID)

	// Simulate polling
	fmt.Println("Polling for completion...")
	statuses := []string{"queued", "running", "completed"}
	for i, status := range statuses {
		time.Sleep(100 * time.Millisecond)
		fmt.Printf("Poll %d: status=%s\n", i+1, status)
	}

	// Simulate result
	fmt.Println("Job completed!")
	fmt.Println("Status: completed")
	fmt.Println("Output: Calculation result: 55")
}
