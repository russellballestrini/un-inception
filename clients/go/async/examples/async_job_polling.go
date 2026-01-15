/*
Async Job Polling example for unsandbox Go SDK - Asynchronous Version

This example demonstrates submitting a job asynchronously and polling for results.
Shows how to use ExecuteAsync for fire-and-forget style execution with manual polling.

To run:
    export UNSANDBOX_PUBLIC_KEY="your-public-key"
    export UNSANDBOX_SECRET_KEY="your-secret-key"
    go run async_job_polling.go

Expected output:
    Submitting async job...
    Job submitted with ID: <job-id>
    Waiting for job completion...
    Job completed!
    Status: completed
    Output: Calculation result: 55
*/
package main

import (
	"fmt"
	"log"
	"os"
	"time"

	un_async "github.com/unsandbox/un-go-async/src"
)

func main() {
	// Code that takes a bit longer to execute
	code := `
import time
total = sum(range(11))
print(f"Calculation result: {total}")
`

	// Resolve credentials
	creds, err := un_async.ResolveCredentials("", "")
	if err != nil {
		log.Printf("Error: UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY environment variables required")
		os.Exit(1)
	}

	// Submit job asynchronously (returns immediately with job ID)
	fmt.Println("Submitting async job...")
	jobChan := un_async.ExecuteAsync(creds, "python", code)
	jobResult := <-jobChan

	if jobResult.Err != nil {
		log.Fatalf("Failed to submit job: %v", jobResult.Err)
	}

	fmt.Printf("Job submitted with ID: %s\n", jobResult.JobID)

	// Wait for job completion with timeout
	fmt.Println("Waiting for job completion...")
	waitChan := un_async.WaitForJob(creds, jobResult.JobID, 60*time.Second)
	waitResult := <-waitChan

	if waitResult.Err != nil {
		log.Fatalf("Error waiting for job: %v", waitResult.Err)
	}

	fmt.Println("Job completed!")
	fmt.Printf("Status: %v\n", waitResult.Data["status"])
	if stdout, ok := waitResult.Data["stdout"].(string); ok {
		fmt.Printf("Output: %s", stdout)
	}
}
