/*
Hello World example - standalone version

This example demonstrates the async execution pattern with Go.
Shows goroutines and channels for non-blocking operations.

To run:
    go run hello_world.go

Expected output:
    Executing code asynchronously...
    Waiting for result on channel...
    Result status: completed
    Output: Hello from async unsandbox!
*/
package main

import (
	"fmt"
)

// Simulated result type
type Result struct {
	Status string
	Stdout string
	Stderr string
}

// Simulated async execution using goroutine and channel
func executeAsync(language, code string) <-chan Result {
	resultChan := make(chan Result, 1)

	go func() {
		// In real SDK, this would call the API
		// Here we simulate the expected response
		resultChan <- Result{
			Status: "completed",
			Stdout: "Hello from async unsandbox!\n",
			Stderr: "",
		}
	}()

	return resultChan
}

func main() {
	code := `print("Hello from async unsandbox!")`

	fmt.Println("Executing code asynchronously...")
	resultChan := executeAsync("python", code)

	fmt.Println("Waiting for result on channel...")
	result := <-resultChan

	if result.Status == "completed" {
		fmt.Printf("Result status: %s\n", result.Status)
		fmt.Printf("Output: %s", result.Stdout)
	} else {
		fmt.Printf("Execution failed with status: %s\n", result.Status)
	}
}
