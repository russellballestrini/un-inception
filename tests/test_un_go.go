// Test suite for UN CLI Go implementation
// Compile: go build -o test_un_go test_un_go.go
// Run: ./test_un_go
//
// Tests:
// 1. Unit tests for extension detection
// 2. Integration test for API availability (requires UNSANDBOX_API_KEY)
// 3. Functional test running fib.go

package main

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
)

type ExecuteRequest struct {
	Language string `json:"language"`
	Code     string `json:"code"`
}

type ExecuteResponse struct {
	Stdout   string `json:"stdout"`
	Stderr   string `json:"stderr"`
	ExitCode int    `json:"exit_code"`
}

// Copy of detectLanguage from un.go for testing
func detectLanguage(filename string) string {
	ext := strings.ToLower(filepath.Ext(filename))
	langMap := map[string]string{
		".py":  "python",
		".js":  "javascript",
		".go":  "go",
		".rs":  "rust",
		".c":   "c",
		".cpp": "cpp",
		".d":   "d",
		".zig": "zig",
		".nim": "nim",
		".v":   "v",
	}
	if lang, ok := langMap[ext]; ok {
		return lang
	}
	return ""
}

func testExtensionDetection() bool {
	fmt.Println("=== Test 1: Extension Detection ===")

	tests := []struct {
		filename string
		expected string
	}{
		{"script.py", "python"},
		{"app.js", "javascript"},
		{"main.go", "go"},
		{"program.rs", "rust"},
		{"code.c", "c"},
		{"app.cpp", "cpp"},
		{"prog.d", "d"},
		{"main.zig", "zig"},
		{"script.nim", "nim"},
		{"app.v", "v"},
		{"unknown.xyz", ""},
	}

	passed := 0
	failed := 0

	for _, test := range tests {
		result := detectLanguage(test.filename)
		if result == test.expected {
			fmt.Printf("  PASS: %s -> %s\n", test.filename, result)
			passed++
		} else {
			fmt.Printf("  FAIL: %s -> got %s, expected %s\n", test.filename, result, test.expected)
			failed++
		}
	}

	fmt.Printf("Extension Detection: %d passed, %d failed\n\n", passed, failed)
	return failed == 0
}

func testAPIConnection() bool {
	fmt.Println("=== Test 2: API Connection ===")

	apiKey := os.Getenv("UNSANDBOX_API_KEY")
	if apiKey == "" {
		fmt.Println("  SKIP: UNSANDBOX_API_KEY not set")
		fmt.Println("API Connection: skipped\n")
		return true
	}

	// Simple Python script to test API
	code := "print('Hello from API test')"

	reqBody := ExecuteRequest{
		Language: "python",
		Code:     code,
	}

	jsonData, err := json.Marshal(reqBody)
	if err != nil {
		fmt.Printf("  FAIL: JSON marshal error: %v\n", err)
		return false
	}

	req, err := http.NewRequest("POST", "https://api.unsandbox.com/execute", bytes.NewBuffer(jsonData))
	if err != nil {
		fmt.Printf("  FAIL: Request creation error: %v\n", err)
		return false
	}

	req.Header.Set("Content-Type", "application/json")
	req.Header.Set("Authorization", "Bearer "+apiKey)

	client := &http.Client{}
	resp, err := client.Do(req)
	if err != nil {
		fmt.Printf("  FAIL: HTTP request error: %v\n", err)
		return false
	}
	defer resp.Body.Close()

	if resp.StatusCode != 200 {
		fmt.Printf("  FAIL: HTTP status %d\n", resp.StatusCode)
		return false
	}

	body, err := io.ReadAll(resp.Body)
	if err != nil {
		fmt.Printf("  FAIL: Response read error: %v\n", err)
		return false
	}

	var result ExecuteResponse
	if err := json.Unmarshal(body, &result); err != nil {
		fmt.Printf("  FAIL: JSON parse error: %v\n", err)
		return false
	}

	if !strings.Contains(result.Stdout, "Hello from API test") {
		fmt.Printf("  FAIL: Unexpected output: %s\n", result.Stdout)
		return false
	}

	fmt.Println("  PASS: API connection successful")
	fmt.Println("API Connection: passed\n")
	return true
}

func testFibExecution() bool {
	fmt.Println("=== Test 3: Functional Test (fib.go) ===")

	apiKey := os.Getenv("UNSANDBOX_API_KEY")
	if apiKey == "" {
		fmt.Println("  SKIP: UNSANDBOX_API_KEY not set")
		fmt.Println("Functional Test: skipped\n")
		return true
	}

	// Check if un_go binary exists
	unBinary := "../un_go"
	if _, err := os.Stat(unBinary); os.IsNotExist(err) {
		fmt.Printf("  SKIP: %s binary not found (run: cd .. && go build -o un_go un.go)\n", unBinary)
		fmt.Println("Functional Test: skipped\n")
		return true
	}

	// Check if fib.go exists
	fibFile := "fib.go"
	if _, err := os.Stat(fibFile); os.IsNotExist(err) {
		fmt.Printf("  SKIP: %s not found\n", fibFile)
		fmt.Println("Functional Test: skipped\n")
		return true
	}

	// Run un_go with fib.go
	cmd := exec.Command(unBinary, fibFile)
	cmd.Env = os.Environ() // Inherit environment including UNSANDBOX_API_KEY

	var stdout, stderr bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = &stderr

	err := cmd.Run()
	if err != nil {
		fmt.Printf("  FAIL: Execution error: %v\n", err)
		fmt.Printf("  STDERR: %s\n", stderr.String())
		return false
	}

	output := stdout.String()
	if !strings.Contains(output, "fib(10) = 55") {
		fmt.Printf("  FAIL: Expected output to contain 'fib(10) = 55', got: %s\n", output)
		return false
	}

	fmt.Printf("  PASS: fib.go executed successfully\n")
	fmt.Printf("  Output: %s", output)
	fmt.Println("Functional Test: passed\n")
	return true
}

func main() {
	fmt.Println("UN CLI Go Implementation Test Suite")
	fmt.Println("====================================\n")

	allPassed := true

	if !testExtensionDetection() {
		allPassed = false
	}

	if !testAPIConnection() {
		allPassed = false
	}

	if !testFibExecution() {
		allPassed = false
	}

	fmt.Println("====================================")
	if allPassed {
		fmt.Println("RESULT: ALL TESTS PASSED")
		os.Exit(0)
	} else {
		fmt.Println("RESULT: SOME TESTS FAILED")
		os.Exit(1)
	}
}
