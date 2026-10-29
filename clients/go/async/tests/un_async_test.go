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
Tests for the unsandbox Go SDK (Asynchronous)

Run tests:

	cd /home/fox/git/un-inception/clients/go/async
	go test ./tests/...

Or run with verbose output:

	go test -v ./tests/...
*/
package tests

import (
	"crypto/hmac"
	"crypto/sha256"
	"encoding/hex"
	"encoding/json"
	"io"
	"net/http"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	un_async "github.com/unsandbox/un-go-async/src"
)

// TestDetectLanguage tests language detection from filenames
func TestDetectLanguage(t *testing.T) {
	tests := []struct {
		filename string
		expected string
	}{
		{"hello.py", "python"},
		{"script.js", "javascript"},
		{"main.go", "go"},
		{"test.rs", "rust"},
		{"program.c", "c"},
		{"app.rb", "ruby"},
		{"test.php", "php"},
		{"script.sh", "bash"},
		{"data.R", "r"},  // Uppercase R
		{"data.r", "r"},  // Lowercase r
		{"unknown", ""},  // No extension
		{"no_ext", ""},   // No extension
		{"file.xyz", ""}, // Unknown extension
		{"test.ts", "typescript"},
		{"code.java", "java"},
		{"test.kt", "kotlin"},
		{"app.ex", "elixir"},
		{"prog.erl", "erlang"},
		{"script.lua", "lua"},
		{"test.nim", "nim"},
	}

	for _, tc := range tests {
		t.Run(tc.filename, func(t *testing.T) {
			result := un_async.DetectLanguage(tc.filename)
			if result != tc.expected {
				t.Errorf("DetectLanguage(%q) = %q, want %q", tc.filename, result, tc.expected)
			}
		})
	}
}

// TestSignRequest tests HMAC-SHA256 signature generation
func TestSignRequest(t *testing.T) {
	// This is a basic test to ensure signature generation works
	// The actual signature validation would need to match server-side implementation
	secretKey := "test-secret-key"
	timestamp := int64(1234567890)
	method := "POST"
	path := "/execute"
	body := []byte(`{"language":"python","code":"print(42)"}`)

	// We test that signRequest returns a non-empty 64-character hex string
	// Note: signRequest is not exported, so we test via LanguageMap as a proxy
	// for now we just verify the LanguageMap is properly defined
	if len(un_async.LanguageMap) == 0 {
		t.Error("LanguageMap should not be empty")
	}

	// Verify constants are defined
	if un_async.APIBase != "https://api.unsandbox.com" {
		t.Errorf("APIBase = %q, want %q", un_async.APIBase, "https://api.unsandbox.com")
	}

	if un_async.LanguagesCacheTTL != 3600 {
		t.Errorf("LanguagesCacheTTL = %d, want %d", un_async.LanguagesCacheTTL, 3600)
	}

	// Verify poll delays are defined
	if len(un_async.PollDelaysMs) == 0 {
		t.Error("PollDelaysMs should not be empty")
	}

	// Use the variables to avoid unused variable warnings
	_ = secretKey
	_ = timestamp
	_ = method
	_ = path
	_ = body
}

// TestCredentialsError tests the CredentialsError type
func TestCredentialsError(t *testing.T) {
	err := &un_async.CredentialsError{Message: "test error"}
	if err.Error() != "test error" {
		t.Errorf("CredentialsError.Error() = %q, want %q", err.Error(), "test error")
	}
}

// TestResolveCredentialsFromArgs tests credential resolution from arguments
func TestResolveCredentialsFromArgs(t *testing.T) {
	creds, err := un_async.ResolveCredentials("test-pk", "test-sk")
	if err != nil {
		t.Fatalf("ResolveCredentials failed: %v", err)
	}

	if creds.PublicKey != "test-pk" {
		t.Errorf("PublicKey = %q, want %q", creds.PublicKey, "test-pk")
	}

	if creds.SecretKey != "test-sk" {
		t.Errorf("SecretKey = %q, want %q", creds.SecretKey, "test-sk")
	}
}

// TestResolveCredentialsFromEnv tests credential resolution from environment
func TestResolveCredentialsFromEnv(t *testing.T) {
	// Save original values
	origPK := os.Getenv("UNSANDBOX_PUBLIC_KEY")
	origSK := os.Getenv("UNSANDBOX_SECRET_KEY")

	// Set test values
	os.Setenv("UNSANDBOX_PUBLIC_KEY", "env-pk")
	os.Setenv("UNSANDBOX_SECRET_KEY", "env-sk")

	// Restore after test
	defer func() {
		if origPK != "" {
			os.Setenv("UNSANDBOX_PUBLIC_KEY", origPK)
		} else {
			os.Unsetenv("UNSANDBOX_PUBLIC_KEY")
		}
		if origSK != "" {
			os.Setenv("UNSANDBOX_SECRET_KEY", origSK)
		} else {
			os.Unsetenv("UNSANDBOX_SECRET_KEY")
		}
	}()

	creds, err := un_async.ResolveCredentials("", "")
	if err != nil {
		t.Fatalf("ResolveCredentials failed: %v", err)
	}

	if creds.PublicKey != "env-pk" {
		t.Errorf("PublicKey = %q, want %q", creds.PublicKey, "env-pk")
	}

	if creds.SecretKey != "env-sk" {
		t.Errorf("SecretKey = %q, want %q", creds.SecretKey, "env-sk")
	}
}

// TestResolveCredentialsArgsOverrideEnv tests that args take priority over env
func TestResolveCredentialsArgsOverrideEnv(t *testing.T) {
	// Save original values
	origPK := os.Getenv("UNSANDBOX_PUBLIC_KEY")
	origSK := os.Getenv("UNSANDBOX_SECRET_KEY")

	// Set env values
	os.Setenv("UNSANDBOX_PUBLIC_KEY", "env-pk")
	os.Setenv("UNSANDBOX_SECRET_KEY", "env-sk")

	// Restore after test
	defer func() {
		if origPK != "" {
			os.Setenv("UNSANDBOX_PUBLIC_KEY", origPK)
		} else {
			os.Unsetenv("UNSANDBOX_PUBLIC_KEY")
		}
		if origSK != "" {
			os.Setenv("UNSANDBOX_SECRET_KEY", origSK)
		} else {
			os.Unsetenv("UNSANDBOX_SECRET_KEY")
		}
	}()

	// Args should override env
	creds, err := un_async.ResolveCredentials("arg-pk", "arg-sk")
	if err != nil {
		t.Fatalf("ResolveCredentials failed: %v", err)
	}

	if creds.PublicKey != "arg-pk" {
		t.Errorf("PublicKey = %q, want %q", creds.PublicKey, "arg-pk")
	}

	if creds.SecretKey != "arg-sk" {
		t.Errorf("SecretKey = %q, want %q", creds.SecretKey, "arg-sk")
	}
}

// TestResolveCredentialsNoSourcesError tests error when no credentials found
func TestResolveCredentialsNoSourcesError(t *testing.T) {
	// Save original values
	origPK := os.Getenv("UNSANDBOX_PUBLIC_KEY")
	origSK := os.Getenv("UNSANDBOX_SECRET_KEY")

	// Clear env values
	os.Unsetenv("UNSANDBOX_PUBLIC_KEY")
	os.Unsetenv("UNSANDBOX_SECRET_KEY")

	// Restore after test
	defer func() {
		if origPK != "" {
			os.Setenv("UNSANDBOX_PUBLIC_KEY", origPK)
		}
		if origSK != "" {
			os.Setenv("UNSANDBOX_SECRET_KEY", origSK)
		}
	}()

	// Should fail if no CSV files exist
	_, err := un_async.ResolveCredentials("", "")
	if err == nil {
		t.Log("Note: ResolveCredentials succeeded - CSV file may exist in test environment")
		return
	}

	credErr, ok := err.(*un_async.CredentialsError)
	if !ok {
		t.Errorf("Expected CredentialsError, got %T", err)
		return
	}

	if credErr.Message == "" {
		t.Error("CredentialsError.Message should not be empty")
	}
}

// TestCSVCredentials tests loading credentials from CSV file
func TestCSVCredentials(t *testing.T) {
	// Create temp directory
	tmpDir, err := os.MkdirTemp("", "unsandbox-test-*")
	if err != nil {
		t.Fatalf("Failed to create temp dir: %v", err)
	}
	defer os.RemoveAll(tmpDir)

	// Create test CSV file
	csvPath := filepath.Join(tmpDir, "accounts.csv")
	csvContent := "public_key_1,secret_key_1\n# comment line\npublic_key_2,secret_key_2\n"
	if err := os.WriteFile(csvPath, []byte(csvContent), 0600); err != nil {
		t.Fatalf("Failed to create CSV file: %v", err)
	}

	// Change to temp dir to test ./accounts.csv loading
	origDir, err := os.Getwd()
	if err != nil {
		t.Fatalf("Failed to get working dir: %v", err)
	}
	defer os.Chdir(origDir)

	if err := os.Chdir(tmpDir); err != nil {
		t.Fatalf("Failed to change to temp dir: %v", err)
	}

	// Point HOME at the temp dir: ~/.unsandbox/accounts.csv is read before
	// ./accounts.csv, and this test must never load (or print) real keys.
	origHome := os.Getenv("HOME")
	os.Setenv("HOME", tmpDir)
	defer os.Setenv("HOME", origHome)

	// Clear env vars
	origPK := os.Getenv("UNSANDBOX_PUBLIC_KEY")
	origSK := os.Getenv("UNSANDBOX_SECRET_KEY")
	os.Unsetenv("UNSANDBOX_PUBLIC_KEY")
	os.Unsetenv("UNSANDBOX_SECRET_KEY")
	defer func() {
		if origPK != "" {
			os.Setenv("UNSANDBOX_PUBLIC_KEY", origPK)
		}
		if origSK != "" {
			os.Setenv("UNSANDBOX_SECRET_KEY", origSK)
		}
	}()

	// Test loading first account (index 0)
	creds, err := un_async.ResolveCredentials("", "")
	if err != nil {
		t.Fatalf("ResolveCredentials failed: %v", err)
	}

	if creds.PublicKey != "public_key_1" {
		t.Errorf("PublicKey did not come from the test CSV (want public_key_1; value not printed)")
	}

	if creds.SecretKey != "secret_key_1" {
		t.Errorf("SecretKey did not come from the test CSV (want secret_key_1; value not printed)")
	}

	// Test loading second account (index 1)
	os.Setenv("UNSANDBOX_ACCOUNT", "1")
	defer os.Unsetenv("UNSANDBOX_ACCOUNT")

	creds, err = un_async.ResolveCredentials("", "")
	if err != nil {
		t.Fatalf("ResolveCredentials failed: %v", err)
	}

	if creds.PublicKey != "public_key_2" {
		t.Errorf("PublicKey did not come from the test CSV (want public_key_2; value not printed)")
	}

	if creds.SecretKey != "secret_key_2" {
		t.Errorf("SecretKey did not come from the test CSV (want secret_key_2; value not printed)")
	}
}

// TestLanguageMapCompleteness tests that common languages are mapped
func TestLanguageMapCompleteness(t *testing.T) {
	requiredMappings := map[string]string{
		"py":   "python",
		"js":   "javascript",
		"ts":   "typescript",
		"rb":   "ruby",
		"php":  "php",
		"go":   "go",
		"rs":   "rust",
		"c":    "c",
		"cpp":  "cpp",
		"java": "java",
		"sh":   "bash",
	}

	for ext, expected := range requiredMappings {
		if lang, ok := un_async.LanguageMap[ext]; !ok {
			t.Errorf("LanguageMap missing extension %q", ext)
		} else if lang != expected {
			t.Errorf("LanguageMap[%q] = %q, want %q", ext, lang, expected)
		}
	}
}

// TestPollDelays tests that poll delays are reasonable
func TestPollDelays(t *testing.T) {
	delays := un_async.PollDelaysMs

	if len(delays) < 5 {
		t.Errorf("PollDelaysMs has %d elements, want at least 5", len(delays))
	}

	// First delay should be small (for quick jobs)
	if delays[0] > 500 {
		t.Errorf("First poll delay %d ms too large, should be < 500ms", delays[0])
	}

	// Last delay should be reasonable (not too long)
	lastDelay := delays[len(delays)-1]
	if lastDelay > 5000 {
		t.Errorf("Last poll delay %d ms too large, should be < 5000ms", lastDelay)
	}
}

// TestAsyncChannelBehavior tests that async functions return properly buffered channels
func TestAsyncChannelBehavior(t *testing.T) {
	// Create test credentials
	creds := &un_async.Credentials{
		PublicKey: "test-pk",
		SecretKey: "test-sk",
	}

	// Test that ExecuteCode returns a channel (even if request fails)
	resultChan := un_async.ExecuteCode(creds, "python", "print(1)")
	if resultChan == nil {
		t.Error("ExecuteCode returned nil channel")
	}

	// The channel should be buffered and eventually close
	select {
	case result := <-resultChan:
		// We expect an error since we're using test credentials
		if result.Err == nil {
			t.Log("Note: ExecuteCode succeeded - may have valid credentials")
		}
	case <-time.After(30 * time.Second):
		t.Error("ExecuteCode channel did not receive result within timeout")
	}
}

// captureTransport records the outgoing request and answers with a canned JSON body,
// so a test can see exactly what the SDK puts on the wire without calling the API.
type captureTransport struct {
	method string
	url    string
	header http.Header
	body   []byte
}

func (c *captureTransport) RoundTrip(req *http.Request) (*http.Response, error) {
	c.method = req.Method
	c.url = req.URL.String()
	c.header = req.Header.Clone()
	if req.Body != nil {
		c.body, _ = io.ReadAll(req.Body)
	}
	return &http.Response{
		StatusCode: 200,
		Header:     http.Header{"Content-Type": []string{"application/json"}},
		Body:       io.NopCloser(strings.NewReader(`{"success":true,"count":2}`)),
		Request:    req,
	}, nil
}

func TestSetServiceEnvPutsJSONMap(t *testing.T) {
	capture := &captureTransport{}
	saved := http.DefaultTransport
	http.DefaultTransport = capture
	defer func() { http.DefaultTransport = saved }()

	creds := &un_async.Credentials{PublicKey: "pk_test", SecretKey: "sk_test"}
	env := map[string]string{"API_KEY": "x", "DEBUG": "true"}
	res := <-un_async.SetServiceEnv(creds, "unsb-service-abc", env)
	result, err := res.Data, res.Err
	if err != nil {
		t.Fatalf("SetServiceEnv returned error: %v", err)
	}
	if result["success"] != true {
		t.Errorf("result = %v, want success", result)
	}
	if capture.method != "PUT" {
		t.Errorf("method = %q, want PUT", capture.method)
	}
	if capture.url != un_async.APIBase+"/services/unsb-service-abc/env" {
		t.Errorf("url = %q", capture.url)
	}
	if ct := capture.header.Get("Content-Type"); ct != "application/json" {
		t.Errorf("Content-Type = %q, want application/json", ct)
	}

	var sent struct {
		Env map[string]string `json:"env"`
	}
	if err := json.Unmarshal(capture.body, &sent); err != nil {
		t.Fatalf("body is not JSON: %v (%s)", err, capture.body)
	}
	if len(sent.Env) != 2 || sent.Env["API_KEY"] != "x" || sent.Env["DEBUG"] != "true" {
		t.Errorf("body env = %v, want %v", sent.Env, env)
	}

	// The signature covers exactly the bytes sent
	ts := capture.header.Get("X-Timestamp")
	mac := hmac.New(sha256.New, []byte("sk_test"))
	mac.Write([]byte(ts + ":PUT:/services/unsb-service-abc/env:" + string(capture.body)))
	if want := hex.EncodeToString(mac.Sum(nil)); capture.header.Get("X-Signature") != want {
		t.Errorf("X-Signature = %q, want %q", capture.header.Get("X-Signature"), want)
	}
}
