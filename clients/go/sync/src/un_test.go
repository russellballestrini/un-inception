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

// Tests for the Go unsandbox SDK
// Run with: go test -v ./tests/
package un

import (
	"os"
	"testing"
)

// ============================================================================
// Unit Tests - Test exported library functions
// ============================================================================

func TestDetectLanguage(t *testing.T) {
	tests := []struct {
		filename string
		expected string
	}{
		{"script.py", "python"},
		{"script.js", "javascript"},
		{"script.ts", "typescript"},
		{"script.rb", "ruby"},
		{"script.go", "go"},
		{"script.rs", "rust"},
		{"script.c", "c"},
		{"script.cpp", "cpp"},
		{"script.d", "d"},
		{"script.zig", "zig"},
		{"script.sh", "bash"},
		{"script.lua", "lua"},
		{"script.php", "php"},
		{"script.unknown", ""},
		{"script", ""},
	}

	for _, tt := range tests {
		t.Run(tt.filename, func(t *testing.T) {
			result := DetectLanguage(tt.filename)
			if result != tt.expected {
				t.Errorf("DetectLanguage(%q) = %q, want %q", tt.filename, result, tt.expected)
			}
		})
	}
}

func TestHmacSign(t *testing.T) {
	// Test with known values
	secretKey := "test-secret"
	message := "test-message"

	result := HmacSign(secretKey, message)

	// Should return a 64-character hex string
	if len(result) != 64 {
		t.Errorf("HmacSign returned %d characters, want 64", len(result))
	}

	// Should be deterministic
	result2 := HmacSign(secretKey, message)
	if result != result2 {
		t.Error("HmacSign is not deterministic")
	}

	// Different inputs should produce different outputs
	result3 := HmacSign(secretKey, "different-message")
	if result == result3 {
		t.Error("HmacSign returned same result for different inputs")
	}
}

func TestVersion(t *testing.T) {
	version := Version()
	if version == "" {
		t.Error("Version() returned empty string")
	}
	// Should be in semver format
	if len(version) < 5 { // At minimum "0.0.0"
		t.Errorf("Version() = %q, expected semver format", version)
	}
}

func TestLastError(t *testing.T) {
	// Set an error
	SetLastError("test error message")

	// Retrieve it
	err := LastError()
	if err != "test error message" {
		t.Errorf("LastError() = %q, want %q", err, "test error message")
	}

	// Clear it
	SetLastError("")
	err = LastError()
	if err != "" {
		t.Errorf("LastError() after clear = %q, want empty", err)
	}
}

func TestCredentialsNew(t *testing.T) {
	pk := "unsb-pk-test-test-test-test"
	sk := "unsb-sk-test1-test2-test3-test4"

	creds := &Credentials{
		PublicKey: pk,
		SecretKey: sk,
	}

	if creds.PublicKey != pk {
		t.Errorf("PublicKey = %q, want %q", creds.PublicKey, pk)
	}
	if creds.SecretKey != sk {
		t.Errorf("SecretKey = %q, want %q", creds.SecretKey, sk)
	}
}

// ============================================================================
// Integration Tests - Test SDK internal consistency
// ============================================================================

func TestSignRequest(t *testing.T) {
	secretKey := "test-secret-key"
	timestamp := int64(1704067200) // 2024-01-01 00:00:00 UTC
	method := "POST"
	path := "/execute"
	body := `{"language":"python","code":"print(1)"}`

	signature := signRequest(secretKey, timestamp, method, path, []byte(body))

	// Should return a 64-character hex string
	if len(signature) != 64 {
		t.Errorf("signRequest returned %d characters, want 64", len(signature))
	}

	// Should be deterministic
	signature2 := signRequest(secretKey, timestamp, method, path, []byte(body))
	if signature != signature2 {
		t.Error("signRequest is not deterministic")
	}

	// Different timestamps should produce different signatures
	signature3 := signRequest(secretKey, timestamp+1, method, path, []byte(body))
	if signature == signature3 {
		t.Error("signRequest returned same result for different timestamps")
	}
}

func TestResolveCredentialsFromEnv(t *testing.T) {
	// Save original env vars
	origPK := os.Getenv("UNSANDBOX_PUBLIC_KEY")
	origSK := os.Getenv("UNSANDBOX_SECRET_KEY")

	// Set test env vars
	testPK := "unsb-pk-test-test-test-test"
	testSK := "unsb-sk-test1-test2-test3-test4"
	os.Setenv("UNSANDBOX_PUBLIC_KEY", testPK)
	os.Setenv("UNSANDBOX_SECRET_KEY", testSK)

	// Test
	creds, err := ResolveCredentials("", "")
	if err != nil {
		t.Fatalf("ResolveCredentials failed: %v", err)
	}
	if creds.PublicKey != testPK {
		t.Errorf("PublicKey = %q, want %q", creds.PublicKey, testPK)
	}
	if creds.SecretKey != testSK {
		t.Errorf("SecretKey = %q, want %q", creds.SecretKey, testSK)
	}

	// Restore original env vars
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
}

func TestResolveCredentialsFromArgs(t *testing.T) {
	testPK := "unsb-pk-arg1-arg2-arg3-arg4"
	testSK := "unsb-sk-arg11-arg22-arg33-arg44"

	creds, err := ResolveCredentials(testPK, testSK)
	if err != nil {
		t.Fatalf("ResolveCredentials failed: %v", err)
	}
	if creds.PublicKey != testPK {
		t.Errorf("PublicKey = %q, want %q", creds.PublicKey, testPK)
	}
	if creds.SecretKey != testSK {
		t.Errorf("SecretKey = %q, want %q", creds.SecretKey, testSK)
	}
}

// ============================================================================
// Functional Tests - Test against real API (requires credentials)
// ============================================================================

func getTestCredentials(t *testing.T) *Credentials {
	creds, err := ResolveCredentials("", "")
	if err != nil {
		t.Skip("No credentials available for functional tests")
	}
	return creds
}

func TestHealthCheck(t *testing.T) {
	if testing.Short() {
		t.Skip("Skipping functional test in short mode")
	}

	healthy := HealthCheck()
	if !healthy {
		t.Log("API health check returned unhealthy (API may be unreachable)")
	}
}

func TestGetLanguages(t *testing.T) {
	if testing.Short() {
		t.Skip("Skipping functional test in short mode")
	}

	creds := getTestCredentials(t)
	languages, err := GetLanguages(creds)
	if err != nil {
		t.Fatalf("GetLanguages failed: %v", err)
	}

	if len(languages) == 0 {
		t.Error("GetLanguages returned empty list")
	}

	// Should include common languages
	hasPython := false
	hasJavascript := false
	for _, lang := range languages {
		if lang == "python" {
			hasPython = true
		}
		if lang == "javascript" {
			hasJavascript = true
		}
	}

	if !hasPython {
		t.Error("GetLanguages missing 'python'")
	}
	if !hasJavascript {
		t.Error("GetLanguages missing 'javascript'")
	}
}

func TestValidateKeys(t *testing.T) {
	if testing.Short() {
		t.Skip("Skipping functional test in short mode")
	}

	creds := getTestCredentials(t)
	result, err := ValidateKeys(creds)
	if err != nil {
		t.Fatalf("ValidateKeys failed: %v", err)
	}

	if result == nil {
		t.Error("ValidateKeys returned nil")
	}
}

func TestExecuteCode(t *testing.T) {
	if testing.Short() {
		t.Skip("Skipping functional test in short mode")
	}

	creds := getTestCredentials(t)
	result, err := ExecuteCode(creds, "python", "print('hello from go test')")
	if err != nil {
		t.Fatalf("ExecuteCode failed: %v", err)
	}

	if result == nil {
		t.Error("ExecuteCode returned nil")
	}

	// Check for stdout in result
	if stdout, ok := result["stdout"].(string); ok {
		if stdout == "" {
			t.Error("ExecuteCode returned empty stdout")
		}
	}
}

func TestListSessions(t *testing.T) {
	if testing.Short() {
		t.Skip("Skipping functional test in short mode")
	}

	creds := getTestCredentials(t)
	sessions, err := ListSessions(creds)
	if err != nil {
		t.Fatalf("ListSessions failed: %v", err)
	}

	// Should return a list (possibly empty)
	if sessions == nil {
		t.Error("ListSessions returned nil")
	}
}

func TestListServices(t *testing.T) {
	if testing.Short() {
		t.Skip("Skipping functional test in short mode")
	}

	creds := getTestCredentials(t)
	services, err := ListServices(creds)
	if err != nil {
		t.Fatalf("ListServices failed: %v", err)
	}

	// Should return a list (possibly empty)
	if services == nil {
		t.Error("ListServices returned nil")
	}
}

func TestListSnapshots(t *testing.T) {
	if testing.Short() {
		t.Skip("Skipping functional test in short mode")
	}

	creds := getTestCredentials(t)
	snapshots, err := ListSnapshots(creds)
	if err != nil {
		t.Fatalf("ListSnapshots failed: %v", err)
	}

	// Should return a list (possibly empty)
	if snapshots == nil {
		t.Error("ListSnapshots returned nil")
	}
}

func TestListImages(t *testing.T) {
	if testing.Short() {
		t.Skip("Skipping functional test in short mode")
	}

	creds := getTestCredentials(t)
	images, err := ListImages(creds, "")
	if err != nil {
		t.Fatalf("ListImages failed: %v", err)
	}

	// Should return a list (possibly empty)
	if images == nil {
		t.Error("ListImages returned nil")
	}
}
