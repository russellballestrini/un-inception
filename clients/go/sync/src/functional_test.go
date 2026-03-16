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

// UN Go SDK - Functional Tests
//
// Tests library functions against real API.
// Requires: UNSANDBOX_PUBLIC_KEY, UNSANDBOX_SECRET_KEY
//
// Usage:
//   Copy to sync/src/ then: go test -v -run TestFunctional
package un

import (
	"os"
	"strings"
	"testing"
)

func skipIfNoCreds(t *testing.T) *Credentials {
	t.Helper()
	pk := os.Getenv("UNSANDBOX_PUBLIC_KEY")
	sk := os.Getenv("UNSANDBOX_SECRET_KEY")
	if pk == "" || sk == "" {
		t.Skip("UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY required")
	}
	return &Credentials{PublicKey: pk, SecretKey: sk}
}

func TestFunctionalHealthCheck(t *testing.T) {
	_ = skipIfNoCreds(t)
	result := HealthCheck()
	// HealthCheck returns a bool - just verify it runs without panic
	t.Logf("HealthCheck: %v", result)
}

func TestFunctionalValidateKeys(t *testing.T) {
	creds := skipIfNoCreds(t)
	info, err := ValidateKeys(creds)
	if err != nil {
		t.Fatalf("ValidateKeys error: %v", err)
	}
	if info == nil {
		t.Fatal("ValidateKeys returned nil")
	}
	valid, ok := info["valid"]
	if !ok {
		t.Fatal("ValidateKeys result missing 'valid' key")
	}
	if valid != true {
		t.Errorf("Keys should be valid, got: %v", valid)
	}
}

func TestFunctionalGetLanguages(t *testing.T) {
	creds := skipIfNoCreds(t)
	langs, err := GetLanguages(creds)
	if err != nil {
		t.Fatalf("GetLanguages error: %v", err)
	}
	if len(langs) == 0 {
		t.Fatal("GetLanguages returned empty list")
	}
	foundPython := false
	for _, l := range langs {
		if l == "python" {
			foundPython = true
			break
		}
	}
	if !foundPython {
		t.Error("python not found in languages list")
	}
	t.Logf("Found %d languages", len(langs))
}

func TestFunctionalExecute(t *testing.T) {
	creds := skipIfNoCreds(t)
	result, err := ExecuteCode(creds, "python", "print('hello from Go SDK')")
	if err != nil {
		t.Fatalf("ExecuteCode error: %v", err)
	}
	if result == nil {
		t.Fatal("ExecuteCode returned nil")
	}
	stdout, _ := result["stdout"].(string)
	if !strings.Contains(stdout, "hello from Go SDK") {
		t.Errorf("stdout should contain 'hello from Go SDK', got: %s", stdout)
	}
	exitCode, _ := result["exit_code"].(float64)
	if exitCode != 0 {
		t.Errorf("exit_code should be 0, got: %v", exitCode)
	}
}

func TestFunctionalExecuteError(t *testing.T) {
	creds := skipIfNoCreds(t)
	result, err := ExecuteCode(creds, "python", "import sys; sys.exit(1)")
	if err != nil {
		t.Fatalf("ExecuteCode error: %v", err)
	}
	if result == nil {
		t.Fatal("ExecuteCode returned nil")
	}
	exitCode, _ := result["exit_code"].(float64)
	if exitCode != 1 {
		t.Errorf("exit_code should be 1, got: %v", exitCode)
	}
}

func TestFunctionalSessionList(t *testing.T) {
	creds := skipIfNoCreds(t)
	sessions, err := ListSessions(creds)
	if err != nil {
		t.Fatalf("ListSessions error: %v", err)
	}
	t.Logf("Found %d sessions", len(sessions))
}

func TestFunctionalSessionLifecycle(t *testing.T) {
	creds := skipIfNoCreds(t)

	// Create
	session, err := CreateSession(creds, nil)
	if err != nil {
		t.Fatalf("CreateSession error: %v", err)
	}
	if session == nil {
		t.Fatal("CreateSession returned nil")
	}
	sessionID, _ := session["id"].(string)
	if sessionID == "" {
		t.Fatal("Session missing id")
	}
	t.Logf("Created session: %s", sessionID)

	// Destroy
	_, err = DeleteSession(creds, sessionID)
	if err != nil {
		t.Errorf("DeleteSession error: %v", err)
	}
}

func TestFunctionalServiceList(t *testing.T) {
	creds := skipIfNoCreds(t)
	services, err := ListServices(creds)
	if err != nil {
		t.Fatalf("ListServices error: %v", err)
	}
	t.Logf("Found %d services", len(services))
}

func TestFunctionalSnapshotList(t *testing.T) {
	creds := skipIfNoCreds(t)
	snapshots, err := ListSnapshots(creds)
	if err != nil {
		t.Fatalf("ListSnapshots error: %v", err)
	}
	t.Logf("Found %d snapshots", len(snapshots))
}

func TestFunctionalImageList(t *testing.T) {
	creds := skipIfNoCreds(t)
	images, err := ListImages(creds, "")
	if err != nil {
		t.Fatalf("ListImages error: %v", err)
	}
	t.Logf("Found %d images", len(images))
}
