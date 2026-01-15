/*
PUBLIC DOMAIN - NO LICENSE, NO WARRANTY

unsandbox.com Go SDK (Synchronous)

Library Usage:
    import "un"

    // Create credentials
    creds, err := un.ResolveCredentials("", "")
    if err != nil {
        log.Fatal(err)
    }

    // Execute code synchronously
    result, err := un.ExecuteCode(creds, "python", `print("hello")`)
    if err != nil {
        log.Fatal(err)
    }

    // Execute asynchronously
    jobID, err := un.ExecuteAsync(creds, "javascript", `console.log("hello")`)
    if err != nil {
        log.Fatal(err)
    }

    // Wait for job completion with exponential backoff
    result, err := un.WaitForJob(creds, jobID)
    if err != nil {
        log.Fatal(err)
    }

    // List all jobs
    jobs, err := un.ListJobs(creds)
    if err != nil {
        log.Fatal(err)
    }

    // Get supported languages
    languages, err := un.GetLanguages(creds)
    if err != nil {
        log.Fatal(err)
    }

    // Detect language from filename
    lang := un.DetectLanguage("script.py")  // Returns "python"

    // Snapshot operations (NEW)
    snapshotID, err := un.SessionSnapshot(creds, sessionID, "my_snapshot", false)
    snapshots, err := un.ListSnapshots(creds)
    result, err := un.RestoreSnapshot(creds, snapshotID)
    err = un.DeleteSnapshot(creds, snapshotID)

Authentication Priority (4-tier):
    1. Function arguments (publicKey, secretKey)
    2. Environment variables (UNSANDBOX_PUBLIC_KEY, UNSANDBOX_SECRET_KEY)
    3. Config file (~/.unsandbox/accounts.csv, line 0 by default)
    4. Local directory (./accounts.csv, line 0 by default)

    Format: public_key,secret_key (one per line)
    Account selection: UNSANDBOX_ACCOUNT=N env var (0-based index)

Request Authentication (HMAC-SHA256):
    Authorization: Bearer <public_key>                  (identifies account)
    X-Timestamp: <unix_seconds>                         (replay prevention)
    X-Signature: HMAC-SHA256(secret_key, msg)           (proves secret + body integrity)

    Message format: "timestamp:METHOD:path:body"
    - timestamp: seconds since epoch
    - METHOD: GET, POST, DELETE, etc. (uppercase)
    - path: e.g., "/execute", "/jobs/123"
    - body: JSON payload (empty string for GET/DELETE)

Languages Cache:
    - Cached in ~/.unsandbox/languages.json
    - TTL: 1 hour
    - Updated on successful API calls
*/

package un

import (
	"bytes"
	"crypto/hmac"
	"crypto/sha256"
	"encoding/hex"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"os"
	"os/user"
	"path/filepath"
	"strconv"
	"strings"
	"time"
)

const (
	APIBase              = "https://api.unsandbox.com"
	LanguagesCacheTTL    = 3600 // 1 hour
)

var (
	PollDelaysMs = []int{300, 450, 700, 900, 650, 1600, 2000}

	// Language detection mapping (file extension -> language)
	LanguageMap = map[string]string{
		"py":     "python",
		"js":     "javascript",
		"ts":     "typescript",
		"rb":     "ruby",
		"php":    "php",
		"pl":     "perl",
		"sh":     "bash",
		"r":      "r",
		"R":      "r",
		"lua":    "lua",
		"go":     "go",
		"rs":     "rust",
		"c":      "c",
		"cpp":    "cpp",
		"cc":     "cpp",
		"cxx":    "cpp",
		"java":   "java",
		"kt":     "kotlin",
		"m":      "objc",
		"cs":     "csharp",
		"fs":     "fsharp",
		"hs":     "haskell",
		"ml":     "ocaml",
		"clj":    "clojure",
		"scm":    "scheme",
		"ss":     "scheme",
		"erl":    "erlang",
		"ex":     "elixir",
		"exs":    "elixir",
		"jl":     "julia",
		"d":      "d",
		"nim":    "nim",
		"zig":    "zig",
		"v":      "v",
		"cr":     "crystal",
		"dart":   "dart",
		"groovy": "groovy",
		"f90":    "fortran",
		"f95":    "fortran",
		"lisp":   "commonlisp",
		"lsp":    "commonlisp",
		"cob":    "cobol",
		"tcl":    "tcl",
		"raku":   "raku",
		"pro":    "prolog",
		"p":      "prolog",
		"4th":    "forth",
		"forth":  "forth",
		"fth":    "forth",
	}
)

// CredentialsError is raised when credentials cannot be found or are invalid
type CredentialsError struct {
	Message string
}

func (e *CredentialsError) Error() string {
	return e.Message
}

// Credentials holds the public and secret API keys
type Credentials struct {
	PublicKey string
	SecretKey string
}

// getUnsandboxDir returns ~/.unsandbox directory path, creating if necessary
func getUnsandboxDir() (string, error) {
	user, err := user.Current()
	if err != nil {
		return "", err
	}

	dir := filepath.Join(user.HomeDir, ".unsandbox")
	if err := os.MkdirAll(dir, 0700); err != nil {
		return "", err
	}
	return dir, nil
}

// loadCredentialsFromCsv loads credentials from CSV file (public_key,secret_key per line)
func loadCredentialsFromCsv(csvPath string, accountIndex int) *Credentials {
	data, err := os.ReadFile(csvPath)
	if err != nil {
		return nil
	}

	lines := strings.Split(string(data), "\n")
	currentIndex := 0

	for _, line := range lines {
		line = strings.TrimSpace(line)
		if line == "" || strings.HasPrefix(line, "#") {
			continue
		}

		if currentIndex == accountIndex {
			parts := strings.Split(line, ",")
			if len(parts) >= 2 {
				return &Credentials{
					PublicKey: strings.TrimSpace(parts[0]),
					SecretKey: strings.TrimSpace(parts[1]),
				}
			}
		}
		currentIndex++
	}

	return nil
}

// ResolveCredentials resolves credentials from 4-tier priority system.
//
// Priority:
//   1. Function arguments (publicKey, secretKey non-empty)
//   2. Environment variables (UNSANDBOX_PUBLIC_KEY, UNSANDBOX_SECRET_KEY)
//   3. ~/.unsandbox/accounts.csv
//   4. ./accounts.csv
func ResolveCredentials(publicKey, secretKey string) (*Credentials, error) {
	// Tier 1: Function arguments
	if publicKey != "" && secretKey != "" {
		return &Credentials{
			PublicKey: publicKey,
			SecretKey: secretKey,
		}, nil
	}

	// Tier 2: Environment variables
	envPk := os.Getenv("UNSANDBOX_PUBLIC_KEY")
	envSk := os.Getenv("UNSANDBOX_SECRET_KEY")
	if envPk != "" && envSk != "" {
		return &Credentials{
			PublicKey: envPk,
			SecretKey: envSk,
		}, nil
	}

	// Determine account index
	accountIndex := 0
	if envAccount := os.Getenv("UNSANDBOX_ACCOUNT"); envAccount != "" {
		var err error
		accountIndex, err = strconv.Atoi(envAccount)
		if err != nil {
			accountIndex = 0
		}
	}

	// Tier 3: ~/.unsandbox/accounts.csv
	unsandboxDir, err := getUnsandboxDir()
	if err == nil {
		if creds := loadCredentialsFromCsv(filepath.Join(unsandboxDir, "accounts.csv"), accountIndex); creds != nil {
			return creds, nil
		}
	}

	// Tier 4: ./accounts.csv
	if creds := loadCredentialsFromCsv("accounts.csv", accountIndex); creds != nil {
		return creds, nil
	}

	return nil, &CredentialsError{
		Message: "No credentials found. Please provide via:\n" +
			"  1. Function arguments (publicKey, secretKey)\n" +
			"  2. Environment variables (UNSANDBOX_PUBLIC_KEY, UNSANDBOX_SECRET_KEY)\n" +
			"  3. ~/.unsandbox/accounts.csv\n" +
			"  4. ./accounts.csv",
	}
}

// signRequest signs a request using HMAC-SHA256
//
// Message format: "timestamp:METHOD:path:body"
// Returns: 64-character hex string
func signRequest(secretKey string, timestamp int64, method, path string, body []byte) string {
	bodyStr := ""
	if body != nil {
		bodyStr = string(body)
	}
	message := fmt.Sprintf("%d:%s:%s:%s", timestamp, method, path, bodyStr)

	h := hmac.New(sha256.New, []byte(secretKey))
	h.Write([]byte(message))
	return hex.EncodeToString(h.Sum(nil))
}

// makeRequest makes an authenticated HTTP request to the API
func makeRequest(method, path string, creds *Credentials, data interface{}) (map[string]interface{}, error) {
	url := APIBase + path
	timestamp := time.Now().Unix()

	var body []byte
	var err error
	if data != nil {
		body, err = json.Marshal(data)
		if err != nil {
			return nil, err
		}
	}

	signature := signRequest(creds.SecretKey, timestamp, method, path, body)

	req, err := http.NewRequest(method, url, bytes.NewReader(body))
	if err != nil {
		return nil, err
	}

	req.Header.Set("Authorization", fmt.Sprintf("Bearer %s", creds.PublicKey))
	req.Header.Set("X-Timestamp", fmt.Sprintf("%d", timestamp))
	req.Header.Set("X-Signature", signature)
	req.Header.Set("Content-Type", "application/json")
	req.Header.Set("User-Agent", "un-go/2.0")

	client := &http.Client{Timeout: 120 * time.Second}
	resp, err := client.Do(req)
	if err != nil {
		return nil, err
	}
	defer resp.Body.Close()

	respBody, err := io.ReadAll(resp.Body)
	if err != nil {
		return nil, err
	}

	if resp.StatusCode < 200 || resp.StatusCode >= 300 {
		return nil, fmt.Errorf("HTTP %d: %s", resp.StatusCode, string(respBody))
	}

	var result map[string]interface{}
	if err := json.Unmarshal(respBody, &result); err != nil {
		return nil, fmt.Errorf("failed to parse response: %w", err)
	}

	return result, nil
}

// getLanguagesCachePath returns path to languages cache file
func getLanguagesCachePath() (string, error) {
	unsandboxDir, err := getUnsandboxDir()
	if err != nil {
		return "", err
	}
	return filepath.Join(unsandboxDir, "languages.json"), nil
}

// loadLanguagesCache loads languages from cache if valid (< 1 hour old)
func loadLanguagesCache() ([]string, bool) {
	cachePath, err := getLanguagesCachePath()
	if err != nil {
		return nil, false
	}

	data, err := os.ReadFile(cachePath)
	if err != nil {
		return nil, false
	}

	stat, err := os.Stat(cachePath)
	if err != nil {
		return nil, false
	}

	ageSeconds := int(time.Since(stat.ModTime()).Seconds())
	if ageSeconds >= LanguagesCacheTTL {
		return nil, false
	}

	var cacheData map[string]interface{}
	if err := json.Unmarshal(data, &cacheData); err != nil {
		return nil, false
	}

	if langs, ok := cacheData["languages"].([]interface{}); ok {
		result := make([]string, len(langs))
		for i, lang := range langs {
			if s, ok := lang.(string); ok {
				result[i] = s
			}
		}
		return result, true
	}

	return nil, false
}

// saveLanguagesCache saves languages to cache
func saveLanguagesCache(languages []string) {
	cachePath, err := getLanguagesCachePath()
	if err != nil {
		return
	}

	cacheData := map[string]interface{}{
		"languages": languages,
		"timestamp": time.Now().Unix(),
	}

	data, err := json.MarshalIndent(cacheData, "", "  ")
	if err != nil {
		return
	}

	_ = os.WriteFile(cachePath, data, 0600)
}

// ExecuteCode executes code synchronously (blocks until completion)
func ExecuteCode(creds *Credentials, language, code string) (map[string]interface{}, error) {
	data := map[string]string{
		"language": language,
		"code":     code,
	}

	response, err := makeRequest("POST", "/execute", creds, data)
	if err != nil {
		return nil, err
	}

	// If we got a job_id, poll until completion
	if jobID, ok := response["job_id"].(string); ok {
		if status, ok := response["status"].(string); ok && (status == "pending" || status == "running") {
			return WaitForJob(creds, jobID)
		}
	}

	return response, nil
}

// ExecuteAsync executes code asynchronously (returns immediately with job_id)
func ExecuteAsync(creds *Credentials, language, code string) (string, error) {
	data := map[string]string{
		"language": language,
		"code":     code,
	}

	response, err := makeRequest("POST", "/execute", creds, data)
	if err != nil {
		return "", err
	}

	if jobID, ok := response["job_id"].(string); ok {
		return jobID, nil
	}

	return "", fmt.Errorf("no job_id in response")
}

// GetJob gets current status/result of a job (single poll, no waiting)
func GetJob(creds *Credentials, jobID string) (map[string]interface{}, error) {
	return makeRequest("GET", fmt.Sprintf("/jobs/%s", jobID), creds, nil)
}

// WaitForJob waits for job completion with exponential backoff polling
//
// Polling delays (ms): [300, 450, 700, 900, 650, 1600, 2000, ...]
// Cumulative: 300, 750, 1450, 2350, 3000, 4600, 6600ms+
func WaitForJob(creds *Credentials, jobID string) (map[string]interface{}, error) {
	pollCount := 0

	for {
		// Sleep before polling
		delayIdx := pollCount
		if delayIdx >= len(PollDelaysMs) {
			delayIdx = len(PollDelaysMs) - 1
		}
		time.Sleep(time.Duration(PollDelaysMs[delayIdx]) * time.Millisecond)
		pollCount++

		response, err := GetJob(creds, jobID)
		if err != nil {
			return nil, err
		}

		if status, ok := response["status"].(string); ok {
			if status == "completed" || status == "failed" || status == "timeout" || status == "cancelled" {
				return response, nil
			}
		}

		// Still running, continue polling
	}
}

// CancelJob cancels a running job
func CancelJob(creds *Credentials, jobID string) (map[string]interface{}, error) {
	return makeRequest("DELETE", fmt.Sprintf("/jobs/%s", jobID), creds, nil)
}

// ListJobs lists all jobs for the authenticated account
func ListJobs(creds *Credentials) ([]map[string]interface{}, error) {
	response, err := makeRequest("GET", "/jobs", creds, nil)
	if err != nil {
		return nil, err
	}

	if jobs, ok := response["jobs"].([]interface{}); ok {
		result := make([]map[string]interface{}, len(jobs))
		for i, job := range jobs {
			if m, ok := job.(map[string]interface{}); ok {
				result[i] = m
			}
		}
		return result, nil
	}

	return []map[string]interface{}{}, nil
}

// GetLanguages gets list of supported programming languages
//
// Results are cached for 1 hour in ~/.unsandbox/languages.json
func GetLanguages(creds *Credentials) ([]string, error) {
	// Try cache first
	if cached, ok := loadLanguagesCache(); ok {
		return cached, nil
	}

	response, err := makeRequest("GET", "/languages", creds, nil)
	if err != nil {
		return nil, err
	}

	var languages []string
	if langs, ok := response["languages"].([]interface{}); ok {
		for _, lang := range langs {
			if s, ok := lang.(string); ok {
				languages = append(languages, s)
			}
		}
	}

	// Cache the result
	saveLanguagesCache(languages)
	return languages, nil
}

// DetectLanguage detects programming language from filename extension
//
// Args:
//   filename: Filename to detect language from (e.g., "script.py")
//
// Returns:
//   Language identifier (e.g., "python") or empty string if unknown
//
// Examples:
//   DetectLanguage("hello.py")   // -> "python"
//   DetectLanguage("script.js")  // -> "javascript"
//   DetectLanguage("main.go")    // -> "go"
//   DetectLanguage("unknown")    // -> ""
func DetectLanguage(filename string) string {
	if !strings.Contains(filename, ".") {
		return ""
	}

	parts := strings.Split(filename, ".")
	ext := parts[len(parts)-1]

	// Try exact match first (for case-sensitive extensions like .R)
	if lang, ok := LanguageMap[ext]; ok {
		return lang
	}

	// Try lowercase match
	if lang, ok := LanguageMap[strings.ToLower(ext)]; ok {
		return lang
	}

	return ""
}

// SessionSnapshot creates a snapshot of a session (NEW)
func SessionSnapshot(creds *Credentials, sessionID, name string, hot bool) (string, error) {
	data := map[string]interface{}{
		"session_id": sessionID,
		"hot":        hot,
	}
	if name != "" {
		data["name"] = name
	}

	response, err := makeRequest("POST", "/snapshots", creds, data)
	if err != nil {
		return "", err
	}

	if snapshotID, ok := response["snapshot_id"].(string); ok {
		return snapshotID, nil
	}

	return "", fmt.Errorf("no snapshot_id in response")
}

// ServiceSnapshot creates a snapshot of a service (NEW)
func ServiceSnapshot(creds *Credentials, serviceID, name string, hot bool) (string, error) {
	data := map[string]interface{}{
		"service_id": serviceID,
		"hot":        hot,
	}
	if name != "" {
		data["name"] = name
	}

	response, err := makeRequest("POST", "/snapshots", creds, data)
	if err != nil {
		return "", err
	}

	if snapshotID, ok := response["snapshot_id"].(string); ok {
		return snapshotID, nil
	}

	return "", fmt.Errorf("no snapshot_id in response")
}

// ListSnapshots lists all snapshots (NEW)
func ListSnapshots(creds *Credentials) ([]map[string]interface{}, error) {
	response, err := makeRequest("GET", "/snapshots", creds, nil)
	if err != nil {
		return nil, err
	}

	if snapshots, ok := response["snapshots"].([]interface{}); ok {
		result := make([]map[string]interface{}, len(snapshots))
		for i, snapshot := range snapshots {
			if m, ok := snapshot.(map[string]interface{}); ok {
				result[i] = m
			}
		}
		return result, nil
	}

	return []map[string]interface{}{}, nil
}

// RestoreSnapshot restores a snapshot (NEW)
func RestoreSnapshot(creds *Credentials, snapshotID string) (map[string]interface{}, error) {
	return makeRequest("POST", fmt.Sprintf("/snapshots/%s/restore", snapshotID), creds, map[string]interface{}{})
}

// DeleteSnapshot deletes a snapshot (NEW)
func DeleteSnapshot(creds *Credentials, snapshotID string) (map[string]interface{}, error) {
	return makeRequest("DELETE", fmt.Sprintf("/snapshots/%s", snapshotID), creds, nil)
}
