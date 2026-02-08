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
	"bufio"
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

// SudoChallengeError represents a 428 response requiring OTP confirmation
type SudoChallengeError struct {
	ChallengeID string
	Message     string
	StatusCode  int
	Body        []byte
}

func (e *SudoChallengeError) Error() string {
	return fmt.Sprintf("HTTP 428: sudo challenge required (challenge_id: %s)", e.ChallengeID)
}

// makeRequestWithSudo makes an authenticated HTTP request with optional sudo headers
func makeRequestWithSudo(method, path string, creds *Credentials, data interface{}, sudoOTP, sudoChallengeID string) (map[string]interface{}, int, []byte, error) {
	url := APIBase + path
	timestamp := time.Now().Unix()

	var body []byte
	var err error
	if data != nil {
		body, err = json.Marshal(data)
		if err != nil {
			return nil, 0, nil, err
		}
	}

	signature := signRequest(creds.SecretKey, timestamp, method, path, body)

	req, err := http.NewRequest(method, url, bytes.NewReader(body))
	if err != nil {
		return nil, 0, nil, err
	}

	req.Header.Set("Authorization", fmt.Sprintf("Bearer %s", creds.PublicKey))
	req.Header.Set("X-Timestamp", fmt.Sprintf("%d", timestamp))
	req.Header.Set("X-Signature", signature)
	req.Header.Set("Content-Type", "application/json")
	req.Header.Set("User-Agent", "un-go/2.0")

	// Add sudo headers if provided
	if sudoOTP != "" {
		req.Header.Set("X-Sudo-OTP", sudoOTP)
	}
	if sudoChallengeID != "" {
		req.Header.Set("X-Sudo-Challenge", sudoChallengeID)
	}

	client := &http.Client{Timeout: 120 * time.Second}
	resp, err := client.Do(req)
	if err != nil {
		return nil, 0, nil, err
	}
	defer resp.Body.Close()

	respBody, err := io.ReadAll(resp.Body)
	if err != nil {
		return nil, resp.StatusCode, nil, err
	}

	if resp.StatusCode < 200 || resp.StatusCode >= 300 {
		return nil, resp.StatusCode, respBody, fmt.Errorf("HTTP %d: %s", resp.StatusCode, string(respBody))
	}

	var result map[string]interface{}
	if err := json.Unmarshal(respBody, &result); err != nil {
		return nil, resp.StatusCode, respBody, fmt.Errorf("failed to parse response: %w", err)
	}

	return result, resp.StatusCode, respBody, nil
}

// handleSudoChallenge handles 428 sudo OTP challenge - prompts user for OTP and retries request
func handleSudoChallenge(method, path string, creds *Credentials, data interface{}, responseBody []byte) (map[string]interface{}, error) {
	// Extract challenge_id from response
	var resp map[string]interface{}
	if err := json.Unmarshal(responseBody, &resp); err != nil {
		return nil, fmt.Errorf("failed to parse 428 response: %w", err)
	}

	challengeID, _ := resp["challenge_id"].(string)

	// Prompt user for OTP
	fmt.Fprintf(os.Stderr, "\033[33mConfirmation required. Check your email for a one-time code.\033[0m\n")
	fmt.Fprintf(os.Stderr, "Enter OTP: ")

	reader := bufio.NewReader(os.Stdin)
	otp, err := reader.ReadString('\n')
	if err != nil {
		return nil, fmt.Errorf("failed to read OTP: %w", err)
	}
	otp = strings.TrimSpace(otp)

	if otp == "" {
		return nil, fmt.Errorf("operation cancelled")
	}

	// Retry the request with sudo headers
	result, statusCode, retryBody, err := makeRequestWithSudo(method, path, creds, data, otp, challengeID)
	if err != nil {
		if statusCode >= 200 && statusCode < 300 {
			return result, nil
		}
		// Extract error message from response if available
		if retryBody != nil {
			var errResp map[string]interface{}
			if json.Unmarshal(retryBody, &errResp) == nil {
				if errMsg, ok := errResp["error"].(string); ok {
					return nil, fmt.Errorf("%s", errMsg)
				}
			}
		}
		return nil, err
	}

	fmt.Fprintf(os.Stderr, "\033[32mOperation completed successfully\033[0m\n")
	return result, nil
}

// makeDestructiveRequest makes a request that may require sudo OTP confirmation (for 428 responses)
func makeDestructiveRequest(method, path string, creds *Credentials, data interface{}) (map[string]interface{}, error) {
	result, statusCode, respBody, err := makeRequestWithSudo(method, path, creds, data, "", "")
	if statusCode == 428 {
		return handleSudoChallenge(method, path, creds, data, respBody)
	}
	if err != nil {
		return nil, err
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
// This operation may require sudo OTP confirmation (428 response handling)
func DeleteSnapshot(creds *Credentials, snapshotID string) (map[string]interface{}, error) {
	return makeDestructiveRequest("DELETE", fmt.Sprintf("/snapshots/%s", snapshotID), creds, nil)
}

// ============================================================================
// Session Operations
// ============================================================================

// SessionOptions contains optional parameters for session creation.
type SessionOptions struct {
	NetworkMode string // "zerotrust" (default) or "semitrusted"
	Shell       string // Shell to use (e.g., "bash", "python3")
	TTL         int    // Time-to-live in seconds (default: 3600)
	VCPU        int    // Number of virtual CPUs (default: 1)
	Multiplexer string // Multiplexer to use (e.g., "tmux")
}

// ListSessions lists all active sessions for the authenticated account.
func ListSessions(creds *Credentials) ([]map[string]interface{}, error) {
	response, err := makeRequest("GET", "/sessions", creds, nil)
	if err != nil {
		return nil, err
	}

	if sessions, ok := response["sessions"].([]interface{}); ok {
		result := make([]map[string]interface{}, len(sessions))
		for i, session := range sessions {
			if m, ok := session.(map[string]interface{}); ok {
				result[i] = m
			}
		}
		return result, nil
	}

	return []map[string]interface{}{}, nil
}

// GetSession gets details of a specific session.
func GetSession(creds *Credentials, sessionID string) (map[string]interface{}, error) {
	return makeRequest("GET", fmt.Sprintf("/sessions/%s", sessionID), creds, nil)
}

// CreateSession creates a new interactive session.
//
// Args:
//
//	creds: API credentials
//	opts: Optional session configuration (can be nil for defaults)
//
// Returns:
//
//	Session info including session_id and container_name
func CreateSession(creds *Credentials, opts *SessionOptions) (map[string]interface{}, error) {
	data := make(map[string]interface{})

	if opts != nil {
		if opts.NetworkMode != "" {
			data["network_mode"] = opts.NetworkMode
		}
		if opts.Shell != "" {
			data["shell"] = opts.Shell
		}
		if opts.TTL > 0 {
			data["ttl"] = opts.TTL
		}
		if opts.VCPU > 0 {
			data["vcpu"] = opts.VCPU
		}
		if opts.Multiplexer != "" {
			data["multiplexer"] = opts.Multiplexer
		}
	}

	return makeRequest("POST", "/sessions", creds, data)
}

// DeleteSession terminates a session.
func DeleteSession(creds *Credentials, sessionID string) (map[string]interface{}, error) {
	return makeRequest("DELETE", fmt.Sprintf("/sessions/%s", sessionID), creds, nil)
}

// FreezeSession freezes a session (pauses execution, preserves state).
func FreezeSession(creds *Credentials, sessionID string) (map[string]interface{}, error) {
	return makeRequest("POST", fmt.Sprintf("/sessions/%s/freeze", sessionID), creds, map[string]interface{}{})
}

// UnfreezeSession unfreezes a previously frozen session.
func UnfreezeSession(creds *Credentials, sessionID string) (map[string]interface{}, error) {
	return makeRequest("POST", fmt.Sprintf("/sessions/%s/unfreeze", sessionID), creds, map[string]interface{}{})
}

// BoostSession increases the vCPU allocation for a session.
//
// Args:
//
//	creds: API credentials
//	sessionID: Session ID to boost
//	vcpu: Number of vCPUs (2, 4, 8, etc.)
func BoostSession(creds *Credentials, sessionID string, vcpu int) (map[string]interface{}, error) {
	data := map[string]interface{}{
		"vcpu": vcpu,
	}
	return makeRequest("POST", fmt.Sprintf("/sessions/%s/boost", sessionID), creds, data)
}

// UnboostSession resets the vCPU allocation for a session to default.
func UnboostSession(creds *Credentials, sessionID string) (map[string]interface{}, error) {
	return makeRequest("POST", fmt.Sprintf("/sessions/%s/unboost", sessionID), creds, map[string]interface{}{})
}

// ShellSession executes a command in a session's shell.
//
// Note: For interactive shell access, use the WebSocket-based shell endpoint.
// This function is for executing single commands.
func ShellSession(creds *Credentials, sessionID, command string) (map[string]interface{}, error) {
	data := map[string]interface{}{
		"command": command,
	}
	return makeRequest("POST", fmt.Sprintf("/sessions/%s/shell", sessionID), creds, data)
}

// ============================================================================
// Service Operations
// ============================================================================

// ServiceOptions contains optional parameters for service creation.
type ServiceOptions struct {
	NetworkMode      string // "zerotrust" (default) or "semitrusted"
	Shell            string // Shell to use for bootstrap
	VCPU             int    // Number of virtual CPUs
	UnfreezeOnDemand bool   // Enable automatic unfreezing on HTTP request
}

// ServiceUpdateOptions contains optional parameters for service updates.
type ServiceUpdateOptions struct {
	VCPU int // Number of virtual CPUs
}

// ListServices lists all services for the authenticated account.
func ListServices(creds *Credentials) ([]map[string]interface{}, error) {
	response, err := makeRequest("GET", "/services", creds, nil)
	if err != nil {
		return nil, err
	}

	if services, ok := response["services"].([]interface{}); ok {
		result := make([]map[string]interface{}, len(services))
		for i, service := range services {
			if m, ok := service.(map[string]interface{}); ok {
				result[i] = m
			}
		}
		return result, nil
	}

	return []map[string]interface{}{}, nil
}

// CreateService creates a new persistent service.
//
// Args:
//
//	creds: API credentials
//	name: Service name
//	ports: Array of port numbers to expose
//	bootstrap: Bootstrap script to run on service start
//	opts: Optional service configuration (can be nil for defaults)
func CreateService(creds *Credentials, name string, ports []int, bootstrap string, opts *ServiceOptions) (map[string]interface{}, error) {
	data := map[string]interface{}{
		"name":      name,
		"ports":     ports,
		"bootstrap": bootstrap,
	}

	if opts != nil {
		if opts.NetworkMode != "" {
			data["network_mode"] = opts.NetworkMode
		}
		if opts.Shell != "" {
			data["shell"] = opts.Shell
		}
		if opts.VCPU > 0 {
			data["vcpu"] = opts.VCPU
		}
		if opts.UnfreezeOnDemand {
			data["unfreeze_on_demand"] = true
		}
	}

	return makeRequest("POST", "/services", creds, data)
}

// GetService gets details of a specific service.
func GetService(creds *Credentials, serviceID string) (map[string]interface{}, error) {
	return makeRequest("GET", fmt.Sprintf("/services/%s", serviceID), creds, nil)
}

// UpdateService updates a service's configuration.
func UpdateService(creds *Credentials, serviceID string, opts *ServiceUpdateOptions) (map[string]interface{}, error) {
	data := make(map[string]interface{})
	if opts != nil {
		if opts.VCPU > 0 {
			data["vcpu"] = opts.VCPU
		}
	}
	return makeRequest("PATCH", fmt.Sprintf("/services/%s", serviceID), creds, data)
}

// DeleteService destroys a service.
// This operation may require sudo OTP confirmation (428 response handling)
func DeleteService(creds *Credentials, serviceID string) (map[string]interface{}, error) {
	return makeDestructiveRequest("DELETE", fmt.Sprintf("/services/%s", serviceID), creds, nil)
}

// FreezeService freezes a service (pauses execution, preserves state).
func FreezeService(creds *Credentials, serviceID string) (map[string]interface{}, error) {
	return makeRequest("POST", fmt.Sprintf("/services/%s/freeze", serviceID), creds, map[string]interface{}{})
}

// UnfreezeService unfreezes a previously frozen service.
func UnfreezeService(creds *Credentials, serviceID string) (map[string]interface{}, error) {
	return makeRequest("POST", fmt.Sprintf("/services/%s/unfreeze", serviceID), creds, map[string]interface{}{})
}

// LockService locks a service to prevent modifications or deletion.
func LockService(creds *Credentials, serviceID string) (map[string]interface{}, error) {
	return makeRequest("POST", fmt.Sprintf("/services/%s/lock", serviceID), creds, map[string]interface{}{})
}

// UnlockService unlocks a previously locked service.
// This operation may require sudo OTP confirmation (428 response handling)
func UnlockService(creds *Credentials, serviceID string) (map[string]interface{}, error) {
	return makeDestructiveRequest("POST", fmt.Sprintf("/services/%s/unlock", serviceID), creds, map[string]interface{}{})
}

// SetUnfreezeOnDemand enables or disables automatic unfreezing on HTTP request.
func SetUnfreezeOnDemand(creds *Credentials, serviceID string, enabled bool) (map[string]interface{}, error) {
	return makeRequest("PATCH", fmt.Sprintf("/services/%s", serviceID), creds, map[string]interface{}{
		"unfreeze_on_demand": enabled,
	})
}

// GetServiceLogs retrieves logs from a service.
//
// Args:
//
//	creds: API credentials
//	serviceID: Service ID
//	all: If true, returns all logs; if false, returns only recent logs
func GetServiceLogs(creds *Credentials, serviceID string, all bool) (map[string]interface{}, error) {
	path := fmt.Sprintf("/services/%s/logs", serviceID)
	if all {
		path = fmt.Sprintf("/services/%s/logs?all=true", serviceID)
	}
	return makeRequest("GET", path, creds, nil)
}

// GetServiceEnv retrieves the environment variable names for a service.
// Note: Values are not returned for security; use ExportServiceEnv for full export.
func GetServiceEnv(creds *Credentials, serviceID string) (map[string]interface{}, error) {
	return makeRequest("GET", fmt.Sprintf("/services/%s/env", serviceID), creds, nil)
}

// SetServiceEnv sets environment variables for a service.
//
// Args:
//
//	creds: API credentials
//	serviceID: Service ID
//	env: Map of environment variable names to values
func SetServiceEnv(creds *Credentials, serviceID string, env map[string]string) (map[string]interface{}, error) {
	return makeRequest("POST", fmt.Sprintf("/services/%s/env", serviceID), creds, env)
}

// DeleteServiceEnv deletes environment variables from a service.
//
// Args:
//
//	creds: API credentials
//	serviceID: Service ID
//	keys: List of environment variable names to delete (nil deletes all)
func DeleteServiceEnv(creds *Credentials, serviceID string, keys []string) (map[string]interface{}, error) {
	var data interface{}
	if keys != nil {
		data = map[string]interface{}{"keys": keys}
	}
	return makeRequest("DELETE", fmt.Sprintf("/services/%s/env", serviceID), creds, data)
}

// ExportServiceEnv exports all environment variables for a service.
// Returns the full .env format content with values.
func ExportServiceEnv(creds *Credentials, serviceID string) (map[string]interface{}, error) {
	return makeRequest("POST", fmt.Sprintf("/services/%s/env/export", serviceID), creds, map[string]interface{}{})
}

// RedeployService redeploys a service with optional new bootstrap script.
//
// Args:
//
//	creds: API credentials
//	serviceID: Service ID
//	bootstrap: New bootstrap script (empty string to keep existing)
func RedeployService(creds *Credentials, serviceID string, bootstrap string) (map[string]interface{}, error) {
	data := make(map[string]interface{})
	if bootstrap != "" {
		data["bootstrap"] = bootstrap
	}
	return makeRequest("POST", fmt.Sprintf("/services/%s/redeploy", serviceID), creds, data)
}

// ExecuteInService executes a command in a running service.
func ExecuteInService(creds *Credentials, serviceID, command string) (map[string]interface{}, error) {
	data := map[string]interface{}{
		"command": command,
	}
	return makeRequest("POST", fmt.Sprintf("/services/%s/execute", serviceID), creds, data)
}

// ResizeService changes the vCPU count for a running service.
//
// Args:
//
//	creds: API credentials
//	serviceID: Service ID
//	vcpu: New vCPU count (1-8)
func ResizeService(creds *Credentials, serviceID string, vcpu int) (map[string]interface{}, error) {
	data := map[string]interface{}{
		"vcpu": vcpu,
	}
	return makeRequest("POST", fmt.Sprintf("/services/%s/resize", serviceID), creds, data)
}

// ============================================================================
// Additional Snapshot Operations
// ============================================================================

// LockSnapshot locks a snapshot to prevent deletion.
func LockSnapshot(creds *Credentials, snapshotID string) (map[string]interface{}, error) {
	return makeRequest("POST", fmt.Sprintf("/snapshots/%s/lock", snapshotID), creds, map[string]interface{}{})
}

// UnlockSnapshot unlocks a previously locked snapshot.
// This operation may require sudo OTP confirmation (428 response handling)
func UnlockSnapshot(creds *Credentials, snapshotID string) (map[string]interface{}, error) {
	return makeDestructiveRequest("POST", fmt.Sprintf("/snapshots/%s/unlock", snapshotID), creds, map[string]interface{}{})
}

// CloneSnapshotOptions contains optional parameters for snapshot cloning.
type CloneSnapshotOptions struct {
	Name   string   // Name for the cloned resource
	Shell  string   // Shell to use (for session clones)
	Ports  []int    // Ports to expose (for service clones)
}

// CloneSnapshot clones a snapshot into a new session or service.
//
// Args:
//
//	creds: API credentials
//	snapshotID: Snapshot ID to clone
//	cloneType: "session" or "service"
//	opts: Optional clone configuration (can be nil)
func CloneSnapshot(creds *Credentials, snapshotID, cloneType string, opts *CloneSnapshotOptions) (map[string]interface{}, error) {
	data := map[string]interface{}{
		"type": cloneType,
	}

	if opts != nil {
		if opts.Name != "" {
			data["name"] = opts.Name
		}
		if opts.Shell != "" {
			data["shell"] = opts.Shell
		}
		if opts.Ports != nil {
			data["ports"] = opts.Ports
		}
	}

	return makeRequest("POST", fmt.Sprintf("/snapshots/%s/clone", snapshotID), creds, data)
}

// ============================================================================
// Key Validation
// ============================================================================

// ValidateKeys validates the API credentials with the server.
// Returns account information if valid, error if invalid.
func ValidateKeys(creds *Credentials) (map[string]interface{}, error) {
	return makeRequest("POST", "/keys/validate", creds, map[string]interface{}{})
}

// ============================================================================
// Image Generation
// ============================================================================

// ImageOptions contains options for image generation
type ImageOptions struct {
	Model   string
	Size    string // default "1024x1024"
	Quality string // "standard" or "hd"
	N       int    // number of images
}

// ImageResult contains the generated images
type ImageResult struct {
	Images    []string `json:"images"`
	CreatedAt string   `json:"created_at"`
}

// Image generates images from a text prompt using AI.
func Image(creds *Credentials, prompt string, opts *ImageOptions) (*ImageResult, error) {
	if opts == nil {
		opts = &ImageOptions{}
	}
	if opts.Size == "" {
		opts.Size = "1024x1024"
	}
	if opts.Quality == "" {
		opts.Quality = "standard"
	}
	if opts.N == 0 {
		opts.N = 1
	}

	payload := map[string]interface{}{
		"prompt":  prompt,
		"size":    opts.Size,
		"quality": opts.Quality,
		"n":       opts.N,
	}
	if opts.Model != "" {
		payload["model"] = opts.Model
	}

	response, err := makeRequest("POST", "/image", creds, payload)
	if err != nil {
		return nil, err
	}

	// Extract images array from response
	var images []string
	if imagesInterface, ok := response["images"].([]interface{}); ok {
		for _, img := range imagesInterface {
			if s, ok := img.(string); ok {
				images = append(images, s)
			}
		}
	}

	createdAt := ""
	if ca, ok := response["created_at"].(string); ok {
		createdAt = ca
	}

	return &ImageResult{
		Images:    images,
		CreatedAt: createdAt,
	}, nil
}

// ============================================================================
// Snapshot Info
// ============================================================================

// GetSnapshot gets details of a specific snapshot.
func GetSnapshot(creds *Credentials, snapshotID string) (map[string]interface{}, error) {
	return makeRequest("GET", fmt.Sprintf("/snapshots/%s", snapshotID), creds, nil)
}

// ============================================================================
// LXD Container Images API
// ============================================================================

// ImagePublishOptions contains options for publishing an LXD container image.
type ImagePublishOptions struct {
	Name        string // Optional name for the image
	Description string // Optional description for the image
}

// ImagePublish publishes an LXD container image from a session or service.
//
// Args:
//
//	creds: API credentials
//	sourceType: Source type ("session" or "service")
//	sourceID: ID of the session or service to publish
//	opts: Optional image configuration (can be nil)
//
// Returns:
//
//	Image info including image_id
func ImagePublish(creds *Credentials, sourceType, sourceID string, opts *ImagePublishOptions) (map[string]interface{}, error) {
	data := map[string]interface{}{
		"source_type": sourceType,
		"source_id":   sourceID,
	}

	if opts != nil {
		if opts.Name != "" {
			data["name"] = opts.Name
		}
		if opts.Description != "" {
			data["description"] = opts.Description
		}
	}

	return makeRequest("POST", "/images", creds, data)
}

// ListImages lists LXD container images.
//
// Args:
//
//	creds: API credentials
//	filterType: Optional filter type ("mine", "shared", "public", or empty for all accessible)
//
// Returns:
//
//	List of images
func ListImages(creds *Credentials, filterType string) ([]map[string]interface{}, error) {
	path := "/images"
	if filterType != "" {
		path = fmt.Sprintf("/images/%s", filterType)
	}

	response, err := makeRequest("GET", path, creds, nil)
	if err != nil {
		return nil, err
	}

	if images, ok := response["images"].([]interface{}); ok {
		result := make([]map[string]interface{}, len(images))
		for i, image := range images {
			if m, ok := image.(map[string]interface{}); ok {
				result[i] = m
			}
		}
		return result, nil
	}

	return []map[string]interface{}{}, nil
}

// GetImage gets details of a specific LXD container image.
func GetImage(creds *Credentials, imageID string) (map[string]interface{}, error) {
	return makeRequest("GET", fmt.Sprintf("/images/%s", imageID), creds, nil)
}

// DeleteImage deletes an LXD container image.
//
// Note: Locked images cannot be deleted. Use UnlockImage first if needed.
// This operation may require sudo OTP confirmation (428 response handling)
func DeleteImage(creds *Credentials, imageID string) (map[string]interface{}, error) {
	return makeDestructiveRequest("DELETE", fmt.Sprintf("/images/%s", imageID), creds, nil)
}

// LockImage locks an LXD container image to prevent deletion.
func LockImage(creds *Credentials, imageID string) (map[string]interface{}, error) {
	return makeRequest("POST", fmt.Sprintf("/images/%s/lock", imageID), creds, map[string]interface{}{})
}

// UnlockImage unlocks a previously locked LXD container image.
// This operation may require sudo OTP confirmation (428 response handling)
func UnlockImage(creds *Credentials, imageID string) (map[string]interface{}, error) {
	return makeDestructiveRequest("POST", fmt.Sprintf("/images/%s/unlock", imageID), creds, map[string]interface{}{})
}

// SetImageVisibility sets the visibility of an LXD container image.
//
// Args:
//
//	creds: API credentials
//	imageID: Image ID
//	visibility: Visibility level ("private", "shared", or "public")
func SetImageVisibility(creds *Credentials, imageID, visibility string) (map[string]interface{}, error) {
	data := map[string]interface{}{
		"visibility": visibility,
	}
	return makeRequest("POST", fmt.Sprintf("/images/%s/visibility", imageID), creds, data)
}

// GrantImageAccess grants access to an LXD container image for another API key.
//
// Args:
//
//	creds: API credentials
//	imageID: Image ID
//	trustedKey: The public API key to grant access to
func GrantImageAccess(creds *Credentials, imageID, trustedKey string) (map[string]interface{}, error) {
	data := map[string]interface{}{
		"trusted_key": trustedKey,
	}
	return makeRequest("POST", fmt.Sprintf("/images/%s/grant", imageID), creds, data)
}

// RevokeImageAccess revokes access to an LXD container image from another API key.
//
// Args:
//
//	creds: API credentials
//	imageID: Image ID
//	trustedKey: The public API key to revoke access from
func RevokeImageAccess(creds *Credentials, imageID, trustedKey string) (map[string]interface{}, error) {
	data := map[string]interface{}{
		"trusted_key": trustedKey,
	}
	return makeRequest("POST", fmt.Sprintf("/images/%s/revoke", imageID), creds, data)
}

// ListImageTrusted lists the API keys that have been granted access to an image.
func ListImageTrusted(creds *Credentials, imageID string) ([]map[string]interface{}, error) {
	response, err := makeRequest("GET", fmt.Sprintf("/images/%s/trusted", imageID), creds, nil)
	if err != nil {
		return nil, err
	}

	if trusted, ok := response["trusted"].([]interface{}); ok {
		result := make([]map[string]interface{}, len(trusted))
		for i, t := range trusted {
			if m, ok := t.(map[string]interface{}); ok {
				result[i] = m
			}
		}
		return result, nil
	}

	return []map[string]interface{}{}, nil
}

// TransferImage transfers ownership of an LXD container image to another API key.
//
// Args:
//
//	creds: API credentials
//	imageID: Image ID
//	toAPIKey: The public API key to transfer ownership to
func TransferImage(creds *Credentials, imageID, toAPIKey string) (map[string]interface{}, error) {
	data := map[string]interface{}{
		"to_api_key": toAPIKey,
	}
	return makeRequest("POST", fmt.Sprintf("/images/%s/transfer", imageID), creds, data)
}

// SpawnFromImageOptions contains options for spawning a service from an image.
type SpawnFromImageOptions struct {
	Name        string // Service name
	Ports       []int  // Ports to expose
	Bootstrap   string // Bootstrap script
	NetworkMode string // "zerotrust" or "semitrusted"
}

// SpawnFromImage spawns a new service from an LXD container image.
//
// Args:
//
//	creds: API credentials
//	imageID: Image ID to spawn from
//	opts: Spawn configuration options
//
// Returns:
//
//	Service info including service_id
func SpawnFromImage(creds *Credentials, imageID string, opts *SpawnFromImageOptions) (map[string]interface{}, error) {
	data := make(map[string]interface{})

	if opts != nil {
		if opts.Name != "" {
			data["name"] = opts.Name
		}
		if opts.Ports != nil {
			data["ports"] = opts.Ports
		}
		if opts.Bootstrap != "" {
			data["bootstrap"] = opts.Bootstrap
		}
		if opts.NetworkMode != "" {
			data["network_mode"] = opts.NetworkMode
		}
	}

	return makeRequest("POST", fmt.Sprintf("/images/%s/spawn", imageID), creds, data)
}

// CloneImageOptions contains options for cloning an LXD container image.
type CloneImageOptions struct {
	Name        string // Name for the cloned image
	Description string // Description for the cloned image
}

// CloneImage creates a copy of an LXD container image.
//
// Args:
//
//	creds: API credentials
//	imageID: Image ID to clone
//	opts: Clone configuration options (can be nil)
//
// Returns:
//
//	New image info including image_id
func CloneImage(creds *Credentials, imageID string, opts *CloneImageOptions) (map[string]interface{}, error) {
	data := make(map[string]interface{})

	if opts != nil {
		if opts.Name != "" {
			data["name"] = opts.Name
		}
		if opts.Description != "" {
			data["description"] = opts.Description
		}
	}

	return makeRequest("POST", fmt.Sprintf("/images/%s/clone", imageID), creds, data)
}

// ============================================================================
// PaaS Logs API
// ============================================================================

// LogsFetchOptions contains options for fetching logs.
type LogsFetchOptions struct {
	Lines int    // Number of lines (1-10000)
	Since string // Time window ("1m", "5m", "1h", "1d")
	Grep  string // Optional filter pattern
}

// LogsFetch fetches batch logs from the portal.
//
// Args:
//
//	creds: API credentials
//	source: Log source ("all", "api", "portal", "pool/cammy", "pool/ai")
//	opts: Fetch options (can be nil for defaults)
//
// Returns:
//
//	JSON response with log entries
func LogsFetch(creds *Credentials, source string, opts *LogsFetchOptions) (map[string]interface{}, error) {
	path := "/paas/logs"
	params := []string{}

	if source != "" {
		params = append(params, fmt.Sprintf("source=%s", source))
	}

	if opts != nil {
		if opts.Lines > 0 {
			params = append(params, fmt.Sprintf("lines=%d", opts.Lines))
		}
		if opts.Since != "" {
			params = append(params, fmt.Sprintf("since=%s", opts.Since))
		}
		if opts.Grep != "" {
			params = append(params, fmt.Sprintf("grep=%s", opts.Grep))
		}
	}

	if len(params) > 0 {
		path = path + "?" + strings.Join(params, "&")
	}

	return makeRequest("GET", path, creds, nil)
}

// LogCallback is called for each log line received during streaming.
type LogCallback func(source, line string)

// LogsStream streams logs via Server-Sent Events.
// This function blocks until the stream is closed or an error occurs.
//
// Args:
//
//	creds: API credentials
//	source: Log source ("all", "api", "portal", "pool/cammy", "pool/ai")
//	grep: Optional filter pattern (empty string for no filter)
//	callback: Function called for each log line
//
// Returns:
//
//	nil on clean shutdown, error on failure
func LogsStream(creds *Credentials, source, grep string, callback LogCallback) error {
	path := "/paas/logs/stream"
	params := []string{}

	if source != "" {
		params = append(params, fmt.Sprintf("source=%s", source))
	}
	if grep != "" {
		params = append(params, fmt.Sprintf("grep=%s", grep))
	}

	if len(params) > 0 {
		path = path + "?" + strings.Join(params, "&")
	}

	url := APIBase + path
	timestamp := time.Now().Unix()
	message := fmt.Sprintf("%d:GET:%s:", timestamp, path)
	mac := hmac.New(sha256.New, []byte(creds.SecretKey))
	mac.Write([]byte(message))
	signature := hex.EncodeToString(mac.Sum(nil))

	req, err := http.NewRequest("GET", url, nil)
	if err != nil {
		return err
	}

	req.Header.Set("Authorization", "Bearer "+creds.PublicKey)
	req.Header.Set("X-Timestamp", fmt.Sprintf("%d", timestamp))
	req.Header.Set("X-Signature", signature)
	req.Header.Set("Accept", "text/event-stream")

	client := &http.Client{Timeout: 0} // No timeout for streaming
	resp, err := client.Do(req)
	if err != nil {
		return err
	}
	defer resp.Body.Close()

	if resp.StatusCode != 200 {
		body, _ := io.ReadAll(resp.Body)
		return fmt.Errorf("stream error (HTTP %d): %s", resp.StatusCode, string(body))
	}

	reader := bufio.NewReader(resp.Body)
	currentSource := source

	for {
		line, err := reader.ReadString('\n')
		if err != nil {
			if err == io.EOF {
				return nil // Clean shutdown
			}
			return err
		}

		line = strings.TrimSpace(line)
		if line == "" {
			continue
		}

		// Parse SSE format
		if strings.HasPrefix(line, "event:") {
			// New source from event type
			currentSource = strings.TrimSpace(strings.TrimPrefix(line, "event:"))
		} else if strings.HasPrefix(line, "data:") {
			data := strings.TrimSpace(strings.TrimPrefix(line, "data:"))
			if callback != nil && data != "" {
				callback(currentSource, data)
			}
		}
	}
}

// ============================================================================
// Utility Functions
// ============================================================================

// SDKVersion is the version of this SDK.
const SDKVersion = "4.3.2"

// HmacSign computes an HMAC-SHA256 signature for the given message using the secret key.
// Returns the signature as a lowercase hex string.
func HmacSign(secretKey, message string) string {
	mac := hmac.New(sha256.New, []byte(secretKey))
	mac.Write([]byte(message))
	return hex.EncodeToString(mac.Sum(nil))
}

// HealthCheck checks if the API is reachable and responding.
// Returns true if healthy, false otherwise.
func HealthCheck() bool {
	client := &http.Client{Timeout: 10 * time.Second}
	resp, err := client.Get(APIBase + "/health")
	if err != nil {
		return false
	}
	defer resp.Body.Close()
	return resp.StatusCode == 200
}

// Version returns the SDK version string.
func Version() string {
	return SDKVersion
}

// lastError holds the most recent error message for thread-safe access.
var lastError string

// SetLastError sets the last error message (internal use).
func SetLastError(msg string) {
	lastError = msg
}

// LastError returns the most recent error message from the SDK.
func LastError() string {
	return lastError
}

// ============================================================================
// CLI Implementation
// ============================================================================

// Exit codes
const (
	ExitSuccess        = 0
	ExitGeneralError   = 1
	ExitInvalidArgs    = 2
	ExitAuthError      = 3
	ExitAPIError       = 4
	ExitTimeout        = 5
)

// CLIOptions holds parsed CLI arguments
type CLIOptions struct {
	// Global options
	Shell      string
	Env        []string
	Files      []string
	FilePaths  []string
	Artifacts  bool
	OutputDir  string
	PublicKey  string
	SecretKey  string
	Network    string
	VCPU       int
	Yes        bool
	Help       bool

	// Command
	Command    string
	SubCommand string
	Args       []string
}

// printUsage prints the main help message
func printUsage() {
	fmt.Fprintf(os.Stderr, `unsandbox - Execute code securely in sandboxed containers

Usage:
  un [options] <source_file>        Execute code file
  un [options] -s LANG 'code'       Execute inline code
  un session [options]              Interactive session
  un service [options]              Manage services
  un snapshot [options]             Manage snapshots
  un key                            Check API key
  un languages [--json]             List available languages

Global Options:
  -s, --shell LANG       Language for inline code
  -e, --env KEY=VAL      Set environment variable (can repeat)
  -f, --file FILE        Add input file to /tmp/
  -F, --file-path FILE   Add input file with path preserved
  -a, --artifacts        Return compiled artifacts
  -o, --output DIR       Output directory for artifacts
  -p, --public-key KEY   API public key
  -k, --secret-key KEY   API secret key
  -n, --network MODE     Network: zerotrust (default) or semitrusted
  -v, --vcpu N           vCPU count (1-8)
  -y, --yes              Skip confirmation prompts
  -h, --help             Show help

Examples:
  un script.py                      Execute Python script
  un -s bash 'echo hello'           Inline bash command
  un -e DEBUG=1 script.py           With environment variable
  un -n semitrusted crawler.py      With network access
  un session --tmux                 Persistent interactive session
  un service --list                 List all services
  un languages                      List available languages
  un languages --json               List languages as JSON
`)
}

// printSessionUsage prints session subcommand help
func printSessionUsage() {
	fmt.Fprintf(os.Stderr, `un session - Interactive session management

Usage:
  un session [options]              Start new interactive session
  un session --list                 List active sessions
  un session --attach ID            Reconnect to session
  un session --kill ID              Terminate session

Options:
  --shell SHELL          Shell/REPL to use (default: bash)
  -l, --list             List active sessions
  --attach ID            Reconnect to existing session
  --kill ID              Terminate a session
  --freeze ID            Pause session
  --unfreeze ID          Resume session
  --boost ID             Add vCPUs/RAM
  --unboost ID           Remove boost
  --tmux                 Enable persistence with tmux
  --screen               Enable persistence with screen
  --snapshot ID          Create snapshot
  --snapshot-name NAME   Name for snapshot
  --hot                  Live snapshot (no freeze)
  --audit                Record session
  -n, --network MODE     Network: zerotrust or semitrusted
  -v, --vcpu N           vCPU count (1-8)

Examples:
  un session                        Interactive bash
  un session --shell python3        Python REPL
  un session --tmux                 Persistent session
  un session --list                 List sessions
  un session --attach abc123        Reconnect to session
`)
}

// printServiceUsage prints service subcommand help
func printServiceUsage() {
	fmt.Fprintf(os.Stderr, `un service - Service management

Usage:
  un service --list                 List all services
  un service --name NAME --ports PORTS --bootstrap CMD
  un service --info ID              Get service details
  un service --logs ID              Get bootstrap logs
  un service env [status|set|export|delete] ID

Options:
  --name NAME            Service name (creates new)
  --ports PORTS          Comma-separated ports
  --domains DOMAINS      Custom domains
  --type TYPE            Service type (minecraft, tcp, udp)
  --bootstrap CMD        Bootstrap command
  --bootstrap-file FILE  Bootstrap from file
  --env-file FILE        Load env from .env file
  -l, --list             List all services
  --info ID              Get service details
  --logs ID              Get all logs
  --tail ID              Get last 9000 lines
  --freeze ID            Pause service
  --unfreeze ID          Resume service
  --destroy ID           Delete service
  --lock ID              Prevent deletion
  --unlock ID            Allow deletion
  --resize ID            Resize (with --vcpu)
  --redeploy ID          Re-run bootstrap
  --execute ID CMD       Run command
  --snapshot ID          Create snapshot

Service Environment Vault:
  un service env status ID          Show vault status
  un service env set ID             Set from --env-file or stdin
  un service env export ID          Export to stdout
  un service env delete ID          Delete vault

Examples:
  un service --list
  un service --name web --ports 80 --bootstrap "python -m http.server 80"
  un service --info abc123
  un service --execute abc123 'ls -la'
  un service env set abc123 --env-file .env
`)
}

// printSnapshotUsage prints snapshot subcommand help
func printSnapshotUsage() {
	fmt.Fprintf(os.Stderr, `un snapshot - Snapshot management

Usage:
  un snapshot --list                List all snapshots
  un snapshot --info ID             Get details
  un snapshot --delete ID           Delete snapshot
  un snapshot --clone ID            Clone to new session/service

Options:
  -l, --list             List all snapshots
  --info ID              Get snapshot details
  --delete ID            Delete snapshot
  --lock ID              Prevent deletion
  --unlock ID            Allow deletion
  --clone ID             Clone snapshot
  --type TYPE            Clone type: session or service
  --name NAME            Name for cloned service
  --shell SHELL          Shell for cloned session
  --ports PORTS          Ports for cloned service

Examples:
  un snapshot --list
  un snapshot --info abc123
  un snapshot --delete abc123
  un snapshot --clone abc123 --type service --name myapp --ports 80
`)
}

// printImageUsage prints image subcommand help
func printImageUsage() {
	fmt.Fprintf(os.Stderr, `un image - Image management

Usage:
  un image --list                 List all images
  un image --info ID              Get details
  un image --delete ID            Delete image
  un image --publish ID           Publish image from service/snapshot

Options:
  -l, --list             List all images
  --info ID              Get image details
  --delete ID            Delete image
  --lock ID              Prevent deletion
  --unlock ID            Allow deletion
  --publish ID           Publish image (requires --source-type)
  --source-type TYPE     Source type: service or snapshot
  --visibility ID MODE   Set visibility (private, unlisted, public)
  --spawn ID             Spawn new service from image
  --clone ID             Clone an image
  --name NAME            Name for spawned service or cloned image
  --ports PORTS          Ports for spawned service

Examples:
  un image --list
  un image --info abc123
  un image --publish svc123 --source-type service --name myimage
  un image --spawn img123 --name myservice --ports 80,443
  un image --visibility img123 public
`)
}

// cliError prints error to stderr and returns exit code
func cliError(msg string, code int) int {
	fmt.Fprintf(os.Stderr, "Error: %s\n", msg)
	return code
}

// parsePorts parses comma-separated port numbers
func parsePorts(s string) ([]int, error) {
	if s == "" {
		return nil, nil
	}
	parts := strings.Split(s, ",")
	ports := make([]int, 0, len(parts))
	for _, p := range parts {
		p = strings.TrimSpace(p)
		if p == "" {
			continue
		}
		port, err := strconv.Atoi(p)
		if err != nil {
			return nil, fmt.Errorf("invalid port: %s", p)
		}
		ports = append(ports, port)
	}
	return ports, nil
}

// formatList formats a list of items for display
func formatList(items []map[string]interface{}, fields []string) {
	if len(items) == 0 {
		fmt.Println("No items found.")
		return
	}

	// Print header
	header := strings.Join(fields, "\t")
	fmt.Println(header)

	// Print items
	for _, item := range items {
		values := make([]string, len(fields))
		for i, field := range fields {
			if val, ok := item[field]; ok {
				values[i] = fmt.Sprintf("%v", val)
			} else {
				values[i] = "-"
			}
		}
		fmt.Println(strings.Join(values, "\t"))
	}
}

// readFileContents reads file contents for bootstrapping
func readFileContents(path string) (string, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return "", err
	}
	return string(data), nil
}

// readEnvFile reads environment variables from a .env file
func readEnvFile(path string) (map[string]string, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}

	env := make(map[string]string)
	lines := strings.Split(string(data), "\n")
	for _, line := range lines {
		line = strings.TrimSpace(line)
		if line == "" || strings.HasPrefix(line, "#") {
			continue
		}
		parts := strings.SplitN(line, "=", 2)
		if len(parts) == 2 {
			key := strings.TrimSpace(parts[0])
			value := strings.TrimSpace(parts[1])
			// Remove surrounding quotes if present
			if len(value) >= 2 && (value[0] == '"' && value[len(value)-1] == '"' ||
				value[0] == '\'' && value[len(value)-1] == '\'') {
				value = value[1 : len(value)-1]
			}
			env[key] = value
		}
	}
	return env, nil
}

// runExecute handles the execute command (default)
func runExecute(creds *Credentials, opts *CLIOptions) int {
	var language, code string

	if opts.Shell != "" {
		// Inline code mode: -s LANG 'code'
		language = opts.Shell
		if len(opts.Args) == 0 {
			return cliError("no code provided for inline execution", ExitInvalidArgs)
		}
		code = opts.Args[0]
	} else {
		// File mode
		if len(opts.Args) == 0 {
			return cliError("no source file provided", ExitInvalidArgs)
		}
		filename := opts.Args[0]
		language = DetectLanguage(filename)
		if language == "" {
			return cliError(fmt.Sprintf("cannot detect language for: %s", filename), ExitInvalidArgs)
		}
		fileContent, err := readFileContents(filename)
		if err != nil {
			return cliError(fmt.Sprintf("cannot read file: %s", err), ExitGeneralError)
		}
		code = fileContent
	}

	// Build request data
	data := map[string]interface{}{
		"language": language,
		"code":     code,
	}

	if opts.Network != "" {
		data["network_mode"] = opts.Network
	}
	if opts.VCPU > 0 {
		data["vcpu"] = opts.VCPU
	}
	if len(opts.Env) > 0 {
		envMap := make(map[string]string)
		for _, e := range opts.Env {
			parts := strings.SplitN(e, "=", 2)
			if len(parts) == 2 {
				envMap[parts[0]] = parts[1]
			}
		}
		data["env"] = envMap
	}

	// Execute
	result, err := makeRequest("POST", "/execute", creds, data)
	if err != nil {
		return cliError(err.Error(), ExitAPIError)
	}

	// Poll if job is pending/running
	if jobID, ok := result["job_id"].(string); ok {
		if status, ok := result["status"].(string); ok && (status == "pending" || status == "running") {
			result, err = WaitForJob(creds, jobID)
			if err != nil {
				return cliError(err.Error(), ExitAPIError)
			}
		}
	}

	// Print output
	if stdout, ok := result["stdout"].(string); ok && stdout != "" {
		fmt.Print(stdout)
	}
	if stderr, ok := result["stderr"].(string); ok && stderr != "" {
		fmt.Fprint(os.Stderr, stderr)
	}

	// Print summary
	fmt.Println("---")
	if exitCode, ok := result["exit_code"].(float64); ok {
		fmt.Printf("Exit code: %d\n", int(exitCode))
	}
	if execTime, ok := result["execution_time_ms"].(float64); ok {
		fmt.Printf("Execution time: %dms\n", int(execTime))
	}

	if exitCode, ok := result["exit_code"].(float64); ok && exitCode != 0 {
		return int(exitCode)
	}
	return ExitSuccess
}

// runSession handles the session command
func runSession(creds *Credentials, args []string, opts *CLIOptions) int {
	fs := &sessionFlags{}
	parseSessionFlags(args, fs)

	if fs.help {
		printSessionUsage()
		return ExitSuccess
	}

	// List sessions
	if fs.list {
		sessions, err := ListSessions(creds)
		if err != nil {
			return cliError(err.Error(), ExitAPIError)
		}
		formatList(sessions, []string{"session_id", "status", "shell", "created_at"})
		return ExitSuccess
	}

	// Attach to session
	if fs.attach != "" {
		session, err := GetSession(creds, fs.attach)
		if err != nil {
			return cliError(err.Error(), ExitAPIError)
		}
		// Print connection info
		fmt.Printf("Session: %s\n", fs.attach)
		if wsURL, ok := session["websocket_url"].(string); ok {
			fmt.Printf("Connect via: %s\n", wsURL)
		}
		return ExitSuccess
	}

	// Kill session
	if fs.kill != "" {
		_, err := DeleteSession(creds, fs.kill)
		if err != nil {
			return cliError(err.Error(), ExitAPIError)
		}
		fmt.Printf("Session %s terminated\n", fs.kill)
		return ExitSuccess
	}

	// Freeze session
	if fs.freeze != "" {
		_, err := FreezeSession(creds, fs.freeze)
		if err != nil {
			return cliError(err.Error(), ExitAPIError)
		}
		fmt.Printf("Session %s frozen\n", fs.freeze)
		return ExitSuccess
	}

	// Unfreeze session
	if fs.unfreeze != "" {
		_, err := UnfreezeSession(creds, fs.unfreeze)
		if err != nil {
			return cliError(err.Error(), ExitAPIError)
		}
		fmt.Printf("Session %s unfrozen\n", fs.unfreeze)
		return ExitSuccess
	}

	// Boost session
	if fs.boost != "" {
		vcpu := opts.VCPU
		if vcpu == 0 {
			vcpu = 2
		}
		_, err := BoostSession(creds, fs.boost, vcpu)
		if err != nil {
			return cliError(err.Error(), ExitAPIError)
		}
		fmt.Printf("Session %s boosted to %d vCPUs\n", fs.boost, vcpu)
		return ExitSuccess
	}

	// Unboost session
	if fs.unboost != "" {
		_, err := UnboostSession(creds, fs.unboost)
		if err != nil {
			return cliError(err.Error(), ExitAPIError)
		}
		fmt.Printf("Session %s unboost\n", fs.unboost)
		return ExitSuccess
	}

	// Snapshot session
	if fs.snapshot != "" {
		snapshotID, err := SessionSnapshot(creds, fs.snapshot, fs.snapshotName, fs.hot)
		if err != nil {
			return cliError(err.Error(), ExitAPIError)
		}
		fmt.Printf("Snapshot created: %s\n", snapshotID)
		return ExitSuccess
	}

	// Create new session
	sessionOpts := &SessionOptions{}
	if fs.shell != "" {
		sessionOpts.Shell = fs.shell
	}
	if opts.Network != "" {
		sessionOpts.NetworkMode = opts.Network
	}
	if opts.VCPU > 0 {
		sessionOpts.VCPU = opts.VCPU
	}
	if fs.tmux {
		sessionOpts.Multiplexer = "tmux"
	} else if fs.screen {
		sessionOpts.Multiplexer = "screen"
	}

	session, err := CreateSession(creds, sessionOpts)
	if err != nil {
		return cliError(err.Error(), ExitAPIError)
	}

	fmt.Printf("Session created: %v\n", session["session_id"])
	if wsURL, ok := session["websocket_url"].(string); ok {
		fmt.Printf("Connect via: %s\n", wsURL)
	}
	return ExitSuccess
}

type sessionFlags struct {
	shell        string
	list         bool
	attach       string
	kill         string
	freeze       string
	unfreeze     string
	boost        string
	unboost      string
	tmux         bool
	screen       bool
	snapshot     string
	snapshotName string
	hot          bool
	audit        bool
	help         bool
}

func parseSessionFlags(args []string, fs *sessionFlags) {
	for i := 0; i < len(args); i++ {
		arg := args[i]
		switch arg {
		case "--shell":
			if i+1 < len(args) {
				fs.shell = args[i+1]
				i++
			}
		case "-l", "--list":
			fs.list = true
		case "--attach":
			if i+1 < len(args) {
				fs.attach = args[i+1]
				i++
			}
		case "--kill":
			if i+1 < len(args) {
				fs.kill = args[i+1]
				i++
			}
		case "--freeze":
			if i+1 < len(args) {
				fs.freeze = args[i+1]
				i++
			}
		case "--unfreeze":
			if i+1 < len(args) {
				fs.unfreeze = args[i+1]
				i++
			}
		case "--boost":
			if i+1 < len(args) {
				fs.boost = args[i+1]
				i++
			}
		case "--unboost":
			if i+1 < len(args) {
				fs.unboost = args[i+1]
				i++
			}
		case "--tmux":
			fs.tmux = true
		case "--screen":
			fs.screen = true
		case "--snapshot":
			if i+1 < len(args) {
				fs.snapshot = args[i+1]
				i++
			}
		case "--snapshot-name":
			if i+1 < len(args) {
				fs.snapshotName = args[i+1]
				i++
			}
		case "--hot":
			fs.hot = true
		case "--audit":
			fs.audit = true
		case "-h", "--help":
			fs.help = true
		}
	}
}

// runService handles the service command
func runService(creds *Credentials, args []string, opts *CLIOptions) int {
	fs := &serviceFlags{}
	parseServiceFlags(args, fs)

	if fs.help {
		printServiceUsage()
		return ExitSuccess
	}

	// Handle env subcommand
	if fs.envCmd != "" {
		return runServiceEnv(creds, fs, opts)
	}

	// List services
	if fs.list {
		services, err := ListServices(creds)
		if err != nil {
			return cliError(err.Error(), ExitAPIError)
		}
		formatList(services, []string{"service_id", "name", "status", "created_at"})
		return ExitSuccess
	}

	// Get service info
	if fs.info != "" {
		service, err := GetService(creds, fs.info)
		if err != nil {
			return cliError(err.Error(), ExitAPIError)
		}
		jsonOut, _ := json.MarshalIndent(service, "", "  ")
		fmt.Println(string(jsonOut))
		return ExitSuccess
	}

	// Get service logs
	if fs.logs != "" {
		logs, err := GetServiceLogs(creds, fs.logs, true)
		if err != nil {
			return cliError(err.Error(), ExitAPIError)
		}
		if content, ok := logs["logs"].(string); ok {
			fmt.Print(content)
		}
		return ExitSuccess
	}

	// Get service tail logs
	if fs.tail != "" {
		logs, err := GetServiceLogs(creds, fs.tail, false)
		if err != nil {
			return cliError(err.Error(), ExitAPIError)
		}
		if content, ok := logs["logs"].(string); ok {
			fmt.Print(content)
		}
		return ExitSuccess
	}

	// Freeze service
	if fs.freeze != "" {
		_, err := FreezeService(creds, fs.freeze)
		if err != nil {
			return cliError(err.Error(), ExitAPIError)
		}
		fmt.Printf("Service %s frozen\n", fs.freeze)
		return ExitSuccess
	}

	// Unfreeze service
	if fs.unfreeze != "" {
		_, err := UnfreezeService(creds, fs.unfreeze)
		if err != nil {
			return cliError(err.Error(), ExitAPIError)
		}
		fmt.Printf("Service %s unfrozen\n", fs.unfreeze)
		return ExitSuccess
	}

	// Destroy service
	if fs.destroy != "" {
		_, err := DeleteService(creds, fs.destroy)
		if err != nil {
			return cliError(err.Error(), ExitAPIError)
		}
		fmt.Printf("Service %s destroyed\n", fs.destroy)
		return ExitSuccess
	}

	// Lock service
	if fs.lock != "" {
		_, err := LockService(creds, fs.lock)
		if err != nil {
			return cliError(err.Error(), ExitAPIError)
		}
		fmt.Printf("Service %s locked\n", fs.lock)
		return ExitSuccess
	}

	// Unlock service
	if fs.unlock != "" {
		_, err := UnlockService(creds, fs.unlock)
		if err != nil {
			return cliError(err.Error(), ExitAPIError)
		}
		fmt.Printf("Service %s unlocked\n", fs.unlock)
		return ExitSuccess
	}

	// Resize service
	if fs.resize != "" {
		if opts.VCPU == 0 {
			return cliError("--vcpu required for resize", ExitInvalidArgs)
		}
		_, err := UpdateService(creds, fs.resize, &ServiceUpdateOptions{VCPU: opts.VCPU})
		if err != nil {
			return cliError(err.Error(), ExitAPIError)
		}
		fmt.Printf("Service %s resized to %d vCPUs\n", fs.resize, opts.VCPU)
		return ExitSuccess
	}

	// Redeploy service
	if fs.redeploy != "" {
		_, err := RedeployService(creds, fs.redeploy, fs.bootstrap)
		if err != nil {
			return cliError(err.Error(), ExitAPIError)
		}
		fmt.Printf("Service %s redeployed\n", fs.redeploy)
		return ExitSuccess
	}

	// Execute in service
	if fs.execute != "" && fs.executeCmd != "" {
		result, err := ExecuteInService(creds, fs.execute, fs.executeCmd)
		if err != nil {
			return cliError(err.Error(), ExitAPIError)
		}
		if stdout, ok := result["stdout"].(string); ok {
			fmt.Print(stdout)
		}
		if stderr, ok := result["stderr"].(string); ok {
			fmt.Fprint(os.Stderr, stderr)
		}
		return ExitSuccess
	}

	// Snapshot service
	if fs.snapshot != "" {
		snapshotID, err := ServiceSnapshot(creds, fs.snapshot, fs.snapshotName, fs.hot)
		if err != nil {
			return cliError(err.Error(), ExitAPIError)
		}
		fmt.Printf("Snapshot created: %s\n", snapshotID)
		return ExitSuccess
	}

	// Create new service
	if fs.name != "" {
		if fs.ports == "" {
			return cliError("--ports required for new service", ExitInvalidArgs)
		}
		ports, err := parsePorts(fs.ports)
		if err != nil {
			return cliError(err.Error(), ExitInvalidArgs)
		}

		bootstrap := fs.bootstrap
		if fs.bootstrapFile != "" {
			content, err := readFileContents(fs.bootstrapFile)
			if err != nil {
				return cliError(fmt.Sprintf("cannot read bootstrap file: %s", err), ExitGeneralError)
			}
			bootstrap = content
		}

		serviceOpts := &ServiceOptions{}
		if opts.Network != "" {
			serviceOpts.NetworkMode = opts.Network
		}
		if opts.VCPU > 0 {
			serviceOpts.VCPU = opts.VCPU
		}

		service, err := CreateService(creds, fs.name, ports, bootstrap, serviceOpts)
		if err != nil {
			return cliError(err.Error(), ExitAPIError)
		}

		fmt.Printf("Service created: %v\n", service["service_id"])
		if url, ok := service["url"].(string); ok {
			fmt.Printf("URL: %s\n", url)
		}
		return ExitSuccess
	}

	printServiceUsage()
	return ExitInvalidArgs
}

type serviceFlags struct {
	name          string
	ports         string
	domains       string
	serviceType   string
	bootstrap     string
	bootstrapFile string
	envFile       string
	list          bool
	info          string
	logs          string
	tail          string
	freeze        string
	unfreeze      string
	destroy       string
	lock          string
	unlock        string
	resize        string
	redeploy      string
	execute       string
	executeCmd    string
	snapshot      string
	snapshotName  string
	hot           bool
	envCmd        string
	envID         string
	help          bool
}

func parseServiceFlags(args []string, fs *serviceFlags) {
	for i := 0; i < len(args); i++ {
		arg := args[i]
		switch arg {
		case "--name":
			if i+1 < len(args) {
				fs.name = args[i+1]
				i++
			}
		case "--ports":
			if i+1 < len(args) {
				fs.ports = args[i+1]
				i++
			}
		case "--domains":
			if i+1 < len(args) {
				fs.domains = args[i+1]
				i++
			}
		case "--type":
			if i+1 < len(args) {
				fs.serviceType = args[i+1]
				i++
			}
		case "--bootstrap":
			if i+1 < len(args) {
				fs.bootstrap = args[i+1]
				i++
			}
		case "--bootstrap-file":
			if i+1 < len(args) {
				fs.bootstrapFile = args[i+1]
				i++
			}
		case "--env-file":
			if i+1 < len(args) {
				fs.envFile = args[i+1]
				i++
			}
		case "-l", "--list":
			fs.list = true
		case "--info":
			if i+1 < len(args) {
				fs.info = args[i+1]
				i++
			}
		case "--logs":
			if i+1 < len(args) {
				fs.logs = args[i+1]
				i++
			}
		case "--tail":
			if i+1 < len(args) {
				fs.tail = args[i+1]
				i++
			}
		case "--freeze":
			if i+1 < len(args) {
				fs.freeze = args[i+1]
				i++
			}
		case "--unfreeze":
			if i+1 < len(args) {
				fs.unfreeze = args[i+1]
				i++
			}
		case "--destroy":
			if i+1 < len(args) {
				fs.destroy = args[i+1]
				i++
			}
		case "--lock":
			if i+1 < len(args) {
				fs.lock = args[i+1]
				i++
			}
		case "--unlock":
			if i+1 < len(args) {
				fs.unlock = args[i+1]
				i++
			}
		case "--resize":
			if i+1 < len(args) {
				fs.resize = args[i+1]
				i++
			}
		case "--redeploy":
			if i+1 < len(args) {
				fs.redeploy = args[i+1]
				i++
			}
		case "--execute":
			if i+1 < len(args) {
				fs.execute = args[i+1]
				i++
				// Next arg is the command
				if i+1 < len(args) && !strings.HasPrefix(args[i+1], "-") {
					fs.executeCmd = args[i+1]
					i++
				}
			}
		case "--snapshot":
			if i+1 < len(args) {
				fs.snapshot = args[i+1]
				i++
			}
		case "--snapshot-name":
			if i+1 < len(args) {
				fs.snapshotName = args[i+1]
				i++
			}
		case "--hot":
			fs.hot = true
		case "env":
			// Handle env subcommand
			if i+1 < len(args) {
				fs.envCmd = args[i+1]
				i++
				if i+1 < len(args) {
					fs.envID = args[i+1]
					i++
				}
			}
		case "-h", "--help":
			fs.help = true
		}
	}
}

// runServiceEnv handles service env subcommands
func runServiceEnv(creds *Credentials, fs *serviceFlags, opts *CLIOptions) int {
	if fs.envID == "" {
		return cliError("service ID required for env command", ExitInvalidArgs)
	}

	switch fs.envCmd {
	case "status":
		env, err := GetServiceEnv(creds, fs.envID)
		if err != nil {
			return cliError(err.Error(), ExitAPIError)
		}
		jsonOut, _ := json.MarshalIndent(env, "", "  ")
		fmt.Println(string(jsonOut))

	case "set":
		var env map[string]string
		var err error
		if fs.envFile != "" {
			env, err = readEnvFile(fs.envFile)
			if err != nil {
				return cliError(fmt.Sprintf("cannot read env file: %s", err), ExitGeneralError)
			}
		} else {
			// Read from stdin
			data, err := io.ReadAll(os.Stdin)
			if err != nil {
				return cliError(fmt.Sprintf("cannot read stdin: %s", err), ExitGeneralError)
			}
			env = make(map[string]string)
			lines := strings.Split(string(data), "\n")
			for _, line := range lines {
				line = strings.TrimSpace(line)
				if line == "" || strings.HasPrefix(line, "#") {
					continue
				}
				parts := strings.SplitN(line, "=", 2)
				if len(parts) == 2 {
					env[parts[0]] = parts[1]
				}
			}
		}
		_, err = SetServiceEnv(creds, fs.envID, env)
		if err != nil {
			return cliError(err.Error(), ExitAPIError)
		}
		fmt.Println("Environment variables set")

	case "export":
		env, err := ExportServiceEnv(creds, fs.envID)
		if err != nil {
			return cliError(err.Error(), ExitAPIError)
		}
		if content, ok := env["content"].(string); ok {
			fmt.Print(content)
		}

	case "delete":
		_, err := DeleteServiceEnv(creds, fs.envID, nil)
		if err != nil {
			return cliError(err.Error(), ExitAPIError)
		}
		fmt.Println("Environment vault deleted")

	default:
		return cliError(fmt.Sprintf("unknown env command: %s", fs.envCmd), ExitInvalidArgs)
	}

	return ExitSuccess
}

// runSnapshot handles the snapshot command
func runSnapshot(creds *Credentials, args []string, opts *CLIOptions) int {
	fs := &snapshotFlags{}
	parseSnapshotFlags(args, fs)

	if fs.help {
		printSnapshotUsage()
		return ExitSuccess
	}

	// List snapshots
	if fs.list {
		snapshots, err := ListSnapshots(creds)
		if err != nil {
			return cliError(err.Error(), ExitAPIError)
		}
		formatList(snapshots, []string{"snapshot_id", "name", "type", "created_at"})
		return ExitSuccess
	}

	// Get snapshot info
	if fs.info != "" {
		snapshot, err := GetSnapshot(creds, fs.info)
		if err != nil {
			return cliError(err.Error(), ExitAPIError)
		}
		jsonOut, _ := json.MarshalIndent(snapshot, "", "  ")
		fmt.Println(string(jsonOut))
		return ExitSuccess
	}

	// Delete snapshot
	if fs.deleteID != "" {
		_, err := DeleteSnapshot(creds, fs.deleteID)
		if err != nil {
			return cliError(err.Error(), ExitAPIError)
		}
		fmt.Printf("Snapshot %s deleted\n", fs.deleteID)
		return ExitSuccess
	}

	// Lock snapshot
	if fs.lock != "" {
		_, err := LockSnapshot(creds, fs.lock)
		if err != nil {
			return cliError(err.Error(), ExitAPIError)
		}
		fmt.Printf("Snapshot %s locked\n", fs.lock)
		return ExitSuccess
	}

	// Unlock snapshot
	if fs.unlock != "" {
		_, err := UnlockSnapshot(creds, fs.unlock)
		if err != nil {
			return cliError(err.Error(), ExitAPIError)
		}
		fmt.Printf("Snapshot %s unlocked\n", fs.unlock)
		return ExitSuccess
	}

	// Clone snapshot
	if fs.clone != "" {
		cloneOpts := &CloneSnapshotOptions{}
		if fs.name != "" {
			cloneOpts.Name = fs.name
		}
		if fs.shell != "" {
			cloneOpts.Shell = fs.shell
		}
		if fs.ports != "" {
			ports, err := parsePorts(fs.ports)
			if err != nil {
				return cliError(err.Error(), ExitInvalidArgs)
			}
			cloneOpts.Ports = ports
		}

		cloneType := fs.cloneType
		if cloneType == "" {
			cloneType = "session"
		}

		result, err := CloneSnapshot(creds, fs.clone, cloneType, cloneOpts)
		if err != nil {
			return cliError(err.Error(), ExitAPIError)
		}
		jsonOut, _ := json.MarshalIndent(result, "", "  ")
		fmt.Println(string(jsonOut))
		return ExitSuccess
	}

	printSnapshotUsage()
	return ExitInvalidArgs
}

type snapshotFlags struct {
	list      bool
	info      string
	deleteID  string
	lock      string
	unlock    string
	clone     string
	cloneType string
	name      string
	shell     string
	ports     string
	help      bool
}

func parseSnapshotFlags(args []string, fs *snapshotFlags) {
	for i := 0; i < len(args); i++ {
		arg := args[i]
		switch arg {
		case "-l", "--list":
			fs.list = true
		case "--info":
			if i+1 < len(args) {
				fs.info = args[i+1]
				i++
			}
		case "--delete":
			if i+1 < len(args) {
				fs.deleteID = args[i+1]
				i++
			}
		case "--lock":
			if i+1 < len(args) {
				fs.lock = args[i+1]
				i++
			}
		case "--unlock":
			if i+1 < len(args) {
				fs.unlock = args[i+1]
				i++
			}
		case "--clone":
			if i+1 < len(args) {
				fs.clone = args[i+1]
				i++
			}
		case "--type":
			if i+1 < len(args) {
				fs.cloneType = args[i+1]
				i++
			}
		case "--name":
			if i+1 < len(args) {
				fs.name = args[i+1]
				i++
			}
		case "--shell":
			if i+1 < len(args) {
				fs.shell = args[i+1]
				i++
			}
		case "--ports":
			if i+1 < len(args) {
				fs.ports = args[i+1]
				i++
			}
		case "-h", "--help":
			fs.help = true
		}
	}
}

type imageFlags struct {
	list       bool
	info       string
	deleteID   string
	lock       string
	unlock     string
	publish    string
	sourceType string
	visibility string
	visMode    string
	spawn      string
	clone      string
	name       string
	ports      string
	help       bool
}

func parseImageFlags(args []string, fs *imageFlags) {
	for i := 0; i < len(args); i++ {
		arg := args[i]
		switch arg {
		case "-l", "--list":
			fs.list = true
		case "--info":
			if i+1 < len(args) {
				fs.info = args[i+1]
				i++
			}
		case "--delete":
			if i+1 < len(args) {
				fs.deleteID = args[i+1]
				i++
			}
		case "--lock":
			if i+1 < len(args) {
				fs.lock = args[i+1]
				i++
			}
		case "--unlock":
			if i+1 < len(args) {
				fs.unlock = args[i+1]
				i++
			}
		case "--publish":
			if i+1 < len(args) {
				fs.publish = args[i+1]
				i++
			}
		case "--source-type":
			if i+1 < len(args) {
				fs.sourceType = args[i+1]
				i++
			}
		case "--visibility":
			if i+1 < len(args) {
				fs.visibility = args[i+1]
				i++
			}
			if i+1 < len(args) && !strings.HasPrefix(args[i+1], "-") {
				fs.visMode = args[i+1]
				i++
			}
		case "--spawn":
			if i+1 < len(args) {
				fs.spawn = args[i+1]
				i++
			}
		case "--clone":
			if i+1 < len(args) {
				fs.clone = args[i+1]
				i++
			}
		case "--name":
			if i+1 < len(args) {
				fs.name = args[i+1]
				i++
			}
		case "--ports":
			if i+1 < len(args) {
				fs.ports = args[i+1]
				i++
			}
		case "-h", "--help":
			fs.help = true
		}
	}
}

func runImage(creds *Credentials, args []string, opts *CLIOptions) int {
	fs := &imageFlags{}
	parseImageFlags(args, fs)

	if fs.help {
		printImageUsage()
		return ExitSuccess
	}

	// List images
	if fs.list {
		images, err := ListImages(creds, "")
		if err != nil {
			return cliError(err.Error(), ExitAPIError)
		}
		formatList(images, []string{"image_id", "name", "visibility", "source_type", "created_at"})
		return ExitSuccess
	}

	// Get image info
	if fs.info != "" {
		image, err := GetImage(creds, fs.info)
		if err != nil {
			return cliError(err.Error(), ExitAPIError)
		}
		jsonOut, _ := json.MarshalIndent(image, "", "  ")
		fmt.Println(string(jsonOut))
		return ExitSuccess
	}

	// Delete image
	if fs.deleteID != "" {
		_, err := DeleteImage(creds, fs.deleteID)
		if err != nil {
			return cliError(err.Error(), ExitAPIError)
		}
		fmt.Printf("Image %s deleted\n", fs.deleteID)
		return ExitSuccess
	}

	// Lock image
	if fs.lock != "" {
		_, err := LockImage(creds, fs.lock)
		if err != nil {
			return cliError(err.Error(), ExitAPIError)
		}
		fmt.Printf("Image %s locked\n", fs.lock)
		return ExitSuccess
	}

	// Unlock image
	if fs.unlock != "" {
		_, err := UnlockImage(creds, fs.unlock)
		if err != nil {
			return cliError(err.Error(), ExitAPIError)
		}
		fmt.Printf("Image %s unlocked\n", fs.unlock)
		return ExitSuccess
	}

	// Publish image
	if fs.publish != "" {
		if fs.sourceType == "" {
			return cliError("--source-type required for --publish", ExitInvalidArgs)
		}
		pubOpts := &ImagePublishOptions{}
		if fs.name != "" {
			pubOpts.Name = fs.name
		}
		result, err := ImagePublish(creds, fs.sourceType, fs.publish, pubOpts)
		if err != nil {
			return cliError(err.Error(), ExitAPIError)
		}
		imageID := ""
		if id, ok := result["image_id"].(string); ok {
			imageID = id
		} else if id, ok := result["id"].(string); ok {
			imageID = id
		}
		fmt.Printf("Image published: %s\n", imageID)
		return ExitSuccess
	}

	// Set visibility
	if fs.visibility != "" && fs.visMode != "" {
		if fs.visMode != "private" && fs.visMode != "unlisted" && fs.visMode != "public" {
			return cliError("visibility must be private, unlisted, or public", ExitInvalidArgs)
		}
		_, err := SetImageVisibility(creds, fs.visibility, fs.visMode)
		if err != nil {
			return cliError(err.Error(), ExitAPIError)
		}
		fmt.Printf("Image %s visibility set to %s\n", fs.visibility, fs.visMode)
		return ExitSuccess
	}

	// Spawn from image
	if fs.spawn != "" {
		if fs.name == "" {
			return cliError("--name required for --spawn", ExitInvalidArgs)
		}
		spawnOpts := &SpawnFromImageOptions{
			Name: fs.name,
		}
		if fs.ports != "" {
			ports, err := parsePorts(fs.ports)
			if err != nil {
				return cliError(err.Error(), ExitInvalidArgs)
			}
			spawnOpts.Ports = ports
		}
		result, err := SpawnFromImage(creds, fs.spawn, spawnOpts)
		if err != nil {
			return cliError(err.Error(), ExitAPIError)
		}
		serviceID := ""
		if id, ok := result["service_id"].(string); ok {
			serviceID = id
		} else if id, ok := result["id"].(string); ok {
			serviceID = id
		}
		fmt.Printf("Service spawned: %s\n", serviceID)
		return ExitSuccess
	}

	// Clone image
	if fs.clone != "" {
		cloneOpts := &CloneImageOptions{}
		if fs.name != "" {
			cloneOpts.Name = fs.name
		}
		result, err := CloneImage(creds, fs.clone, cloneOpts)
		if err != nil {
			return cliError(err.Error(), ExitAPIError)
		}
		imageID := ""
		if id, ok := result["image_id"].(string); ok {
			imageID = id
		} else if id, ok := result["id"].(string); ok {
			imageID = id
		}
		fmt.Printf("Image cloned: %s\n", imageID)
		return ExitSuccess
	}

	printImageUsage()
	return ExitInvalidArgs
}

// runKey handles the key command
func runKey(creds *Credentials) int {
	result, err := ValidateKeys(creds)
	if err != nil {
		return cliError(err.Error(), ExitAuthError)
	}

	fmt.Println("API key is valid")
	if account, ok := result["account"].(map[string]interface{}); ok {
		jsonOut, _ := json.MarshalIndent(account, "", "  ")
		fmt.Println(string(jsonOut))
	}
	return ExitSuccess
}

// runLanguages handles the languages command
func runLanguages(creds *Credentials, args []string) int {
	// Check for --json flag
	jsonOutput := false
	for _, arg := range args {
		if arg == "--json" {
			jsonOutput = true
		}
	}

	languages, err := GetLanguages(creds)
	if err != nil {
		return cliError(err.Error(), ExitAPIError)
	}

	if jsonOutput {
		// Output as JSON array
		jsonOut, _ := json.Marshal(languages)
		fmt.Println(string(jsonOut))
	} else {
		// Output one language per line (pipe-friendly)
		for _, lang := range languages {
			fmt.Println(lang)
		}
	}
	return ExitSuccess
}

// parseGlobalFlags parses global CLI options
func parseGlobalFlags(args []string) (*CLIOptions, []string) {
	opts := &CLIOptions{}
	remaining := []string{}

	for i := 0; i < len(args); i++ {
		arg := args[i]
		switch {
		case arg == "-s" || arg == "--shell":
			if i+1 < len(args) {
				opts.Shell = args[i+1]
				i++
			}
		case arg == "-e" || arg == "--env":
			if i+1 < len(args) {
				opts.Env = append(opts.Env, args[i+1])
				i++
			}
		case arg == "-f" || arg == "--file":
			if i+1 < len(args) {
				opts.Files = append(opts.Files, args[i+1])
				i++
			}
		case arg == "-F" || arg == "--file-path":
			if i+1 < len(args) {
				opts.FilePaths = append(opts.FilePaths, args[i+1])
				i++
			}
		case arg == "-a" || arg == "--artifacts":
			opts.Artifacts = true
		case arg == "-o" || arg == "--output":
			if i+1 < len(args) {
				opts.OutputDir = args[i+1]
				i++
			}
		case arg == "-p" || arg == "--public-key":
			if i+1 < len(args) {
				opts.PublicKey = args[i+1]
				i++
			}
		case arg == "-k" || arg == "--secret-key":
			if i+1 < len(args) {
				opts.SecretKey = args[i+1]
				i++
			}
		case arg == "-n" || arg == "--network":
			if i+1 < len(args) {
				opts.Network = args[i+1]
				i++
			}
		case arg == "-v" || arg == "--vcpu":
			if i+1 < len(args) {
				if v, err := strconv.Atoi(args[i+1]); err == nil {
					opts.VCPU = v
				}
				i++
			}
		case arg == "-y" || arg == "--yes":
			opts.Yes = true
		case arg == "-h" || arg == "--help":
			opts.Help = true
		default:
			remaining = append(remaining, arg)
		}
	}

	return opts, remaining
}

// CliMain is the main entry point for the CLI
func CliMain() {
	if len(os.Args) < 2 {
		printUsage()
		os.Exit(ExitInvalidArgs)
	}

	opts, remaining := parseGlobalFlags(os.Args[1:])

	if opts.Help && len(remaining) == 0 {
		printUsage()
		os.Exit(ExitSuccess)
	}

	// Determine command
	command := ""
	cmdArgs := remaining
	if len(remaining) > 0 {
		switch remaining[0] {
		case "session", "service", "snapshot", "key", "languages":
			command = remaining[0]
			cmdArgs = remaining[1:]
		default:
			command = "execute"
			opts.Args = remaining
		}
	}

	// Resolve credentials
	creds, err := ResolveCredentials(opts.PublicKey, opts.SecretKey)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error: %s\n", err)
		os.Exit(ExitAuthError)
	}

	// Dispatch command
	var exitCode int
	switch command {
	case "execute", "":
		if opts.Help {
			printUsage()
			exitCode = ExitSuccess
		} else {
			exitCode = runExecute(creds, opts)
		}
	case "session":
		exitCode = runSession(creds, cmdArgs, opts)
	case "service":
		exitCode = runService(creds, cmdArgs, opts)
	case "snapshot":
		exitCode = runSnapshot(creds, cmdArgs, opts)
	case "image":
		exitCode = runImage(creds, cmdArgs, opts)
	case "key":
		exitCode = runKey(creds)
	case "languages":
		exitCode = runLanguages(creds, cmdArgs)
	default:
		printUsage()
		exitCode = ExitInvalidArgs
	}

	os.Exit(exitCode)
}
