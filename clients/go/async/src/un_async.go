/*
PUBLIC DOMAIN - NO LICENSE, NO WARRANTY

unsandbox.com Go SDK (Asynchronous)

Library Usage:
    import "un_async"

    // Create credentials
    creds, err := un_async.ResolveCredentials("", "")
    if err != nil {
        log.Fatal(err)
    }

    // Execute code asynchronously (returns channel)
    resultChan := un_async.ExecuteCode(creds, "python", `print("hello")`)
    result := <-resultChan
    if result.Err != nil {
        log.Fatal(result.Err)
    }
    fmt.Println(result.Data)

    // Submit async job and get job ID
    jobChan := un_async.ExecuteAsync(creds, "javascript", `console.log("hello")`)
    jobResult := <-jobChan
    if jobResult.Err != nil {
        log.Fatal(jobResult.Err)
    }
    fmt.Println(jobResult.JobID)

    // Wait for job completion with timeout
    waitChan := un_async.WaitForJob(creds, jobResult.JobID, 60*time.Second)
    waitResult := <-waitChan
    if waitResult.Err != nil {
        log.Fatal(waitResult.Err)
    }

    // List all jobs
    listChan := un_async.ListJobs(creds)
    listResult := <-listChan
    if listResult.Err == nil {
        for _, job := range listResult.Jobs {
            fmt.Println(job)
        }
    }

    // Get supported languages (cached)
    langChan := un_async.GetLanguages(creds)
    langResult := <-langChan
    if langResult.Err == nil {
        for _, lang := range langResult.Languages {
            fmt.Println(lang)
        }
    }

    // Detect language from filename (synchronous, no I/O)
    lang := un_async.DetectLanguage("script.py")  // Returns "python"

    // Snapshot operations
    snapChan := un_async.SessionSnapshot(creds, sessionID, "my_snapshot", false)
    snapResult := <-snapChan

Authentication Priority (4-tier):
    1. Function arguments (creds struct with PublicKey, SecretKey)
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

package un_async

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
	APIBase           = "https://api.unsandbox.com"
	LanguagesCacheTTL = 3600 // 1 hour in seconds
)

var (
	// PollDelaysMs defines exponential backoff delays for job polling
	PollDelaysMs = []int{300, 450, 700, 900, 650, 1600, 2000}

	// LanguageMap maps file extensions to programming languages
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

// CredentialsError is returned when credentials cannot be found or are invalid
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

// Result types for channel-based async responses

// ExecuteResult contains the result of an execution request
type ExecuteResult struct {
	Data map[string]interface{}
	Err  error
}

// JobIDResult contains the result of an async execution request
type JobIDResult struct {
	JobID string
	Err   error
}

// JobResult contains the result of a job query
type JobResult struct {
	Data map[string]interface{}
	Err  error
}

// JobListResult contains the result of listing jobs
type JobListResult struct {
	Jobs []map[string]interface{}
	Err  error
}

// LanguagesResult contains the result of a languages query
type LanguagesResult struct {
	Languages []string
	Err       error
}

// SnapshotResult contains the result of a snapshot operation
type SnapshotResult struct {
	SnapshotID string
	Err        error
}

// SnapshotListResult contains the result of listing snapshots
type SnapshotListResult struct {
	Snapshots []map[string]interface{}
	Err       error
}

// RestoreResult contains the result of restoring a snapshot
type RestoreResult struct {
	Data map[string]interface{}
	Err  error
}

// DeleteResult contains the result of a delete operation
type DeleteResult struct {
	Data map[string]interface{}
	Err  error
}

// CancelResult contains the result of cancelling a job
type CancelResult struct {
	Data map[string]interface{}
	Err  error
}

// getUnsandboxDir returns ~/.unsandbox directory path, creating if necessary
func getUnsandboxDir() (string, error) {
	currentUser, err := user.Current()
	if err != nil {
		return "", err
	}

	dir := filepath.Join(currentUser.HomeDir, ".unsandbox")
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
//  1. Credentials struct fields (PublicKey, SecretKey non-empty)
//  2. Environment variables (UNSANDBOX_PUBLIC_KEY, UNSANDBOX_SECRET_KEY)
//  3. ~/.unsandbox/accounts.csv
//  4. ./accounts.csv
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

// makeRequest makes an authenticated HTTP request to the API (synchronous, internal use)
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
	req.Header.Set("User-Agent", "un-go-async/2.0")

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

// ExecuteCode executes code and returns a channel that receives the result.
// The channel receives exactly one ExecuteResult then closes.
// This function blocks until completion (polls until job finishes).
func ExecuteCode(creds *Credentials, language, code string) <-chan ExecuteResult {
	resultChan := make(chan ExecuteResult, 1)

	go func() {
		defer close(resultChan)

		data := map[string]string{
			"language": language,
			"code":     code,
		}

		response, err := makeRequest("POST", "/execute", creds, data)
		if err != nil {
			resultChan <- ExecuteResult{Err: err}
			return
		}

		// If we got a job_id with pending/running status, poll until completion
		if jobID, ok := response["job_id"].(string); ok {
			if status, ok := response["status"].(string); ok && (status == "pending" || status == "running") {
				waitChan := WaitForJob(creds, jobID, 0)
				waitResult := <-waitChan
				resultChan <- ExecuteResult{Data: waitResult.Data, Err: waitResult.Err}
				return
			}
		}

		resultChan <- ExecuteResult{Data: response}
	}()

	return resultChan
}

// ExecuteAsync executes code asynchronously and returns a channel that receives the job ID.
// The channel receives exactly one JobIDResult then closes.
// This returns immediately with a job_id without waiting for completion.
func ExecuteAsync(creds *Credentials, language, code string) <-chan JobIDResult {
	resultChan := make(chan JobIDResult, 1)

	go func() {
		defer close(resultChan)

		data := map[string]string{
			"language": language,
			"code":     code,
		}

		response, err := makeRequest("POST", "/execute", creds, data)
		if err != nil {
			resultChan <- JobIDResult{Err: err}
			return
		}

		if jobID, ok := response["job_id"].(string); ok {
			resultChan <- JobIDResult{JobID: jobID}
			return
		}

		resultChan <- JobIDResult{Err: fmt.Errorf("no job_id in response")}
	}()

	return resultChan
}

// GetJob gets current status/result of a job (single poll, no waiting).
// Returns a channel that receives exactly one JobResult then closes.
func GetJob(creds *Credentials, jobID string) <-chan JobResult {
	resultChan := make(chan JobResult, 1)

	go func() {
		defer close(resultChan)

		response, err := makeRequest("GET", fmt.Sprintf("/jobs/%s", jobID), creds, nil)
		resultChan <- JobResult{Data: response, Err: err}
	}()

	return resultChan
}

// WaitForJob waits for job completion with exponential backoff polling.
// Returns a channel that receives exactly one JobResult then closes.
//
// Polling delays (ms): [300, 450, 700, 900, 650, 1600, 2000, ...]
// Cumulative: 300, 750, 1450, 2350, 3000, 4600, 6600ms+
//
// timeout: Maximum time to wait (0 = no timeout, wait indefinitely)
func WaitForJob(creds *Credentials, jobID string, timeout time.Duration) <-chan JobResult {
	resultChan := make(chan JobResult, 1)

	go func() {
		defer close(resultChan)

		pollCount := 0
		var deadline time.Time
		if timeout > 0 {
			deadline = time.Now().Add(timeout)
		}

		for {
			// Check timeout
			if timeout > 0 && time.Now().After(deadline) {
				resultChan <- JobResult{Err: fmt.Errorf("timeout waiting for job %s", jobID)}
				return
			}

			// Sleep before polling
			delayIdx := pollCount
			if delayIdx >= len(PollDelaysMs) {
				delayIdx = len(PollDelaysMs) - 1
			}
			time.Sleep(time.Duration(PollDelaysMs[delayIdx]) * time.Millisecond)
			pollCount++

			response, err := makeRequest("GET", fmt.Sprintf("/jobs/%s", jobID), creds, nil)
			if err != nil {
				resultChan <- JobResult{Err: err}
				return
			}

			if status, ok := response["status"].(string); ok {
				if status == "completed" || status == "failed" || status == "timeout" || status == "cancelled" {
					resultChan <- JobResult{Data: response}
					return
				}
			}

			// Still running, continue polling
		}
	}()

	return resultChan
}

// CancelJob cancels a running job.
// Returns a channel that receives exactly one CancelResult then closes.
func CancelJob(creds *Credentials, jobID string) <-chan CancelResult {
	resultChan := make(chan CancelResult, 1)

	go func() {
		defer close(resultChan)

		response, err := makeRequest("DELETE", fmt.Sprintf("/jobs/%s", jobID), creds, nil)
		resultChan <- CancelResult{Data: response, Err: err}
	}()

	return resultChan
}

// ListJobs lists all jobs for the authenticated account.
// Returns a channel that receives exactly one JobListResult then closes.
func ListJobs(creds *Credentials) <-chan JobListResult {
	resultChan := make(chan JobListResult, 1)

	go func() {
		defer close(resultChan)

		response, err := makeRequest("GET", "/jobs", creds, nil)
		if err != nil {
			resultChan <- JobListResult{Err: err}
			return
		}

		var jobs []map[string]interface{}
		if jobsInterface, ok := response["jobs"].([]interface{}); ok {
			jobs = make([]map[string]interface{}, len(jobsInterface))
			for i, job := range jobsInterface {
				if m, ok := job.(map[string]interface{}); ok {
					jobs[i] = m
				}
			}
		}

		resultChan <- JobListResult{Jobs: jobs}
	}()

	return resultChan
}

// GetLanguages gets list of supported programming languages.
// Results are cached for 1 hour in ~/.unsandbox/languages.json
// Returns a channel that receives exactly one LanguagesResult then closes.
func GetLanguages(creds *Credentials) <-chan LanguagesResult {
	resultChan := make(chan LanguagesResult, 1)

	go func() {
		defer close(resultChan)

		// Try cache first
		if cached, ok := loadLanguagesCache(); ok {
			resultChan <- LanguagesResult{Languages: cached}
			return
		}

		response, err := makeRequest("GET", "/languages", creds, nil)
		if err != nil {
			resultChan <- LanguagesResult{Err: err}
			return
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
		resultChan <- LanguagesResult{Languages: languages}
	}()

	return resultChan
}

// DetectLanguage detects programming language from filename extension.
// This is a synchronous function (no I/O, no goroutines).
//
// Args:
//
//	filename: Filename to detect language from (e.g., "script.py")
//
// Returns:
//
//	Language identifier (e.g., "python") or empty string if unknown
//
// Examples:
//
//	DetectLanguage("hello.py")   // -> "python"
//	DetectLanguage("script.js")  // -> "javascript"
//	DetectLanguage("main.go")    // -> "go"
//	DetectLanguage("unknown")    // -> ""
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

// SessionSnapshot creates a snapshot of a session.
// Returns a channel that receives exactly one SnapshotResult then closes.
//
// Args:
//
//	creds: API credentials
//	sessionID: Session ID to snapshot
//	name: Optional snapshot name (empty string for no name)
//	hot: If true, creates a hot snapshot of running session
func SessionSnapshot(creds *Credentials, sessionID, name string, hot bool) <-chan SnapshotResult {
	resultChan := make(chan SnapshotResult, 1)

	go func() {
		defer close(resultChan)

		data := map[string]interface{}{
			"session_id": sessionID,
			"hot":        hot,
		}
		if name != "" {
			data["name"] = name
		}

		response, err := makeRequest("POST", "/snapshots", creds, data)
		if err != nil {
			resultChan <- SnapshotResult{Err: err}
			return
		}

		if snapshotID, ok := response["snapshot_id"].(string); ok {
			resultChan <- SnapshotResult{SnapshotID: snapshotID}
			return
		}

		resultChan <- SnapshotResult{Err: fmt.Errorf("no snapshot_id in response")}
	}()

	return resultChan
}

// ServiceSnapshot creates a snapshot of a service.
// Returns a channel that receives exactly one SnapshotResult then closes.
//
// Args:
//
//	creds: API credentials
//	serviceID: Service ID to snapshot
//	name: Optional snapshot name (empty string for no name)
func ServiceSnapshot(creds *Credentials, serviceID, name string) <-chan SnapshotResult {
	resultChan := make(chan SnapshotResult, 1)

	go func() {
		defer close(resultChan)

		data := map[string]interface{}{
			"service_id": serviceID,
		}
		if name != "" {
			data["name"] = name
		}

		response, err := makeRequest("POST", "/snapshots", creds, data)
		if err != nil {
			resultChan <- SnapshotResult{Err: err}
			return
		}

		if snapshotID, ok := response["snapshot_id"].(string); ok {
			resultChan <- SnapshotResult{SnapshotID: snapshotID}
			return
		}

		resultChan <- SnapshotResult{Err: fmt.Errorf("no snapshot_id in response")}
	}()

	return resultChan
}

// ListSnapshots lists all snapshots.
// Returns a channel that receives exactly one SnapshotListResult then closes.
func ListSnapshots(creds *Credentials) <-chan SnapshotListResult {
	resultChan := make(chan SnapshotListResult, 1)

	go func() {
		defer close(resultChan)

		response, err := makeRequest("GET", "/snapshots", creds, nil)
		if err != nil {
			resultChan <- SnapshotListResult{Err: err}
			return
		}

		var snapshots []map[string]interface{}
		if snapshotsInterface, ok := response["snapshots"].([]interface{}); ok {
			snapshots = make([]map[string]interface{}, len(snapshotsInterface))
			for i, snapshot := range snapshotsInterface {
				if m, ok := snapshot.(map[string]interface{}); ok {
					snapshots[i] = m
				}
			}
		}

		resultChan <- SnapshotListResult{Snapshots: snapshots}
	}()

	return resultChan
}

// RestoreSnapshot restores a snapshot.
// Returns a channel that receives exactly one RestoreResult then closes.
func RestoreSnapshot(creds *Credentials, snapshotID string) <-chan RestoreResult {
	resultChan := make(chan RestoreResult, 1)

	go func() {
		defer close(resultChan)

		response, err := makeRequest("POST", fmt.Sprintf("/snapshots/%s/restore", snapshotID), creds, map[string]interface{}{})
		resultChan <- RestoreResult{Data: response, Err: err}
	}()

	return resultChan
}

// DeleteSnapshot deletes a snapshot.
// Returns a channel that receives exactly one DeleteResult then closes.
func DeleteSnapshot(creds *Credentials, snapshotID string) <-chan DeleteResult {
	resultChan := make(chan DeleteResult, 1)

	go func() {
		defer close(resultChan)

		response, err := makeRequest("DELETE", fmt.Sprintf("/snapshots/%s", snapshotID), creds, nil)
		resultChan <- DeleteResult{Data: response, Err: err}
	}()

	return resultChan
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

// SessionListResult contains the result of listing sessions.
type SessionListResult struct {
	Sessions []map[string]interface{}
	Err      error
}

// SessionResult contains the result of a session operation.
type SessionResult struct {
	Data map[string]interface{}
	Err  error
}

// ListSessions lists all active sessions for the authenticated account.
// Returns a channel that receives exactly one SessionListResult then closes.
func ListSessions(creds *Credentials) <-chan SessionListResult {
	resultChan := make(chan SessionListResult, 1)

	go func() {
		defer close(resultChan)

		response, err := makeRequest("GET", "/sessions", creds, nil)
		if err != nil {
			resultChan <- SessionListResult{Err: err}
			return
		}

		var sessions []map[string]interface{}
		if sessionsInterface, ok := response["sessions"].([]interface{}); ok {
			sessions = make([]map[string]interface{}, len(sessionsInterface))
			for i, session := range sessionsInterface {
				if m, ok := session.(map[string]interface{}); ok {
					sessions[i] = m
				}
			}
		}

		resultChan <- SessionListResult{Sessions: sessions}
	}()

	return resultChan
}

// GetSession gets details of a specific session.
// Returns a channel that receives exactly one SessionResult then closes.
func GetSession(creds *Credentials, sessionID string) <-chan SessionResult {
	resultChan := make(chan SessionResult, 1)

	go func() {
		defer close(resultChan)

		response, err := makeRequest("GET", fmt.Sprintf("/sessions/%s", sessionID), creds, nil)
		resultChan <- SessionResult{Data: response, Err: err}
	}()

	return resultChan
}

// CreateSession creates a new interactive session.
// Returns a channel that receives exactly one SessionResult then closes.
//
// Args:
//
//	creds: API credentials
//	opts: Optional session configuration (can be nil for defaults)
func CreateSession(creds *Credentials, opts *SessionOptions) <-chan SessionResult {
	resultChan := make(chan SessionResult, 1)

	go func() {
		defer close(resultChan)

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

		response, err := makeRequest("POST", "/sessions", creds, data)
		resultChan <- SessionResult{Data: response, Err: err}
	}()

	return resultChan
}

// DeleteSession terminates a session.
// Returns a channel that receives exactly one SessionResult then closes.
func DeleteSession(creds *Credentials, sessionID string) <-chan SessionResult {
	resultChan := make(chan SessionResult, 1)

	go func() {
		defer close(resultChan)

		response, err := makeRequest("DELETE", fmt.Sprintf("/sessions/%s", sessionID), creds, nil)
		resultChan <- SessionResult{Data: response, Err: err}
	}()

	return resultChan
}

// FreezeSession freezes a session (pauses execution, preserves state).
// Returns a channel that receives exactly one SessionResult then closes.
func FreezeSession(creds *Credentials, sessionID string) <-chan SessionResult {
	resultChan := make(chan SessionResult, 1)

	go func() {
		defer close(resultChan)

		response, err := makeRequest("POST", fmt.Sprintf("/sessions/%s/freeze", sessionID), creds, map[string]interface{}{})
		resultChan <- SessionResult{Data: response, Err: err}
	}()

	return resultChan
}

// UnfreezeSession unfreezes a previously frozen session.
// Returns a channel that receives exactly one SessionResult then closes.
func UnfreezeSession(creds *Credentials, sessionID string) <-chan SessionResult {
	resultChan := make(chan SessionResult, 1)

	go func() {
		defer close(resultChan)

		response, err := makeRequest("POST", fmt.Sprintf("/sessions/%s/unfreeze", sessionID), creds, map[string]interface{}{})
		resultChan <- SessionResult{Data: response, Err: err}
	}()

	return resultChan
}

// BoostSession increases the vCPU allocation for a session.
// Returns a channel that receives exactly one SessionResult then closes.
//
// Args:
//
//	creds: API credentials
//	sessionID: Session ID to boost
//	vcpu: Number of vCPUs (2, 4, 8, etc.)
func BoostSession(creds *Credentials, sessionID string, vcpu int) <-chan SessionResult {
	resultChan := make(chan SessionResult, 1)

	go func() {
		defer close(resultChan)

		data := map[string]interface{}{
			"vcpu": vcpu,
		}
		response, err := makeRequest("POST", fmt.Sprintf("/sessions/%s/boost", sessionID), creds, data)
		resultChan <- SessionResult{Data: response, Err: err}
	}()

	return resultChan
}

// UnboostSession resets the vCPU allocation for a session to default.
// Returns a channel that receives exactly one SessionResult then closes.
func UnboostSession(creds *Credentials, sessionID string) <-chan SessionResult {
	resultChan := make(chan SessionResult, 1)

	go func() {
		defer close(resultChan)

		response, err := makeRequest("POST", fmt.Sprintf("/sessions/%s/unboost", sessionID), creds, map[string]interface{}{})
		resultChan <- SessionResult{Data: response, Err: err}
	}()

	return resultChan
}

// ShellSession executes a command in a session's shell.
// Returns a channel that receives exactly one SessionResult then closes.
//
// Note: For interactive shell access, use the WebSocket-based shell endpoint.
// This function is for executing single commands.
func ShellSession(creds *Credentials, sessionID, command string) <-chan SessionResult {
	resultChan := make(chan SessionResult, 1)

	go func() {
		defer close(resultChan)

		data := map[string]interface{}{
			"command": command,
		}
		response, err := makeRequest("POST", fmt.Sprintf("/sessions/%s/shell", sessionID), creds, data)
		resultChan <- SessionResult{Data: response, Err: err}
	}()

	return resultChan
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

// ServiceListResult contains the result of listing services.
type ServiceListResult struct {
	Services []map[string]interface{}
	Err      error
}

// ServiceResult contains the result of a service operation.
type ServiceResult struct {
	Data map[string]interface{}
	Err  error
}

// ListServices lists all services for the authenticated account.
// Returns a channel that receives exactly one ServiceListResult then closes.
func ListServices(creds *Credentials) <-chan ServiceListResult {
	resultChan := make(chan ServiceListResult, 1)

	go func() {
		defer close(resultChan)

		response, err := makeRequest("GET", "/services", creds, nil)
		if err != nil {
			resultChan <- ServiceListResult{Err: err}
			return
		}

		var services []map[string]interface{}
		if servicesInterface, ok := response["services"].([]interface{}); ok {
			services = make([]map[string]interface{}, len(servicesInterface))
			for i, service := range servicesInterface {
				if m, ok := service.(map[string]interface{}); ok {
					services[i] = m
				}
			}
		}

		resultChan <- ServiceListResult{Services: services}
	}()

	return resultChan
}

// CreateService creates a new persistent service.
// Returns a channel that receives exactly one ServiceResult then closes.
//
// Args:
//
//	creds: API credentials
//	name: Service name
//	ports: Array of port numbers to expose
//	bootstrap: Bootstrap script to run on service start
//	opts: Optional service configuration (can be nil for defaults)
func CreateService(creds *Credentials, name string, ports []int, bootstrap string, opts *ServiceOptions) <-chan ServiceResult {
	resultChan := make(chan ServiceResult, 1)

	go func() {
		defer close(resultChan)

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

		response, err := makeRequest("POST", "/services", creds, data)
		resultChan <- ServiceResult{Data: response, Err: err}
	}()

	return resultChan
}

// GetService gets details of a specific service.
// Returns a channel that receives exactly one ServiceResult then closes.
func GetService(creds *Credentials, serviceID string) <-chan ServiceResult {
	resultChan := make(chan ServiceResult, 1)

	go func() {
		defer close(resultChan)

		response, err := makeRequest("GET", fmt.Sprintf("/services/%s", serviceID), creds, nil)
		resultChan <- ServiceResult{Data: response, Err: err}
	}()

	return resultChan
}

// UpdateService updates a service's configuration.
// Returns a channel that receives exactly one ServiceResult then closes.
func UpdateService(creds *Credentials, serviceID string, opts *ServiceUpdateOptions) <-chan ServiceResult {
	resultChan := make(chan ServiceResult, 1)

	go func() {
		defer close(resultChan)

		data := make(map[string]interface{})
		if opts != nil {
			if opts.VCPU > 0 {
				data["vcpu"] = opts.VCPU
			}
		}
		response, err := makeRequest("PATCH", fmt.Sprintf("/services/%s", serviceID), creds, data)
		resultChan <- ServiceResult{Data: response, Err: err}
	}()

	return resultChan
}

// DeleteService destroys a service.
// Returns a channel that receives exactly one ServiceResult then closes.
func DeleteService(creds *Credentials, serviceID string) <-chan ServiceResult {
	resultChan := make(chan ServiceResult, 1)

	go func() {
		defer close(resultChan)

		response, err := makeRequest("DELETE", fmt.Sprintf("/services/%s", serviceID), creds, nil)
		resultChan <- ServiceResult{Data: response, Err: err}
	}()

	return resultChan
}

// FreezeService freezes a service (pauses execution, preserves state).
// Returns a channel that receives exactly one ServiceResult then closes.
func FreezeService(creds *Credentials, serviceID string) <-chan ServiceResult {
	resultChan := make(chan ServiceResult, 1)

	go func() {
		defer close(resultChan)

		response, err := makeRequest("POST", fmt.Sprintf("/services/%s/freeze", serviceID), creds, map[string]interface{}{})
		resultChan <- ServiceResult{Data: response, Err: err}
	}()

	return resultChan
}

// UnfreezeService unfreezes a previously frozen service.
// Returns a channel that receives exactly one ServiceResult then closes.
func UnfreezeService(creds *Credentials, serviceID string) <-chan ServiceResult {
	resultChan := make(chan ServiceResult, 1)

	go func() {
		defer close(resultChan)

		response, err := makeRequest("POST", fmt.Sprintf("/services/%s/unfreeze", serviceID), creds, map[string]interface{}{})
		resultChan <- ServiceResult{Data: response, Err: err}
	}()

	return resultChan
}

// LockService locks a service to prevent modifications or deletion.
// Returns a channel that receives exactly one ServiceResult then closes.
func LockService(creds *Credentials, serviceID string) <-chan ServiceResult {
	resultChan := make(chan ServiceResult, 1)

	go func() {
		defer close(resultChan)

		response, err := makeRequest("POST", fmt.Sprintf("/services/%s/lock", serviceID), creds, map[string]interface{}{})
		resultChan <- ServiceResult{Data: response, Err: err}
	}()

	return resultChan
}

// UnlockService unlocks a previously locked service.
// Returns a channel that receives exactly one ServiceResult then closes.
func UnlockService(creds *Credentials, serviceID string) <-chan ServiceResult {
	resultChan := make(chan ServiceResult, 1)

	go func() {
		defer close(resultChan)

		response, err := makeRequest("POST", fmt.Sprintf("/services/%s/unlock", serviceID), creds, map[string]interface{}{})
		resultChan <- ServiceResult{Data: response, Err: err}
	}()

	return resultChan
}

// SetUnfreezeOnDemand enables or disables automatic unfreezing on HTTP request.
// Returns a channel that receives exactly one ServiceResult then closes.
func SetUnfreezeOnDemand(creds *Credentials, serviceID string, enabled bool) <-chan ServiceResult {
	resultChan := make(chan ServiceResult, 1)

	go func() {
		defer close(resultChan)

		response, err := makeRequest("PATCH", fmt.Sprintf("/services/%s", serviceID), creds, map[string]interface{}{
			"unfreeze_on_demand": enabled,
		})
		resultChan <- ServiceResult{Data: response, Err: err}
	}()

	return resultChan
}

// SetShowFreezePage enables or disables the freeze page display for a service.
// When enabled, frozen services show a branded "waking up" page instead of an error.
// Returns a channel that receives exactly one ServiceResult then closes.
func SetShowFreezePage(creds *Credentials, serviceID string, enabled bool) <-chan ServiceResult {
	resultChan := make(chan ServiceResult, 1)

	go func() {
		defer close(resultChan)

		response, err := makeRequest("PATCH", fmt.Sprintf("/services/%s", serviceID), creds, map[string]interface{}{
			"show_freeze_page": enabled,
		})
		resultChan <- ServiceResult{Data: response, Err: err}
	}()

	return resultChan
}

// GetServiceLogs retrieves logs from a service.
// Returns a channel that receives exactly one ServiceResult then closes.
//
// Args:
//
//	creds: API credentials
//	serviceID: Service ID
//	all: If true, returns all logs; if false, returns only recent logs
func GetServiceLogs(creds *Credentials, serviceID string, all bool) <-chan ServiceResult {
	resultChan := make(chan ServiceResult, 1)

	go func() {
		defer close(resultChan)

		path := fmt.Sprintf("/services/%s/logs", serviceID)
		if all {
			path = fmt.Sprintf("/services/%s/logs?all=true", serviceID)
		}
		response, err := makeRequest("GET", path, creds, nil)
		resultChan <- ServiceResult{Data: response, Err: err}
	}()

	return resultChan
}

// GetServiceEnv retrieves the environment variable names for a service.
// Returns a channel that receives exactly one ServiceResult then closes.
// Note: Values are not returned for security; use ExportServiceEnv for full export.
func GetServiceEnv(creds *Credentials, serviceID string) <-chan ServiceResult {
	resultChan := make(chan ServiceResult, 1)

	go func() {
		defer close(resultChan)

		response, err := makeRequest("GET", fmt.Sprintf("/services/%s/env", serviceID), creds, nil)
		resultChan <- ServiceResult{Data: response, Err: err}
	}()

	return resultChan
}

// SetServiceEnv sets environment variables for a service.
// Returns a channel that receives exactly one ServiceResult then closes.
//
// Args:
//
//	creds: API credentials
//	serviceID: Service ID
//	env: Map of environment variable names to values
func SetServiceEnv(creds *Credentials, serviceID string, env map[string]string) <-chan ServiceResult {
	resultChan := make(chan ServiceResult, 1)

	go func() {
		defer close(resultChan)

		response, err := makeRequest("POST", fmt.Sprintf("/services/%s/env", serviceID), creds, env)
		resultChan <- ServiceResult{Data: response, Err: err}
	}()

	return resultChan
}

// DeleteServiceEnv deletes environment variables from a service.
// Returns a channel that receives exactly one ServiceResult then closes.
//
// Args:
//
//	creds: API credentials
//	serviceID: Service ID
//	keys: List of environment variable names to delete (nil deletes all)
func DeleteServiceEnv(creds *Credentials, serviceID string, keys []string) <-chan ServiceResult {
	resultChan := make(chan ServiceResult, 1)

	go func() {
		defer close(resultChan)

		var data interface{}
		if keys != nil {
			data = map[string]interface{}{"keys": keys}
		}
		response, err := makeRequest("DELETE", fmt.Sprintf("/services/%s/env", serviceID), creds, data)
		resultChan <- ServiceResult{Data: response, Err: err}
	}()

	return resultChan
}

// ExportServiceEnv exports all environment variables for a service.
// Returns a channel that receives exactly one ServiceResult then closes.
// Returns the full .env format content with values.
func ExportServiceEnv(creds *Credentials, serviceID string) <-chan ServiceResult {
	resultChan := make(chan ServiceResult, 1)

	go func() {
		defer close(resultChan)

		response, err := makeRequest("POST", fmt.Sprintf("/services/%s/env/export", serviceID), creds, map[string]interface{}{})
		resultChan <- ServiceResult{Data: response, Err: err}
	}()

	return resultChan
}

// RedeployService redeploys a service with optional new bootstrap script.
// Returns a channel that receives exactly one ServiceResult then closes.
//
// Args:
//
//	creds: API credentials
//	serviceID: Service ID
//	bootstrap: New bootstrap script (empty string to keep existing)
func RedeployService(creds *Credentials, serviceID string, bootstrap string) <-chan ServiceResult {
	resultChan := make(chan ServiceResult, 1)

	go func() {
		defer close(resultChan)

		data := make(map[string]interface{})
		if bootstrap != "" {
			data["bootstrap"] = bootstrap
		}
		response, err := makeRequest("POST", fmt.Sprintf("/services/%s/redeploy", serviceID), creds, data)
		resultChan <- ServiceResult{Data: response, Err: err}
	}()

	return resultChan
}

// ExecuteInService executes a command in a running service.
// Returns a channel that receives exactly one ServiceResult then closes.
func ExecuteInService(creds *Credentials, serviceID, command string) <-chan ServiceResult {
	resultChan := make(chan ServiceResult, 1)

	go func() {
		defer close(resultChan)

		data := map[string]interface{}{
			"command": command,
		}
		response, err := makeRequest("POST", fmt.Sprintf("/services/%s/execute", serviceID), creds, data)
		resultChan <- ServiceResult{Data: response, Err: err}
	}()

	return resultChan
}

// ============================================================================
// Additional Snapshot Operations
// ============================================================================

// LockSnapshot locks a snapshot to prevent deletion.
// Returns a channel that receives exactly one DeleteResult then closes.
func LockSnapshot(creds *Credentials, snapshotID string) <-chan DeleteResult {
	resultChan := make(chan DeleteResult, 1)

	go func() {
		defer close(resultChan)

		response, err := makeRequest("POST", fmt.Sprintf("/snapshots/%s/lock", snapshotID), creds, map[string]interface{}{})
		resultChan <- DeleteResult{Data: response, Err: err}
	}()

	return resultChan
}

// UnlockSnapshot unlocks a previously locked snapshot.
// Returns a channel that receives exactly one DeleteResult then closes.
func UnlockSnapshot(creds *Credentials, snapshotID string) <-chan DeleteResult {
	resultChan := make(chan DeleteResult, 1)

	go func() {
		defer close(resultChan)

		response, err := makeRequest("POST", fmt.Sprintf("/snapshots/%s/unlock", snapshotID), creds, map[string]interface{}{})
		resultChan <- DeleteResult{Data: response, Err: err}
	}()

	return resultChan
}

// CloneSnapshotOptions contains optional parameters for snapshot cloning.
type CloneSnapshotOptions struct {
	Name  string // Name for the cloned resource
	Shell string // Shell to use (for session clones)
	Ports []int  // Ports to expose (for service clones)
}

// CloneSnapshotResult contains the result of cloning a snapshot.
type CloneSnapshotResult struct {
	Data map[string]interface{}
	Err  error
}

// CloneSnapshot clones a snapshot into a new session or service.
// Returns a channel that receives exactly one CloneSnapshotResult then closes.
//
// Args:
//
//	creds: API credentials
//	snapshotID: Snapshot ID to clone
//	cloneType: "session" or "service"
//	opts: Optional clone configuration (can be nil)
func CloneSnapshot(creds *Credentials, snapshotID, cloneType string, opts *CloneSnapshotOptions) <-chan CloneSnapshotResult {
	resultChan := make(chan CloneSnapshotResult, 1)

	go func() {
		defer close(resultChan)

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

		response, err := makeRequest("POST", fmt.Sprintf("/snapshots/%s/clone", snapshotID), creds, data)
		resultChan <- CloneSnapshotResult{Data: response, Err: err}
	}()

	return resultChan
}

// ============================================================================
// Key Validation
// ============================================================================

// ValidateKeysResult contains the result of validating API keys.
type ValidateKeysResult struct {
	Data map[string]interface{}
	Err  error
}

// ValidateKeys validates the API credentials with the server.
// Returns a channel that receives exactly one ValidateKeysResult then closes.
// Returns account information if valid, error if invalid.
func ValidateKeys(creds *Credentials) <-chan ValidateKeysResult {
	resultChan := make(chan ValidateKeysResult, 1)

	go func() {
		defer close(resultChan)

		response, err := makeRequest("POST", "/keys/validate", creds, map[string]interface{}{})
		resultChan <- ValidateKeysResult{Data: response, Err: err}
	}()

	return resultChan
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
	Images    []string
	CreatedAt string
	Err       error
}

// Image generates images from a text prompt using AI.
// Returns a channel that receives exactly one ImageResult then closes.
func Image(creds *Credentials, prompt string, opts *ImageOptions) <-chan ImageResult {
	resultChan := make(chan ImageResult, 1)

	go func() {
		defer close(resultChan)

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
			resultChan <- ImageResult{Err: err}
			return
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

		resultChan <- ImageResult{
			Images:    images,
			CreatedAt: createdAt,
		}
	}()

	return resultChan
}

// ============================================================================
// Snapshot Info
// ============================================================================

// GetSnapshotResult contains the result of getting a snapshot
type GetSnapshotResult struct {
	Data map[string]interface{}
	Err  error
}

// GetSnapshot gets details of a specific snapshot.
// Returns a channel that receives exactly one GetSnapshotResult then closes.
func GetSnapshot(creds *Credentials, snapshotID string) <-chan GetSnapshotResult {
	resultChan := make(chan GetSnapshotResult, 1)

	go func() {
		defer close(resultChan)

		response, err := makeRequest("GET", fmt.Sprintf("/snapshots/%s", snapshotID), creds, nil)
		resultChan <- GetSnapshotResult{Data: response, Err: err}
	}()

	return resultChan
}

// ============================================================================
// CLI Implementation
// ============================================================================

// Exit codes
const (
	ExitSuccess      = 0
	ExitGeneralError = 1
	ExitInvalidArgs  = 2
	ExitAuthError    = 3
	ExitAPIError     = 4
	ExitTimeout      = 5
)

// CLIOptions holds parsed CLI arguments
type CLIOptions struct {
	// Global options
	Shell     string
	Env       []string
	Files     []string
	FilePaths []string
	Artifacts bool
	OutputDir string
	PublicKey string
	SecretKey string
	Network   string
	VCPU      int
	Yes       bool
	Help      bool

	// Command
	Command    string
	SubCommand string
	Args       []string
}

// printUsage prints the main help message
func printUsage() {
	fmt.Fprintf(os.Stderr, `unsandbox - Execute code securely in sandboxed containers (async)

Usage:
  un [options] <source_file>        Execute code file
  un [options] -s LANG 'code'       Execute inline code
  un session [options]              Interactive session
  un service [options]              Manage services
  un snapshot [options]             Manage snapshots
  un key                            Check API key

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

	// Execute asynchronously and wait for result
	resultChan := ExecuteCode(creds, language, code)
	result := <-resultChan
	if result.Err != nil {
		return cliError(result.Err.Error(), ExitAPIError)
	}

	// Print output
	if stdout, ok := result.Data["stdout"].(string); ok && stdout != "" {
		fmt.Print(stdout)
	}
	if stderr, ok := result.Data["stderr"].(string); ok && stderr != "" {
		fmt.Fprint(os.Stderr, stderr)
	}

	// Print summary
	fmt.Println("---")
	if exitCode, ok := result.Data["exit_code"].(float64); ok {
		fmt.Printf("Exit code: %d\n", int(exitCode))
	}
	if execTime, ok := result.Data["execution_time_ms"].(float64); ok {
		fmt.Printf("Execution time: %dms\n", int(execTime))
	}

	if exitCode, ok := result.Data["exit_code"].(float64); ok && exitCode != 0 {
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
		resultChan := ListSessions(creds)
		result := <-resultChan
		if result.Err != nil {
			return cliError(result.Err.Error(), ExitAPIError)
		}
		formatList(result.Sessions, []string{"session_id", "status", "shell", "created_at"})
		return ExitSuccess
	}

	// Attach to session
	if fs.attach != "" {
		resultChan := GetSession(creds, fs.attach)
		result := <-resultChan
		if result.Err != nil {
			return cliError(result.Err.Error(), ExitAPIError)
		}
		// Print connection info
		fmt.Printf("Session: %s\n", fs.attach)
		if wsURL, ok := result.Data["websocket_url"].(string); ok {
			fmt.Printf("Connect via: %s\n", wsURL)
		}
		return ExitSuccess
	}

	// Kill session
	if fs.kill != "" {
		resultChan := DeleteSession(creds, fs.kill)
		result := <-resultChan
		if result.Err != nil {
			return cliError(result.Err.Error(), ExitAPIError)
		}
		fmt.Printf("Session %s terminated\n", fs.kill)
		return ExitSuccess
	}

	// Freeze session
	if fs.freeze != "" {
		resultChan := FreezeSession(creds, fs.freeze)
		result := <-resultChan
		if result.Err != nil {
			return cliError(result.Err.Error(), ExitAPIError)
		}
		fmt.Printf("Session %s frozen\n", fs.freeze)
		return ExitSuccess
	}

	// Unfreeze session
	if fs.unfreeze != "" {
		resultChan := UnfreezeSession(creds, fs.unfreeze)
		result := <-resultChan
		if result.Err != nil {
			return cliError(result.Err.Error(), ExitAPIError)
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
		resultChan := BoostSession(creds, fs.boost, vcpu)
		result := <-resultChan
		if result.Err != nil {
			return cliError(result.Err.Error(), ExitAPIError)
		}
		fmt.Printf("Session %s boosted to %d vCPUs\n", fs.boost, vcpu)
		return ExitSuccess
	}

	// Unboost session
	if fs.unboost != "" {
		resultChan := UnboostSession(creds, fs.unboost)
		result := <-resultChan
		if result.Err != nil {
			return cliError(result.Err.Error(), ExitAPIError)
		}
		fmt.Printf("Session %s unboost\n", fs.unboost)
		return ExitSuccess
	}

	// Snapshot session
	if fs.snapshot != "" {
		resultChan := SessionSnapshot(creds, fs.snapshot, fs.snapshotName, fs.hot)
		result := <-resultChan
		if result.Err != nil {
			return cliError(result.Err.Error(), ExitAPIError)
		}
		fmt.Printf("Snapshot created: %s\n", result.SnapshotID)
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

	resultChan := CreateSession(creds, sessionOpts)
	result := <-resultChan
	if result.Err != nil {
		return cliError(result.Err.Error(), ExitAPIError)
	}

	fmt.Printf("Session created: %v\n", result.Data["session_id"])
	if wsURL, ok := result.Data["websocket_url"].(string); ok {
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
		resultChan := ListServices(creds)
		result := <-resultChan
		if result.Err != nil {
			return cliError(result.Err.Error(), ExitAPIError)
		}
		formatList(result.Services, []string{"service_id", "name", "status", "created_at"})
		return ExitSuccess
	}

	// Get service info
	if fs.info != "" {
		resultChan := GetService(creds, fs.info)
		result := <-resultChan
		if result.Err != nil {
			return cliError(result.Err.Error(), ExitAPIError)
		}
		jsonOut, _ := json.MarshalIndent(result.Data, "", "  ")
		fmt.Println(string(jsonOut))
		return ExitSuccess
	}

	// Get service logs
	if fs.logs != "" {
		resultChan := GetServiceLogs(creds, fs.logs, true)
		result := <-resultChan
		if result.Err != nil {
			return cliError(result.Err.Error(), ExitAPIError)
		}
		if content, ok := result.Data["logs"].(string); ok {
			fmt.Print(content)
		}
		return ExitSuccess
	}

	// Get service tail logs
	if fs.tail != "" {
		resultChan := GetServiceLogs(creds, fs.tail, false)
		result := <-resultChan
		if result.Err != nil {
			return cliError(result.Err.Error(), ExitAPIError)
		}
		if content, ok := result.Data["logs"].(string); ok {
			fmt.Print(content)
		}
		return ExitSuccess
	}

	// Freeze service
	if fs.freeze != "" {
		resultChan := FreezeService(creds, fs.freeze)
		result := <-resultChan
		if result.Err != nil {
			return cliError(result.Err.Error(), ExitAPIError)
		}
		fmt.Printf("Service %s frozen\n", fs.freeze)
		return ExitSuccess
	}

	// Unfreeze service
	if fs.unfreeze != "" {
		resultChan := UnfreezeService(creds, fs.unfreeze)
		result := <-resultChan
		if result.Err != nil {
			return cliError(result.Err.Error(), ExitAPIError)
		}
		fmt.Printf("Service %s unfrozen\n", fs.unfreeze)
		return ExitSuccess
	}

	// Destroy service
	if fs.destroy != "" {
		resultChan := DeleteService(creds, fs.destroy)
		result := <-resultChan
		if result.Err != nil {
			return cliError(result.Err.Error(), ExitAPIError)
		}
		fmt.Printf("Service %s destroyed\n", fs.destroy)
		return ExitSuccess
	}

	// Lock service
	if fs.lock != "" {
		resultChan := LockService(creds, fs.lock)
		result := <-resultChan
		if result.Err != nil {
			return cliError(result.Err.Error(), ExitAPIError)
		}
		fmt.Printf("Service %s locked\n", fs.lock)
		return ExitSuccess
	}

	// Unlock service
	if fs.unlock != "" {
		resultChan := UnlockService(creds, fs.unlock)
		result := <-resultChan
		if result.Err != nil {
			return cliError(result.Err.Error(), ExitAPIError)
		}
		fmt.Printf("Service %s unlocked\n", fs.unlock)
		return ExitSuccess
	}

	// Resize service
	if fs.resize != "" {
		if opts.VCPU == 0 {
			return cliError("--vcpu required for resize", ExitInvalidArgs)
		}
		resultChan := UpdateService(creds, fs.resize, &ServiceUpdateOptions{VCPU: opts.VCPU})
		result := <-resultChan
		if result.Err != nil {
			return cliError(result.Err.Error(), ExitAPIError)
		}
		fmt.Printf("Service %s resized to %d vCPUs\n", fs.resize, opts.VCPU)
		return ExitSuccess
	}

	// Redeploy service
	if fs.redeploy != "" {
		resultChan := RedeployService(creds, fs.redeploy, fs.bootstrap)
		result := <-resultChan
		if result.Err != nil {
			return cliError(result.Err.Error(), ExitAPIError)
		}
		fmt.Printf("Service %s redeployed\n", fs.redeploy)
		return ExitSuccess
	}

	// Execute in service
	if fs.execute != "" && fs.executeCmd != "" {
		resultChan := ExecuteInService(creds, fs.execute, fs.executeCmd)
		result := <-resultChan
		if result.Err != nil {
			return cliError(result.Err.Error(), ExitAPIError)
		}
		if stdout, ok := result.Data["stdout"].(string); ok {
			fmt.Print(stdout)
		}
		if stderr, ok := result.Data["stderr"].(string); ok {
			fmt.Fprint(os.Stderr, stderr)
		}
		return ExitSuccess
	}

	// Snapshot service
	if fs.snapshot != "" {
		resultChan := ServiceSnapshot(creds, fs.snapshot, fs.snapshotName)
		result := <-resultChan
		if result.Err != nil {
			return cliError(result.Err.Error(), ExitAPIError)
		}
		fmt.Printf("Snapshot created: %s\n", result.SnapshotID)
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

		resultChan := CreateService(creds, fs.name, ports, bootstrap, serviceOpts)
		result := <-resultChan
		if result.Err != nil {
			return cliError(result.Err.Error(), ExitAPIError)
		}

		fmt.Printf("Service created: %v\n", result.Data["service_id"])
		if url, ok := result.Data["url"].(string); ok {
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
		resultChan := GetServiceEnv(creds, fs.envID)
		result := <-resultChan
		if result.Err != nil {
			return cliError(result.Err.Error(), ExitAPIError)
		}
		jsonOut, _ := json.MarshalIndent(result.Data, "", "  ")
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
		resultChan := SetServiceEnv(creds, fs.envID, env)
		result := <-resultChan
		if result.Err != nil {
			return cliError(result.Err.Error(), ExitAPIError)
		}
		fmt.Println("Environment variables set")

	case "export":
		resultChan := ExportServiceEnv(creds, fs.envID)
		result := <-resultChan
		if result.Err != nil {
			return cliError(result.Err.Error(), ExitAPIError)
		}
		if content, ok := result.Data["content"].(string); ok {
			fmt.Print(content)
		}

	case "delete":
		resultChan := DeleteServiceEnv(creds, fs.envID, nil)
		result := <-resultChan
		if result.Err != nil {
			return cliError(result.Err.Error(), ExitAPIError)
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
		resultChan := ListSnapshots(creds)
		result := <-resultChan
		if result.Err != nil {
			return cliError(result.Err.Error(), ExitAPIError)
		}
		formatList(result.Snapshots, []string{"snapshot_id", "name", "type", "created_at"})
		return ExitSuccess
	}

	// Get snapshot info
	if fs.info != "" {
		resultChan := GetSnapshot(creds, fs.info)
		result := <-resultChan
		if result.Err != nil {
			return cliError(result.Err.Error(), ExitAPIError)
		}
		jsonOut, _ := json.MarshalIndent(result.Data, "", "  ")
		fmt.Println(string(jsonOut))
		return ExitSuccess
	}

	// Delete snapshot
	if fs.deleteID != "" {
		resultChan := DeleteSnapshot(creds, fs.deleteID)
		result := <-resultChan
		if result.Err != nil {
			return cliError(result.Err.Error(), ExitAPIError)
		}
		fmt.Printf("Snapshot %s deleted\n", fs.deleteID)
		return ExitSuccess
	}

	// Lock snapshot
	if fs.lock != "" {
		resultChan := LockSnapshot(creds, fs.lock)
		result := <-resultChan
		if result.Err != nil {
			return cliError(result.Err.Error(), ExitAPIError)
		}
		fmt.Printf("Snapshot %s locked\n", fs.lock)
		return ExitSuccess
	}

	// Unlock snapshot
	if fs.unlock != "" {
		resultChan := UnlockSnapshot(creds, fs.unlock)
		result := <-resultChan
		if result.Err != nil {
			return cliError(result.Err.Error(), ExitAPIError)
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

		resultChan := CloneSnapshot(creds, fs.clone, cloneType, cloneOpts)
		result := <-resultChan
		if result.Err != nil {
			return cliError(result.Err.Error(), ExitAPIError)
		}
		jsonOut, _ := json.MarshalIndent(result.Data, "", "  ")
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

// runKey handles the key command
func runKey(creds *Credentials) int {
	resultChan := ValidateKeys(creds)
	result := <-resultChan
	if result.Err != nil {
		return cliError(result.Err.Error(), ExitAuthError)
	}

	fmt.Println("API key is valid")
	if account, ok := result.Data["account"].(map[string]interface{}); ok {
		jsonOut, _ := json.MarshalIndent(account, "", "  ")
		fmt.Println(string(jsonOut))
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
		case "session", "service", "snapshot", "key":
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
	case "key":
		exitCode = runKey(creds)
	default:
		printUsage()
		exitCode = ExitInvalidArgs
	}

	os.Exit(exitCode)
}
