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
