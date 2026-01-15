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
	NetworkMode string // "zerotrust" (default) or "semitrusted"
	Shell       string // Shell to use for bootstrap
	VCPU        int    // Number of virtual CPUs
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
func DeleteService(creds *Credentials, serviceID string) (map[string]interface{}, error) {
	return makeRequest("DELETE", fmt.Sprintf("/services/%s", serviceID), creds, nil)
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
func UnlockService(creds *Credentials, serviceID string) (map[string]interface{}, error) {
	return makeRequest("POST", fmt.Sprintf("/services/%s/unlock", serviceID), creds, map[string]interface{}{})
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

// ============================================================================
// Additional Snapshot Operations
// ============================================================================

// LockSnapshot locks a snapshot to prevent deletion.
func LockSnapshot(creds *Credentials, snapshotID string) (map[string]interface{}, error) {
	return makeRequest("POST", fmt.Sprintf("/snapshots/%s/lock", snapshotID), creds, map[string]interface{}{})
}

// UnlockSnapshot unlocks a previously locked snapshot.
func UnlockSnapshot(creds *Credentials, snapshotID string) (map[string]interface{}, error) {
	return makeRequest("POST", fmt.Sprintf("/snapshots/%s/unlock", snapshotID), creds, map[string]interface{}{})
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
