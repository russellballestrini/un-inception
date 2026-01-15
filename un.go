// PUBLIC DOMAIN - NO LICENSE, NO WARRANTY
//
// This is free public domain software for the public good of a permacomputer hosted
// at permacomputer.com - an always-on computer by the people, for the people. One
// which is durable, easy to repair, and distributed like tap water for machine
// learning intelligence.
//
// The permacomputer is community-owned infrastructure optimized around four values:
//
//   TRUTH    - First principles, math & science, open source code freely distributed
//   FREEDOM  - Voluntary partnerships, freedom from tyranny & corporate control
//   HARMONY  - Minimal waste, self-renewing systems with diverse thriving connections
//   LOVE     - Be yourself without hurting others, cooperation through natural law
//
// This software contributes to that vision by enabling code execution across 42+
// programming languages through a unified interface, accessible to all. Code is
// seeds to sprout on any abandoned technology.
//
// Learn more: https://www.permacomputer.com
//
// Anyone is free to copy, modify, publish, use, compile, sell, or distribute this
// software, either in source code form or as a compiled binary, for any purpose,
// commercial or non-commercial, and by any means.
//
// NO WARRANTY. THE SOFTWARE IS PROVIDED "AS IS" WITHOUT WARRANTY OF ANY KIND.
//
// That said, our permacomputer's digital membrane stratum continuously runs unit,
// integration, and functional tests on all of it's own software - with our
// permacomputer monitoring itself, repairing itself, with minimal human in the
// loop guidance. Our agents do their best.
//
// Copyright 2025 TimeHexOn & foxhop & russell@unturf
// https://www.timehexon.com
// https://www.foxhop.net
// https://www.unturf.com/software
//
// unsandbox SDK for Go - Execute code in secure sandboxes
// https://unsandbox.com | https://api.unsandbox.com/openapi
//
// Library Usage:
//   // Change "package main" to "package unsandbox" to use as library
//   import "unsandbox"
//   result, err := unsandbox.Execute("python", `print("Hello")`, nil)
//   job, err := unsandbox.ExecuteAsync("python", code, nil)
//   result, err := unsandbox.Wait(job.JobID, nil)
//
// CLI Usage:
//   go run un.go script.py
//   go run un.go -s python 'print("Hello")'
//   go run un.go session --shell python3
//
// Authentication (in priority order):
//   1. Function arguments: Execute(..., &Options{PublicKey: "...", SecretKey: "..."})
//   2. Environment variables: UNSANDBOX_PUBLIC_KEY + UNSANDBOX_SECRET_KEY
//   3. Config file: ~/.unsandbox/accounts.csv (public_key,secret_key per line)

package main

import (
	"bufio"
	"bytes"
	"crypto/hmac"
	"crypto/sha256"
	"encoding/base64"
	"encoding/hex"
	"encoding/json"
	"errors"
	"flag"
	"fmt"
	"io"
	"net/http"
	"net/url"
	"os"
	"os/exec"
	"os/user"
	"path/filepath"
	"runtime"
	"strconv"
	"strings"
	"time"
)

// ============================================================================
// Configuration
// ============================================================================

const (
	// APIBase is the base URL for the unsandbox API
	APIBase = "https://api.unsandbox.com"
	// PortalBase is the base URL for the unsandbox portal
	PortalBase = "https://unsandbox.com"
	// DefaultTimeout is the default HTTP request timeout in seconds
	DefaultTimeout = 300
	// DefaultTTL is the default execution timeout in seconds
	DefaultTTL = 60
	// Version is the SDK version
	Version = "2.0.0"
)

// ANSI color codes for terminal output
const (
	Blue   = "\033[34m"
	Red    = "\033[31m"
	Green  = "\033[32m"
	Yellow = "\033[33m"
	Reset  = "\033[0m"
)

// PollDelays defines the exponential backoff delays in milliseconds
var PollDelays = []int{300, 450, 700, 900, 650, 1600, 2000}

// ExtMap maps file extensions to language names
var ExtMap = map[string]string{
	".py": "python", ".js": "javascript", ".ts": "typescript",
	".rb": "ruby", ".php": "php", ".pl": "perl", ".lua": "lua",
	".sh": "bash", ".go": "go", ".rs": "rust", ".c": "c",
	".cpp": "cpp", ".cc": "cpp", ".cxx": "cpp",
	".java": "java", ".kt": "kotlin", ".cs": "csharp", ".fs": "fsharp",
	".hs": "haskell", ".ml": "ocaml", ".clj": "clojure", ".scm": "scheme",
	".lisp": "commonlisp", ".erl": "erlang", ".ex": "elixir", ".exs": "elixir",
	".jl": "julia", ".r": "r", ".R": "r", ".cr": "crystal",
	".d": "d", ".nim": "nim", ".zig": "zig", ".v": "v",
	".dart": "dart", ".groovy": "groovy", ".scala": "scala",
	".f90": "fortran", ".f95": "fortran", ".cob": "cobol",
	".pro": "prolog", ".forth": "forth", ".4th": "forth",
	".tcl": "tcl", ".raku": "raku", ".m": "objc", ".awk": "awk",
}

// ============================================================================
// Errors
// ============================================================================

var (
	// ErrNoCredentials is returned when no API credentials are found
	ErrNoCredentials = errors.New("no credentials found: set UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY, or create ~/.unsandbox/accounts.csv, or pass credentials to function")
	// ErrAuthenticationFailed is returned when authentication fails
	ErrAuthenticationFailed = errors.New("authentication failed")
	// ErrTimestampExpired is returned when the request timestamp is expired
	ErrTimestampExpired = errors.New("request timestamp expired: your system clock may be out of sync")
	// ErrTimeout is returned when a job times out
	ErrTimeout = errors.New("job timed out")
	// ErrMaxPollsExceeded is returned when max polling attempts are exceeded
	ErrMaxPollsExceeded = errors.New("max polls exceeded")
)

// UnsandboxError represents an API error with status code and response
type UnsandboxError struct {
	Message    string
	StatusCode int
	Response   string
}

func (e *UnsandboxError) Error() string {
	return e.Message
}

// ExecutionError represents a code execution failure
type ExecutionError struct {
	Message  string
	ExitCode int
	Stderr   string
}

func (e *ExecutionError) Error() string {
	return e.Message
}

// ============================================================================
// Types
// ============================================================================

// Options contains optional parameters for API requests
type Options struct {
	PublicKey        string
	SecretKey        string
	AccountIndex     int
	Env              map[string]string
	InputFiles       []InputFile
	NetworkMode      string // "zerotrust" or "semitrusted"
	TTL              int    // Execution timeout in seconds (1-900)
	VCPU             int    // Virtual CPUs (1-8)
	ReturnArtifact   bool
	ReturnWasm       bool
	Timeout          int // HTTP request timeout in seconds
	MaxPolls         int // Maximum polling attempts for wait()
}

// InputFile represents a file to be sent with the execution request
type InputFile struct {
	Filename      string
	Content       string
	ContentBase64 string
}

// ExecuteResult represents the result of a code execution
type ExecuteResult struct {
	Success     bool        `json:"success"`
	Stdout      string      `json:"stdout"`
	Stderr      string      `json:"stderr"`
	ExitCode    int         `json:"exit_code"`
	Language    string      `json:"language"`
	JobID       string      `json:"job_id"`
	TotalTimeMs int         `json:"total_time_ms"`
	NetworkMode string      `json:"network_mode"`
	Artifacts   []Artifact  `json:"artifacts,omitempty"`
	Error       string      `json:"error,omitempty"`
}

// JobResult represents an async job status
type JobResult struct {
	JobID            string        `json:"job_id"`
	Status           string        `json:"status"`
	DetectedLanguage string        `json:"detected_language,omitempty"`
	Result           *ExecuteResult `json:"result,omitempty"`
	Error            string        `json:"error,omitempty"`
	SubmittedAt      string        `json:"submitted_at,omitempty"`
	CompletedAt      string        `json:"completed_at,omitempty"`
}

// Artifact represents a build artifact
type Artifact struct {
	Filename      string `json:"filename"`
	ContentBase64 string `json:"content_base64"`
	Size          int    `json:"size,omitempty"`
}

// LanguagesResult represents the result of languages() call
type LanguagesResult struct {
	Languages []string          `json:"languages"`
	Count     int               `json:"count"`
	Aliases   map[string]string `json:"aliases,omitempty"`
}

// ImageResult represents the result of image generation
type ImageResult struct {
	Images    []string `json:"images"`
	CreatedAt string   `json:"created_at,omitempty"`
}

// Credentials holds API key pair
type Credentials struct {
	PublicKey string
	SecretKey string
}

// ============================================================================
// HMAC Authentication
// ============================================================================

// SignRequest generates an HMAC-SHA256 signature for an API request.
// Signature = HMAC-SHA256(secretKey, "timestamp:METHOD:path:body")
func SignRequest(secretKey string, timestamp int64, method, path, body string) string {
	message := fmt.Sprintf("%d:%s:%s:%s", timestamp, method, path, body)
	h := hmac.New(sha256.New, []byte(secretKey))
	h.Write([]byte(message))
	return hex.EncodeToString(h.Sum(nil))
}

// GetCredentials retrieves API credentials in priority order:
// 1. Function arguments (publicKey, secretKey)
// 2. Environment variables (UNSANDBOX_PUBLIC_KEY, UNSANDBOX_SECRET_KEY)
// 3. Config file ~/.unsandbox/accounts.csv
func GetCredentials(publicKey, secretKey string, accountIndex int) (*Credentials, error) {
	// Priority 1: Function arguments
	if publicKey != "" && secretKey != "" {
		return &Credentials{PublicKey: publicKey, SecretKey: secretKey}, nil
	}

	// Priority 2: Environment variables
	envPk := os.Getenv("UNSANDBOX_PUBLIC_KEY")
	envSk := os.Getenv("UNSANDBOX_SECRET_KEY")
	if envPk != "" && envSk != "" {
		return &Credentials{PublicKey: envPk, SecretKey: envSk}, nil
	}

	// Priority 3: Config file
	usr, err := user.Current()
	if err == nil {
		accountsPath := filepath.Join(usr.HomeDir, ".unsandbox", "accounts.csv")
		if data, err := os.ReadFile(accountsPath); err == nil {
			var validAccounts []Credentials
			scanner := bufio.NewScanner(bytes.NewReader(data))
			for scanner.Scan() {
				line := strings.TrimSpace(scanner.Text())
				if line == "" || strings.HasPrefix(line, "#") {
					continue
				}
				parts := strings.SplitN(line, ",", 2)
				if len(parts) == 2 {
					pk := strings.TrimSpace(parts[0])
					sk := strings.TrimSpace(parts[1])
					if strings.HasPrefix(pk, "unsb-pk-") && strings.HasPrefix(sk, "unsb-sk-") {
						validAccounts = append(validAccounts, Credentials{PublicKey: pk, SecretKey: sk})
					}
				}
			}
			if len(validAccounts) > accountIndex {
				return &validAccounts[accountIndex], nil
			}
		}
	}

	return nil, ErrNoCredentials
}

// ============================================================================
// HTTP Client
// ============================================================================

// apiRequest makes an authenticated API request with HMAC signature
func apiRequest(endpoint, method string, data interface{}, contentType string, opts *Options) (map[string]interface{}, error) {
	if opts == nil {
		opts = &Options{}
	}

	creds, err := GetCredentials(opts.PublicKey, opts.SecretKey, opts.AccountIndex)
	if err != nil {
		return nil, err
	}

	urlStr := APIBase + endpoint
	var reqBody io.Reader
	bodyStr := ""

	if data != nil {
		switch v := data.(type) {
		case string:
			bodyStr = v
			reqBody = strings.NewReader(v)
		default:
			jsonData, err := json.Marshal(data)
			if err != nil {
				return nil, fmt.Errorf("error marshaling JSON: %w", err)
			}
			bodyStr = string(jsonData)
			reqBody = bytes.NewBuffer(jsonData)
		}
	}

	req, err := http.NewRequest(method, urlStr, reqBody)
	if err != nil {
		return nil, fmt.Errorf("error creating request: %w", err)
	}

	// HMAC authentication
	timestamp := time.Now().Unix()
	signature := SignRequest(creds.SecretKey, timestamp, method, endpoint, bodyStr)

	req.Header.Set("Authorization", "Bearer "+creds.PublicKey)
	req.Header.Set("X-Timestamp", strconv.FormatInt(timestamp, 10))
	req.Header.Set("X-Signature", signature)
	if contentType == "" {
		contentType = "application/json"
	}
	req.Header.Set("Content-Type", contentType)

	timeout := opts.Timeout
	if timeout <= 0 {
		timeout = DefaultTimeout
	}

	client := &http.Client{Timeout: time.Duration(timeout) * time.Second}
	resp, err := client.Do(req)
	if err != nil {
		return nil, fmt.Errorf("error making request: %w", err)
	}
	defer resp.Body.Close()

	body, err := io.ReadAll(resp.Body)
	if err != nil {
		return nil, fmt.Errorf("error reading response: %w", err)
	}

	if resp.StatusCode >= 400 {
		if resp.StatusCode == 401 && strings.Contains(strings.ToLower(string(body)), "timestamp") {
			return nil, ErrTimestampExpired
		}
		if resp.StatusCode == 401 {
			return nil, &UnsandboxError{
				Message:    fmt.Sprintf("authentication failed: %s", string(body)),
				StatusCode: resp.StatusCode,
				Response:   string(body),
			}
		}
		return nil, &UnsandboxError{
			Message:    fmt.Sprintf("HTTP %d: %s", resp.StatusCode, string(body)),
			StatusCode: resp.StatusCode,
			Response:   string(body),
		}
	}

	var result map[string]interface{}
	if len(body) > 0 {
		if err := json.Unmarshal(body, &result); err != nil {
			return nil, fmt.Errorf("error parsing response: %w", err)
		}
	}

	return result, nil
}

// ============================================================================
// Core Execution Functions
// ============================================================================

// Execute runs code synchronously and returns results.
//
// Parameters:
//   - language: Programming language (python, javascript, go, rust, etc.)
//   - code: Source code to execute
//   - opts: Optional parameters (env, inputFiles, networkMode, ttl, vcpu, etc.)
//
// Returns ExecuteResult with stdout, stderr, exit_code, etc.
//
// Example:
//   result, err := un.Execute("python", `print("Hello World")`, nil)
//   if err != nil { log.Fatal(err) }
//   fmt.Println(result.Stdout)
func Execute(language, code string, opts *Options) (*ExecuteResult, error) {
	if opts == nil {
		opts = &Options{}
	}

	payload := map[string]interface{}{
		"language":     language,
		"code":         code,
		"network_mode": getNetworkMode(opts.NetworkMode),
		"ttl":          getTTL(opts.TTL),
		"vcpu":         getVCPU(opts.VCPU),
	}

	if opts.Env != nil && len(opts.Env) > 0 {
		payload["env"] = opts.Env
	}

	if len(opts.InputFiles) > 0 {
		payload["input_files"] = processInputFiles(opts.InputFiles)
	}

	if opts.ReturnArtifact {
		payload["return_artifact"] = true
	}
	if opts.ReturnWasm {
		payload["return_wasm_artifact"] = true
	}

	result, err := apiRequest("/execute", "POST", payload, "application/json", opts)
	if err != nil {
		return nil, err
	}

	return parseExecuteResult(result), nil
}

// ExecuteAsync executes code asynchronously and returns a job ID for polling.
//
// Parameters:
//   - language: Programming language
//   - code: Source code to execute
//   - opts: Optional parameters
//
// Returns JobResult with job_id and status ("pending")
//
// Example:
//   job, err := un.ExecuteAsync("python", longRunningCode, nil)
//   fmt.Printf("Job submitted: %s\n", job.JobID)
//   result, err := un.Wait(job.JobID, nil)
func ExecuteAsync(language, code string, opts *Options) (*JobResult, error) {
	if opts == nil {
		opts = &Options{}
	}

	payload := map[string]interface{}{
		"language":     language,
		"code":         code,
		"network_mode": getNetworkMode(opts.NetworkMode),
		"ttl":          getTTL(opts.TTL),
		"vcpu":         getVCPU(opts.VCPU),
	}

	if opts.Env != nil && len(opts.Env) > 0 {
		payload["env"] = opts.Env
	}

	if len(opts.InputFiles) > 0 {
		payload["input_files"] = processInputFiles(opts.InputFiles)
	}

	if opts.ReturnArtifact {
		payload["return_artifact"] = true
	}
	if opts.ReturnWasm {
		payload["return_wasm_artifact"] = true
	}

	result, err := apiRequest("/execute/async", "POST", payload, "application/json", opts)
	if err != nil {
		return nil, err
	}

	return parseJobResult(result), nil
}

// Run executes code with automatic language detection from shebang.
//
// Parameters:
//   - code: Source code with shebang (e.g., #!/usr/bin/env python3)
//   - opts: Optional parameters
//
// Returns ExecuteResult with detected_language, stdout, stderr, etc.
//
// Example:
//   code := "#!/usr/bin/env python3\nprint('Auto-detected!')"
//   result, err := un.Run(code, nil)
//   fmt.Println(result.Language) // "python"
func Run(code string, opts *Options) (*ExecuteResult, error) {
	if opts == nil {
		opts = &Options{}
	}

	params := url.Values{}
	params.Set("ttl", strconv.Itoa(getTTL(opts.TTL)))
	params.Set("network_mode", getNetworkMode(opts.NetworkMode))

	if opts.Env != nil && len(opts.Env) > 0 {
		envJSON, _ := json.Marshal(opts.Env)
		params.Set("env", string(envJSON))
	}

	endpoint := "/run?" + params.Encode()
	result, err := apiRequest(endpoint, "POST", code, "text/plain", opts)
	if err != nil {
		return nil, err
	}

	return parseExecuteResult(result), nil
}

// RunAsync executes code asynchronously with automatic language detection.
//
// Parameters:
//   - code: Source code with shebang
//   - opts: Optional parameters
//
// Returns JobResult with job_id, detected_language, status ("pending")
func RunAsync(code string, opts *Options) (*JobResult, error) {
	if opts == nil {
		opts = &Options{}
	}

	params := url.Values{}
	params.Set("ttl", strconv.Itoa(getTTL(opts.TTL)))
	params.Set("network_mode", getNetworkMode(opts.NetworkMode))

	if opts.Env != nil && len(opts.Env) > 0 {
		envJSON, _ := json.Marshal(opts.Env)
		params.Set("env", string(envJSON))
	}

	endpoint := "/run/async?" + params.Encode()
	result, err := apiRequest(endpoint, "POST", code, "text/plain", opts)
	if err != nil {
		return nil, err
	}

	return parseJobResult(result), nil
}

// ============================================================================
// Job Management
// ============================================================================

// GetJob retrieves job status and results.
//
// Parameters:
//   - jobID: Job ID from ExecuteAsync or RunAsync
//   - opts: Optional parameters (credentials)
//
// Returns JobResult with status: pending, running, completed, failed, timeout, cancelled
func GetJob(jobID string, opts *Options) (*JobResult, error) {
	result, err := apiRequest("/jobs/"+jobID, "GET", nil, "", opts)
	if err != nil {
		return nil, err
	}
	return parseJobResult(result), nil
}

// Wait polls for job completion with exponential backoff.
//
// Parameters:
//   - jobID: Job ID from ExecuteAsync or RunAsync
//   - opts: Optional parameters (MaxPolls defaults to 100)
//
// Returns final JobResult when job completes
//
// Example:
//   job, _ := un.ExecuteAsync("python", code, nil)
//   result, err := un.Wait(job.JobID, nil)
//   fmt.Println(result.Result.Stdout)
func Wait(jobID string, opts *Options) (*JobResult, error) {
	if opts == nil {
		opts = &Options{}
	}

	maxPolls := opts.MaxPolls
	if maxPolls <= 0 {
		maxPolls = 100
	}

	terminalStates := map[string]bool{
		"completed": true,
		"failed":    true,
		"timeout":   true,
		"cancelled": true,
	}

	for i := 0; i < maxPolls; i++ {
		// Exponential backoff delay
		delayIdx := i
		if delayIdx >= len(PollDelays) {
			delayIdx = len(PollDelays) - 1
		}
		time.Sleep(time.Duration(PollDelays[delayIdx]) * time.Millisecond)

		result, err := GetJob(jobID, opts)
		if err != nil {
			return nil, err
		}

		if terminalStates[result.Status] {
			if result.Status == "failed" {
				return nil, &ExecutionError{
					Message:  fmt.Sprintf("job failed: %s", result.Error),
					ExitCode: -1,
					Stderr:   result.Error,
				}
			}
			if result.Status == "timeout" {
				return nil, fmt.Errorf("%w: %s", ErrTimeout, jobID)
			}
			return result, nil
		}
	}

	return nil, fmt.Errorf("%w: job %s after %d polls", ErrMaxPollsExceeded, jobID, maxPolls)
}

// CancelJob cancels a running job.
//
// Returns partial output and artifacts collected before cancellation.
func CancelJob(jobID string, opts *Options) (*JobResult, error) {
	result, err := apiRequest("/jobs/"+jobID, "DELETE", nil, "", opts)
	if err != nil {
		return nil, err
	}
	return parseJobResult(result), nil
}

// ListJobs returns all active jobs for this API key.
//
// Returns slice of JobResult with job_id, language, status, submitted_at
func ListJobs(opts *Options) ([]JobResult, error) {
	result, err := apiRequest("/jobs", "GET", nil, "", opts)
	if err != nil {
		return nil, err
	}

	var jobs []JobResult
	if jobsRaw, ok := result["jobs"].([]interface{}); ok {
		for _, j := range jobsRaw {
			if jMap, ok := j.(map[string]interface{}); ok {
				jobs = append(jobs, *parseJobResult(jMap))
			}
		}
	}
	return jobs, nil
}

// ============================================================================
// Image Generation
// ============================================================================

// ImageOptions contains options for image generation
type ImageOptions struct {
	PublicKey string
	SecretKey string
	Model     string
	Size      string // e.g., "1024x1024", "512x512"
	Quality   string // "standard" or "hd"
	N         int    // Number of images to generate
}

// Image generates images from a text prompt.
//
// Parameters:
//   - prompt: Text description of the image to generate
//   - opts: Optional parameters (model, size, quality, n)
//
// Example:
//   result, err := un.Image("A sunset over mountains", nil)
//   fmt.Println(result.Images[0])
func Image(prompt string, opts *ImageOptions) (*ImageResult, error) {
	if opts == nil {
		opts = &ImageOptions{}
	}

	payload := map[string]interface{}{
		"prompt": prompt,
	}

	size := opts.Size
	if size == "" {
		size = "1024x1024"
	}
	payload["size"] = size

	quality := opts.Quality
	if quality == "" {
		quality = "standard"
	}
	payload["quality"] = quality

	n := opts.N
	if n <= 0 {
		n = 1
	}
	payload["n"] = n

	if opts.Model != "" {
		payload["model"] = opts.Model
	}

	apiOpts := &Options{
		PublicKey: opts.PublicKey,
		SecretKey: opts.SecretKey,
	}

	result, err := apiRequest("/image", "POST", payload, "application/json", apiOpts)
	if err != nil {
		return nil, err
	}

	imgResult := &ImageResult{}
	if images, ok := result["images"].([]interface{}); ok {
		for _, img := range images {
			if imgStr, ok := img.(string); ok {
				imgResult.Images = append(imgResult.Images, imgStr)
			}
		}
	}
	if createdAt, ok := result["created_at"].(string); ok {
		imgResult.CreatedAt = createdAt
	}

	return imgResult, nil
}

// ============================================================================
// Utility Functions
// ============================================================================

// Languages returns the list of supported programming languages.
// Results are cached in ~/.unsandbox/languages.json for 1 hour.
//
// Parameters:
//   - forceRefresh: Bypass cache and fetch fresh data
//   - opts: Optional parameters (credentials)
func Languages(forceRefresh bool, opts *Options) (*LanguagesResult, error) {
	usr, err := user.Current()
	if err == nil && !forceRefresh {
		cachePath := filepath.Join(usr.HomeDir, ".unsandbox", "languages.json")
		if info, err := os.Stat(cachePath); err == nil {
			cacheMaxAge := time.Hour
			if time.Since(info.ModTime()) < cacheMaxAge {
				if data, err := os.ReadFile(cachePath); err == nil {
					var cached LanguagesResult
					if json.Unmarshal(data, &cached) == nil {
						return &cached, nil
					}
				}
			}
		}
	}

	result, err := apiRequest("/languages", "GET", nil, "", opts)
	if err != nil {
		return nil, err
	}

	langResult := &LanguagesResult{}
	if langs, ok := result["languages"].([]interface{}); ok {
		for _, l := range langs {
			if lStr, ok := l.(string); ok {
				langResult.Languages = append(langResult.Languages, lStr)
			}
		}
	}
	if count, ok := result["count"].(float64); ok {
		langResult.Count = int(count)
	}
	if aliases, ok := result["aliases"].(map[string]interface{}); ok {
		langResult.Aliases = make(map[string]string)
		for k, v := range aliases {
			if vStr, ok := v.(string); ok {
				langResult.Aliases[k] = vStr
			}
		}
	}

	// Save to cache
	if usr != nil {
		cacheDir := filepath.Join(usr.HomeDir, ".unsandbox")
		os.MkdirAll(cacheDir, 0755)
		cachePath := filepath.Join(cacheDir, "languages.json")
		if data, err := json.Marshal(langResult); err == nil {
			os.WriteFile(cachePath, data, 0644)
		}
	}

	return langResult, nil
}

// DetectLanguage detects programming language from file extension or shebang.
// Returns language name or empty string if undetected.
func DetectLanguage(filename string) string {
	ext := strings.ToLower(filepath.Ext(filename))
	if lang, ok := ExtMap[ext]; ok {
		return lang
	}

	// Try shebang
	data, err := os.ReadFile(filename)
	if err == nil && len(data) > 0 {
		firstLine := strings.Split(string(data), "\n")[0]
		if strings.HasPrefix(firstLine, "#!") {
			if strings.Contains(firstLine, "python") {
				return "python"
			}
			if strings.Contains(firstLine, "node") {
				return "javascript"
			}
			if strings.Contains(firstLine, "ruby") {
				return "ruby"
			}
			if strings.Contains(firstLine, "perl") {
				return "perl"
			}
			if strings.Contains(firstLine, "bash") || strings.Contains(firstLine, "/sh") {
				return "bash"
			}
			if strings.Contains(firstLine, "lua") {
				return "lua"
			}
			if strings.Contains(firstLine, "php") {
				return "php"
			}
		}
	}

	return ""
}

// ============================================================================
// Client
// ============================================================================

// Client is an API client with stored credentials.
//
// Example:
//   client, err := un.NewClient("unsb-pk-...", "unsb-sk-...")
//   result, err := client.Execute("python", `print("Hello")`, nil)
//
//   // Or load from environment/config automatically:
//   client, err := un.NewClientFromEnv()
//   result, err := client.Execute("python", code, nil)
type Client struct {
	PublicKey string
	SecretKey string
}

// NewClient creates a new Client with explicit credentials.
func NewClient(publicKey, secretKey string) (*Client, error) {
	if publicKey == "" || secretKey == "" {
		return nil, ErrNoCredentials
	}
	return &Client{
		PublicKey: publicKey,
		SecretKey: secretKey,
	}, nil
}

// NewClientFromEnv creates a new Client loading credentials from environment or config.
func NewClientFromEnv() (*Client, error) {
	creds, err := GetCredentials("", "", 0)
	if err != nil {
		return nil, err
	}
	return &Client{
		PublicKey: creds.PublicKey,
		SecretKey: creds.SecretKey,
	}, nil
}

// NewClientFromConfig creates a new Client loading credentials from config file at specified index.
func NewClientFromConfig(accountIndex int) (*Client, error) {
	creds, err := GetCredentials("", "", accountIndex)
	if err != nil {
		return nil, err
	}
	return &Client{
		PublicKey: creds.PublicKey,
		SecretKey: creds.SecretKey,
	}, nil
}

func (c *Client) opts(opts *Options) *Options {
	if opts == nil {
		opts = &Options{}
	}
	opts.PublicKey = c.PublicKey
	opts.SecretKey = c.SecretKey
	return opts
}

// Execute runs code synchronously. See package-level Execute() for details.
func (c *Client) Execute(language, code string, opts *Options) (*ExecuteResult, error) {
	return Execute(language, code, c.opts(opts))
}

// ExecuteAsync executes code asynchronously. See package-level ExecuteAsync() for details.
func (c *Client) ExecuteAsync(language, code string, opts *Options) (*JobResult, error) {
	return ExecuteAsync(language, code, c.opts(opts))
}

// Run executes with auto-detect. See package-level Run() for details.
func (c *Client) Run(code string, opts *Options) (*ExecuteResult, error) {
	return Run(code, c.opts(opts))
}

// RunAsync executes async with auto-detect. See package-level RunAsync() for details.
func (c *Client) RunAsync(code string, opts *Options) (*JobResult, error) {
	return RunAsync(code, c.opts(opts))
}

// GetJob retrieves job status. See package-level GetJob() for details.
func (c *Client) GetJob(jobID string) (*JobResult, error) {
	return GetJob(jobID, c.opts(nil))
}

// Wait polls for job completion. See package-level Wait() for details.
func (c *Client) Wait(jobID string, opts *Options) (*JobResult, error) {
	return Wait(jobID, c.opts(opts))
}

// CancelJob cancels a job. See package-level CancelJob() for details.
func (c *Client) CancelJob(jobID string) (*JobResult, error) {
	return CancelJob(jobID, c.opts(nil))
}

// ListJobs lists active jobs. See package-level ListJobs() for details.
func (c *Client) ListJobs() ([]JobResult, error) {
	return ListJobs(c.opts(nil))
}

// Image generates an image. See package-level Image() for details.
func (c *Client) Image(prompt string, opts *ImageOptions) (*ImageResult, error) {
	if opts == nil {
		opts = &ImageOptions{}
	}
	opts.PublicKey = c.PublicKey
	opts.SecretKey = c.SecretKey
	return Image(prompt, opts)
}

// Languages returns supported languages. See package-level Languages() for details.
func (c *Client) Languages(forceRefresh bool) (*LanguagesResult, error) {
	return Languages(forceRefresh, c.opts(nil))
}

// ============================================================================
// Helper Functions
// ============================================================================

func getNetworkMode(mode string) string {
	if mode == "" {
		return "zerotrust"
	}
	return mode
}

func getTTL(ttl int) int {
	if ttl <= 0 {
		return DefaultTTL
	}
	return ttl
}

func getVCPU(vcpu int) int {
	if vcpu <= 0 {
		return 1
	}
	return vcpu
}

func processInputFiles(files []InputFile) []map[string]string {
	result := make([]map[string]string, 0, len(files))
	for _, f := range files {
		entry := map[string]string{"filename": f.Filename}
		if f.ContentBase64 != "" {
			entry["content_base64"] = f.ContentBase64
		} else if f.Content != "" {
			entry["content_base64"] = base64.StdEncoding.EncodeToString([]byte(f.Content))
		}
		result = append(result, entry)
	}
	return result
}

func parseExecuteResult(m map[string]interface{}) *ExecuteResult {
	r := &ExecuteResult{}
	if v, ok := m["success"].(bool); ok {
		r.Success = v
	}
	if v, ok := m["stdout"].(string); ok {
		r.Stdout = v
	}
	if v, ok := m["stderr"].(string); ok {
		r.Stderr = v
	}
	if v, ok := m["exit_code"].(float64); ok {
		r.ExitCode = int(v)
	}
	if v, ok := m["language"].(string); ok {
		r.Language = v
	}
	if v, ok := m["detected_language"].(string); ok {
		r.Language = v
	}
	if v, ok := m["job_id"].(string); ok {
		r.JobID = v
	}
	if v, ok := m["total_time_ms"].(float64); ok {
		r.TotalTimeMs = int(v)
	}
	if v, ok := m["network_mode"].(string); ok {
		r.NetworkMode = v
	}
	if v, ok := m["error"].(string); ok {
		r.Error = v
	}
	if arts, ok := m["artifacts"].([]interface{}); ok {
		for _, a := range arts {
			if aMap, ok := a.(map[string]interface{}); ok {
				art := Artifact{}
				if fn, ok := aMap["filename"].(string); ok {
					art.Filename = fn
				}
				if cb, ok := aMap["content_base64"].(string); ok {
					art.ContentBase64 = cb
				}
				if sz, ok := aMap["size"].(float64); ok {
					art.Size = int(sz)
				}
				r.Artifacts = append(r.Artifacts, art)
			}
		}
	}
	return r
}

func parseJobResult(m map[string]interface{}) *JobResult {
	r := &JobResult{}
	if v, ok := m["job_id"].(string); ok {
		r.JobID = v
	}
	if v, ok := m["status"].(string); ok {
		r.Status = v
	}
	if v, ok := m["detected_language"].(string); ok {
		r.DetectedLanguage = v
	}
	if v, ok := m["error"].(string); ok {
		r.Error = v
	}
	if v, ok := m["submitted_at"].(string); ok {
		r.SubmittedAt = v
	}
	if v, ok := m["completed_at"].(string); ok {
		r.CompletedAt = v
	}
	if res, ok := m["result"].(map[string]interface{}); ok {
		r.Result = parseExecuteResult(res)
	}
	return r
}

// ============================================================================
// CLI Interface
// ============================================================================

const MaxEnvContentSize = 64 * 1024 // 64KB max env vault size

type envVars []string

func (e *envVars) String() string { return "" }
func (e *envVars) Set(value string) error {
	*e = append(*e, value)
	return nil
}

type inputFilesFlag []string

func (i *inputFilesFlag) String() string { return "" }
func (i *inputFilesFlag) Set(value string) error {
	*i = append(*i, value)
	return nil
}

func cliGetAPIKeys(keyArg string) (string, string) {
	creds, err := GetCredentials("", "", 0)
	if err != nil {
		// Fall back to UNSANDBOX_API_KEY for backwards compatibility
		fallbackKey := keyArg
		if fallbackKey == "" {
			fallbackKey = os.Getenv("UNSANDBOX_API_KEY")
		}
		if fallbackKey == "" {
			fmt.Fprintf(os.Stderr, "%sError: UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY not set (or UNSANDBOX_API_KEY for backwards compat)%s\n", Red, Reset)
			os.Exit(1)
		}
		return fallbackKey, fallbackKey
	}
	return creds.PublicKey, creds.SecretKey
}

func cliApiRequest(endpoint, method string, data map[string]interface{}, publicKey, secretKey string) map[string]interface{} {
	opts := &Options{PublicKey: publicKey, SecretKey: secretKey}
	result, err := apiRequest(endpoint, method, data, "application/json", opts)
	if err != nil {
		if errors.Is(err, ErrTimestampExpired) {
			fmt.Fprintf(os.Stderr, "%sError: Request timestamp expired (must be within 5 minutes of server time)%s\n", Red, Reset)
			fmt.Fprintf(os.Stderr, "%sYour computer's clock may have drifted.%s\n", Yellow, Reset)
			fmt.Fprintf(os.Stderr, "%sCheck your system time and sync with NTP if needed:%s\n", Yellow, Reset)
			fmt.Fprintf(os.Stderr, "  Linux:   sudo ntpdate -s time.nist.gov\n")
			fmt.Fprintf(os.Stderr, "  macOS:   sudo sntp -sS time.apple.com\n")
			fmt.Fprintf(os.Stderr, "  Windows: w32tm /resync\n")
		} else {
			fmt.Fprintf(os.Stderr, "%sError: %v%s\n", Red, err, Reset)
		}
		os.Exit(1)
	}
	return result
}

func cliApiRequestText(endpoint, method, body, publicKey, secretKey string) (map[string]interface{}, error) {
	opts := &Options{PublicKey: publicKey, SecretKey: secretKey}
	return apiRequest(endpoint, method, body, "text/plain", opts)
}

// ============================================================================
// Environment Secrets Vault Functions
// ============================================================================

func serviceEnvStatus(serviceID, publicKey, secretKey string) {
	result := cliApiRequest("/services/"+serviceID+"/env", "GET", nil, publicKey, secretKey)
	hasVault, _ := result["has_vault"].(bool)

	if !hasVault {
		fmt.Println("Vault exists: no")
		fmt.Println("Variable count: 0")
	} else {
		fmt.Println("Vault exists: yes")
		if count, ok := result["count"].(float64); ok {
			fmt.Printf("Variable count: %d\n", int(count))
		}
		if updatedAt, ok := result["updated_at"].(float64); ok {
			t := time.Unix(int64(updatedAt), 0)
			fmt.Printf("Last updated: %s\n", t.Format("2006-01-02 15:04:05"))
		}
	}
}

func serviceEnvSet(serviceID, envContent, publicKey, secretKey string) bool {
	if envContent == "" {
		fmt.Fprintf(os.Stderr, "%sError: No environment content provided%s\n", Red, Reset)
		return false
	}

	if len(envContent) > MaxEnvContentSize {
		fmt.Fprintf(os.Stderr, "%sError: Environment content too large (max %d bytes)%s\n", Red, MaxEnvContentSize, Reset)
		return false
	}

	result, err := cliApiRequestText("/services/"+serviceID+"/env", "PUT", envContent, publicKey, secretKey)
	if err != nil {
		fmt.Fprintf(os.Stderr, "%sError: %v%s\n", Red, err, Reset)
		return false
	}

	if count, ok := result["count"].(float64); ok {
		plural := "s"
		if int(count) == 1 {
			plural = ""
		}
		fmt.Printf("%sEnvironment vault updated: %d variable%s%s\n", Green, int(count), plural, Reset)
	} else {
		fmt.Printf("%sEnvironment vault updated%s\n", Green, Reset)
	}

	if message, ok := result["message"].(string); ok && message != "" {
		fmt.Println(message)
	}

	return true
}

func serviceEnvExport(serviceID, publicKey, secretKey string) {
	result := cliApiRequest("/services/"+serviceID+"/env/export", "POST", map[string]interface{}{}, publicKey, secretKey)
	if envContent, ok := result["env"].(string); ok && envContent != "" {
		fmt.Print(envContent)
		if !strings.HasSuffix(envContent, "\n") {
			fmt.Println()
		}
	}
}

func serviceEnvDelete(serviceID, publicKey, secretKey string) {
	cliApiRequest("/services/"+serviceID+"/env", "DELETE", nil, publicKey, secretKey)
	fmt.Printf("%sEnvironment vault deleted%s\n", Green, Reset)
}

func readEnvFile(filepath string) (string, error) {
	content, err := os.ReadFile(filepath)
	if err != nil {
		return "", err
	}
	return string(content), nil
}

func buildEnvContent(envs envVars, envFile string) string {
	var parts []string

	// Read from env file first
	if envFile != "" {
		content, err := readEnvFile(envFile)
		if err != nil {
			fmt.Fprintf(os.Stderr, "%sError: Env file not found: %s%s\n", Red, envFile, Reset)
			os.Exit(1)
		}
		parts = append(parts, content)
	}

	// Add -e flags
	for _, e := range envs {
		if strings.Contains(e, "=") {
			parts = append(parts, e)
		}
	}

	return strings.Join(parts, "\n")
}

func cmdServiceEnv(action, target string, envs envVars, envFile, publicKey, secretKey string) {
	if action == "" {
		fmt.Fprintf(os.Stderr, "%sError: env action required (status, set, export, delete)%s\n", Red, Reset)
		os.Exit(1)
	}

	if target == "" {
		fmt.Fprintf(os.Stderr, "%sError: Service ID required for env command%s\n", Red, Reset)
		os.Exit(1)
	}

	switch action {
	case "status":
		serviceEnvStatus(target, publicKey, secretKey)
	case "set":
		envContent := buildEnvContent(envs, envFile)
		if envContent == "" {
			fmt.Fprintf(os.Stderr, "%sError: No env content provided. Use -e KEY=VAL, --env-file, or pipe to stdin%s\n", Red, Reset)
			os.Exit(1)
		}
		serviceEnvSet(target, envContent, publicKey, secretKey)
	case "export":
		serviceEnvExport(target, publicKey, secretKey)
	case "delete":
		serviceEnvDelete(target, publicKey, secretKey)
	default:
		fmt.Fprintf(os.Stderr, "%sError: Unknown env action '%s'. Use: status, set, export, delete%s\n", Red, action, Reset)
		os.Exit(1)
	}
}

func cmdExecute(sourceFile string, envs envVars, files inputFilesFlag, artifacts bool, outputDir, network string, vcpu int, publicKey, secretKey string) {
	code, err := os.ReadFile(sourceFile)
	if err != nil {
		fmt.Fprintf(os.Stderr, "%sError reading file: %v%s\n", Red, err, Reset)
		os.Exit(1)
	}

	language := DetectLanguage(sourceFile)
	if language == "" {
		fmt.Fprintf(os.Stderr, "%sError: cannot detect language from extension%s\n", Red, Reset)
		os.Exit(1)
	}

	payload := map[string]interface{}{
		"language": language,
		"code":     string(code),
	}

	// Environment variables
	if len(envs) > 0 {
		envMap := make(map[string]string)
		for _, e := range envs {
			parts := strings.SplitN(e, "=", 2)
			if len(parts) == 2 {
				envMap[parts[0]] = parts[1]
			}
		}
		if len(envMap) > 0 {
			payload["env"] = envMap
		}
	}

	// Input files
	if len(files) > 0 {
		var inputFilesList []map[string]string
		for _, f := range files {
			content, err := os.ReadFile(f)
			if err != nil {
				fmt.Fprintf(os.Stderr, "%sError reading input file %s: %v%s\n", Red, f, err, Reset)
				os.Exit(1)
			}
			inputFilesList = append(inputFilesList, map[string]string{
				"filename":       filepath.Base(f),
				"content_base64": base64.StdEncoding.EncodeToString(content),
			})
		}
		payload["input_files"] = inputFilesList
	}

	if artifacts {
		payload["return_artifacts"] = true
	}
	if network != "" {
		payload["network"] = network
	}
	if vcpu > 0 {
		payload["vcpu"] = vcpu
	}

	result := cliApiRequest("/execute", "POST", payload, publicKey, secretKey)

	// Print output
	if stdout, ok := result["stdout"].(string); ok && stdout != "" {
		fmt.Printf("%s%s%s", Blue, stdout, Reset)
	}
	if stderr, ok := result["stderr"].(string); ok && stderr != "" {
		fmt.Fprintf(os.Stderr, "%s%s%s", Red, stderr, Reset)
	}

	// Save artifacts
	if artifacts {
		if artList, ok := result["artifacts"].([]interface{}); ok {
			outDir := outputDir
			if outDir == "" {
				outDir = "."
			}
			os.MkdirAll(outDir, 0755)

			for _, art := range artList {
				artMap := art.(map[string]interface{})
				filename := "artifact"
				if fn, ok := artMap["filename"].(string); ok {
					filename = fn
				}
				content, _ := base64.StdEncoding.DecodeString(artMap["content_base64"].(string))
				path := filepath.Join(outDir, filename)
				os.WriteFile(path, content, 0755)
				fmt.Fprintf(os.Stderr, "%sSaved: %s%s\n", Green, path, Reset)
			}
		}
	}

	exitCode := 0
	if ec, ok := result["exit_code"].(float64); ok {
		exitCode = int(ec)
	}
	os.Exit(exitCode)
}

func cmdSession(sessionList, sessionKill, sessionShell, sessionSnapshot, sessionRestore, sessionSnapshotName string, sessionHot bool, network string, vcpu int, tmux, screen bool, files inputFilesFlag, publicKey, secretKey string) {
	if sessionSnapshot != "" {
		payload := map[string]interface{}{}
		if sessionSnapshotName != "" {
			payload["name"] = sessionSnapshotName
		}
		if sessionHot {
			payload["hot"] = true
		}
		result := cliApiRequest("/sessions/"+sessionSnapshot+"/snapshot", "POST", payload, publicKey, secretKey)
		fmt.Printf("%sSnapshot created%s\n", Green, Reset)
		jsonData, _ := json.MarshalIndent(result, "", "  ")
		fmt.Println(string(jsonData))
		return
	}

	if sessionRestore != "" {
		result := cliApiRequest("/snapshots/"+sessionRestore+"/restore", "POST", nil, publicKey, secretKey)
		fmt.Printf("%sSession restored from snapshot%s\n", Green, Reset)
		jsonData, _ := json.MarshalIndent(result, "", "  ")
		fmt.Println(string(jsonData))
		return
	}

	if sessionList != "" {
		result := cliApiRequest("/sessions", "GET", nil, publicKey, secretKey)
		sessions := result["sessions"].([]interface{})
		if len(sessions) == 0 {
			fmt.Println("No active sessions")
		} else {
			fmt.Printf("%-40s %-10s %-10s %s\n", "ID", "Shell", "Status", "Created")
			for _, s := range sessions {
				sess := s.(map[string]interface{})
				fmt.Printf("%-40s %-10s %-10s %s\n",
					sess["id"], sess["shell"], sess["status"], sess["created_at"])
			}
		}
		return
	}

	if sessionKill != "" {
		cliApiRequest("/sessions/"+sessionKill, "DELETE", nil, publicKey, secretKey)
		fmt.Printf("%sSession terminated: %s%s\n", Green, sessionKill, Reset)
		return
	}

	// Create session
	payload := map[string]interface{}{}
	if sessionShell != "" {
		payload["shell"] = sessionShell
	} else {
		payload["shell"] = "bash"
	}
	if network != "" {
		payload["network"] = network
	}
	if vcpu > 0 {
		payload["vcpu"] = vcpu
	}
	if tmux {
		payload["persistence"] = "tmux"
	}
	if screen {
		payload["persistence"] = "screen"
	}

	// Input files
	if len(files) > 0 {
		var inputFilesList []map[string]string
		for _, f := range files {
			content, err := os.ReadFile(f)
			if err != nil {
				fmt.Fprintf(os.Stderr, "%sError reading input file %s: %v%s\n", Red, f, err, Reset)
				os.Exit(1)
			}
			inputFilesList = append(inputFilesList, map[string]string{
				"filename":       filepath.Base(f),
				"content_base64": base64.StdEncoding.EncodeToString(content),
			})
		}
		payload["input_files"] = inputFilesList
	}

	fmt.Printf("%sCreating session...%s\n", Yellow, Reset)
	result := cliApiRequest("/sessions", "POST", payload, publicKey, secretKey)
	fmt.Printf("%sSession created: %s%s\n", Green, result["id"], Reset)
}

func cmdService(serviceName, servicePorts, serviceDomains, serviceType, serviceBootstrap, serviceBootstrapFile, serviceList, serviceInfo, serviceLogs, serviceTail, serviceSleep, serviceWake, serviceDestroy, serviceResize, serviceExecute, serviceCommand, serviceDumpBootstrap, serviceDumpFile, serviceSnapshot, serviceRestore, serviceSnapshotName string, serviceHot bool, network string, vcpu int, files inputFilesFlag, envs envVars, envFile, publicKey, secretKey string) {
	if serviceSnapshot != "" {
		payload := map[string]interface{}{}
		if serviceSnapshotName != "" {
			payload["name"] = serviceSnapshotName
		}
		if serviceHot {
			payload["hot"] = true
		}
		result := cliApiRequest("/services/"+serviceSnapshot+"/snapshot", "POST", payload, publicKey, secretKey)
		fmt.Printf("%sSnapshot created%s\n", Green, Reset)
		jsonData, _ := json.MarshalIndent(result, "", "  ")
		fmt.Println(string(jsonData))
		return
	}

	if serviceRestore != "" {
		result := cliApiRequest("/snapshots/"+serviceRestore+"/restore", "POST", nil, publicKey, secretKey)
		fmt.Printf("%sService restored from snapshot%s\n", Green, Reset)
		jsonData, _ := json.MarshalIndent(result, "", "  ")
		fmt.Println(string(jsonData))
		return
	}

	if serviceList != "" {
		result := cliApiRequest("/services", "GET", nil, publicKey, secretKey)
		services := result["services"].([]interface{})
		if len(services) == 0 {
			fmt.Println("No services")
		} else {
			fmt.Printf("%-20s %-15s %-10s %-15s %s\n", "ID", "Name", "Status", "Ports", "Domains")
			for _, s := range services {
				svc := s.(map[string]interface{})
				ports := ""
				if p, ok := svc["ports"].([]interface{}); ok {
					var ps []string
					for _, port := range p {
						ps = append(ps, fmt.Sprintf("%v", port))
					}
					ports = strings.Join(ps, ",")
				}
				domains := ""
				if d, ok := svc["domains"].([]interface{}); ok {
					var ds []string
					for _, domain := range d {
						ds = append(ds, domain.(string))
					}
					domains = strings.Join(ds, ",")
				}
				fmt.Printf("%-20s %-15s %-10s %-15s %s\n",
					svc["id"], svc["name"], svc["status"], ports, domains)
			}
		}
		return
	}

	if serviceInfo != "" {
		result := cliApiRequest("/services/"+serviceInfo, "GET", nil, publicKey, secretKey)
		jsonData, _ := json.MarshalIndent(result, "", "  ")
		fmt.Println(string(jsonData))
		return
	}

	if serviceLogs != "" {
		result := cliApiRequest("/services/"+serviceLogs+"/logs", "GET", nil, publicKey, secretKey)
		fmt.Print(result["logs"])
		return
	}

	if serviceTail != "" {
		result := cliApiRequest("/services/"+serviceTail+"/logs?lines=9000", "GET", nil, publicKey, secretKey)
		fmt.Print(result["logs"])
		return
	}

	if serviceSleep != "" {
		cliApiRequest("/services/"+serviceSleep+"/freeze", "POST", nil, publicKey, secretKey)
		fmt.Printf("%sService frozen: %s%s\n", Green, serviceSleep, Reset)
		return
	}

	if serviceWake != "" {
		cliApiRequest("/services/"+serviceWake+"/unfreeze", "POST", nil, publicKey, secretKey)
		fmt.Printf("%sService unfreezing: %s%s\n", Green, serviceWake, Reset)
		return
	}

	if serviceDestroy != "" {
		cliApiRequest("/services/"+serviceDestroy, "DELETE", nil, publicKey, secretKey)
		fmt.Printf("%sService destroyed: %s%s\n", Green, serviceDestroy, Reset)
		return
	}

	if serviceResize != "" {
		if vcpu <= 0 {
			fmt.Fprintf(os.Stderr, "%sError: --resize requires -v <vcpu>%s\n", Red, Reset)
			os.Exit(1)
		}
		payload := map[string]interface{}{"vcpu": vcpu}
		cliApiRequest("/services/"+serviceResize, "PATCH", payload, publicKey, secretKey)
		fmt.Printf("%sService resized to %d vCPU, %d GB RAM%s\n", Green, vcpu, vcpu*2, Reset)
		return
	}

	if serviceExecute != "" {
		payload := map[string]interface{}{"command": serviceCommand}
		result := cliApiRequest("/services/"+serviceExecute+"/execute", "POST", payload, publicKey, secretKey)
		if stdout, ok := result["stdout"].(string); ok {
			fmt.Printf("%s%s%s", Blue, stdout, Reset)
		}
		if stderr, ok := result["stderr"].(string); ok {
			fmt.Fprintf(os.Stderr, "%s%s%s", Red, stderr, Reset)
		}
		return
	}

	if serviceDumpBootstrap != "" {
		fmt.Fprintf(os.Stderr, "Fetching bootstrap script from %s...\n", serviceDumpBootstrap)
		payload := map[string]interface{}{"command": "cat /tmp/bootstrap.sh"}
		result := cliApiRequest("/services/"+serviceDumpBootstrap+"/execute", "POST", payload, publicKey, secretKey)

		if bootstrap, ok := result["stdout"].(string); ok && bootstrap != "" {
			if serviceDumpFile != "" {
				err := os.WriteFile(serviceDumpFile, []byte(bootstrap), 0755)
				if err != nil {
					fmt.Fprintf(os.Stderr, "%sError: Could not write to %s: %v%s\n", Red, serviceDumpFile, err, Reset)
					os.Exit(1)
				}
				fmt.Printf("Bootstrap saved to %s\n", serviceDumpFile)
			} else {
				fmt.Print(bootstrap)
			}
		} else {
			fmt.Fprintf(os.Stderr, "%sError: Failed to fetch bootstrap (service not running or no bootstrap file)%s\n", Red, Reset)
			os.Exit(1)
		}
		return
	}

	// Create service
	if serviceName != "" {
		payload := map[string]interface{}{"name": serviceName}
		if servicePorts != "" {
			var ports []int
			for _, p := range strings.Split(servicePorts, ",") {
				port, _ := strconv.Atoi(strings.TrimSpace(p))
				ports = append(ports, port)
			}
			payload["ports"] = ports
		}
		if serviceDomains != "" {
			payload["domains"] = strings.Split(serviceDomains, ",")
		}
		if serviceType != "" {
			payload["service_type"] = serviceType
		}
		if serviceBootstrap != "" {
			payload["bootstrap"] = serviceBootstrap
		}
		if serviceBootstrapFile != "" {
			content, err := os.ReadFile(serviceBootstrapFile)
			if err != nil {
				fmt.Fprintf(os.Stderr, "%sError: Bootstrap file not found: %s%s\n", Red, serviceBootstrapFile, Reset)
				os.Exit(1)
			}
			payload["bootstrap_content"] = string(content)
		}
		// Input files
		if len(files) > 0 {
			var inputFilesList []map[string]string
			for _, f := range files {
				content, err := os.ReadFile(f)
				if err != nil {
					fmt.Fprintf(os.Stderr, "%sError reading input file %s: %v%s\n", Red, f, err, Reset)
					os.Exit(1)
				}
				inputFilesList = append(inputFilesList, map[string]string{
					"filename":       filepath.Base(f),
					"content_base64": base64.StdEncoding.EncodeToString(content),
				})
			}
			payload["input_files"] = inputFilesList
		}
		if network != "" {
			payload["network"] = network
		}
		if vcpu > 0 {
			payload["vcpu"] = vcpu
		}

		result := cliApiRequest("/services", "POST", payload, publicKey, secretKey)
		serviceID := result["id"].(string)
		fmt.Printf("%sService created: %s%s\n", Green, serviceID, Reset)
		fmt.Printf("Name: %s\n", result["name"])
		if url, ok := result["url"]; ok {
			fmt.Printf("URL: %s\n", url)
		}

		// Auto-set vault if -e or --env-file provided
		envContent := buildEnvContent(envs, envFile)
		if envContent != "" {
			serviceEnvSet(serviceID, envContent, publicKey, secretKey)
		}
		return
	}

	fmt.Fprintf(os.Stderr, "%sError: Specify --name to create a service, or use --list, --info, etc.%s\n", Red, Reset)
	os.Exit(1)
}

func cmdSnapshot(snapshotList, snapshotInfo, snapshotDelete, snapshotClone, snapshotType, snapshotName, snapshotShell, snapshotPorts, publicKey, secretKey string) {
	if snapshotList != "" || snapshotList == "" && snapshotInfo == "" && snapshotDelete == "" && snapshotClone == "" {
		result := cliApiRequest("/snapshots", "GET", nil, publicKey, secretKey)
		jsonData, _ := json.MarshalIndent(result, "", "  ")
		fmt.Println(string(jsonData))
		return
	}

	if snapshotInfo != "" {
		result := cliApiRequest("/snapshots/"+snapshotInfo, "GET", nil, publicKey, secretKey)
		jsonData, _ := json.MarshalIndent(result, "", "  ")
		fmt.Println(string(jsonData))
		return
	}

	if snapshotDelete != "" {
		cliApiRequest("/snapshots/"+snapshotDelete, "DELETE", nil, publicKey, secretKey)
		fmt.Printf("%sSnapshot deleted: %s%s\n", Green, snapshotDelete, Reset)
		return
	}

	if snapshotClone != "" {
		if snapshotType == "" {
			fmt.Fprintf(os.Stderr, "%sError: --type required (session or service)%s\n", Red, Reset)
			os.Exit(1)
		}
		payload := map[string]interface{}{
			"type": snapshotType,
		}
		if snapshotName != "" {
			payload["name"] = snapshotName
		}
		if snapshotShell != "" {
			payload["shell"] = snapshotShell
		}
		if snapshotPorts != "" {
			var ports []int
			for _, p := range strings.Split(snapshotPorts, ",") {
				port, _ := strconv.Atoi(strings.TrimSpace(p))
				ports = append(ports, port)
			}
			payload["ports"] = ports
		}
		result := cliApiRequest("/snapshots/"+snapshotClone+"/clone", "POST", payload, publicKey, secretKey)
		fmt.Printf("%sCreated from snapshot%s\n", Green, Reset)
		jsonData, _ := json.MarshalIndent(result, "", "  ")
		fmt.Println(string(jsonData))
		return
	}

	fmt.Fprintf(os.Stderr, "%sError: Use --list, --info ID, --delete ID, or --clone ID --type TYPE%s\n", Red, Reset)
	os.Exit(1)
}

func openBrowser(url string) error {
	var cmd *exec.Cmd
	switch runtime.GOOS {
	case "linux":
		cmd = exec.Command("xdg-open", url)
	case "darwin":
		cmd = exec.Command("open", url)
	case "windows":
		cmd = exec.Command("cmd", "/c", "start", url)
	default:
		return fmt.Errorf("unsupported platform")
	}
	return cmd.Start()
}

func formatDuration(d time.Duration) string {
	days := int(d.Hours() / 24)
	hours := int(d.Hours()) % 24
	minutes := int(d.Minutes()) % 60

	if days > 0 {
		return fmt.Sprintf("%dd %dh %dm", days, hours, minutes)
	} else if hours > 0 {
		return fmt.Sprintf("%dh %dm", hours, minutes)
	} else {
		return fmt.Sprintf("%dm", minutes)
	}
}

func validateKey(publicKey, secretKey string, extend bool) {
	opts := &Options{PublicKey: publicKey, SecretKey: secretKey}
	result, err := apiRequest("/keys/validate", "POST", nil, "application/json", &Options{
		PublicKey: publicKey,
		SecretKey: secretKey,
		Timeout:   30,
	})

	if err != nil {
		// Try portal endpoint
		url := PortalBase + "/keys/validate"
		reqBody := bytes.NewBuffer(nil)

		req, err := http.NewRequest("POST", url, reqBody)
		if err != nil {
			fmt.Fprintf(os.Stderr, "%sError creating request: %v%s\n", Red, err, Reset)
			os.Exit(1)
		}

		timestamp := time.Now().Unix()
		signature := SignRequest(secretKey, timestamp, "POST", "/keys/validate", "")

		req.Header.Set("Authorization", "Bearer "+publicKey)
		req.Header.Set("X-Timestamp", strconv.FormatInt(timestamp, 10))
		req.Header.Set("X-Signature", signature)
		req.Header.Set("Content-Type", "application/json")

		client := &http.Client{Timeout: 30 * time.Second}
		resp, err := client.Do(req)
		if err != nil {
			fmt.Fprintf(os.Stderr, "%sError making request: %v%s\n", Red, err, Reset)
			os.Exit(1)
		}
		defer resp.Body.Close()

		body, _ := io.ReadAll(resp.Body)
		if err := json.Unmarshal(body, &result); err != nil {
			fmt.Fprintf(os.Stderr, "%sError parsing response: %v%s\n", Red, err, Reset)
			os.Exit(1)
		}

		if resp.StatusCode >= 400 {
			fmt.Printf("%sInvalid%s\n", Red, Reset)
			if reason, ok := result["error"].(string); ok {
				fmt.Printf("Reason: %s\n", reason)
			}
			os.Exit(1)
		}
	}

	_ = opts // suppress unused warning

	valid, _ := result["valid"].(bool)
	expired, _ := result["expired"].(bool)
	pubKey, _ := result["public_key"].(string)
	tier, _ := result["tier"].(string)
	status, _ := result["status"].(string)

	if expired {
		fmt.Printf("%sExpired%s\n", Red, Reset)
		fmt.Printf("Public Key: %s\n", pubKey)
		fmt.Printf("Tier: %s\n", tier)
		if expiresAt, ok := result["expires_at"].(string); ok {
			fmt.Printf("Expired: %s\n", expiresAt)
		}
		fmt.Printf("%sTo renew: Visit https://unsandbox.com/keys/extend%s\n", Yellow, Reset)

		if extend {
			extendURL := PortalBase + "/keys/extend?pk=" + pubKey
			fmt.Printf("\n%sOpening browser to extend key...%s\n", Green, Reset)
			if err := openBrowser(extendURL); err != nil {
				fmt.Fprintf(os.Stderr, "%sError opening browser: %v%s\n", Red, err, Reset)
				fmt.Printf("Please visit: %s\n", extendURL)
			}
		}
		os.Exit(1)
	}

	if valid {
		fmt.Printf("%sValid%s\n", Green, Reset)
		fmt.Printf("Public Key: %s\n", pubKey)
		fmt.Printf("Tier: %s\n", tier)
		fmt.Printf("Status: %s\n", status)

		if expiresAt, ok := result["expires_at"].(string); ok {
			fmt.Printf("Expires: %s\n", expiresAt)

			expireTime, err := time.Parse(time.RFC3339, expiresAt)
			if err == nil {
				remaining := time.Until(expireTime)
				if remaining > 0 {
					fmt.Printf("Time Remaining: %s\n", formatDuration(remaining))
				}
			}
		}

		if rateLimit, ok := result["rate_limit"].(float64); ok {
			fmt.Printf("Rate Limit: %.0f req/min\n", rateLimit)
		}
		if burst, ok := result["burst"].(float64); ok {
			fmt.Printf("Burst: %.0f req\n", burst)
		}
		if concurrency, ok := result["concurrency"].(float64); ok {
			fmt.Printf("Concurrency: %.0f\n", concurrency)
		}

		if extend {
			extendURL := PortalBase + "/keys/extend?pk=" + pubKey
			fmt.Printf("\n%sOpening browser to extend key...%s\n", Green, Reset)
			if err := openBrowser(extendURL); err != nil {
				fmt.Fprintf(os.Stderr, "%sError opening browser: %v%s\n", Red, err, Reset)
				fmt.Printf("Please visit: %s\n", extendURL)
			}
		}
	} else {
		fmt.Printf("%sInvalid%s\n", Red, Reset)
		if reason, ok := result["error"].(string); ok {
			fmt.Printf("Reason: %s\n", reason)
		}
		os.Exit(1)
	}
}

func main() {
	// Common flags
	apiKey := flag.String("k", "", "API key (or set UNSANDBOX_API_KEY)")
	network := flag.String("n", "", "Network mode (zerotrust|semitrusted)")
	vcpu := flag.Int("v", 0, "vCPU count (1-8)")

	// Execute flags
	var envs envVars
	var files inputFilesFlag
	flag.Var(&envs, "e", "Environment variable (KEY=VALUE)")
	flag.Var(&files, "f", "Input file")
	artifacts := flag.Bool("a", false, "Return artifacts")
	outputDir := flag.String("o", "", "Output directory for artifacts")

	// Session flags
	sessionCmd := flag.NewFlagSet("session", flag.ExitOnError)
	sessionList := sessionCmd.String("list", "", "List active sessions")
	sessionKill := sessionCmd.String("kill", "", "Kill session by ID")
	sessionShell := sessionCmd.String("shell", "", "Shell/REPL to use")
	sessionTmux := sessionCmd.Bool("tmux", false, "Enable tmux persistence")
	sessionScreen := sessionCmd.Bool("screen", false, "Enable screen persistence")
	sessionSnapshot := sessionCmd.String("snapshot", "", "Create snapshot of session")
	sessionRestore := sessionCmd.String("restore", "", "Restore from snapshot ID")
	sessionSnapshotName := sessionCmd.String("snapshot-name", "", "Name for snapshot")
	sessionHot := sessionCmd.Bool("hot", false, "Take snapshot without freezing")
	var sessionFiles inputFilesFlag
	sessionCmd.Var(&sessionFiles, "f", "Input file")
	sessionNetwork := sessionCmd.String("n", "", "Network mode")
	sessionVcpu := sessionCmd.Int("v", 0, "vCPU count")
	sessionKey := sessionCmd.String("k", "", "API key")

	// Service flags
	serviceCmd := flag.NewFlagSet("service", flag.ExitOnError)
	serviceName := serviceCmd.String("name", "", "Service name")
	servicePorts := serviceCmd.String("ports", "", "Ports (comma-separated)")
	serviceDomains := serviceCmd.String("domains", "", "Custom domains (comma-separated)")
	serviceType := serviceCmd.String("type", "", "Service type for SRV records")
	serviceBootstrap := serviceCmd.String("bootstrap", "", "Bootstrap command or URI")
	serviceBootstrapFile := serviceCmd.String("bootstrap-file", "", "Upload local file as bootstrap script")
	var serviceFiles inputFilesFlag
	serviceCmd.Var(&serviceFiles, "f", "Input file")
	var serviceEnvs envVars
	serviceCmd.Var(&serviceEnvs, "e", "Environment variable (KEY=VALUE)")
	serviceEnvFile := serviceCmd.String("env-file", "", "Environment file (.env format)")
	serviceList := serviceCmd.String("list", "", "List services")
	serviceInfo := serviceCmd.String("info", "", "Get service info")
	serviceLogs := serviceCmd.String("logs", "", "Get service logs")
	serviceTail := serviceCmd.String("tail", "", "Get last 9000 lines")
	serviceSleep := serviceCmd.String("sleep", "", "Freeze service")
	serviceWake := serviceCmd.String("wake", "", "Unfreeze service")
	serviceDestroy := serviceCmd.String("destroy", "", "Destroy service")
	serviceResize := serviceCmd.String("resize", "", "Resize service vCPU")
	serviceExecute := serviceCmd.String("execute", "", "Execute command in service")
	serviceCommand := serviceCmd.String("command", "", "Command to execute (with -execute)")
	serviceDumpBootstrap := serviceCmd.String("dump-bootstrap", "", "Dump bootstrap script")
	serviceDumpFile := serviceCmd.String("dump-file", "", "File to save bootstrap")
	serviceSnapshot := serviceCmd.String("snapshot", "", "Create snapshot of service")
	serviceRestore := serviceCmd.String("restore", "", "Restore from snapshot ID")
	serviceSnapshotName := serviceCmd.String("snapshot-name", "", "Name for snapshot")
	serviceHot := serviceCmd.Bool("hot", false, "Take snapshot without freezing")
	serviceNetwork := serviceCmd.String("n", "", "Network mode")
	serviceVcpu := serviceCmd.Int("v", 0, "vCPU count")
	serviceKey := serviceCmd.String("k", "", "API key")

	// Snapshot flags
	snapshotCmd := flag.NewFlagSet("snapshot", flag.ExitOnError)
	snapshotList := snapshotCmd.String("list", "", "List snapshots")
	snapshotInfo := snapshotCmd.String("info", "", "Get snapshot info")
	snapshotDelete := snapshotCmd.String("delete", "", "Delete snapshot")
	snapshotClone := snapshotCmd.String("clone", "", "Clone snapshot")
	snapshotType := snapshotCmd.String("type", "", "Clone type (session/service)")
	snapshotName := snapshotCmd.String("name", "", "Name for cloned resource")
	snapshotShell := snapshotCmd.String("shell", "", "Shell for cloned session")
	snapshotPorts := snapshotCmd.String("ports", "", "Ports for cloned service")
	snapshotKey := snapshotCmd.String("k", "", "API key")

	// Key flags
	keyCmd := flag.NewFlagSet("key", flag.ExitOnError)
	keyExtend := keyCmd.Bool("extend", false, "Open browser to extend key")
	keyKey := keyCmd.String("k", "", "API key")

	// Parse
	flag.Parse()

	if len(os.Args) > 1 {
		switch os.Args[1] {
		case "session":
			sessionCmd.Parse(os.Args[2:])
			publicKey, secretKey := cliGetAPIKeys(*sessionKey)
			net := *sessionNetwork
			if net == "" {
				net = *network
			}
			vc := *sessionVcpu
			if vc == 0 {
				vc = *vcpu
			}
			cmdSession(*sessionList, *sessionKill, *sessionShell, *sessionSnapshot, *sessionRestore, *sessionSnapshotName, *sessionHot, net, vc, *sessionTmux, *sessionScreen, sessionFiles, publicKey, secretKey)
			return

		case "service":
			// Check for "service env" subcommand
			if len(os.Args) > 2 && os.Args[2] == "env" {
				envCmd := flag.NewFlagSet("service env", flag.ExitOnError)
				var envFlags envVars
				envCmd.Var(&envFlags, "e", "Environment variable (KEY=VALUE)")
				envFile := envCmd.String("env-file", "", "Environment file")
				envKey := envCmd.String("k", "", "API key")

				if len(os.Args) < 4 {
					fmt.Fprintf(os.Stderr, "%sError: env action required (status, set, export, delete)%s\n", Red, Reset)
					os.Exit(1)
				}
				action := os.Args[3]
				target := ""
				argsStart := 4
				if len(os.Args) > 4 && !strings.HasPrefix(os.Args[4], "-") {
					target = os.Args[4]
					argsStart = 5
				}
				envCmd.Parse(os.Args[argsStart:])

				publicKey, secretKey := cliGetAPIKeys(*envKey)
				cmdServiceEnv(action, target, envFlags, *envFile, publicKey, secretKey)
				return
			}

			serviceCmd.Parse(os.Args[2:])
			publicKey, secretKey := cliGetAPIKeys(*serviceKey)
			net := *serviceNetwork
			if net == "" {
				net = *network
			}
			vc := *serviceVcpu
			if vc == 0 {
				vc = *vcpu
			}
			cmdService(*serviceName, *servicePorts, *serviceDomains, *serviceType, *serviceBootstrap, *serviceBootstrapFile, *serviceList, *serviceInfo, *serviceLogs, *serviceTail, *serviceSleep, *serviceWake, *serviceDestroy, *serviceResize, *serviceExecute, *serviceCommand, *serviceDumpBootstrap, *serviceDumpFile, *serviceSnapshot, *serviceRestore, *serviceSnapshotName, *serviceHot, net, vc, serviceFiles, serviceEnvs, *serviceEnvFile, publicKey, secretKey)
			return

		case "snapshot":
			snapshotCmd.Parse(os.Args[2:])
			publicKey, secretKey := cliGetAPIKeys(*snapshotKey)
			cmdSnapshot(*snapshotList, *snapshotInfo, *snapshotDelete, *snapshotClone, *snapshotType, *snapshotName, *snapshotShell, *snapshotPorts, publicKey, secretKey)
			return

		case "key":
			keyCmd.Parse(os.Args[2:])
			publicKey, secretKey := cliGetAPIKeys(*keyKey)
			validateKey(publicKey, secretKey, *keyExtend)
			return
		}
	}

	// Execute mode
	if flag.NArg() == 0 {
		fmt.Fprintf(os.Stderr, "Usage: %s [options] <source_file>\n", os.Args[0])
		fmt.Fprintf(os.Stderr, "       %s session [options]\n", os.Args[0])
		fmt.Fprintf(os.Stderr, "       %s service [options]\n", os.Args[0])
		fmt.Fprintf(os.Stderr, "       %s snapshot [options]\n", os.Args[0])
		fmt.Fprintf(os.Stderr, "       %s key [options]\n", os.Args[0])
		os.Exit(1)
	}

	sourceFile := flag.Arg(0)
	publicKey, secretKey := cliGetAPIKeys(*apiKey)
	cmdExecute(sourceFile, envs, files, *artifacts, *outputDir, *network, *vcpu, publicKey, secretKey)
}
