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


// UN CLI - Go Implementation
// Compile: go build -o un_go un.go
// Usage:
//   un.go script.py
//   un.go -e KEY=VALUE -f data.txt script.py
//   un.go session --list
//   un.go service --name web --ports 8080 --bootstrap "python -m http.server"

package main

import (
	"bytes"
	"crypto/hmac"
	"crypto/sha256"
	"encoding/base64"
	"encoding/hex"
	"encoding/json"
	"flag"
	"fmt"
	"io"
	"net/http"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strconv"
	"strings"
	"time"
)

const (
	APIBase     = "https://api.unsandbox.com"
	PortalBase  = "https://unsandbox.com"
	Blue        = "\033[34m"
	Red         = "\033[31m"
	Green       = "\033[32m"
	Yellow      = "\033[33m"
	Reset       = "\033[0m"
)

var extMap = map[string]string{
	".py": "python", ".js": "javascript", ".ts": "typescript",
	".rb": "ruby", ".php": "php", ".pl": "perl", ".lua": "lua",
	".sh": "bash", ".go": "go", ".rs": "rust", ".c": "c",
	".cpp": "cpp", ".cc": "cpp", ".cxx": "cpp",
	".java": "java", ".kt": "kotlin", ".cs": "csharp", ".fs": "fsharp",
	".hs": "haskell", ".ml": "ocaml", ".clj": "clojure", ".scm": "scheme",
	".lisp": "commonlisp", ".erl": "erlang", ".ex": "elixir", ".exs": "elixir",
	".jl": "julia", ".r": "r", ".cr": "crystal",
	".d": "d", ".nim": "nim", ".zig": "zig", ".v": "v",
	".dart": "dart", ".groovy": "groovy", ".scala": "scala",
	".f90": "fortran", ".f95": "fortran", ".cob": "cobol",
	".pro": "prolog", ".forth": "forth", ".4th": "forth",
	".tcl": "tcl", ".raku": "raku", ".m": "objc",
}

type envVars []string

func (e *envVars) String() string { return "" }
func (e *envVars) Set(value string) error {
	*e = append(*e, value)
	return nil
}

type inputFiles []string

func (i *inputFiles) String() string { return "" }
func (i *inputFiles) Set(value string) error {
	*i = append(*i, value)
	return nil
}

func detectLanguage(filename string) (string, error) {
	ext := strings.ToLower(filepath.Ext(filename))
	if lang, ok := extMap[ext]; ok {
		return lang, nil
	}

	// Try shebang
	data, err := os.ReadFile(filename)
	if err == nil {
		firstLine := strings.Split(string(data), "\n")[0]
		if strings.HasPrefix(firstLine, "#!") {
			if strings.Contains(firstLine, "python") {
				return "python", nil
			}
			if strings.Contains(firstLine, "node") {
				return "javascript", nil
			}
			if strings.Contains(firstLine, "ruby") {
				return "ruby", nil
			}
			if strings.Contains(firstLine, "bash") || strings.Contains(firstLine, "/sh") {
				return "bash", nil
			}
		}
	}

	return "", fmt.Errorf("cannot detect language from extension")
}

func getAPIKeys(keyArg string) (string, string) {
	publicKey := os.Getenv("UNSANDBOX_PUBLIC_KEY")
	secretKey := os.Getenv("UNSANDBOX_SECRET_KEY")

	// Fall back to UNSANDBOX_API_KEY for backwards compatibility
	if publicKey == "" || secretKey == "" {
		fallbackKey := keyArg
		if fallbackKey == "" {
			fallbackKey = os.Getenv("UNSANDBOX_API_KEY")
		}
		if fallbackKey == "" {
			fmt.Fprintf(os.Stderr, "%sError: UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY not set (or UNSANDBOX_API_KEY for backwards compat)%s\n", Red, Reset)
			os.Exit(1)
		}
		// Use fallback key as both public and secret for backwards compatibility
		return fallbackKey, fallbackKey
	}

	return publicKey, secretKey
}

func computeHMAC(secretKey, timestamp, method, path, body string) string {
	message := fmt.Sprintf("%s:%s:%s:%s", timestamp, method, path, body)
	h := hmac.New(sha256.New, []byte(secretKey))
	h.Write([]byte(message))
	return hex.EncodeToString(h.Sum(nil))
}

func apiRequest(endpoint, method string, data map[string]interface{}, publicKey, secretKey string) map[string]interface{} {
	url := APIBase + endpoint
	var reqBody io.Reader
	bodyStr := ""

	if data != nil {
		jsonData, err := json.Marshal(data)
		if err != nil {
			fmt.Fprintf(os.Stderr, "%sError marshaling JSON: %v%s\n", Red, err, Reset)
			os.Exit(1)
		}
		bodyStr = string(jsonData)
		reqBody = bytes.NewBuffer(jsonData)
	}

	req, err := http.NewRequest(method, url, reqBody)
	if err != nil {
		fmt.Fprintf(os.Stderr, "%sError creating request: %v%s\n", Red, err, Reset)
		os.Exit(1)
	}

	// HMAC authentication
	timestamp := fmt.Sprintf("%d", time.Now().Unix())
	signature := computeHMAC(secretKey, timestamp, method, endpoint, bodyStr)

	req.Header.Set("Authorization", "Bearer "+publicKey)
	req.Header.Set("X-Timestamp", timestamp)
	req.Header.Set("X-Signature", signature)
	req.Header.Set("Content-Type", "application/json")

	client := &http.Client{}
	resp, err := client.Do(req)
	if err != nil {
		fmt.Fprintf(os.Stderr, "%sError making request: %v%s\n", Red, err, Reset)
		os.Exit(1)
	}
	defer resp.Body.Close()

	body, err := io.ReadAll(resp.Body)
	if err != nil {
		fmt.Fprintf(os.Stderr, "%sError reading response: %v%s\n", Red, err, Reset)
		os.Exit(1)
	}

	if resp.StatusCode >= 400 {
		if resp.StatusCode == 401 && strings.Contains(strings.ToLower(string(body)), "timestamp") {
			fmt.Fprintf(os.Stderr, "%sError: Request timestamp expired (must be within 5 minutes of server time)%s\n", Red, Reset)
			fmt.Fprintf(os.Stderr, "%sYour computer's clock may have drifted.%s\n", Yellow, Reset)
			fmt.Fprintf(os.Stderr, "%sCheck your system time and sync with NTP if needed:%s\n", Yellow, Reset)
			fmt.Fprintf(os.Stderr, "  Linux:   sudo ntpdate -s time.nist.gov\n")
			fmt.Fprintf(os.Stderr, "  macOS:   sudo sntp -sS time.apple.com\n")
			fmt.Fprintf(os.Stderr, "  Windows: w32tm /resync\n")
		} else {
			fmt.Fprintf(os.Stderr, "%sError: HTTP %d - %s%s\n", Red, resp.StatusCode, string(body), Reset)
		}
		os.Exit(1)
	}

	var result map[string]interface{}
	if err := json.Unmarshal(body, &result); err != nil {
		fmt.Fprintf(os.Stderr, "%sError parsing response: %v%s\n", Red, err, Reset)
		os.Exit(1)
	}

	return result
}

func apiRequestText(endpoint, method, body, publicKey, secretKey string) (map[string]interface{}, error) {
	url := APIBase + endpoint

	req, err := http.NewRequest(method, url, strings.NewReader(body))
	if err != nil {
		return nil, err
	}

	// HMAC authentication
	timestamp := fmt.Sprintf("%d", time.Now().Unix())
	signature := computeHMAC(secretKey, timestamp, method, endpoint, body)

	req.Header.Set("Authorization", "Bearer "+publicKey)
	req.Header.Set("X-Timestamp", timestamp)
	req.Header.Set("X-Signature", signature)
	req.Header.Set("Content-Type", "text/plain")

	client := &http.Client{}
	resp, err := client.Do(req)
	if err != nil {
		return nil, err
	}
	defer resp.Body.Close()

	respBody, err := io.ReadAll(resp.Body)
	if err != nil {
		return nil, err
	}

	if resp.StatusCode >= 400 {
		return nil, fmt.Errorf("HTTP %d - %s", resp.StatusCode, string(respBody))
	}

	var result map[string]interface{}
	if err := json.Unmarshal(respBody, &result); err != nil {
		return nil, err
	}

	return result, nil
}

// ============================================================================
// Environment Secrets Vault Functions
// ============================================================================

const MaxEnvContentSize = 64 * 1024 // 64KB max env vault size

func serviceEnvStatus(serviceID, publicKey, secretKey string) {
	result := apiRequest("/services/"+serviceID+"/env", "GET", nil, publicKey, secretKey)
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

	result, err := apiRequestText("/services/"+serviceID+"/env", "PUT", envContent, publicKey, secretKey)
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
	result := apiRequest("/services/"+serviceID+"/env/export", "POST", map[string]interface{}{}, publicKey, secretKey)
	if envContent, ok := result["env"].(string); ok && envContent != "" {
		fmt.Print(envContent)
		if !strings.HasSuffix(envContent, "\n") {
			fmt.Println()
		}
	}
}

func serviceEnvDelete(serviceID, publicKey, secretKey string) {
	apiRequest("/services/"+serviceID+"/env", "DELETE", nil, publicKey, secretKey)
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

func cmdExecute(sourceFile string, envs envVars, files inputFiles, artifacts bool, outputDir, network string, vcpu int, publicKey, secretKey string) {
	code, err := os.ReadFile(sourceFile)
	if err != nil {
		fmt.Fprintf(os.Stderr, "%sError reading file: %v%s\n", Red, err, Reset)
		os.Exit(1)
	}

	language, err := detectLanguage(sourceFile)
	if err != nil {
		fmt.Fprintf(os.Stderr, "%sError: %v%s\n", Red, err, Reset)
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

	result := apiRequest("/execute", "POST", payload, publicKey, secretKey)

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

func cmdSession(sessionList, sessionKill, sessionShell, sessionSnapshot, sessionRestore, sessionSnapshotName string, sessionHot bool, network string, vcpu int, tmux, screen bool, files inputFiles, publicKey, secretKey string) {
	if sessionSnapshot != "" {
		payload := map[string]interface{}{}
		if sessionSnapshotName != "" {
			payload["name"] = sessionSnapshotName
		}
		if sessionHot {
			payload["hot"] = true
		}
		result := apiRequest("/sessions/"+sessionSnapshot+"/snapshot", "POST", payload, publicKey, secretKey)
		fmt.Printf("%sSnapshot created%s\n", Green, Reset)
		jsonData, _ := json.MarshalIndent(result, "", "  ")
		fmt.Println(string(jsonData))
		return
	}

	if sessionRestore != "" {
		// --restore takes snapshot ID directly, calls /snapshots/:id/restore
		result := apiRequest("/snapshots/"+sessionRestore+"/restore", "POST", nil, publicKey, secretKey)
		fmt.Printf("%sSession restored from snapshot%s\n", Green, Reset)
		jsonData, _ := json.MarshalIndent(result, "", "  ")
		fmt.Println(string(jsonData))
		return
	}

	if sessionList != "" {
		result := apiRequest("/sessions", "GET", nil, publicKey, secretKey)
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
		apiRequest("/sessions/"+sessionKill, "DELETE", nil, publicKey, secretKey)
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
	result := apiRequest("/sessions", "POST", payload, publicKey, secretKey)
	fmt.Printf("%sSession created: %s%s\n", Green, result["id"], Reset)
}

func cmdService(serviceName, servicePorts, serviceDomains, serviceType, serviceBootstrap, serviceBootstrapFile, serviceList, serviceInfo, serviceLogs, serviceTail, serviceSleep, serviceWake, serviceDestroy, serviceResize, serviceExecute, serviceCommand, serviceDumpBootstrap, serviceDumpFile, serviceSnapshot, serviceRestore, serviceSnapshotName string, serviceHot bool, network string, vcpu int, files inputFiles, envs envVars, envFile, publicKey, secretKey string) {
	if serviceSnapshot != "" {
		payload := map[string]interface{}{}
		if serviceSnapshotName != "" {
			payload["name"] = serviceSnapshotName
		}
		if serviceHot {
			payload["hot"] = true
		}
		result := apiRequest("/services/"+serviceSnapshot+"/snapshot", "POST", payload, publicKey, secretKey)
		fmt.Printf("%sSnapshot created%s\n", Green, Reset)
		jsonData, _ := json.MarshalIndent(result, "", "  ")
		fmt.Println(string(jsonData))
		return
	}

	if serviceRestore != "" {
		// --restore takes snapshot ID directly, calls /snapshots/:id/restore
		result := apiRequest("/snapshots/"+serviceRestore+"/restore", "POST", nil, publicKey, secretKey)
		fmt.Printf("%sService restored from snapshot%s\n", Green, Reset)
		jsonData, _ := json.MarshalIndent(result, "", "  ")
		fmt.Println(string(jsonData))
		return
	}

	if serviceList != "" {
		result := apiRequest("/services", "GET", nil, publicKey, secretKey)
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
		result := apiRequest("/services/"+serviceInfo, "GET", nil, publicKey, secretKey)
		jsonData, _ := json.MarshalIndent(result, "", "  ")
		fmt.Println(string(jsonData))
		return
	}

	if serviceLogs != "" {
		result := apiRequest("/services/"+serviceLogs+"/logs", "GET", nil, publicKey, secretKey)
		fmt.Print(result["logs"])
		return
	}

	if serviceTail != "" {
		result := apiRequest("/services/"+serviceTail+"/logs?lines=9000", "GET", nil, publicKey, secretKey)
		fmt.Print(result["logs"])
		return
	}

	if serviceSleep != "" {
		apiRequest("/services/"+serviceSleep+"/sleep", "POST", nil, publicKey, secretKey)
		fmt.Printf("%sService sleeping: %s%s\n", Green, serviceSleep, Reset)
		return
	}

	if serviceWake != "" {
		apiRequest("/services/"+serviceWake+"/wake", "POST", nil, publicKey, secretKey)
		fmt.Printf("%sService waking: %s%s\n", Green, serviceWake, Reset)
		return
	}

	if serviceDestroy != "" {
		apiRequest("/services/"+serviceDestroy, "DELETE", nil, publicKey, secretKey)
		fmt.Printf("%sService destroyed: %s%s\n", Green, serviceDestroy, Reset)
		return
	}

	if serviceResize != "" {
		if vcpu <= 0 {
			fmt.Fprintf(os.Stderr, "%sError: --resize requires -v <vcpu>%s\n", Red, Reset)
			os.Exit(1)
		}
		payload := map[string]interface{}{"vcpu": vcpu}
		apiRequest("/services/"+serviceResize, "PATCH", payload, publicKey, secretKey)
		fmt.Printf("%sService resized to %d vCPU, %d GB RAM%s\n", Green, vcpu, vcpu*2, Reset)
		return
	}

	if serviceExecute != "" {
		payload := map[string]interface{}{"command": serviceCommand}
		result := apiRequest("/services/"+serviceExecute+"/execute", "POST", payload, publicKey, secretKey)
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
		result := apiRequest("/services/"+serviceDumpBootstrap+"/execute", "POST", payload, publicKey, secretKey)

		if bootstrap, ok := result["stdout"].(string); ok && bootstrap != "" {
			if serviceDumpFile != "" {
				// Write to file
				err := os.WriteFile(serviceDumpFile, []byte(bootstrap), 0755)
				if err != nil {
					fmt.Fprintf(os.Stderr, "%sError: Could not write to %s: %v%s\n", Red, serviceDumpFile, err, Reset)
					os.Exit(1)
				}
				fmt.Printf("Bootstrap saved to %s\n", serviceDumpFile)
			} else {
				// Print to stdout
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

		result := apiRequest("/services", "POST", payload, publicKey, secretKey)
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
		result := apiRequest("/snapshots", "GET", nil, publicKey, secretKey)
		jsonData, _ := json.MarshalIndent(result, "", "  ")
		fmt.Println(string(jsonData))
		return
	}

	if snapshotInfo != "" {
		result := apiRequest("/snapshots/"+snapshotInfo, "GET", nil, publicKey, secretKey)
		jsonData, _ := json.MarshalIndent(result, "", "  ")
		fmt.Println(string(jsonData))
		return
	}

	if snapshotDelete != "" {
		apiRequest("/snapshots/"+snapshotDelete, "DELETE", nil, publicKey, secretKey)
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
		result := apiRequest("/snapshots/"+snapshotClone+"/clone", "POST", payload, publicKey, secretKey)
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
	url := PortalBase + "/keys/validate"
	reqBody := bytes.NewBuffer(nil)

	req, err := http.NewRequest("POST", url, reqBody)
	if err != nil {
		fmt.Fprintf(os.Stderr, "%sError creating request: %v%s\n", Red, err, Reset)
		os.Exit(1)
	}

	// HMAC authentication
	timestamp := fmt.Sprintf("%d", time.Now().Unix())
	signature := computeHMAC(secretKey, timestamp, "POST", "/keys/validate", "")

	req.Header.Set("Authorization", "Bearer "+publicKey)
	req.Header.Set("X-Timestamp", timestamp)
	req.Header.Set("X-Signature", signature)
	req.Header.Set("Content-Type", "application/json")

	client := &http.Client{}
	resp, err := client.Do(req)
	if err != nil {
		fmt.Fprintf(os.Stderr, "%sError making request: %v%s\n", Red, err, Reset)
		os.Exit(1)
	}
	defer resp.Body.Close()

	body, err := io.ReadAll(resp.Body)
	if err != nil {
		fmt.Fprintf(os.Stderr, "%sError reading response: %v%s\n", Red, err, Reset)
		os.Exit(1)
	}

	var result map[string]interface{}
	if err := json.Unmarshal(body, &result); err != nil {
		fmt.Fprintf(os.Stderr, "%sError parsing response: %v%s\n", Red, err, Reset)
		os.Exit(1)
	}

	if resp.StatusCode >= 400 {
		// Invalid key
		fmt.Printf("%sInvalid%s\n", Red, Reset)
		if reason, ok := result["error"].(string); ok {
			fmt.Printf("Reason: %s\n", reason)
		} else if reason, ok := result["message"].(string); ok {
			fmt.Printf("Reason: %s\n", reason)
		}
		os.Exit(1)
	}

	valid, _ := result["valid"].(bool)
	expired, _ := result["expired"].(bool)
	pubKey, _ := result["public_key"].(string)
	tier, _ := result["tier"].(string)
	status, _ := result["status"].(string)

	if expired {
		// Expired key
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
		// Valid key
		fmt.Printf("%sValid%s\n", Green, Reset)
		fmt.Printf("Public Key: %s\n", pubKey)
		fmt.Printf("Tier: %s\n", tier)
		fmt.Printf("Status: %s\n", status)

		if expiresAt, ok := result["expires_at"].(string); ok {
			fmt.Printf("Expires: %s\n", expiresAt)

			// Calculate time remaining
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
		// Invalid key
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
	var files inputFiles
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
	var sessionFiles inputFiles
	sessionCmd.Var(&sessionFiles, "f", "Input file")
	sessionNetwork := sessionCmd.String("n", "", "Network mode")
	sessionVcpu := sessionCmd.Int("v", 0, "vCPU count")
	sessionKey := sessionCmd.String("k", "", "API key")

	// Service flags
	serviceCmd := flag.NewFlagSet("service", flag.ExitOnError)
	serviceName := serviceCmd.String("name", "", "Service name")
	servicePorts := serviceCmd.String("ports", "", "Ports (comma-separated)")
	serviceDomains := serviceCmd.String("domains", "", "Custom domains (comma-separated)")
	serviceType := serviceCmd.String("type", "", "Service type for SRV records (minecraft, mumble, teamspeak, source, tcp, udp)")
	serviceBootstrap := serviceCmd.String("bootstrap", "", "Bootstrap command or URI")
	serviceBootstrapFile := serviceCmd.String("bootstrap-file", "", "Upload local file as bootstrap script")
	var serviceFiles inputFiles
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
	serviceDumpFile := serviceCmd.String("dump-file", "", "File to save bootstrap (with -dump-bootstrap)")
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
			publicKey, secretKey := getAPIKeys(*sessionKey)
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
				// Parse env subcommand: service env <action> <service_id> [options]
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

				publicKey, secretKey := getAPIKeys(*envKey)
				cmdServiceEnv(action, target, envFlags, *envFile, publicKey, secretKey)
				return
			}

			serviceCmd.Parse(os.Args[2:])
			publicKey, secretKey := getAPIKeys(*serviceKey)
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
			publicKey, secretKey := getAPIKeys(*snapshotKey)
			cmdSnapshot(*snapshotList, *snapshotInfo, *snapshotDelete, *snapshotClone, *snapshotType, *snapshotName, *snapshotShell, *snapshotPorts, publicKey, secretKey)
			return

		case "key":
			keyCmd.Parse(os.Args[2:])
			publicKey, secretKey := getAPIKeys(*keyKey)
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
	publicKey, secretKey := getAPIKeys(*apiKey)
	cmdExecute(sourceFile, envs, files, *artifacts, *outputDir, *network, *vcpu, publicKey, secretKey)
}
