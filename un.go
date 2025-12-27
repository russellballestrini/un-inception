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
	"encoding/base64"
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

func getAPIKey(keyArg string) string {
	if keyArg != "" {
		return keyArg
	}
	key := os.Getenv("UNSANDBOX_API_KEY")
	if key == "" {
		fmt.Fprintf(os.Stderr, "%sError: UNSANDBOX_API_KEY not set%s\n", Red, Reset)
		os.Exit(1)
	}
	return key
}

func apiRequest(endpoint, method string, data map[string]interface{}, apiKey string) map[string]interface{} {
	url := APIBase + endpoint
	var reqBody io.Reader

	if data != nil {
		jsonData, err := json.Marshal(data)
		if err != nil {
			fmt.Fprintf(os.Stderr, "%sError marshaling JSON: %v%s\n", Red, err, Reset)
			os.Exit(1)
		}
		reqBody = bytes.NewBuffer(jsonData)
	}

	req, err := http.NewRequest(method, url, reqBody)
	if err != nil {
		fmt.Fprintf(os.Stderr, "%sError creating request: %v%s\n", Red, err, Reset)
		os.Exit(1)
	}

	req.Header.Set("Authorization", "Bearer "+apiKey)
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
		fmt.Fprintf(os.Stderr, "%sError: HTTP %d - %s%s\n", Red, resp.StatusCode, string(body), Reset)
		os.Exit(1)
	}

	var result map[string]interface{}
	if err := json.Unmarshal(body, &result); err != nil {
		fmt.Fprintf(os.Stderr, "%sError parsing response: %v%s\n", Red, err, Reset)
		os.Exit(1)
	}

	return result
}

func cmdExecute(sourceFile string, envs envVars, files inputFiles, artifacts bool, outputDir, network string, vcpu int, apiKey string) {
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

	result := apiRequest("/execute", "POST", payload, apiKey)

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

func cmdSession(sessionList, sessionKill, sessionShell, network string, vcpu int, tmux, screen bool, apiKey string) {
	if sessionList != "" {
		result := apiRequest("/sessions", "GET", nil, apiKey)
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
		apiRequest("/sessions/"+sessionKill, "DELETE", nil, apiKey)
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

	fmt.Printf("%sCreating session...%s\n", Yellow, Reset)
	result := apiRequest("/sessions", "POST", payload, apiKey)
	fmt.Printf("%sSession created: %s%s\n", Green, result["id"], Reset)
}

func cmdService(serviceName, servicePorts, serviceDomains, serviceType, serviceBootstrap, serviceList, serviceInfo, serviceLogs, serviceTail, serviceSleep, serviceWake, serviceDestroy, serviceExecute, serviceCommand, serviceDumpBootstrap, serviceDumpFile, network string, vcpu int, apiKey string) {
	if serviceList != "" {
		result := apiRequest("/services", "GET", nil, apiKey)
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
		result := apiRequest("/services/"+serviceInfo, "GET", nil, apiKey)
		jsonData, _ := json.MarshalIndent(result, "", "  ")
		fmt.Println(string(jsonData))
		return
	}

	if serviceLogs != "" {
		result := apiRequest("/services/"+serviceLogs+"/logs", "GET", nil, apiKey)
		fmt.Print(result["logs"])
		return
	}

	if serviceTail != "" {
		result := apiRequest("/services/"+serviceTail+"/logs?lines=9000", "GET", nil, apiKey)
		fmt.Print(result["logs"])
		return
	}

	if serviceSleep != "" {
		apiRequest("/services/"+serviceSleep+"/sleep", "POST", nil, apiKey)
		fmt.Printf("%sService sleeping: %s%s\n", Green, serviceSleep, Reset)
		return
	}

	if serviceWake != "" {
		apiRequest("/services/"+serviceWake+"/wake", "POST", nil, apiKey)
		fmt.Printf("%sService waking: %s%s\n", Green, serviceWake, Reset)
		return
	}

	if serviceDestroy != "" {
		apiRequest("/services/"+serviceDestroy, "DELETE", nil, apiKey)
		fmt.Printf("%sService destroyed: %s%s\n", Green, serviceDestroy, Reset)
		return
	}

	if serviceExecute != "" {
		payload := map[string]interface{}{"command": serviceCommand}
		result := apiRequest("/services/"+serviceExecute+"/execute", "POST", payload, apiKey)
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
		result := apiRequest("/services/"+serviceDumpBootstrap+"/execute", "POST", payload, apiKey)

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
			// Check if it's a file
			if _, err := os.Stat(serviceBootstrap); err == nil {
				content, _ := os.ReadFile(serviceBootstrap)
				payload["bootstrap"] = string(content)
			} else {
				payload["bootstrap"] = serviceBootstrap
			}
		}
		if network != "" {
			payload["network"] = network
		}
		if vcpu > 0 {
			payload["vcpu"] = vcpu
		}

		result := apiRequest("/services", "POST", payload, apiKey)
		fmt.Printf("%sService created: %s%s\n", Green, result["id"], Reset)
		fmt.Printf("Name: %s\n", result["name"])
		if url, ok := result["url"]; ok {
			fmt.Printf("URL: %s\n", url)
		}
		return
	}

	fmt.Fprintf(os.Stderr, "%sError: Specify --name to create a service, or use --list, --info, etc.%s\n", Red, Reset)
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

func validateKey(apiKey string, extend bool) {
	url := PortalBase + "/keys/validate"
	reqBody := bytes.NewBuffer(nil)

	req, err := http.NewRequest("POST", url, reqBody)
	if err != nil {
		fmt.Fprintf(os.Stderr, "%sError creating request: %v%s\n", Red, err, Reset)
		os.Exit(1)
	}

	req.Header.Set("Authorization", "Bearer "+apiKey)
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
	publicKey, _ := result["public_key"].(string)
	tier, _ := result["tier"].(string)
	status, _ := result["status"].(string)

	if expired {
		// Expired key
		fmt.Printf("%sExpired%s\n", Red, Reset)
		fmt.Printf("Public Key: %s\n", publicKey)
		fmt.Printf("Tier: %s\n", tier)
		if expiresAt, ok := result["expires_at"].(string); ok {
			fmt.Printf("Expired: %s\n", expiresAt)
		}
		fmt.Printf("%sTo renew: Visit https://unsandbox.com/keys/extend%s\n", Yellow, Reset)

		if extend {
			extendURL := PortalBase + "/keys/extend?pk=" + publicKey
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
		fmt.Printf("Public Key: %s\n", publicKey)
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
			extendURL := PortalBase + "/keys/extend?pk=" + publicKey
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
	sessionNetwork := sessionCmd.String("n", "", "Network mode")
	sessionVcpu := sessionCmd.Int("v", 0, "vCPU count")
	sessionKey := sessionCmd.String("k", "", "API key")

	// Service flags
	serviceCmd := flag.NewFlagSet("service", flag.ExitOnError)
	serviceName := serviceCmd.String("name", "", "Service name")
	servicePorts := serviceCmd.String("ports", "", "Ports (comma-separated)")
	serviceDomains := serviceCmd.String("domains", "", "Custom domains (comma-separated)")
	serviceType := serviceCmd.String("type", "", "Service type for SRV records (minecraft, mumble, teamspeak, source, tcp, udp)")
	serviceBootstrap := serviceCmd.String("bootstrap", "", "Bootstrap command/file")
	serviceList := serviceCmd.String("list", "", "List services")
	serviceInfo := serviceCmd.String("info", "", "Get service info")
	serviceLogs := serviceCmd.String("logs", "", "Get service logs")
	serviceTail := serviceCmd.String("tail", "", "Get last 9000 lines")
	serviceSleep := serviceCmd.String("sleep", "", "Freeze service")
	serviceWake := serviceCmd.String("wake", "", "Unfreeze service")
	serviceDestroy := serviceCmd.String("destroy", "", "Destroy service")
	serviceExecute := serviceCmd.String("execute", "", "Execute command in service")
	serviceCommand := serviceCmd.String("command", "", "Command to execute (with -execute)")
	serviceDumpBootstrap := serviceCmd.String("dump-bootstrap", "", "Dump bootstrap script")
	serviceDumpFile := serviceCmd.String("dump-file", "", "File to save bootstrap (with -dump-bootstrap)")
	serviceNetwork := serviceCmd.String("n", "", "Network mode")
	serviceVcpu := serviceCmd.Int("v", 0, "vCPU count")
	serviceKey := serviceCmd.String("k", "", "API key")

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
			key := getAPIKey(*sessionKey)
			net := *sessionNetwork
			if net == "" {
				net = *network
			}
			vc := *sessionVcpu
			if vc == 0 {
				vc = *vcpu
			}
			cmdSession(*sessionList, *sessionKill, *sessionShell, net, vc, *sessionTmux, *sessionScreen, key)
			return

		case "service":
			serviceCmd.Parse(os.Args[2:])
			key := getAPIKey(*serviceKey)
			net := *serviceNetwork
			if net == "" {
				net = *network
			}
			vc := *serviceVcpu
			if vc == 0 {
				vc = *vcpu
			}
			cmdService(*serviceName, *servicePorts, *serviceDomains, *serviceType, *serviceBootstrap, *serviceList, *serviceInfo, *serviceLogs, *serviceTail, *serviceSleep, *serviceWake, *serviceDestroy, *serviceExecute, *serviceCommand, *serviceDumpBootstrap, *serviceDumpFile, net, vc, key)
			return

		case "key":
			keyCmd.Parse(os.Args[2:])
			key := getAPIKey(*keyKey)
			validateKey(key, *keyExtend)
			return
		}
	}

	// Execute mode
	if flag.NArg() == 0 {
		fmt.Fprintf(os.Stderr, "Usage: %s [options] <source_file>\n", os.Args[0])
		fmt.Fprintf(os.Stderr, "       %s session [options]\n", os.Args[0])
		fmt.Fprintf(os.Stderr, "       %s service [options]\n", os.Args[0])
		fmt.Fprintf(os.Stderr, "       %s key [options]\n", os.Args[0])
		os.Exit(1)
	}

	sourceFile := flag.Arg(0)
	key := getAPIKey(*apiKey)
	cmdExecute(sourceFile, envs, files, *artifacts, *outputDir, *network, *vcpu, key)
}
