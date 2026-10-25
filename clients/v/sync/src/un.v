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


// UN CLI - V Implementation (using curl subprocess for simplicity)
// Compile: v un.v -o un_v
// Usage:
//   un_v script.py
//   un_v -e KEY=VALUE script.py
//   un_v session --list
//   un_v service --name web --ports 8080

import os
import crypto.hmac
import crypto.sha256
import net.http
import time
import json as vjson

const api_base = resolve_api_base()
const portal_base = resolve_portal_base()
const max_env_content_size = 65536
const languages_cache_ttl = 3600  // 1 hour in seconds
const blue = '\x1b[34m'
const red = '\x1b[31m'
const green = '\x1b[32m'
const yellow = '\x1b[33m'
const reset = '\x1b[0m'

// API base URL: UNSANDBOX_API_URL (trailing slash stripped), default https://api.unsandbox.com
fn resolve_api_base() string {
	v := os.getenv('UNSANDBOX_API_URL').trim_right('/')
	return if v != '' { v } else { 'https://api.unsandbox.com' }
}

// Portal base URL: UNSANDBOX_PORTAL_URL (trailing slash stripped), default https://unsandbox.com.
// Serves /logs/{source} (partner log access) and /keys/extend.
fn resolve_portal_base() string {
	v := os.getenv('UNSANDBOX_PORTAL_URL').trim_right('/')
	return if v != '' { v } else { 'https://unsandbox.com' }
}

fn detect_language(filename string) !string {
	ext := os.file_ext(filename)
	lang_map := {
		'.py':  'python'
		'.js':  'javascript'
		'.ts':  'typescript'
		'.go':  'go'
		'.rs':  'rust'
		'.c':   'c'
		'.cpp': 'cpp'
		'.d':   'd'
		'.zig': 'zig'
		'.nim': 'nim'
		'.v':   'v'
		'.rb':  'ruby'
		'.php': 'php'
		'.sh':  'bash'
	}

	if lang := lang_map[ext] {
		return lang
	}
	return error('Cannot detect language from file extension')
}

fn escape_json(s string) string {
	mut result := ''
	for c in s {
		match c {
			`"` { result += '\\"' }
			`\\` { result += '\\\\' }
			`\n` { result += '\\n' }
			`\r` { result += '\\r' }
			`\t` { result += '\\t' }
			else { result += c.ascii_str() }
		}
	}
	return result
}

fn base64_encode_file(filename string) string {
	cmd := "base64 -w0 '${filename}'"
	result := os.execute(cmd)
	return result.output.trim_space()
}

fn build_input_files_json(files []string) string {
	if files.len == 0 {
		return ''
	}
	mut entries := []string{}
	for f in files {
		basename := os.file_name(f)
		content := base64_encode_file(f)
		entries << '{"filename":"${basename}","content":"${content}"}'
	}
	return ',"input_files":[' + entries.join(',') + ']'
}

// HMAC-SHA256 over "ts:METHOD:path:body", computed in-process so our secret
// never reaches a command line (openssl -hmac would put it in argv).
fn sign_request(method string, path string, body string, secret_key string) (i64, string) {
	ts := time.now().unix()
	message := '${ts}:${method}:${path}:${body}'
	return ts, hmac.new(secret_key.bytes(), message.bytes(), sha256.sum, sha256.block_size).hex()
}

// Shell prefix setting TIMESTAMP and SIGNATURE for the curl that follows.
fn signed_shell_prefix(method string, path string, body string, secret_key string) string {
	ts, sig := sign_request(method, path, body, secret_key)
	return 'TIMESTAMP=${ts}; SIGNATURE=${sig}; '
}

fn exec_curl(cmd string) string {
	return check_curl_output(os.execute(cmd).output)
}

fn check_curl_output(output string) string {

	// Check for timestamp authentication errors
	if output.contains('timestamp') &&
		(output.contains('401') || output.contains('expired') || output.contains('invalid')) {
		eprintln('${red}Error: Request timestamp expired (must be within 5 minutes of server time)${reset}')
		eprintln('${yellow}Your computer\'s clock may have drifted.${reset}')
		eprintln('Check your system time and sync with NTP if needed:')
		eprintln('  Linux:   sudo ntpdate -s time.nist.gov')
		eprintln('  macOS:   sudo sntp -sS time.apple.com')
		eprintln('  Windows: w32tm /resync')
		exit(1)
	}

	return output
}

// Run a curl command whose request body travels through a 0600 file inside a
// fresh 0700 temp dir: cmd carries the placeholder @BODYFILE where curl's
// --data-binary @path goes. The body never passes through shell quoting
// (quotes, backslashes, newlines, $ survive intact) and never reaches an argv.
fn curl_body(cmd string, body string) os.Result {
	dir := os.join_path(os.temp_dir(), 'un_v_${os.getpid()}_${time.now().unix_nano()}')
	os.mkdir(dir, os.MkdirParams{ mode: 0o700 }) or {
		return os.Result{
			exit_code: 1
			output: 'Error: cannot create temp dir: ${err}'
		}
	}
	defer {
		os.rmdir_all(dir) or {}
	}
	path := os.join_path(dir, 'body')
	mut f := os.open_file(path, 'w', 0o600) or {
		return os.Result{
			exit_code: 1
			output: 'Error: cannot write temp file: ${err}'
		}
	}
	f.write_string(body) or {}
	f.close()
	return os.execute(cmd.replace('@BODYFILE', "@'${path}'"))
}

fn exec_curl_body(cmd string, body string) string {
	return check_curl_output(curl_body(cmd, body).output)
}

fn extract_json_string(json string, key string) string {
	search := '"${key}":"'
	start_idx := json.index(search) or { return '' }
	start := start_idx + search.len

	mut end := start
	for end < json.len {
		if json[end] == `"` && (end == 0 || json[end - 1] != `\\`) {
			break
		}
		end++
	}

	if end > start {
		raw := json[start..end]
		// Unescape JSON string
		return raw.replace('\\n', '\n')
			.replace('\\r', '\r')
			.replace('\\t', '\t')
			.replace('\\"', '"')
			.replace('\\\\', '\\')
	}
	return ''
}

fn handle_sudo_challenge(response_data string, meth string, endpoint string, body string, public_key string, secret_key string) bool {
	challenge_id := extract_json_string(response_data, 'challenge_id')

	eprintln('${yellow}Confirmation required. Check your email for a one-time code.${reset}')
	eprint('Enter OTP: ')

	// Read OTP from stdin
	mut otp := ''
	mut buf := []u8{len: 64}
	n := C.read(0, buf.data, buf.len)
	if n > 0 {
		otp = buf[..n].bytestr().trim_space()
	}

	if otp.len == 0 {
		eprintln('${red}Error: Operation cancelled${reset}')
		return false
	}

	// Build sudo headers
	mut otp_header := "-H 'X-Sudo-OTP: ${otp}'"
	mut challenge_header := ''
	if challenge_id != '' {
		challenge_header = " -H 'X-Sudo-Challenge: ${challenge_id}'"
	}

	// Retry with sudo headers
	mut cmd := ''
	if meth == 'DELETE' {
		sig_prefix := signed_shell_prefix('DELETE', endpoint, '', secret_key)
		cmd = "${sig_prefix}curl -s -o /dev/null -w '%{http_code}' -X DELETE '${api_base}${endpoint}' -H 'Authorization: Bearer ${public_key}' -H \"X-Timestamp: \$TIMESTAMP\" -H \"X-Signature: \$SIGNATURE\" ${otp_header}${challenge_header}"
	} else if meth == 'POST' {
		sig_prefix := signed_shell_prefix('POST', endpoint, body, secret_key)
		cmd = "${sig_prefix}curl -s -o /dev/null -w '%{http_code}' -X POST '${api_base}${endpoint}' -H 'Content-Type: application/json' -H 'Authorization: Bearer ${public_key}' -H \"X-Timestamp: \$TIMESTAMP\" -H \"X-Signature: \$SIGNATURE\" ${otp_header}${challenge_header} --data-binary @BODYFILE"
	} else {
		return false
	}

	result := if meth == 'POST' { curl_body(cmd, body) } else { os.execute(cmd) }
	status := result.output.trim_space().int()
	if status >= 200 && status < 300 {
		println('${green}Operation completed successfully${reset}')
		return true
	}
	eprintln('${red}Error: HTTP ${status}${reset}')
	return false
}

fn exec_curl_delete_with_sudo(endpoint string, public_key string, secret_key string) int {
	sig_prefix := signed_shell_prefix('DELETE', endpoint, '', secret_key)
	cmd := "${sig_prefix}curl -s -w '\\n%{http_code}' -X DELETE '${api_base}${endpoint}' -H 'Authorization: Bearer ${public_key}' -H \"X-Timestamp: \$TIMESTAMP\" -H \"X-Signature: \$SIGNATURE\""
	result := os.execute(cmd)
	output := result.output.trim_space()

	// Parse response body and status code
	lines := output.split('\n')
	if lines.len < 1 {
		return 500
	}

	status_line := lines[lines.len - 1]
	response_body := if lines.len > 1 { lines[..lines.len - 1].join('\n') } else { '' }

	status := status_line.int()
	if status == 428 {
		if handle_sudo_challenge(response_body, 'DELETE', endpoint, '', public_key, secret_key) {
			return 200
		}
		return 428
	}
	return status
}

fn exec_curl_post_with_sudo(endpoint string, body string, public_key string, secret_key string) int {
	sig_prefix := signed_shell_prefix('POST', endpoint, body, secret_key)
	cmd := "${sig_prefix}curl -s -w '\\n%{http_code}' -X POST '${api_base}${endpoint}' -H 'Content-Type: application/json' -H 'Authorization: Bearer ${public_key}' -H \"X-Timestamp: \$TIMESTAMP\" -H \"X-Signature: \$SIGNATURE\" --data-binary @BODYFILE"
	result := curl_body(cmd, body)
	output := result.output.trim_space()

	// Parse response body and status code
	lines := output.split('\n')
	if lines.len < 1 {
		return 500
	}

	status_line := lines[lines.len - 1]
	response_body := if lines.len > 1 { lines[..lines.len - 1].join('\n') } else { '' }

	status := status_line.int()
	if status == 428 {
		if handle_sudo_challenge(response_body, 'POST', endpoint, body, public_key, secret_key) {
			return 200
		}
		return 428
	}
	return status
}

fn read_env_file(filename string) string {
	content := os.read_file(filename) or {
		eprintln('${red}Error: Cannot read env file: ${filename}${reset}')
		return ''
	}
	return content
}

fn build_env_content(envs []string, env_file string) string {
	mut result := ''

	// Add -e flags
	for env in envs {
		result += env + '\n'
	}

	// Add content from env file
	if env_file != '' {
		file_content := read_env_file(env_file)
		for line in file_content.split('\n') {
			trimmed := line.trim_space()
			if trimmed.len == 0 || trimmed.starts_with('#') {
				continue
			}
			result += trimmed + '\n'
		}
	}

	return result
}

fn exec_curl_put(endpoint string, body string, public_key string, secret_key string) bool {
	sig_prefix := signed_shell_prefix('PUT', endpoint, body, secret_key)
	cmd := "${sig_prefix}curl -s -X PUT '${api_base}${endpoint}' -H 'Content-Type: text/plain' -H 'Authorization: Bearer ${public_key}' -H \"X-Timestamp: \$TIMESTAMP\" -H \"X-Signature: \$SIGNATURE\" --data-binary @BODYFILE"
	result := curl_body(cmd, body)
	return result.exit_code == 0
}

fn service_env_set(service_id string, content string, public_key string, secret_key string) bool {
	endpoint := '/services/${service_id}/env'
	return exec_curl_put(endpoint, content, public_key, secret_key)
}

fn cmd_service_env(action string, target string, svc_envs []string, svc_env_file string, api_key string) {
	pub_key := get_public_key()
	secret_key := get_secret_key()

	match action {
		'status' {
			sig_prefix := signed_shell_prefix('GET', '/services/${target}/env', '', secret_key)
			cmd := "${sig_prefix}curl -s -X GET '${api_base}/services/${target}/env' -H 'Authorization: Bearer ${pub_key}' -H \"X-Timestamp: \$TIMESTAMP\" -H \"X-Signature: \$SIGNATURE\""
			println(exec_curl(cmd))
		}
		'set' {
			if svc_envs.len == 0 && svc_env_file == '' {
				eprintln('${red}Error: No environment variables specified. Use -e KEY=VALUE or --env-file FILE${reset}')
				return
			}
			content := build_env_content(svc_envs, svc_env_file)
			if content.len > max_env_content_size {
				eprintln('${red}Error: Environment content exceeds 64KB limit${reset}')
				return
			}
			if service_env_set(target, content, pub_key, secret_key) {
				println('${green}Vault updated for service ${target}${reset}')
			}
		}
		'export' {
			sig_prefix := signed_shell_prefix('POST', '/services/${target}/env/export', '', secret_key)
			cmd := "${sig_prefix}curl -s -X POST '${api_base}/services/${target}/env/export' -H 'Authorization: Bearer ${pub_key}' -H \"X-Timestamp: \$TIMESTAMP\" -H \"X-Signature: \$SIGNATURE\""
			println(exec_curl(cmd))
		}
		'delete' {
			sig_prefix := signed_shell_prefix('DELETE', '/services/${target}/env', '', secret_key)
			cmd := "${sig_prefix}curl -s -X DELETE '${api_base}/services/${target}/env' -H 'Authorization: Bearer ${pub_key}' -H \"X-Timestamp: \$TIMESTAMP\" -H \"X-Signature: \$SIGNATURE\""
			exec_curl(cmd)
			println('${green}Vault deleted for service ${target}${reset}')
		}
		else {
			eprintln('${red}Error: Unknown env action: ${action}${reset}')
			eprintln('Usage: un service env <status|set|export|delete> <service_id>')
		}
	}
}

// Bare (unquoted) JSON value for key, e.g. a number: "" when absent or null.
fn extract_json_bare(json string, key string) string {
	search := '"${key}":'
	start_idx := json.index(search) or { return '' }
	mut start := start_idx + search.len
	for start < json.len && json[start] == ` ` {
		start++
	}
	mut end := start
	for end < json.len && json[end] != `,` && json[end] != `}` && json[end] != ` ` {
		end++
	}
	v := json[start..end]
	return if v == 'null' { '' } else { v.trim('"') }
}

// Display per CLI_SPEC.md "`un key` output": the API returns no public key, so
// we print ours; expiry derives from valid_through (ms since epoch).
fn cmd_key(extend bool, api_key string) {
	pub_key := get_public_key()
	secret_key := get_secret_key()

	if extend {
		url := '${portal_base}/keys/extend?pk=${pub_key}'
		println('${blue}Opening browser to extend key...${reset}')
		if os.execute('xdg-open "${url}"').exit_code != 0 {
			if os.execute('open "${url}"').exit_code != 0 {
				eprintln('${red}Error: Could not open browser${reset}')
			}
		}
		return
	}

	sig_prefix := signed_shell_prefix('POST', '/keys/validate', '', secret_key)
	cmd := "${sig_prefix}curl -s -w '\\n%{http_code}' -X POST '${api_base}/keys/validate' -H 'Authorization: Bearer ${pub_key}' -H \"X-Timestamp: \$TIMESTAMP\" -H \"X-Signature: \$SIGNATURE\""
	res := os.execute(cmd)
	nl := res.output.last_index('\n') or { -1 }
	if res.exit_code != 0 || nl < 0 {
		eprintln('${red}Error: request to ${api_base}/keys/validate failed${reset}')
		exit(1)
	}
	code := res.output[nl + 1..].trim_space()
	// Tolerate pretty-printed JSON ("key": value) in the field helpers.
	result := res.output[..nl].replace('": ', '":')

	mut state := 'invalid'
	mut reason := ''
	if code == '200' {
		st := extract_json_string(result, 'status')
		state = if st != '' {
			st
		} else if extract_json_bare(result, 'valid') == 'true' {
			'active'
		} else {
			'invalid'
		}
		reason = 'status ${state}'
	} else if code == '401' {
		reason = extract_json_string(result, 'error')
		if reason == '' {
			reason = 'unauthorized'
		}
		state = match reason {
			':expired', 'expired' { 'expired' }
			':suspended', 'suspended' { 'suspended' }
			else { 'invalid' }
		}
	} else {
		eprintln('${red}Error: HTTP ${code} ${extract_json_string(result, 'error')}${reset}')
		exit(4)
	}

	match state {
		'active' {}
		'expired' {
			println('${red}Expired${reset}')
			println('Public Key: ${pub_key}')
			println('${yellow}To renew: Visit ${portal_base}/keys/extend${reset}')
			exit(3)
		}
		'suspended' {
			println('${red}Suspended${reset}')
			println('Public Key: ${pub_key}')
			exit(3)
		}
		else {
			println('${red}Invalid${reset}')
			println('Public Key: ${pub_key}')
			println('Reason: ${reason}')
			exit(3)
		}
	}

	mut tier := extract_json_bare(result, 'tier')
	if tier == '' {
		tier = 'N/A'
	}
	mut status := extract_json_string(result, 'status')
	if status == '' {
		status = 'N/A'
	}
	mut expires := 'N/A'
	mut remaining := 'N/A'
	vt_str := extract_json_bare(result, 'valid_through')
	if vt_str != '' {
		secs := i64(vt_str.f64()) / 1000
		t := time.unix(secs)
		expires = '${t.year:04d}-${t.month:02d}-${t.day:02d}T${t.hour:02d}:${t.minute:02d}:${t.second:02d}Z'
		left := secs - time.now().unix()
		remaining = if left <= 0 {
			'expired'
		} else {
			'${left / 86400}d ${left % 86400 / 3600}h ${left % 3600 / 60}m'
		}
	}

	println('${green}Valid${reset}')
	println('Public Key: ${pub_key}')
	println('Tier: ${tier}')
	println('Status: ${status}')
	println('Expires: ${expires}')
	println('Time Remaining: ${remaining}')
}

fn cmd_execute(source_file string, envs []string, artifacts bool, network string, vcpu int, api_key string) {
	lang := detect_language(source_file) or {
		eprintln('${red}Error: ${err}${reset}')
		exit(1)
	}

	code := os.read_file(source_file) or {
		eprintln('${red}Error reading file: ${err}${reset}')
		exit(1)
	}

	mut json := '{"language":"${lang}","code":"${escape_json(code)}"'

	if envs.len > 0 {
		json += ',"env":{'
		for i, e in envs {
			parts := e.split_nth('=', 2)
			if parts.len == 2 {
				if i > 0 {
					json += ','
				}
				json += '"${parts[0]}":"${escape_json(parts[1])}"'
			}
		}
		json += '}'
	}

	if artifacts {
		json += ',"return_artifacts":true'
	}
	if network != '' {
		json += ',"network":"${network}"'
	}
	if vcpu > 0 {
		json += ',"vcpu":${vcpu}'
	}
	json += '}'

	pub_key := get_public_key()
	secret_key := get_secret_key()
	sig_prefix := signed_shell_prefix('POST', '/execute', json, secret_key)
	cmd := "${sig_prefix}curl -s -X POST '${api_base}/execute' -H 'Content-Type: application/json' -H 'Authorization: Bearer ${pub_key}' -H \"X-Timestamp: \$TIMESTAMP\" -H \"X-Signature: \$SIGNATURE\" --data-binary @BODYFILE"
	println(exec_curl_body(cmd, json))
}

fn cmd_session(list bool, kill string, shell string, network string, vcpu int, tmux bool, screen bool, input_files []string, api_key string) {
	pub_key := get_public_key()
	secret_key := get_secret_key()

	if list {
		sig_prefix := signed_shell_prefix('GET', '/sessions', '', secret_key)
		cmd := "${sig_prefix}curl -s -X GET '${api_base}/sessions' -H 'Authorization: Bearer ${pub_key}' -H \"X-Timestamp: \$TIMESTAMP\" -H \"X-Signature: \$SIGNATURE\""
		println(exec_curl(cmd))
		return
	}

	if kill != '' {
		sig_prefix := signed_shell_prefix('DELETE', '/sessions/${kill}', '', secret_key)
		cmd := "${sig_prefix}curl -s -X DELETE '${api_base}/sessions/${kill}' -H 'Authorization: Bearer ${pub_key}' -H \"X-Timestamp: \$TIMESTAMP\" -H \"X-Signature: \$SIGNATURE\""
		exec_curl(cmd)
		println('${green}Session terminated: ${kill}${reset}')
		return
	}

	sh := if shell != '' { shell } else { 'bash' }
	mut json := '{"shell":"${sh}"'
	if network != '' {
		json += ',"network":"${network}"'
	}
	if vcpu > 0 {
		json += ',"vcpu":${vcpu}'
	}
	if tmux {
		json += ',"persistence":"tmux"'
	}
	if screen {
		json += ',"persistence":"screen"'
	}
	json += build_input_files_json(input_files)
	json += '}'

	println('${yellow}Creating session...${reset}')
	sig_prefix := signed_shell_prefix('POST', '/sessions', json, secret_key)
	cmd := "${sig_prefix}curl -s -X POST '${api_base}/sessions' -H 'Content-Type: application/json' -H 'Authorization: Bearer ${pub_key}' -H \"X-Timestamp: \$TIMESTAMP\" -H \"X-Signature: \$SIGNATURE\" --data-binary @BODYFILE"
	println(exec_curl_body(cmd, json))
}

fn cmd_service(name string, ports string, service_type string, bootstrap string, bootstrap_file string, list bool, info string, logs string, tail string, sleep string, wake string, destroy string, resize string, execute string, command string, dump_bootstrap string, dump_file string, network string, vcpu int, input_files []string, svc_envs []string, svc_env_file string, unfreeze_on_demand bool, set_unfreeze_on_demand_id string, set_unfreeze_on_demand_enabled string, api_key string) {
	pub_key := get_public_key()
	secret_key := get_secret_key()

	if list {
		sig_prefix := signed_shell_prefix('GET', '/services', '', secret_key)
		cmd := "${sig_prefix}curl -s -X GET '${api_base}/services' -H 'Authorization: Bearer ${pub_key}' -H \"X-Timestamp: \$TIMESTAMP\" -H \"X-Signature: \$SIGNATURE\""
		println(exec_curl(cmd))
		return
	}

	if info != '' {
		sig_prefix := signed_shell_prefix('GET', '/services/${info}', '', secret_key)
		cmd := "${sig_prefix}curl -s -X GET '${api_base}/services/${info}' -H 'Authorization: Bearer ${pub_key}' -H \"X-Timestamp: \$TIMESTAMP\" -H \"X-Signature: \$SIGNATURE\""
		println(exec_curl(cmd))
		return
	}

	if logs != '' {
		sig_prefix := signed_shell_prefix('GET', '/services/${logs}/logs', '', secret_key)
		cmd := "${sig_prefix}curl -s -X GET '${api_base}/services/${logs}/logs' -H 'Authorization: Bearer ${pub_key}' -H \"X-Timestamp: \$TIMESTAMP\" -H \"X-Signature: \$SIGNATURE\""
		print(exec_curl(cmd))
		return
	}

	if tail != '' {
		sig_prefix := signed_shell_prefix('GET', '/services/${tail}/logs', '', secret_key)
		cmd := "${sig_prefix}curl -s -X GET '${api_base}/services/${tail}/logs?lines=9000' -H 'Authorization: Bearer ${pub_key}' -H \"X-Timestamp: \$TIMESTAMP\" -H \"X-Signature: \$SIGNATURE\""
		print(exec_curl(cmd))
		return
	}

	if sleep != '' {
		sig_prefix := signed_shell_prefix('POST', '/services/${sleep}/freeze', '', secret_key)
		cmd := "${sig_prefix}curl -s -X POST '${api_base}/services/${sleep}/freeze' -H 'Authorization: Bearer ${pub_key}' -H \"X-Timestamp: \$TIMESTAMP\" -H \"X-Signature: \$SIGNATURE\""
		exec_curl(cmd)
		println('${green}Service frozen: ${sleep}${reset}')
		return
	}

	if wake != '' {
		sig_prefix := signed_shell_prefix('POST', '/services/${wake}/unfreeze', '', secret_key)
		cmd := "${sig_prefix}curl -s -X POST '${api_base}/services/${wake}/unfreeze' -H 'Authorization: Bearer ${pub_key}' -H \"X-Timestamp: \$TIMESTAMP\" -H \"X-Signature: \$SIGNATURE\""
		exec_curl(cmd)
		println('${green}Service unfreezing: ${wake}${reset}')
		return
	}

	if destroy != '' {
		endpoint := '/services/${destroy}'
		status := exec_curl_delete_with_sudo(endpoint, pub_key, secret_key)
		if status >= 200 && status < 300 {
			println('${green}Service destroyed: ${destroy}${reset}')
		} else if status != 428 {
			eprintln('${red}Error: Failed to destroy service${reset}')
			exit(1)
		}
		return
	}

	if resize != '' {
		if vcpu < 1 || vcpu > 8 {
			eprintln('${red}Error: --resize requires --vcpu N (1-8)${reset}')
			exit(1)
		}
		json := '{"vcpu":${vcpu}}'
		sig_prefix := signed_shell_prefix('PATCH', '/services/${resize}', json, secret_key)
		cmd := "${sig_prefix}curl -s -X PATCH '${api_base}/services/${resize}' -H 'Content-Type: application/json' -H 'Authorization: Bearer ${pub_key}' -H \"X-Timestamp: \$TIMESTAMP\" -H \"X-Signature: \$SIGNATURE\" --data-binary @BODYFILE"
		exec_curl_body(cmd, json)
		ram := vcpu * 2
		println('${green}Service resized to ${vcpu} vCPU, ${ram} GB RAM${reset}')
		return
	}

	if execute != '' {
		json := '{"command":"${escape_json(command)}"}'
		sig_prefix := signed_shell_prefix('POST', '/services/${execute}/execute', json, secret_key)
		cmd := "${sig_prefix}curl -s -X POST '${api_base}/services/${execute}/execute' -H 'Content-Type: application/json' -H 'Authorization: Bearer ${pub_key}' -H \"X-Timestamp: \$TIMESTAMP\" -H \"X-Signature: \$SIGNATURE\" --data-binary @BODYFILE"
		result := exec_curl_body(cmd, json)

		stdout_str := extract_json_string(result, 'stdout')
		stderr_str := extract_json_string(result, 'stderr')
		if stdout_str != '' {
			print(stdout_str)
		}
		if stderr_str != '' {
			eprint(stderr_str)
		}
		return
	}

	// Handle set_unfreeze_on_demand
	if set_unfreeze_on_demand_id != '' {
		enabled := set_unfreeze_on_demand_enabled == 'true' || set_unfreeze_on_demand_enabled == '1'
		enabled_str := if enabled { 'true' } else { 'false' }
		json := '{"unfreeze_on_demand":${enabled_str}}'
		sig_prefix := signed_shell_prefix('PATCH', '/services/${set_unfreeze_on_demand_id}', json, secret_key)
		cmd := "${sig_prefix}curl -s -X PATCH '${api_base}/services/${set_unfreeze_on_demand_id}' -H 'Content-Type: application/json' -H 'Authorization: Bearer ${pub_key}' -H \"X-Timestamp: \$TIMESTAMP\" -H \"X-Signature: \$SIGNATURE\" --data-binary @BODYFILE"
		exec_curl_body(cmd, json)
		if enabled {
			println('${green}Unfreeze-on-demand enabled for service ${set_unfreeze_on_demand_id}${reset}')
		} else {
			println('${green}Unfreeze-on-demand disabled for service ${set_unfreeze_on_demand_id}${reset}')
		}
		return
	}

	if dump_bootstrap != '' {
		eprintln('Fetching bootstrap script from ${dump_bootstrap}...')
		json := '{"command":"cat /tmp/bootstrap.sh"}'
		sig_prefix := signed_shell_prefix('POST', '/services/${dump_bootstrap}/execute', json, secret_key)
		cmd := "${sig_prefix}curl -s -X POST '${api_base}/services/${dump_bootstrap}/execute' -H 'Content-Type: application/json' -H 'Authorization: Bearer ${pub_key}' -H \"X-Timestamp: \$TIMESTAMP\" -H \"X-Signature: \$SIGNATURE\" --data-binary @BODYFILE"
		result := exec_curl_body(cmd, json)

		bootstrap_script := extract_json_string(result, 'stdout')
		if bootstrap_script != '' {
			if dump_file != '' {
				os.write_file(dump_file, bootstrap_script) or {
					eprintln('${red}Error: Could not write to ${dump_file}: ${err}${reset}')
					exit(1)
				}
				os.chmod(dump_file, 0o755) or {}
				println('Bootstrap saved to ${dump_file}')
			} else {
				print(bootstrap_script)
			}
		} else {
			eprintln('${red}Error: Failed to fetch bootstrap (service not running or no bootstrap file)${reset}')
			exit(1)
		}
		return
	}

	if name != '' {
		mut json := '{"name":"${name}"'
		if ports != '' {
			json += ',"ports":[${ports}]'
		}
		if service_type != '' {
			json += ',"service_type":"${service_type}"'
		}
		if bootstrap != '' {
			json += ',"bootstrap":"${escape_json(bootstrap)}"'
		}
		if bootstrap_file != '' {
			if os.exists(bootstrap_file) {
				boot_code := os.read_file(bootstrap_file) or {
					eprintln('${red}Error: Could not read bootstrap file: ${bootstrap_file}${reset}')
					exit(1)
				}
				json += ',"bootstrap_content":"${escape_json(boot_code)}"'
			} else {
				eprintln('${red}Error: Bootstrap file not found: ${bootstrap_file}${reset}')
				exit(1)
			}
		}
		if network != '' {
			json += ',"network":"${network}"'
		}
		if vcpu > 0 {
			json += ',"vcpu":${vcpu}'
		}
		if unfreeze_on_demand {
			json += ',"unfreeze_on_demand":true'
		}
		json += build_input_files_json(input_files)
		json += '}'

		println('${yellow}Creating service...${reset}')
		sig_prefix := signed_shell_prefix('POST', '/services', json, secret_key)
		cmd := "${sig_prefix}curl -s -X POST '${api_base}/services' -H 'Content-Type: application/json' -H 'Authorization: Bearer ${pub_key}' -H \"X-Timestamp: \$TIMESTAMP\" -H \"X-Signature: \$SIGNATURE\" --data-binary @BODYFILE"
		result := exec_curl_body(cmd, json)
		println(result)

		// Auto-set vault if -e or --env-file provided
		if svc_envs.len > 0 || svc_env_file != '' {
			service_id := extract_json_string(result, 'service_id')
			if service_id != '' {
				env_content := build_env_content(svc_envs, svc_env_file)
				if env_content.len > 0 {
					if service_env_set(service_id, env_content, pub_key, secret_key) {
						println('${green}Vault configured for service ${service_id}${reset}')
					}
				}
			}
		}
		return
	}

	eprintln('${red}Error: Specify --name to create a service${reset}')
	exit(1)
}

// Load a row from an accounts.csv file (format: public_key,secret_key per line).
// Lines starting with '#' and blank lines are skipped. Returns the Nth data row.
fn load_accounts_csv(path string, index int) ?[]string {
	content := os.read_file(path) or { return none }
	lines := content.split('\n')
	mut row := 0
	for raw_line in lines {
		line := raw_line.trim_space()
		if line == '' || line.starts_with('#') {
			continue
		}
		if row == index {
			fields := line.split(',')
			if fields.len < 2 {
				return none
			}
			// Extra columns (e.g. a label) follow the secret
			return [fields[0].trim_space(), fields[1].trim_space()]
		}
		row++
	}
	return none
}

fn get_public_key() string {
	pub_key := os.getenv('UNSANDBOX_PUBLIC_KEY')
	if pub_key != '' {
		return pub_key
	}
	api_key := os.getenv('UNSANDBOX_API_KEY')
	if api_key != '' {
		return api_key
	}
	eprintln('${red}Error: UNSANDBOX_PUBLIC_KEY or UNSANDBOX_API_KEY environment variable not set${reset}')
	exit(1)
}

fn get_secret_key() string {
	sec_key := os.getenv('UNSANDBOX_SECRET_KEY')
	if sec_key != '' {
		return sec_key
	}
	api_key := os.getenv('UNSANDBOX_API_KEY')
	if api_key != '' {
		return api_key
	}
	return ''
}

fn cmd_snapshot(list bool, info string, session string, service string, restore string, delete string, lock_id string, unlock_id string, clone string, clone_type string, name string, ports string, hot bool, api_key string) {
	pub_key := get_public_key()
	secret_key := get_secret_key()

	if list {
		sig_prefix := signed_shell_prefix('GET', '/snapshots', '', secret_key)
		cmd := "${sig_prefix}curl -s -X GET '${api_base}/snapshots' -H 'Authorization: Bearer ${pub_key}' -H \"X-Timestamp: \$TIMESTAMP\" -H \"X-Signature: \$SIGNATURE\""
		println(exec_curl(cmd))
		return
	}

	if info != '' {
		sig_prefix := signed_shell_prefix('GET', '/snapshots/${info}', '', secret_key)
		cmd := "${sig_prefix}curl -s -X GET '${api_base}/snapshots/${info}' -H 'Authorization: Bearer ${pub_key}' -H \"X-Timestamp: \$TIMESTAMP\" -H \"X-Signature: \$SIGNATURE\""
		println(exec_curl(cmd))
		return
	}

	if session != '' {
		mut json := '{'
		mut has_content := false
		if name != '' {
			json += '"name":"${name}"'
			has_content = true
		}
		if hot {
			if has_content { json += ',' }
			json += '"hot":true'
		}
		json += '}'
		sig_prefix := signed_shell_prefix('POST', '/sessions/${session}/snapshot', json, secret_key)
		cmd := "${sig_prefix}curl -s -X POST '${api_base}/sessions/${session}/snapshot' -H 'Content-Type: application/json' -H 'Authorization: Bearer ${pub_key}' -H \"X-Timestamp: \$TIMESTAMP\" -H \"X-Signature: \$SIGNATURE\" --data-binary @BODYFILE"
		result := exec_curl_body(cmd, json)
		println('${green}Snapshot created${reset}')
		println(result)
		return
	}

	if service != '' {
		mut json := '{'
		mut has_content := false
		if name != '' {
			json += '"name":"${name}"'
			has_content = true
		}
		if hot {
			if has_content { json += ',' }
			json += '"hot":true'
		}
		json += '}'
		sig_prefix := signed_shell_prefix('POST', '/services/${service}/snapshot', json, secret_key)
		cmd := "${sig_prefix}curl -s -X POST '${api_base}/services/${service}/snapshot' -H 'Content-Type: application/json' -H 'Authorization: Bearer ${pub_key}' -H \"X-Timestamp: \$TIMESTAMP\" -H \"X-Signature: \$SIGNATURE\" --data-binary @BODYFILE"
		result := exec_curl_body(cmd, json)
		println('${green}Snapshot created${reset}')
		println(result)
		return
	}

	if restore != '' {
		sig_prefix := signed_shell_prefix('POST', '/snapshots/${restore}/restore', '{}', secret_key)
		cmd := "${sig_prefix}curl -s -X POST '${api_base}/snapshots/${restore}/restore' -H 'Content-Type: application/json' -H 'Authorization: Bearer ${pub_key}' -H \"X-Timestamp: \$TIMESTAMP\" -H \"X-Signature: \$SIGNATURE\" -d '{}'"
		result := exec_curl(cmd)
		println('${green}Snapshot restored${reset}')
		println(result)
		return
	}

	if delete != '' {
		endpoint := '/snapshots/${delete}'
		status := exec_curl_delete_with_sudo(endpoint, pub_key, secret_key)
		if status >= 200 && status < 300 {
			println('${green}Snapshot deleted: ${delete}${reset}')
		} else if status != 428 {
			eprintln('${red}Error: Failed to delete snapshot${reset}')
			exit(1)
		}
		return
	}

	if lock_id != '' {
		sig_prefix := signed_shell_prefix('POST', '/snapshots/${lock_id}/lock', '', secret_key)
		cmd := "${sig_prefix}curl -s -X POST '${api_base}/snapshots/${lock_id}/lock' -H 'Authorization: Bearer ${pub_key}' -H \"X-Timestamp: \$TIMESTAMP\" -H \"X-Signature: \$SIGNATURE\""
		exec_curl(cmd)
		println('${green}Snapshot locked: ${lock_id}${reset}')
		return
	}

	if unlock_id != '' {
		endpoint := '/snapshots/${unlock_id}/unlock'
		status := exec_curl_post_with_sudo(endpoint, '{}', pub_key, secret_key)
		if status >= 200 && status < 300 {
			println('${green}Snapshot unlocked: ${unlock_id}${reset}')
		} else if status != 428 {
			eprintln('${red}Error: Failed to unlock snapshot${reset}')
			exit(1)
		}
		return
	}

	if clone != '' {
		ct := if clone_type != '' { clone_type } else { 'session' }
		mut json := '{"clone_type":"${ct}"'
		if name != '' { json += ',"name":"${name}"' }
		if ports != '' { json += ',"ports":[${ports}]' }
		json += '}'
		sig_prefix := signed_shell_prefix('POST', '/snapshots/${clone}/clone', json, secret_key)
		cmd := "${sig_prefix}curl -s -X POST '${api_base}/snapshots/${clone}/clone' -H 'Content-Type: application/json' -H 'Authorization: Bearer ${pub_key}' -H \"X-Timestamp: \$TIMESTAMP\" -H \"X-Signature: \$SIGNATURE\" --data-binary @BODYFILE"
		result := exec_curl_body(cmd, json)
		println('${green}Snapshot cloned${reset}')
		println(result)
		return
	}

	eprintln('${red}Error: No snapshot action specified. Use --list, --info, --session, --service, --restore, --delete, --lock, --unlock, or --clone${reset}')
	exit(1)
}

// ---------------------------------------------------------------------------
// PaaS logs (CLI_SPEC.md "PaaS Logs Command"): GET {portal}/logs/{source}[/stream].
// HMAC covers "ts:GET:/logs/{source}:" (path only), computed in-process. Batch
// fetches go through net.http; streams run curl from an argv list (no shell).
// ---------------------------------------------------------------------------

struct PaasLogsSingle {
	source string
	lines  []string
}

struct PaasLogLine {
	source string
	line   string
}

struct PaasLogsAll {
	sources []string
	lines   []PaasLogLine
}

struct PaasError {
	err string @[json: 'error']
}

fn pct_encode(v string) string {
	mut out := ''
	for c in v.bytes() {
		if c.is_alnum() || c == `-` || c == `_` || c == `.` || c == `~` {
			out += c.ascii_str()
		} else {
			out += '%' + '${c:02X}'
		}
	}
	return out
}

fn add_query(url string, key string, val string) string {
	if val == '' {
		return url
	}
	sep := if url.contains('?') { '&' } else { '?' }
	return '${url}${sep}${key}=${pct_encode(val)}'
}

fn logs_path(source string, stream bool) string {
	src := if source == '' { 'all' } else { source }
	return if stream { '/logs/${src}/stream' } else { '/logs/${src}' }
}

const paas_status_mark = '__UNSB_HTTP_STATUS__'

// Raw portal fetch: (status, body); status 0 = no HTTP response.
fn logs_fetch_raw(source string, lines int, since string, grep string, level string, pub_key string, secret_key string) (int, string) {
	path := logs_path(source, false)
	mut url := '${portal_base}${path}?lines=${lines}'
	url = add_query(url, 'since', since)
	url = add_query(url, 'grep', grep)
	url = add_query(url, 'level', level)
	ts, sig := sign_request('GET', path, '', secret_key)
	mut header := http.new_header()
	header.add_custom('Authorization', 'Bearer ${pub_key}') or {}
	header.add_custom('X-Timestamp', '${ts}') or {}
	header.add_custom('X-Signature', sig) or {}
	header.add_custom('User-Agent', 'un-cli/2.0') or {}
	resp := http.fetch(http.FetchConfig{
		url:    url
		method: .get
		header: header
	}) or { return 0, '${err}' }
	return resp.status_code, resp.body
}

// Library-style fetch kept for compatibility; returns the portal's JSON body.
fn logs_fetch(source string, lines int, since string, grep string, pub_key string, secret_key string) string {
	n := if lines > 0 { if lines > 10000 { 10000 } else { lines } } else { 100 }
	_, body := logs_fetch_raw(source, n, since, grep, '', pub_key, secret_key)
	return body
}

fn paas_cursor_path() string {
	home := os.getenv('HOME')
	return if home == '' { '' } else { os.join_path(home, '.unsandbox', 'paas_log_cursor') }
}

fn read_paas_cursor() string {
	p := paas_cursor_path()
	if p == '' || !os.exists(p) {
		return ''
	}
	data := os.read_file(p) or { return '' }
	ts := data.trim_space().i64()
	if ts <= 0 {
		return ''
	}
	mut ago := time.now().unix() - ts
	if ago < 5 {
		ago = 5
	}
	return '${ago}s'
}

fn write_paas_cursor() {
	p := paas_cursor_path()
	if p == '' {
		return
	}
	os.mkdir_all(os.dir(p)) or {}
	os.write_file(p, '${time.now().unix()}\n') or {}
}

fn print_paas_error(status int, body string) {
	e := vjson.decode(PaasError, body) or { PaasError{} }
	if e.err != '' {
		eprintln('Error: HTTP ${status} — ${e.err}')
	} else {
		eprintln('Error: HTTP ${status}')
	}
}

fn fetch_paas_logs(source string, lines int, since string, grep string, json_out bool, level string, pub_key string, secret_key string) int {
	status, body := logs_fetch_raw(source, lines, since, grep, level, pub_key, secret_key)
	if status == 0 {
		eprintln('Error: Network error fetching logs: ${body}')
		return 1
	}
	if status != 200 {
		print_paas_error(status, body)
		return 1
	}
	write_paas_cursor()
	if json_out {
		println(body)
		return 0
	}
	if body.contains('"sources"') {
		all := vjson.decode(PaasLogsAll, body) or {
			eprintln('Error: Invalid response format')
			return 1
		}
		for l in all.lines {
			if l.source != '' {
				println('\x1b[36m${l.source:-12s}\x1b[0m ${l.line}')
			} else {
				println(l.line)
			}
		}
	} else {
		one := vjson.decode(PaasLogsSingle, body) or {
			eprintln('Error: Invalid response format')
			return 1
		}
		for l in one.lines {
			println(l)
		}
	}
	return 0
}

// Handle one SSE line; returns true when a log line printed.
fn paas_sse_line(line string, source string) bool {
	if !line.starts_with('data:') {
		return false
	}
	data := line[5..].trim_left(' ')
	if data == '' || data == ':keepalive' {
		return false
	}
	ev := vjson.decode(PaasLogLine, data) or {
		println(data)
		return true
	}
	if ev.line == '' && !data.contains('"line"') {
		println(data)
		return true
	}
	src := if ev.source != '' { ev.source } else { source }
	println('\x1b[36m${src:-12s}\x1b[0m ${ev.line}')
	return true
}

// Follow {portal}/logs/{source}/stream; reconnect (2s doubling to 30s) with since=
// covering the gap and a fresh signature. 401/403 is fatal.
fn stream_paas_logs(source string, grep string, level string, pub_key string, secret_key string) int {
	path := logs_path(source, true)
	mut base := '${portal_base}${path}'
	base = add_query(base, 'grep', grep)
	base = add_query(base, 'level', level)
	curl := os.find_abs_path_of_executable('curl') or {
		eprintln('Error: Stream error: curl not found')
		return 1
	}
	eprintln('Streaming ${source} logs... (Ctrl+C to stop)')
	mut delay := 2
	mut last_recv := i64(0)
	mut reconnect := false
	for {
		mut url := base
		if reconnect && last_recv > 0 {
			mut gap := time.now().unix() - last_recv + 5
			if gap < 10 {
				gap = 10
			}
			sep := if url.contains('?') { '&' } else { '?' }
			url = '${url}${sep}since=${gap}s'
		}
		ts, sig := sign_request('GET', path, '', secret_key)
		connected_at := time.now().unix()
		mut p := os.new_process(curl)
		p.set_args(['-s', '-N', '--http1.1', '-H', 'Accept: text/event-stream', '-H',
			'Authorization: Bearer ${pub_key}', '-H', 'X-Timestamp: ${ts}', '-H',
			'X-Signature: ${sig}', '-A', 'un-cli/2.0', '-w',
			'\n${paas_status_mark}%{http_code}\n', url])
		p.set_redirect_stdio()
		p.run()
		mut status := 0
		mut buf := ''
		for {
			chunk := p.stdout_read()
			if chunk == '' {
				if !p.is_alive() {
					break
				}
				continue
			}
			buf += chunk
			for {
				nl := buf.index('\n') or { break }
				line := buf[..nl]
				buf = buf[nl + 1..]
				if line.starts_with(paas_status_mark) {
					status = line[paas_status_mark.len..].trim_space().int()
				} else if paas_sse_line(line, source) {
					last_recv = time.now().unix()
				}
			}
		}
		if buf.starts_with(paas_status_mark) {
			status = buf[paas_status_mark.len..].trim_space().int()
		}
		p.wait()
		p.close()
		if status == 401 || status == 403 {
			eprintln('Error: HTTP ${status} (auth failed)')
			return 1
		}
		if status != 0 && status != 200 && status < 500 {
			eprintln('Error: HTTP ${status}')
			return 1
		}
		if status == 0 && !reconnect && last_recv == 0 {
			eprintln('Error: Stream error: could not connect')
			return 1
		}
		if time.now().unix() - connected_at > 30 {
			delay = 2
		}
		time.sleep(delay * time.second)
		delay = if delay * 2 > 30 { 30 } else { delay * 2 }
		reconnect = true
	}
	return 0
}

fn paas_logs_help() {
	eprintln('Usage: un paas logs [options]

Fetch or stream PaaS production logs.

Sources:
  --all              All sources (default)
  --api              API server logs
  --portal           Portal server logs
  --pool NODE        Pool node logs (e.g., cammy, ai)
  --source SOURCE    Source string (api, portal, pool/cammy, all)

Options:
  --lines N, -n N    Number of lines (default: 100, max: 10000)
  --since TIME       Time window: 1m, 5m, 15m, 1h, 6h, 1d (default: 5m)
  --grep PATTERN     Filter log lines by pattern
  --level LVL, -l    Min log level: debug, info, notice, warning, err, crit
  --follow, -f       Follow logs in real-time (Ctrl+C to stop)
  --json             Output raw JSON response
  -p KEY             Public key
  -k KEY             Secret key
  -h                 Show this help

Examples:
  un paas logs                             # last 100 lines from all sources
  un paas logs --api --lines 500           # last 500 API log lines
  un paas logs --portal --grep error       # portal logs matching \'error\'
  un paas logs --pool cammy --since 1h     # cammy pool logs from last hour
  un paas logs -l warning                  # warnings and above from all sources
  un paas logs --follow                    # follow all logs in real-time
  un paas logs --follow -l warning         # follow warnings+ in real-time

Requires a partner API key with log access enabled.')
}

// `un paas logs ...` (args after "logs"); -p/--account resolved by main's first pass.
fn cmd_paas_logs(args []string) int {
	pub_key := get_public_key()
	secret_key := get_secret_key()
	mut source := 'all'
	mut lines := 100
	mut since := ''
	mut grep := ''
	mut level := ''
	mut follow := false
	mut json_out := false
	mut i := 0
	for i < args.len {
		a := args[i]
		has_next := i + 1 < args.len
		if a in ['-p', '-k', '--account'] && has_next {
			i++
		} else if a == '--source' && has_next {
			i++
			source = args[i]
		} else if a == '--all' {
			source = 'all'
		} else if a == '--api' {
			source = 'api'
		} else if a == '--portal' {
			source = 'portal'
		} else if a == '--pool' && has_next {
			i++
			source = 'pool/${args[i]}'
		} else if a in ['--lines', '-n'] && has_next {
			i++
			n := args[i].int()
			lines = if n < 1 { 1 } else if n > 10000 { 10000 } else { n }
		} else if a == '--since' && has_next {
			i++
			since = args[i]
		} else if a == '--grep' && has_next {
			i++
			grep = args[i]
		} else if a in ['--level', '-l'] && has_next {
			i++
			level = args[i]
		} else if a in ['--follow', '-f'] {
			follow = true
		} else if a == '--json' {
			json_out = true
		} else if a in ['-h', '--help'] {
			paas_logs_help()
			return 0
		}
		i++
	}
	if pub_key == '' {
		eprintln('Error: API credentials required.\n  Set UNSANDBOX_PUBLIC_KEY + UNSANDBOX_SECRET_KEY env vars, or\n  Use -p PUBLIC_KEY -k SECRET_KEY flags, or\n  Create ~/.unsandbox/accounts.csv with: public_key,secret_key')
		return 1
	}
	if follow {
		return stream_paas_logs(source, grep, level, pub_key, secret_key)
	}
	if since == '' {
		since = read_paas_cursor()
		if since == '' {
			since = '5m'
		}
	}
	return fetch_paas_logs(source, lines, since, grep, json_out, level, pub_key, secret_key)
}

// Kept for compatibility with older callers: portal /logs/{source}.
fn cmd_logs(source string, lines int, since string, grep string, follow bool, api_key string) {
	pub_key := get_public_key()
	secret_key := get_secret_key()
	src := if source != '' { source } else { 'all' }
	rc := if follow {
		stream_paas_logs(src, grep, '', pub_key, secret_key)
	} else {
		n := if lines > 0 { if lines > 10000 { 10000 } else { lines } } else { 100 }
		fetch_paas_logs(src, n, if since != '' { since } else { '5m' }, grep, false, '', pub_key, secret_key)
	}
	if rc != 0 {
		exit(rc)
	}
}

fn cmd_health() {
	cmd := "curl -s -X GET '${api_base}/health'"
	result := os.execute(cmd)
	if result.output.contains('"status":"healthy"') || result.output.contains('"ok":true') {
		println('${green}API is healthy${reset}')
	} else {
		println('${red}API may be unhealthy${reset}')
	}
	println(result.output)
}

fn cmd_version() {
	println('un.v version 1.0.0')
	println('API: ${api_base}')
	println('Portal: ${portal_base}')
}

fn cmd_image(list bool, info string, delete string, lock_id string, unlock_id string, publish string, source_type string, visibility_id string, visibility_mode string, spawn_id string, clone string, name string, ports string, grant string, revoke string, trusted string, trusted_key string, transfer string, to_key string, api_key string) {
	pub_key := get_public_key()
	secret_key := get_secret_key()

	if list {
		sig_prefix := signed_shell_prefix('GET', '/images', '', secret_key)
		cmd := "${sig_prefix}curl -s -X GET '${api_base}/images' -H 'Authorization: Bearer ${pub_key}' -H \"X-Timestamp: \$TIMESTAMP\" -H \"X-Signature: \$SIGNATURE\""
		println(exec_curl(cmd))
		return
	}

	if info != '' {
		sig_prefix := signed_shell_prefix('GET', '/images/${info}', '', secret_key)
		cmd := "${sig_prefix}curl -s -X GET '${api_base}/images/${info}' -H 'Authorization: Bearer ${pub_key}' -H \"X-Timestamp: \$TIMESTAMP\" -H \"X-Signature: \$SIGNATURE\""
		println(exec_curl(cmd))
		return
	}

	if delete != '' {
		endpoint := '/images/${delete}'
		status := exec_curl_delete_with_sudo(endpoint, pub_key, secret_key)
		if status >= 200 && status < 300 {
			println('${green}Image deleted: ${delete}${reset}')
		} else if status != 428 {
			eprintln('${red}Error: Failed to delete image${reset}')
			exit(1)
		}
		return
	}

	if lock_id != '' {
		sig_prefix := signed_shell_prefix('POST', '/images/${lock_id}/lock', '', secret_key)
		cmd := "${sig_prefix}curl -s -X POST '${api_base}/images/${lock_id}/lock' -H 'Authorization: Bearer ${pub_key}' -H \"X-Timestamp: \$TIMESTAMP\" -H \"X-Signature: \$SIGNATURE\""
		exec_curl(cmd)
		println('${green}Image locked: ${lock_id}${reset}')
		return
	}

	if unlock_id != '' {
		endpoint := '/images/${unlock_id}/unlock'
		status := exec_curl_post_with_sudo(endpoint, '{}', pub_key, secret_key)
		if status >= 200 && status < 300 {
			println('${green}Image unlocked: ${unlock_id}${reset}')
		} else if status != 428 {
			eprintln('${red}Error: Failed to unlock image${reset}')
			exit(1)
		}
		return
	}

	if publish != '' {
		if source_type == '' {
			eprintln('${red}Error: --publish requires --source-type (service or snapshot)${reset}')
			exit(1)
		}
		mut json := '{"source_type":"${source_type}","source_id":"${publish}"'
		if name != '' {
			json += ',"name":"${name}"'
		}
		json += '}'
		sig_prefix := signed_shell_prefix('POST', '/images/publish', json, secret_key)
		cmd := "${sig_prefix}curl -s -X POST '${api_base}/images/publish' -H 'Content-Type: application/json' -H 'Authorization: Bearer ${pub_key}' -H \"X-Timestamp: \$TIMESTAMP\" -H \"X-Signature: \$SIGNATURE\" --data-binary @BODYFILE"
		result := exec_curl_body(cmd, json)
		println(result)
		return
	}

	if visibility_id != '' {
		if visibility_mode == '' {
			eprintln('${red}Error: --visibility requires a mode (private, unlisted, or public)${reset}')
			exit(1)
		}
		json := '{"visibility":"${visibility_mode}"}'
		sig_prefix := signed_shell_prefix('POST', '/images/${visibility_id}/visibility', json, secret_key)
		cmd := "${sig_prefix}curl -s -X POST '${api_base}/images/${visibility_id}/visibility' -H 'Content-Type: application/json' -H 'Authorization: Bearer ${pub_key}' -H \"X-Timestamp: \$TIMESTAMP\" -H \"X-Signature: \$SIGNATURE\" --data-binary @BODYFILE"
		exec_curl_body(cmd, json)
		println('${green}Image visibility set to ${visibility_mode}: ${visibility_id}${reset}')
		return
	}

	if spawn_id != '' {
		mut json := '{"name":"${name}"'
		if ports != '' {
			json += ',"ports":[${ports}]'
		}
		json += '}'
		sig_prefix := signed_shell_prefix('POST', '/images/${spawn_id}/spawn', json, secret_key)
		cmd := "${sig_prefix}curl -s -X POST '${api_base}/images/${spawn_id}/spawn' -H 'Content-Type: application/json' -H 'Authorization: Bearer ${pub_key}' -H \"X-Timestamp: \$TIMESTAMP\" -H \"X-Signature: \$SIGNATURE\" --data-binary @BODYFILE"
		result := exec_curl_body(cmd, json)
		println(result)
		return
	}

	if clone != '' {
		mut json := '{'
		if name != '' {
			json += '"name":"${name}"'
		}
		json += '}'
		sig_prefix := signed_shell_prefix('POST', '/images/${clone}/clone', json, secret_key)
		cmd := "${sig_prefix}curl -s -X POST '${api_base}/images/${clone}/clone' -H 'Content-Type: application/json' -H 'Authorization: Bearer ${pub_key}' -H \"X-Timestamp: \$TIMESTAMP\" -H \"X-Signature: \$SIGNATURE\" --data-binary @BODYFILE"
		result := exec_curl_body(cmd, json)
		println(result)
		return
	}

	if grant != '' {
		if trusted_key == '' {
			eprintln('${red}Error: --grant requires --trusted-key${reset}')
			exit(1)
		}
		json := '{"trusted_api_key":"${trusted_key}"}'
		sig_prefix := signed_shell_prefix('POST', '/images/${grant}/grant', json, secret_key)
		cmd := "${sig_prefix}curl -s -X POST '${api_base}/images/${grant}/grant' -H 'Content-Type: application/json' -H 'Authorization: Bearer ${pub_key}' -H \"X-Timestamp: \$TIMESTAMP\" -H \"X-Signature: \$SIGNATURE\" --data-binary @BODYFILE"
		exec_curl_body(cmd, json)
		println('${green}Access granted to ${trusted_key}${reset}')
		return
	}

	if revoke != '' {
		if trusted_key == '' {
			eprintln('${red}Error: --revoke requires --trusted-key${reset}')
			exit(1)
		}
		json := '{"trusted_api_key":"${trusted_key}"}'
		sig_prefix := signed_shell_prefix('POST', '/images/${revoke}/revoke', json, secret_key)
		cmd := "${sig_prefix}curl -s -X POST '${api_base}/images/${revoke}/revoke' -H 'Content-Type: application/json' -H 'Authorization: Bearer ${pub_key}' -H \"X-Timestamp: \$TIMESTAMP\" -H \"X-Signature: \$SIGNATURE\" --data-binary @BODYFILE"
		exec_curl_body(cmd, json)
		println('${green}Access revoked from ${trusted_key}${reset}')
		return
	}

	if trusted != '' {
		sig_prefix := signed_shell_prefix('GET', '/images/${trusted}/trusted', '', secret_key)
		cmd := "${sig_prefix}curl -s -X GET '${api_base}/images/${trusted}/trusted' -H 'Authorization: Bearer ${pub_key}' -H \"X-Timestamp: \$TIMESTAMP\" -H \"X-Signature: \$SIGNATURE\""
		println(exec_curl(cmd))
		return
	}

	if transfer != '' {
		if to_key == '' {
			eprintln('${red}Error: --transfer requires --to-key${reset}')
			exit(1)
		}
		json := '{"to_api_key":"${to_key}"}'
		endpoint := '/images/${transfer}/transfer'
		status := exec_curl_post_with_sudo(endpoint, json, pub_key, secret_key)
		if status >= 200 && status < 300 {
			println('${green}Image transferred to ${to_key}${reset}')
		} else if status != 428 {
			eprintln('${red}Error: Failed to transfer image${reset}')
			exit(1)
		}
		return
	}

	eprintln('${red}Error: No image action specified. Use --list, --info, --delete, --publish, --grant, --revoke, --trusted, --transfer, etc.${reset}')
	exit(1)
}

fn get_languages_cache_path() string {
	home := os.getenv('HOME')
	cache_dir := '${home}/.unsandbox'
	if !os.exists(cache_dir) {
		os.mkdir(cache_dir) or {}
	}
	return '${cache_dir}/languages.json'
}

fn get_cached_languages() ?[]string {
	cache_path := get_languages_cache_path()
	if !os.exists(cache_path) {
		return none
	}

	content := os.read_file(cache_path) or { return none }

	// Extract timestamp
	timestamp_str := extract_json_string(content, 'timestamp')
	if timestamp_str == '' {
		return none
	}

	// Parse timestamp (it's stored as a number, not a string)
	// Look for "timestamp": followed by digits
	ts_search := '"timestamp":'
	ts_idx := content.index(ts_search) or { return none }
	ts_start := ts_idx + ts_search.len
	mut ts_end := ts_start
	for ts_end < content.len && (content[ts_end].is_digit() || content[ts_end] == ` `) {
		ts_end++
	}
	timestamp_num := content[ts_start..ts_end].trim_space()
	cache_time := timestamp_num.i64()

	// Get current time
	current_time_cmd := 'date +%s'
	current_time_result := os.execute(current_time_cmd)
	current_time := current_time_result.output.trim_space().i64()

	// Check if cache is still valid
	if current_time - cache_time >= languages_cache_ttl {
		return none
	}

	// Extract languages array
	start_idx := content.index('"languages":[') or { return none }
	arr_start := content.index_after('[', start_idx) or { -1 }
	if arr_start < 0 {
		return none
	}
	arr_end := content.index_after(']', arr_start) or { -1 }
	if arr_end < 0 {
		return none
	}

	arr_content := content[arr_start + 1..arr_end]
	mut languages := []string{}
	for item in arr_content.split(',') {
		lang := item.trim_space().trim('"')
		if lang.len > 0 {
			languages << lang
		}
	}

	return languages
}

fn save_languages_cache(languages []string) {
	cache_path := get_languages_cache_path()

	// Get current timestamp
	timestamp_cmd := 'date +%s'
	timestamp_result := os.execute(timestamp_cmd)
	timestamp := timestamp_result.output.trim_space()

	// Build JSON array
	mut lang_json := '['
	for i, lang in languages {
		if i > 0 {
			lang_json += ','
		}
		lang_json += '"${lang}"'
	}
	lang_json += ']'

	cache_content := '{"languages":${lang_json},"timestamp":${timestamp}}'
	os.write_file(cache_path, cache_content) or {}
}

fn cmd_languages(json_output bool, api_key string) {
	// Check cache first
	if languages := get_cached_languages() {
		if json_output {
			mut lang_json := '['
			for i, lang in languages {
				if i > 0 {
					lang_json += ','
				}
				lang_json += '"${lang}"'
			}
			lang_json += ']'
			println(lang_json)
		} else {
			for lang in languages {
				println(lang)
			}
		}
		return
	}

	// Fetch from API
	pub_key := get_public_key()
	secret_key := get_secret_key()

	sig_prefix := signed_shell_prefix('GET', '/languages', '', secret_key)
	cmd := "${sig_prefix}curl -s -X GET '${api_base}/languages' -H 'Authorization: Bearer ${pub_key}' -H \"X-Timestamp: \$TIMESTAMP\" -H \"X-Signature: \$SIGNATURE\""
	result := exec_curl(cmd)

	// Parse and cache the languages
	start_idx := result.index('"languages":[') or {
		println(result)
		return
	}
	arr_start := result.index_after('[', start_idx) or { -1 }
	if arr_start < 0 {
		println(result)
		return
	}
	arr_end := result.index_after(']', arr_start) or { -1 }
	if arr_end < 0 {
		println(result)
		return
	}

	// Extract languages for caching
	arr_content := result[arr_start + 1..arr_end]
	mut languages := []string{}
	for item in arr_content.split(',') {
		lang := item.trim_space().trim('"')
		if lang.len > 0 {
			languages << lang
		}
	}

	// Save to cache
	if languages.len > 0 {
		save_languages_cache(languages)
	}

	if json_output {
		println(result[arr_start..arr_end + 1])
	} else {
		for lang in languages {
			println(lang)
		}
	}
}

// The accounts.csv the credentials came from, or '' (flags / env vars).
// Mirrors resolve_credentials' tiers; call before main injects env vars.
fn credentials_csv_path(account_index int, explicit_pub_key string) string {
	home := os.getenv('HOME')
	home_csv := (if home != '' { home } else { '.' }) + '/.unsandbox/accounts.csv'
	mut row := account_index
	if row < 0 {
		if explicit_pub_key != '' || os.getenv('UNSANDBOX_PUBLIC_KEY') != ''
			|| os.getenv('UNSANDBOX_API_KEY') != '' {
			return ''
		}
		env_acct := os.getenv('UNSANDBOX_ACCOUNT')
		row = if env_acct != '' && env_acct.int() >= 0 { env_acct.int() } else { 0 }
	}
	for path in [home_csv, 'accounts.csv'] {
		if creds := load_accounts_csv(path, row) {
			_ = creds
			return path
		}
	}
	return ''
}

// Replace the secret on the line whose first field is public_key. Every other
// line, field, comment and the order stay as they are. Temp file (0600) + rename.
fn rewrite_accounts_csv(path string, public_key string, new_secret string) bool {
	content := os.read_file(path) or { return false }
	mut lines := content.split('\n')
	mut changed := false
	for i, line in lines {
		stripped := line.trim_space()
		if stripped == '' || stripped.starts_with('#') {
			continue
		}
		mut fields := line.split(',')
		if fields.len >= 2 && fields[0].trim_space() == public_key {
			fields[1] = new_secret
			lines[i] = fields.join(',')
			changed = true
			break
		}
	}
	if !changed {
		return false
	}
	tmp := os.join_path(os.dir(path), '.${os.file_name(path)}.${os.getpid()}.tmp')
	mut f := os.open_file(tmp, 'w', 0o600) or { return false }
	f.write_string(lines.join('\n')) or {
		f.close()
		os.rm(tmp) or {}
		return false
	}
	f.close()
	os.chmod(tmp, 0o600) or {}
	os.rename(tmp, path) or {
		os.rm(tmp) or {}
		return false
	}
	return true
}

struct RotateResponse {
	secret       string
	challenge_id string
	err_code     string
	message      string
}

// String value of "field" in a flat JSON object, tolerating whitespace around ':'.
fn json_string_field(body string, field string) string {
	key := '"${field}"'
	mut pos := 0
	for {
		k := body.index_after(key, pos) or { return '' }
		mut i := k + key.len
		for i < body.len && body[i].is_space() {
			i++
		}
		if i < body.len && body[i] == `:` {
			i++
			for i < body.len && body[i].is_space() {
				i++
			}
			if i >= body.len || body[i] != `"` {
				return ''
			}
			i++
			mut value := []u8{}
			for i < body.len && body[i] != `"` {
				if body[i] == `\\` && i + 1 < body.len {
					i++
				}
				value << body[i]
				i++
			}
			return value.bytestr()
		}
		pos = k + 1
	}
	return ''
}

// POST /keys/self/rotate-secret, body {}; HMAC computed in-process, fresh per call.
// Exits 4 on a transport failure.
fn post_rotate_secret(pub_key string, secret_key string, otp string, challenge string) (int, RotateResponse) {
	path := '/keys/self/rotate-secret'
	body := '{}'
	ts := time.now().unix()
	message := '${ts}:POST:${path}:${body}'
	signature := hmac.new(secret_key.bytes(), message.bytes(), sha256.sum, sha256.block_size).hex()
	mut header := http.new_header()
	header.add_custom('Authorization', 'Bearer ${pub_key}') or {}
	header.add_custom('X-Timestamp', '${ts}') or {}
	header.add_custom('X-Signature', signature) or {}
	header.add_custom('Content-Type', 'application/json') or {}
	if otp != '' {
		header.add_custom('X-Sudo-OTP', otp) or {}
		header.add_custom('X-Sudo-Challenge', challenge) or {}
	}
	resp := http.fetch(http.FetchConfig{
		url:    '${api_base}${path}'
		method: .post
		header: header
		data:   body
	}) or {
		eprintln('Error: ${err}')
		exit(4)
	}
	parsed := RotateResponse{
		secret:       json_string_field(resp.body, 'secret')
		challenge_id: json_string_field(resp.body, 'challenge_id')
		err_code:     json_string_field(resp.body, 'error')
		message:      json_string_field(resp.body, 'message')
	}
	return resp.status_code, parsed
}

// un key --rotate [--otp CODE]: rotate this key's secret (see CLI_SPEC.md)
fn cmd_key_rotate(otp_flag string, creds_csv string) {
	pub_key := get_public_key()
	secret_key := get_secret_key()
	home := os.getenv('HOME')
	unsandbox_dir := (if home != '' { home } else { '.' }) + '/.unsandbox'
	challenge_file := '${unsandbox_dir}/rotate-${sha256.hexhash(pub_key)[..16]}.challenge'

	mut status := 0
	mut data := RotateResponse{}
	if otp_flag != '' {
		saved := os.read_file(challenge_file) or {
			eprintln('Error: no rotation in progress; run: un key --rotate')
			exit(2)
		}
		status, data = post_rotate_secret(pub_key, secret_key, otp_flag.trim_space(), saved.trim_space())
		os.rm(challenge_file) or {}
	} else {
		status, data = post_rotate_secret(pub_key, secret_key, '', '')
		if status == 428 {
			challenge := data.challenge_id
			if os.is_atty(0) > 0 {
				eprintln('One-time code emailed to the key owner.')
				eprint('Code: ')
				otp := os.get_line().trim_space()
				if otp == '' {
					eprintln('Error: cancelled')
					exit(1)
				}
				status, data = post_rotate_secret(pub_key, secret_key, otp, challenge)
			} else {
				os.mkdir_all(unsandbox_dir, mode: 0o700) or {
					eprintln('Error: ${err}')
					exit(1)
				}
				mut f := os.open_file(challenge_file, 'w', 0o600) or {
					eprintln('Error: ${err}')
					exit(1)
				}
				f.write_string(challenge) or {
					eprintln('Error: ${err}')
					exit(1)
				}
				f.close()
				os.chmod(challenge_file, 0o600) or {}
				println('One-time code emailed to the key owner. Finish with: un key --rotate --otp CODE')
				return
			}
		}
	}

	if status != 200 {
		if status == 403 && data.err_code == 'email_required' {
			eprintln('Error: this key has no email; add one or use POST /keys/recover')
			exit(3)
		}
		if status == 401 {
			eprintln('Error: authentication failed')
			exit(3)
		}
		msg := if data.message != '' {
			data.message
		} else if data.err_code != '' {
			data.err_code
		} else {
			'HTTP ${status}'
		}
		eprintln('Error: ${msg}')
		exit(4)
	}
	if data.secret == '' {
		eprintln('Error: API returned no secret')
		exit(4)
	}

	if creds_csv != '' && rewrite_accounts_csv(creds_csv, pub_key, data.secret) {
		println('Secret rotated. Updated ${creds_csv} (public key ${pub_key}).')
	} else {
		println('New secret (shown once, store it now): ${data.secret}')
		println('Warning: UNSANDBOX_SECRET_KEY / -k still hold the old, now invalid, secret.')
	}
}

fn resolve_credentials(account_index int, explicit_pub_key string) (string, string) {
	if account_index >= 0 {
		// --account N: load from ~/.unsandbox/accounts.csv, bypassing env vars
		home := os.getenv('HOME')
		csv_path := (if home != '' { home } else { '.' }) + '/.unsandbox/accounts.csv'
		if creds := load_accounts_csv(csv_path, account_index) {
			pk := if explicit_pub_key != '' { explicit_pub_key } else { creds[0] }
			return pk, creds[1]
		}
		// fall back to ./accounts.csv
		if creds := load_accounts_csv('accounts.csv', account_index) {
			pk := if explicit_pub_key != '' { explicit_pub_key } else { creds[0] }
			return pk, creds[1]
		}
		return explicit_pub_key, ''
	}

	mut pub_key := if explicit_pub_key != '' { explicit_pub_key } else { os.getenv('UNSANDBOX_PUBLIC_KEY') }
	mut sec_key := os.getenv('UNSANDBOX_SECRET_KEY')

	if pub_key == '' {
		api_key := os.getenv('UNSANDBOX_API_KEY')
		if api_key != '' {
			pub_key = api_key
		}
	}

	// Try UNSANDBOX_ACCOUNT env var to pick a row
	mut env_account := -1
	env_acct := os.getenv('UNSANDBOX_ACCOUNT')
	if env_acct != '' {
		env_account = env_acct.int()
	}

	if pub_key == '' {
		home := os.getenv('HOME')
		csv_path := (if home != '' { home } else { '.' }) + '/.unsandbox/accounts.csv'
		row := if env_account >= 0 { env_account } else { 0 }
		if creds := load_accounts_csv(csv_path, row) {
			pub_key = creds[0]
			sec_key = creds[1]
		}
		if pub_key == '' {
			if creds2 := load_accounts_csv('accounts.csv', row) {
				pub_key = creds2[0]
				sec_key = creds2[1]
			}
		}
	}

	if pub_key == '' {
		eprintln('${red}Error: UNSANDBOX_PUBLIC_KEY or UNSANDBOX_API_KEY environment variable not set${reset}')
		exit(1)
	}
	return pub_key, sec_key
}

fn main() {
	// First pass: scan for --account N and -p flags
	mut account_index := -1
	mut explicit_pub_key := ''
	for i := 1; i < os.args.len; i++ {
		if os.args[i] == '--account' && i + 1 < os.args.len {
			account_index = os.args[i + 1].int()
			i++
		} else if os.args[i] == '-p' && i + 1 < os.args.len {
			explicit_pub_key = os.args[i + 1]
			i++
		}
	}

	creds_csv := credentials_csv_path(account_index, explicit_pub_key)
	pub_key, sec_key := resolve_credentials(account_index, explicit_pub_key)
	// Inject resolved credentials into env so get_public_key/get_secret_key pick them up
	os.setenv('UNSANDBOX_PUBLIC_KEY', pub_key, true)
	os.setenv('UNSANDBOX_SECRET_KEY', sec_key, true)
	mut api_key := pub_key

	if os.args.len < 2 {
		eprintln('Usage: ${os.args[0]} [options] <source_file>')
		eprintln('       ${os.args[0]} session [options]')
		eprintln('       ${os.args[0]} service [options]')
		eprintln('       ${os.args[0]} service env <action> <service_id> [options]')
		eprintln('       ${os.args[0]} image [options]')
		eprintln('       ${os.args[0]} key [--extend]')
		eprintln('       ${os.args[0]} key --rotate [--otp CODE]')
		eprintln('       ${os.args[0]} languages [--json]')
		eprintln('')
		eprintln('Vault commands:')
		eprintln('  service env status <id>   Check vault status')
		eprintln('  service env set <id>      Set vault (-e KEY=VAL or --env-file FILE)')
		eprintln('  service env export <id>   Export vault contents')
		eprintln('  service env delete <id>   Delete vault')
		eprintln('')
		eprintln('Image commands:')
		eprintln('  image --list              List all images')
		eprintln('  image --info ID           Get image details')
		eprintln('  image --delete ID         Delete an image')
		eprintln('  image --lock ID           Lock image to prevent deletion')
		eprintln('  image --unlock ID         Unlock image')
		eprintln('  image --publish ID --source-type TYPE  Publish from service/snapshot')
		eprintln('  image --visibility ID MODE  Set visibility (private/unlisted/public)')
		eprintln('  image --spawn ID --name NAME  Spawn service from image')
		eprintln('  image --clone ID --name NAME  Clone an image')
		exit(1)
	}

	if os.args[1] == 'session' {
		mut list := false
		mut kill := ''
		mut shell := ''
		mut network := ''
		mut vcpu := 0
		mut tmux := false
		mut screen := false

		mut input_files := []string{}
		mut i := 2
		for i < os.args.len {
			match os.args[i] {
				'--list' { list = true }
				'--kill' {
					i++
					kill = os.args[i]
				}
				'--shell' {
					i++
					shell = os.args[i]
				}
				'-n' {
					i++
					network = os.args[i]
				}
				'-v' {
					i++
					vcpu = os.args[i].int()
				}
				'--tmux' { tmux = true }
				'--screen' { screen = true }
				'-k' {
					i++
					api_key = os.args[i]
				}
				'--account' {
					i++  // already handled in first pass
				}
				'-f' {
					i++
					f := os.args[i]
					if os.exists(f) {
						input_files << f
					} else {
						eprintln('Error: File not found: ${f}')
						exit(1)
					}
				}
				else {}
			}
			i++
		}

		cmd_session(list, kill, shell, network, vcpu, tmux, screen, input_files, api_key)
		return
	}

	if os.args[1] == 'service' {
		mut name := ''
		mut ports := ''
		mut service_type := ''
		mut bootstrap := ''
		mut bootstrap_file := ''
		mut list := false
		mut info := ''
		mut logs := ''
		mut tail := ''
		mut sleep := ''
		mut wake := ''
		mut destroy := ''
		mut resize := ''
		mut execute := ''
		mut command := ''
		mut dump_bootstrap := ''
		mut dump_file := ''
		mut network := ''
		mut vcpu := 0
		mut input_files := []string{}
		mut svc_envs := []string{}
		mut svc_env_file := ''
		mut env_action := ''
		mut env_target := ''
		mut unfreeze_on_demand := false
		mut set_unfreeze_on_demand_id := ''
		mut set_unfreeze_on_demand_enabled := ''

		mut i := 2
		for i < os.args.len {
			match os.args[i] {
				'env' {
					// service env <action> <service_id>
					if i + 2 < os.args.len {
						i++
						env_action = os.args[i]
						i++
						env_target = os.args[i]
					}
				}
				'--name' {
					i++
					name = os.args[i]
				}
				'--ports' {
					i++
					ports = os.args[i]
				}
				'--type' {
					i++
					service_type = os.args[i]
				}
				'--bootstrap' {
					i++
					bootstrap = os.args[i]
				}
				'--bootstrap-file' {
					i++
					bootstrap_file = os.args[i]
				}
				'--list' { list = true }
				'--info' {
					i++
					info = os.args[i]
				}
				'--logs' {
					i++
					logs = os.args[i]
				}
				'--tail' {
					i++
					tail = os.args[i]
				}
				'--freeze' {
					i++
					sleep = os.args[i]
				}
				'--unfreeze' {
					i++
					wake = os.args[i]
				}
				'--destroy' {
					i++
					destroy = os.args[i]
				}
				'--resize' {
					i++
					resize = os.args[i]
				}
				'--execute' {
					i++
					execute = os.args[i]
				}
				'--command' {
					i++
					command = os.args[i]
				}
				'--dump-bootstrap' {
					i++
					dump_bootstrap = os.args[i]
				}
				'--dump-file' {
					i++
					dump_file = os.args[i]
				}
				'-e' {
					i++
					svc_envs << os.args[i]
				}
				'--env-file' {
					i++
					svc_env_file = os.args[i]
				}
				'--unfreeze-on-demand' { unfreeze_on_demand = true }
				'--set-unfreeze-on-demand' {
					i++
					set_unfreeze_on_demand_id = os.args[i]
					i++
					if i < os.args.len {
						set_unfreeze_on_demand_enabled = os.args[i]
					}
				}
				'-n' {
					i++
					network = os.args[i]
				}
				'-v' {
					i++
					vcpu = os.args[i].int()
				}
				'-k' {
					i++
					api_key = os.args[i]
				}
				'--account' {
					i++  // already handled in first pass
				}
				'-f' {
					i++
					f := os.args[i]
					if os.exists(f) {
						input_files << f
					} else {
						eprintln('Error: File not found: ${f}')
						exit(1)
					}
				}
				else {}
			}
			i++
		}

		// Handle env subcommand
		if env_action != '' && env_target != '' {
			cmd_service_env(env_action, env_target, svc_envs, svc_env_file, api_key)
			return
		}

		cmd_service(name, ports, service_type, bootstrap, bootstrap_file, list, info, logs, tail, sleep, wake, destroy, resize, execute, command, dump_bootstrap, dump_file, network,
			vcpu, input_files, svc_envs, svc_env_file, unfreeze_on_demand, set_unfreeze_on_demand_id, set_unfreeze_on_demand_enabled, api_key)
		return
	}

	if os.args[1] == 'key' {
		mut extend := false
		mut rotate := false
		mut otp_flag := ''

		mut i := 2
		for i < os.args.len {
			match os.args[i] {
				'--extend' { extend = true }
				'--rotate' { rotate = true }
				'--otp' {
					i++
					if i < os.args.len {
						otp_flag = os.args[i]
					}
				}
				'-k' {
					i++
					api_key = os.args[i]
				}
				'--account' {
					i++  // already handled in first pass
				}
				else {}
			}
			i++
		}

		if rotate {
			cmd_key_rotate(otp_flag, creds_csv)
			return
		}
		cmd_key(extend, api_key)
		return
	}

	if os.args[1] == 'languages' {
		mut json_output := false

		mut i := 2
		for i < os.args.len {
			match os.args[i] {
				'--json' { json_output = true }
				'-k' {
					i++
					api_key = os.args[i]
				}
				'--account' {
					i++  // already handled in first pass
				}
				else {}
			}
			i++
		}

		cmd_languages(json_output, api_key)
		return
	}

	if os.args[1] == 'image' {
		mut list := false
		mut info := ''
		mut delete := ''
		mut lock_id := ''
		mut unlock_id := ''
		mut publish := ''
		mut source_type := ''
		mut visibility_id := ''
		mut visibility_mode := ''
		mut spawn_id := ''
		mut clone := ''
		mut name := ''
		mut ports := ''
		mut grant := ''
		mut revoke := ''
		mut trusted := ''
		mut trusted_key := ''
		mut transfer := ''
		mut to_key := ''

		mut i := 2
		for i < os.args.len {
			match os.args[i] {
				'--list', '-l' { list = true }
				'--info' {
					i++
					info = os.args[i]
				}
				'--delete' {
					i++
					delete = os.args[i]
				}
				'--lock' {
					i++
					lock_id = os.args[i]
				}
				'--unlock' {
					i++
					unlock_id = os.args[i]
				}
				'--publish' {
					i++
					publish = os.args[i]
				}
				'--source-type' {
					i++
					source_type = os.args[i]
				}
				'--visibility' {
					i++
					visibility_id = os.args[i]
					i++
					if i < os.args.len {
						visibility_mode = os.args[i]
					}
				}
				'--spawn' {
					i++
					spawn_id = os.args[i]
				}
				'--clone' {
					i++
					clone = os.args[i]
				}
				'--name' {
					i++
					name = os.args[i]
				}
				'--ports' {
					i++
					ports = os.args[i]
				}
				'--grant' {
					i++
					grant = os.args[i]
				}
				'--revoke' {
					i++
					revoke = os.args[i]
				}
				'--trusted' {
					i++
					trusted = os.args[i]
				}
				'--trusted-key' {
					i++
					trusted_key = os.args[i]
				}
				'--transfer' {
					i++
					transfer = os.args[i]
				}
				'--to-key' {
					i++
					to_key = os.args[i]
				}
				'-k' {
					i++
					api_key = os.args[i]
				}
				'--account' {
					i++  // already handled in first pass
				}
				else {}
			}
			i++
		}

		cmd_image(list, info, delete, lock_id, unlock_id, publish, source_type, visibility_id, visibility_mode, spawn_id, clone, name, ports, grant, revoke, trusted, trusted_key, transfer, to_key, api_key)
		return
	}

	if os.args[1] == 'snapshot' {
		mut list := false
		mut info := ''
		mut session := ''
		mut service := ''
		mut restore := ''
		mut delete := ''
		mut lock_id := ''
		mut unlock_id := ''
		mut clone := ''
		mut clone_type := ''
		mut name := ''
		mut ports := ''
		mut hot := false

		mut i := 2
		for i < os.args.len {
			match os.args[i] {
				'--list', '-l' { list = true }
				'--info' { i++; info = os.args[i] }
				'--session' { i++; session = os.args[i] }
				'--service' { i++; service = os.args[i] }
				'--restore' { i++; restore = os.args[i] }
				'--delete' { i++; delete = os.args[i] }
				'--lock' { i++; lock_id = os.args[i] }
				'--unlock' { i++; unlock_id = os.args[i] }
				'--clone' { i++; clone = os.args[i] }
				'--clone-type' { i++; clone_type = os.args[i] }
				'--name' { i++; name = os.args[i] }
				'--ports' { i++; ports = os.args[i] }
				'--hot' { hot = true }
				'-k' { i++; api_key = os.args[i] }
				'--account' { i++  /* already handled in first pass */ }
				else {}
			}
			i++
		}

		cmd_snapshot(list, info, session, service, restore, delete, lock_id, unlock_id, clone, clone_type, name, ports, hot, api_key)
		return
	}

	if os.args[1] == 'paas' {
		if os.args.len < 3 {
			eprintln('Usage: un paas <command> [options]\n\nCommands:\n  logs    Fetch or stream production logs\n\nRun \'un paas <command> -h\' for help on a specific command.')
			exit(1)
		}
		if os.args[2] != 'logs' {
			eprintln('Error: Unknown paas command \'${os.args[2]}\'\nRun \'un paas\' for available commands.')
			exit(1)
		}
		exit(cmd_paas_logs(os.args[3..]))
	}

	if os.args[1] == 'logs' {
		// Alias for `paas logs`
		exit(cmd_paas_logs(os.args[2..]))
	}

	if os.args[1] == 'health' {
		cmd_health()
		return
	}

	if os.args[1] == 'version' {
		cmd_version()
		return
	}

	// Execute mode
	mut envs := []string{}
	mut artifacts := false
	mut network := ''
	mut source_file := ''
	mut vcpu := 0

	mut i := 1
	for i < os.args.len {
		match os.args[i] {
			'-e' {
				i++
				envs << os.args[i]
			}
			'-a' { artifacts = true }
			'-n' {
				i++
				network = os.args[i]
			}
			'-v' {
				i++
				vcpu = os.args[i].int()
			}
			'-k' {
				i++
				api_key = os.args[i]
			}
			'--account' {
				i++  // already handled in first pass
			}
			else {
				if os.args[i].starts_with('-') {
					eprintln('${red}Unknown option: ${os.args[i]}${reset}')
					exit(1)
				} else {
					source_file = os.args[i]
				}
			}
		}
		i++
	}

	if source_file == '' {
		eprintln('${red}Error: No source file specified${reset}')
		exit(1)
	}

	cmd_execute(source_file, envs, artifacts, network, vcpu, api_key)
}
