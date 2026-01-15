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

const api_base = 'https://api.unsandbox.com'
const portal_base = 'https://unsandbox.com'
const max_env_content_size = 65536
const blue = '\x1b[34m'
const red = '\x1b[31m'
const green = '\x1b[32m'
const yellow = '\x1b[33m'
const reset = '\x1b[0m'

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

fn exec_curl(cmd string) string {
	result := os.execute(cmd)
	output := result.output

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
	// Write body to temp file to avoid shell escaping issues
	body_file := '/tmp/unsandbox_env_body.txt'
	os.write_file(body_file, body) or {
		eprintln('${red}Error: Cannot write temp file${reset}')
		return false
	}
	defer {
		os.rm(body_file) or {}
	}

	cmd := "TIMESTAMP=\$(date +%s); MESSAGE=\"\$TIMESTAMP:PUT:${endpoint}:${body}\"; SIGNATURE=\$(echo -n \"\$MESSAGE\" | openssl dgst -sha256 -hmac '${secret_key}' -hex | sed 's/.*= //'); curl -s -X PUT '${api_base}${endpoint}' -H 'Content-Type: text/plain' -H 'Authorization: Bearer ${public_key}' -H \"X-Timestamp: \$TIMESTAMP\" -H \"X-Signature: \$SIGNATURE\" --data-binary @${body_file}"
	result := os.execute(cmd)
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
			cmd := "TIMESTAMP=\$(date +%s); MESSAGE=\"\$TIMESTAMP:GET:/services/${target}/env:\"; SIGNATURE=\$(echo -n \"\$MESSAGE\" | openssl dgst -sha256 -hmac '${secret_key}' -hex | sed 's/.*= //'); curl -s -X GET '${api_base}/services/${target}/env' -H 'Authorization: Bearer ${pub_key}' -H \"X-Timestamp: \$TIMESTAMP\" -H \"X-Signature: \$SIGNATURE\""
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
			cmd := "TIMESTAMP=\$(date +%s); MESSAGE=\"\$TIMESTAMP:POST:/services/${target}/env/export:\"; SIGNATURE=\$(echo -n \"\$MESSAGE\" | openssl dgst -sha256 -hmac '${secret_key}' -hex | sed 's/.*= //'); curl -s -X POST '${api_base}/services/${target}/env/export' -H 'Authorization: Bearer ${pub_key}' -H \"X-Timestamp: \$TIMESTAMP\" -H \"X-Signature: \$SIGNATURE\""
			println(exec_curl(cmd))
		}
		'delete' {
			cmd := "TIMESTAMP=\$(date +%s); MESSAGE=\"\$TIMESTAMP:DELETE:/services/${target}/env:\"; SIGNATURE=\$(echo -n \"\$MESSAGE\" | openssl dgst -sha256 -hmac '${secret_key}' -hex | sed 's/.*= //'); curl -s -X DELETE '${api_base}/services/${target}/env' -H 'Authorization: Bearer ${pub_key}' -H \"X-Timestamp: \$TIMESTAMP\" -H \"X-Signature: \$SIGNATURE\""
			exec_curl(cmd)
			println('${green}Vault deleted for service ${target}${reset}')
		}
		else {
			eprintln('${red}Error: Unknown env action: ${action}${reset}')
			eprintln('Usage: un service env <status|set|export|delete> <service_id>')
		}
	}
}

fn cmd_key(extend bool, api_key string) {
	pub_key := get_public_key()
	secret_key := get_secret_key()
	body := '{}'
	cmd := "BODY='${body}'; TIMESTAMP=\$(date +%s); MESSAGE=\"\$TIMESTAMP:POST:/keys/validate:\$BODY\"; SIGNATURE=\$(echo -n \"\$MESSAGE\" | openssl dgst -sha256 -hmac '${secret_key}' -hex | sed 's/.*= //'); curl -s -X POST '${portal_base}/keys/validate' -H 'Content-Type: application/json' -H 'Authorization: Bearer ${pub_key}' -H \"X-Timestamp: \$TIMESTAMP\" -H \"X-Signature: \$SIGNATURE\" -d \"\$BODY\""
	result := exec_curl(cmd)

	public_key := extract_json_string(result, 'public_key')
	tier := extract_json_string(result, 'tier')
	status := extract_json_string(result, 'status')
	expires_at := extract_json_string(result, 'expires_at')
	time_remaining := extract_json_string(result, 'time_remaining')
	rate_limit := extract_json_string(result, 'rate_limit')
	burst := extract_json_string(result, 'burst')
	concurrency := extract_json_string(result, 'concurrency')
	expired := extract_json_string(result, 'expired')

	if extend && public_key != '' {
		url := '${portal_base}/keys/extend?pk=${public_key}'
		println('${blue}Opening browser to extend key...${reset}')

		// Try xdg-open (Linux), open (macOS), or start (Windows)
		mut opened := false
		xdg_result := os.execute('xdg-open "${url}"')
		if xdg_result.exit_code == 0 {
			opened = true
		}
		if !opened {
			mac_result := os.execute('open "${url}"')
			if mac_result.exit_code == 0 {
				opened = true
			}
		}
		if !opened {
			win_result := os.execute('cmd /c start "${url}"')
			if win_result.exit_code != 0 {
				eprintln('${red}Error: Could not open browser${reset}')
			}
		}
		return
	}

	if expired == 'true' {
		println('${red}Expired${reset}')
		println('Public Key: ${public_key}')
		println('Tier: ${tier}')
		if expires_at != '' {
			println('Expired: ${expires_at}')
		}
		println('${yellow}To renew: Visit https://unsandbox.com/keys/extend${reset}')
		exit(1)
	}

	// Valid key
	println('${green}Valid${reset}')
	println('Public Key: ${public_key}')
	if tier != '' {
		println('Tier: ${tier}')
	}
	if status != '' {
		println('Status: ${status}')
	}
	if expires_at != '' {
		println('Expires: ${expires_at}')
	}
	if time_remaining != '' {
		println('Time Remaining: ${time_remaining}')
	}
	if rate_limit != '' {
		println('Rate Limit: ${rate_limit}')
	}
	if burst != '' {
		println('Burst: ${burst}')
	}
	if concurrency != '' {
		println('Concurrency: ${concurrency}')
	}
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
	cmd := "BODY='${json}'; TIMESTAMP=\$(date +%s); MESSAGE=\"\$TIMESTAMP:POST:/execute:\$BODY\"; SIGNATURE=\$(echo -n \"\$MESSAGE\" | openssl dgst -sha256 -hmac '${secret_key}' -hex | sed 's/.*= //'); curl -s -X POST '${api_base}/execute' -H 'Content-Type: application/json' -H 'Authorization: Bearer ${pub_key}' -H \"X-Timestamp: \$TIMESTAMP\" -H \"X-Signature: \$SIGNATURE\" -d \"\$BODY\""
	println(exec_curl(cmd))
}

fn cmd_session(list bool, kill string, shell string, network string, vcpu int, tmux bool, screen bool, input_files []string, api_key string) {
	pub_key := get_public_key()
	secret_key := get_secret_key()

	if list {
		cmd := "TIMESTAMP=\$(date +%s); MESSAGE=\"\$TIMESTAMP:GET:/sessions:\"; SIGNATURE=\$(echo -n \"\$MESSAGE\" | openssl dgst -sha256 -hmac '${secret_key}' -hex | sed 's/.*= //'); curl -s -X GET '${api_base}/sessions' -H 'Authorization: Bearer ${pub_key}' -H \"X-Timestamp: \$TIMESTAMP\" -H \"X-Signature: \$SIGNATURE\""
		println(exec_curl(cmd))
		return
	}

	if kill != '' {
		cmd := "TIMESTAMP=\$(date +%s); MESSAGE=\"\$TIMESTAMP:DELETE:/sessions/${kill}:\"; SIGNATURE=\$(echo -n \"\$MESSAGE\" | openssl dgst -sha256 -hmac '${secret_key}' -hex | sed 's/.*= //'); curl -s -X DELETE '${api_base}/sessions/${kill}' -H 'Authorization: Bearer ${pub_key}' -H \"X-Timestamp: \$TIMESTAMP\" -H \"X-Signature: \$SIGNATURE\""
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
	cmd := "BODY='${json}'; TIMESTAMP=\$(date +%s); MESSAGE=\"\$TIMESTAMP:POST:/sessions:\$BODY\"; SIGNATURE=\$(echo -n \"\$MESSAGE\" | openssl dgst -sha256 -hmac '${secret_key}' -hex | sed 's/.*= //'); curl -s -X POST '${api_base}/sessions' -H 'Content-Type: application/json' -H 'Authorization: Bearer ${pub_key}' -H \"X-Timestamp: \$TIMESTAMP\" -H \"X-Signature: \$SIGNATURE\" -d \"\$BODY\""
	println(exec_curl(cmd))
}

fn cmd_service(name string, ports string, service_type string, bootstrap string, bootstrap_file string, list bool, info string, logs string, tail string, sleep string, wake string, destroy string, resize string, execute string, command string, dump_bootstrap string, dump_file string, network string, vcpu int, input_files []string, svc_envs []string, svc_env_file string, api_key string) {
	pub_key := get_public_key()
	secret_key := get_secret_key()

	if list {
		cmd := "TIMESTAMP=\$(date +%s); MESSAGE=\"\$TIMESTAMP:GET:/services:\"; SIGNATURE=\$(echo -n \"\$MESSAGE\" | openssl dgst -sha256 -hmac '${secret_key}' -hex | sed 's/.*= //'); curl -s -X GET '${api_base}/services' -H 'Authorization: Bearer ${pub_key}' -H \"X-Timestamp: \$TIMESTAMP\" -H \"X-Signature: \$SIGNATURE\""
		println(exec_curl(cmd))
		return
	}

	if info != '' {
		cmd := "TIMESTAMP=\$(date +%s); MESSAGE=\"\$TIMESTAMP:GET:/services/${info}:\"; SIGNATURE=\$(echo -n \"\$MESSAGE\" | openssl dgst -sha256 -hmac '${secret_key}' -hex | sed 's/.*= //'); curl -s -X GET '${api_base}/services/${info}' -H 'Authorization: Bearer ${pub_key}' -H \"X-Timestamp: \$TIMESTAMP\" -H \"X-Signature: \$SIGNATURE\""
		println(exec_curl(cmd))
		return
	}

	if logs != '' {
		cmd := "TIMESTAMP=\$(date +%s); MESSAGE=\"\$TIMESTAMP:GET:/services/${logs}/logs:\"; SIGNATURE=\$(echo -n \"\$MESSAGE\" | openssl dgst -sha256 -hmac '${secret_key}' -hex | sed 's/.*= //'); curl -s -X GET '${api_base}/services/${logs}/logs' -H 'Authorization: Bearer ${pub_key}' -H \"X-Timestamp: \$TIMESTAMP\" -H \"X-Signature: \$SIGNATURE\""
		print(exec_curl(cmd))
		return
	}

	if tail != '' {
		cmd := "TIMESTAMP=\$(date +%s); MESSAGE=\"\$TIMESTAMP:GET:/services/${tail}/logs:\"; SIGNATURE=\$(echo -n \"\$MESSAGE\" | openssl dgst -sha256 -hmac '${secret_key}' -hex | sed 's/.*= //'); curl -s -X GET '${api_base}/services/${tail}/logs?lines=9000' -H 'Authorization: Bearer ${pub_key}' -H \"X-Timestamp: \$TIMESTAMP\" -H \"X-Signature: \$SIGNATURE\""
		print(exec_curl(cmd))
		return
	}

	if sleep != '' {
		cmd := "TIMESTAMP=\$(date +%s); MESSAGE=\"\$TIMESTAMP:POST:/services/${sleep}/freeze:\"; SIGNATURE=\$(echo -n \"\$MESSAGE\" | openssl dgst -sha256 -hmac '${secret_key}' -hex | sed 's/.*= //'); curl -s -X POST '${api_base}/services/${sleep}/freeze' -H 'Authorization: Bearer ${pub_key}' -H \"X-Timestamp: \$TIMESTAMP\" -H \"X-Signature: \$SIGNATURE\""
		exec_curl(cmd)
		println('${green}Service frozen: ${sleep}${reset}')
		return
	}

	if wake != '' {
		cmd := "TIMESTAMP=\$(date +%s); MESSAGE=\"\$TIMESTAMP:POST:/services/${wake}/unfreeze:\"; SIGNATURE=\$(echo -n \"\$MESSAGE\" | openssl dgst -sha256 -hmac '${secret_key}' -hex | sed 's/.*= //'); curl -s -X POST '${api_base}/services/${wake}/unfreeze' -H 'Authorization: Bearer ${pub_key}' -H \"X-Timestamp: \$TIMESTAMP\" -H \"X-Signature: \$SIGNATURE\""
		exec_curl(cmd)
		println('${green}Service unfreezing: ${wake}${reset}')
		return
	}

	if destroy != '' {
		cmd := "TIMESTAMP=\$(date +%s); MESSAGE=\"\$TIMESTAMP:DELETE:/services/${destroy}:\"; SIGNATURE=\$(echo -n \"\$MESSAGE\" | openssl dgst -sha256 -hmac '${secret_key}' -hex | sed 's/.*= //'); curl -s -X DELETE '${api_base}/services/${destroy}' -H 'Authorization: Bearer ${pub_key}' -H \"X-Timestamp: \$TIMESTAMP\" -H \"X-Signature: \$SIGNATURE\""
		exec_curl(cmd)
		println('${green}Service destroyed: ${destroy}${reset}')
		return
	}

	if resize != '' {
		if vcpu < 1 || vcpu > 8 {
			eprintln('${red}Error: --resize requires --vcpu N (1-8)${reset}')
			exit(1)
		}
		json := '{"vcpu":${vcpu}}'
		cmd := "BODY='${json}'; TIMESTAMP=\$(date +%s); MESSAGE=\"\$TIMESTAMP:PATCH:/services/${resize}:\$BODY\"; SIGNATURE=\$(echo -n \"\$MESSAGE\" | openssl dgst -sha256 -hmac '${secret_key}' -hex | sed 's/.*= //'); curl -s -X PATCH '${api_base}/services/${resize}' -H 'Content-Type: application/json' -H 'Authorization: Bearer ${pub_key}' -H \"X-Timestamp: \$TIMESTAMP\" -H \"X-Signature: \$SIGNATURE\" -d \"\$BODY\""
		exec_curl(cmd)
		ram := vcpu * 2
		println('${green}Service resized to ${vcpu} vCPU, ${ram} GB RAM${reset}')
		return
	}

	if execute != '' {
		json := '{"command":"${escape_json(command)}"}'
		cmd := "BODY='${json}'; TIMESTAMP=\$(date +%s); MESSAGE=\"\$TIMESTAMP:POST:/services/${execute}/execute:\$BODY\"; SIGNATURE=\$(echo -n \"\$MESSAGE\" | openssl dgst -sha256 -hmac '${secret_key}' -hex | sed 's/.*= //'); curl -s -X POST '${api_base}/services/${execute}/execute' -H 'Content-Type: application/json' -H 'Authorization: Bearer ${pub_key}' -H \"X-Timestamp: \$TIMESTAMP\" -H \"X-Signature: \$SIGNATURE\" -d \"\$BODY\""
		result := exec_curl(cmd)

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

	if dump_bootstrap != '' {
		eprintln('Fetching bootstrap script from ${dump_bootstrap}...')
		json := '{"command":"cat /tmp/bootstrap.sh"}'
		cmd := "BODY='${json}'; TIMESTAMP=\$(date +%s); MESSAGE=\"\$TIMESTAMP:POST:/services/${dump_bootstrap}/execute:\$BODY\"; SIGNATURE=\$(echo -n \"\$MESSAGE\" | openssl dgst -sha256 -hmac '${secret_key}' -hex | sed 's/.*= //'); curl -s -X POST '${api_base}/services/${dump_bootstrap}/execute' -H 'Content-Type: application/json' -H 'Authorization: Bearer ${pub_key}' -H \"X-Timestamp: \$TIMESTAMP\" -H \"X-Signature: \$SIGNATURE\" -d \"\$BODY\""
		result := exec_curl(cmd)

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
		json += build_input_files_json(input_files)
		json += '}'

		println('${yellow}Creating service...${reset}')
		cmd := "BODY='${json}'; TIMESTAMP=\$(date +%s); MESSAGE=\"\$TIMESTAMP:POST:/services:\$BODY\"; SIGNATURE=\$(echo -n \"\$MESSAGE\" | openssl dgst -sha256 -hmac '${secret_key}' -hex | sed 's/.*= //'); curl -s -X POST '${api_base}/services' -H 'Content-Type: application/json' -H 'Authorization: Bearer ${pub_key}' -H \"X-Timestamp: \$TIMESTAMP\" -H \"X-Signature: \$SIGNATURE\" -d \"\$BODY\""
		result := exec_curl(cmd)
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

fn main() {
	mut api_key := get_public_key()

	if os.args.len < 2 {
		eprintln('Usage: ${os.args[0]} [options] <source_file>')
		eprintln('       ${os.args[0]} session [options]')
		eprintln('       ${os.args[0]} service [options]')
		eprintln('       ${os.args[0]} service env <action> <service_id> [options]')
		eprintln('       ${os.args[0]} key [--extend]')
		eprintln('')
		eprintln('Vault commands:')
		eprintln('  service env status <id>   Check vault status')
		eprintln('  service env set <id>      Set vault (-e KEY=VAL or --env-file FILE)')
		eprintln('  service env export <id>   Export vault contents')
		eprintln('  service env delete <id>   Delete vault')
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
			vcpu, input_files, svc_envs, svc_env_file, api_key)
		return
	}

	if os.args[1] == 'key' {
		mut extend := false

		mut i := 2
		for i < os.args.len {
			match os.args[i] {
				'--extend' { extend = true }
				'-k' {
					i++
					api_key = os.args[i]
				}
				else {}
			}
			i++
		}

		cmd_key(extend, api_key)
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
