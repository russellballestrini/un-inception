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

const (
	api_base = 'https://api.unsandbox.com'
	blue     = '\x1b[34m'
	red      = '\x1b[31m'
	green    = '\x1b[32m'
	yellow   = '\x1b[33m'
	reset    = '\x1b[0m'
)

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

fn exec_curl(cmd string) string {
	result := os.execute(cmd)
	return result.output
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

	cmd := "curl -s -X POST '${api_base}/execute' -H 'Content-Type: application/json' -H 'Authorization: Bearer ${api_key}' -d '${json}'"
	println(exec_curl(cmd))
}

fn cmd_session(list bool, kill string, shell string, network string, vcpu int, tmux bool, screen bool, api_key string) {
	if list {
		cmd := "curl -s -X GET '${api_base}/sessions' -H 'Authorization: Bearer ${api_key}'"
		println(exec_curl(cmd))
		return
	}

	if kill != '' {
		cmd := "curl -s -X DELETE '${api_base}/sessions/${kill}' -H 'Authorization: Bearer ${api_key}'"
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
	json += '}'

	println('${yellow}Creating session...${reset}')
	cmd := "curl -s -X POST '${api_base}/sessions' -H 'Content-Type: application/json' -H 'Authorization: Bearer ${api_key}' -d '${json}'"
	println(exec_curl(cmd))
}

fn cmd_service(name string, ports string, bootstrap string, list bool, info string, logs string, tail string, sleep string, wake string, destroy string, network string, vcpu int, api_key string) {
	if list {
		cmd := "curl -s -X GET '${api_base}/services' -H 'Authorization: Bearer ${api_key}'"
		println(exec_curl(cmd))
		return
	}

	if info != '' {
		cmd := "curl -s -X GET '${api_base}/services/${info}' -H 'Authorization: Bearer ${api_key}'"
		println(exec_curl(cmd))
		return
	}

	if logs != '' {
		cmd := "curl -s -X GET '${api_base}/services/${logs}/logs' -H 'Authorization: Bearer ${api_key}'"
		print(exec_curl(cmd))
		return
	}

	if tail != '' {
		cmd := "curl -s -X GET '${api_base}/services/${tail}/logs?lines=9000' -H 'Authorization: Bearer ${api_key}'"
		print(exec_curl(cmd))
		return
	}

	if sleep != '' {
		cmd := "curl -s -X POST '${api_base}/services/${sleep}/sleep' -H 'Authorization: Bearer ${api_key}'"
		exec_curl(cmd)
		println('${green}Service sleeping: ${sleep}${reset}')
		return
	}

	if wake != '' {
		cmd := "curl -s -X POST '${api_base}/services/${wake}/wake' -H 'Authorization: Bearer ${api_key}'"
		exec_curl(cmd)
		println('${green}Service waking: ${wake}${reset}')
		return
	}

	if destroy != '' {
		cmd := "curl -s -X DELETE '${api_base}/services/${destroy}' -H 'Authorization: Bearer ${api_key}'"
		exec_curl(cmd)
		println('${green}Service destroyed: ${destroy}${reset}')
		return
	}

	if name != '' {
		mut json := '{"name":"${name}"'
		if ports != '' {
			json += ',"ports":[${ports}]'
		}
		if bootstrap != '' {
			if os.exists(bootstrap) {
				boot_code := os.read_file(bootstrap) or { bootstrap }
				json += ',"bootstrap":"${escape_json(boot_code)}"'
			} else {
				json += ',"bootstrap":"${escape_json(bootstrap)}"'
			}
		}
		if network != '' {
			json += ',"network":"${network}"'
		}
		if vcpu > 0 {
			json += ',"vcpu":${vcpu}'
		}
		json += '}'

		println('${yellow}Creating service...${reset}')
		cmd := "curl -s -X POST '${api_base}/services' -H 'Content-Type: application/json' -H 'Authorization: Bearer ${api_key}' -d '${json}'"
		println(exec_curl(cmd))
		return
	}

	eprintln('${red}Error: Specify --name to create a service${reset}')
	exit(1)
}

fn main() {
	mut api_key := os.getenv('UNSANDBOX_API_KEY')

	if os.args.len < 2 {
		eprintln('Usage: ${os.args[0]} [options] <source_file>')
		eprintln('       ${os.args[0]} session [options]')
		eprintln('       ${os.args[0]} service [options]')
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
				else {}
			}
			i++
		}

		cmd_session(list, kill, shell, network, vcpu, tmux, screen, api_key)
		return
	}

	if os.args[1] == 'service' {
		mut name := ''
		mut ports := ''
		mut bootstrap := ''
		mut list := false
		mut info := ''
		mut logs := ''
		mut tail := ''
		mut sleep := ''
		mut wake := ''
		mut destroy := ''
		mut network := ''
		mut vcpu := 0

		mut i := 2
		for i < os.args.len {
			match os.args[i] {
				'--name' {
					i++
					name = os.args[i]
				}
				'--ports' {
					i++
					ports = os.args[i]
				}
				'--bootstrap' {
					i++
					bootstrap = os.args[i]
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
				'--sleep' {
					i++
					sleep = os.args[i]
				}
				'--wake' {
					i++
					wake = os.args[i]
				}
				'--destroy' {
					i++
					destroy = os.args[i]
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
				else {}
			}
			i++
		}

		cmd_service(name, ports, bootstrap, list, info, logs, tail, sleep, wake, destroy, network,
			vcpu, api_key)
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
				if !os.args[i].starts_with('-') {
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
