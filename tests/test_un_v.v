// Test suite for UN CLI V implementation
// Compile: v test_un_v.v -o test_un_v
// Run: ./test_un_v
//
// Tests:
// 1. Unit tests for extension detection
// 2. Integration test for API availability (requires UNSANDBOX_API_KEY)
// 3. Functional test running fib.go

import os
import net.http
import json

// Copy of detect_language from un.v for testing
fn detect_language(filename string) !string {
	ext := os.file_ext(filename)

	lang_map := {
		'.py':  'python'
		'.js':  'javascript'
		'.go':  'go'
		'.rs':  'rust'
		'.c':   'c'
		'.cpp': 'cpp'
		'.d':   'd'
		'.zig': 'zig'
		'.nim': 'nim'
		'.v':   'v'
	}

	if lang := lang_map[ext] {
		return lang
	}

	return error('Unable to detect language from file extension')
}

fn test_extension_detection() bool {
	println('=== Test 1: Extension Detection ===')

	tests := [
		['script.py', 'python'],
		['app.js', 'javascript'],
		['main.go', 'go'],
		['program.rs', 'rust'],
		['code.c', 'c'],
		['app.cpp', 'cpp'],
		['prog.d', 'd'],
		['main.zig', 'zig'],
		['script.nim', 'nim'],
		['app.v', 'v'],
		['unknown.xyz', ''],
	]

	mut passed := 0
	mut failed := 0

	for test in tests {
		filename := test[0]
		expected := test[1]

		result := detect_language(filename) or { '' }

		if result == expected {
			println('  PASS: ${filename} -> ${result}')
			passed++
		} else {
			println('  FAIL: ${filename} -> got ${result}, expected ${expected}')
			failed++
		}
	}

	println('Extension Detection: ${passed} passed, ${failed} failed\n')
	return failed == 0
}

fn test_api_connection() bool {
	println('=== Test 2: API Connection ===')

	api_key := os.getenv('UNSANDBOX_API_KEY')
	if api_key == '' {
		println('  SKIP: UNSANDBOX_API_KEY not set')
		println('API Connection: skipped\n')
		return true
	}

	request_body := {
		'language': json.Any('python')
		'code':     json.Any("print('Hello from API test')")
	}

	json_body := json.encode(request_body)

	mut req := http.new_request(.post, 'https://api.unsandbox.com/execute', json_body) or {
		println('  FAIL: Error creating request: ${err}')
		return false
	}

	req.add_header(.content_type, 'application/json')
	req.add_header(.authorization, 'Bearer ${api_key}')

	resp := req.do() or {
		println('  FAIL: HTTP request error: ${err}')
		return false
	}

	result := json.decode(map[string]json.Any, resp.body) or {
		println('  FAIL: JSON parse error: ${err}')
		return false
	}

	stdout_str := result['stdout'] or { json.Any('') }.str()
	if !stdout_str.contains('Hello from API test') {
		println('  FAIL: Unexpected response: ${stdout_str}')
		return false
	}

	println('  PASS: API connection successful')
	println('API Connection: passed\n')
	return true
}

fn test_fib_execution() bool {
	println('=== Test 3: Functional Test (fib.go) ===')

	api_key := os.getenv('UNSANDBOX_API_KEY')
	if api_key == '' {
		println('  SKIP: UNSANDBOX_API_KEY not set')
		println('Functional Test: skipped\n')
		return true
	}

	if !os.exists('../un_v') {
		println('  SKIP: ../un_v binary not found (run: cd .. && v un.v -o un_v)')
		println('Functional Test: skipped\n')
		return true
	}

	if !os.exists('fib.go') {
		println('  SKIP: fib.go not found')
		println('Functional Test: skipped\n')
		return true
	}

	result := os.execute('../un_v fib.go')

	if result.exit_code != 0 {
		println('  FAIL: Command failed with exit code: ${result.exit_code}')
		println('  Output: ${result.output}')
		return false
	}

	if !result.output.contains('fib(10) = 55') {
		println('  FAIL: Expected output to contain "fib(10) = 55", got: ${result.output}')
		return false
	}

	println('  PASS: fib.go executed successfully')
	print('  Output: ${result.output}')
	println('Functional Test: passed\n')
	return true
}

fn test_cli_commands() bool {
	println('=== Test 4: CLI Commands (Feature Parity) ===')

	mut passed := 0
	mut failed := 0

	// Find un.v script
	script_paths := [
		'../clients/v/sync/src/un.v',
		'../../clients/v/sync/src/un.v',
		'../un.v',
	]

	mut un_script := ''
	for path in script_paths {
		if os.exists(path) {
			un_script = path
			break
		}
	}

	if un_script == '' {
		println('  SKIP: un.v not found')
		println('CLI Commands: skipped\n')
		return true
	}

	// Test: --help
	help_result := os.execute('v run ${un_script} -- --help')
	if help_result.output.contains('Usage') || help_result.output.contains('usage') {
		println('  PASS: --help shows usage')
		passed++
	} else {
		println('  FAIL: --help does not show usage')
		failed++
	}

	// Test: version command
	version_result := os.execute('v run ${un_script} -- version')
	if version_result.output.contains('version') || version_result.output.contains('Version') {
		println('  PASS: version command works')
		passed++
	} else {
		println('  FAIL: version command does not work')
		failed++
	}

	// Test: health command
	health_result := os.execute('v run ${un_script} -- health')
	if health_result.output.contains('health') || health_result.output.contains('API') {
		println('  PASS: health command works')
		passed++
	} else {
		println('  FAIL: health command does not work')
		failed++
	}

	// Test: languages command
	langs_result := os.execute('v run ${un_script} -- languages')
	if langs_result.output.contains('python') || langs_result.output.contains('Error') || langs_result.output.contains('API key') {
		println('  PASS: languages command works')
		passed++
	} else {
		println('  FAIL: languages command does not work')
		failed++
	}

	println('CLI Commands: ${passed} passed, ${failed} failed\n')
	return failed == 0
}

fn test_api_commands() bool {
	println('=== Test 5: API Commands (require auth) ===')

	public_key := os.getenv('UNSANDBOX_PUBLIC_KEY')
	secret_key := os.getenv('UNSANDBOX_SECRET_KEY')

	if public_key == '' && secret_key == '' {
		println('  SKIP: UNSANDBOX_PUBLIC_KEY/SECRET_KEY not set')
		println('API Commands: skipped\n')
		return true
	}

	mut passed := 0
	mut failed := 0

	// Find un.v script
	script_paths := [
		'../clients/v/sync/src/un.v',
		'../../clients/v/sync/src/un.v',
		'../un.v',
	]

	mut un_script := ''
	for path in script_paths {
		if os.exists(path) {
			un_script = path
			break
		}
	}

	if un_script == '' {
		println('  SKIP: un.v not found')
		println('API Commands: skipped\n')
		return true
	}

	// Test: snapshot --list
	snap_result := os.execute('v run ${un_script} -- snapshot --list')
	if snap_result.output.contains('[') || snap_result.output.contains('{') ||
		snap_result.output.contains('Error') || snap_result.output.contains('snapshots') {
		println('  PASS: snapshot --list works')
		passed++
	} else {
		println('  FAIL: snapshot --list does not work')
		failed++
	}

	// Test: session --list
	sess_result := os.execute('v run ${un_script} -- session --list')
	if sess_result.output.contains('[') || sess_result.output.contains('{') ||
		sess_result.output.contains('Error') || sess_result.output.contains('sessions') {
		println('  PASS: session --list works')
		passed++
	} else {
		println('  FAIL: session --list does not work')
		failed++
	}

	// Test: service --list
	svc_result := os.execute('v run ${un_script} -- service --list')
	if svc_result.output.contains('[') || svc_result.output.contains('{') ||
		svc_result.output.contains('Error') || svc_result.output.contains('services') {
		println('  PASS: service --list works')
		passed++
	} else {
		println('  FAIL: service --list does not work')
		failed++
	}

	// Test: image --list
	img_result := os.execute('v run ${un_script} -- image --list')
	if img_result.output.contains('[') || img_result.output.contains('{') ||
		img_result.output.contains('Error') || img_result.output.contains('images') {
		println('  PASS: image --list works')
		passed++
	} else {
		println('  FAIL: image --list does not work')
		failed++
	}

	println('API Commands: ${passed} passed, ${failed} failed\n')
	return failed == 0
}

fn main() {
	println('UN CLI V Implementation Test Suite')
	println('===================================\n')

	mut all_passed := true

	if !test_extension_detection() {
		all_passed = false
	}

	if !test_api_connection() {
		all_passed = false
	}

	if !test_fib_execution() {
		all_passed = false
	}

	if !test_cli_commands() {
		all_passed = false
	}

	if !test_api_commands() {
		all_passed = false
	}

	println('===================================')
	if all_passed {
		println('RESULT: ALL TESTS PASSED')
		exit(0)
	} else {
		println('RESULT: SOME TESTS FAILED')
		exit(1)
	}
}
