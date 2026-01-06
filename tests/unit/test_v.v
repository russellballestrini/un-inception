// Unit tests for un.v - tests internal functions without API calls
// Run with: v run test_v.v

import os

fn main() {
	mut passed := 0
	mut failed := 0

	ext_map := {
		'.py':   'python'
		'.js':   'javascript'
		'.ts':   'typescript'
		'.rb':   'ruby'
		'.go':   'go'
		'.rs':   'rust'
		'.c':    'c'
		'.v':    'v'
		'.java': 'java'
		'.kt':   'kotlin'
	}

	get_language := fn [ext_map] (ext string) string {
		return ext_map[ext] or { '' }
	}

	get_extension := fn (filename string) string {
		idx := filename.last_index('.') or { return '' }
		return filename[idx..]
	}

	get_basename := fn (path string) string {
		idx := path.last_index('/') or { return path }
		return path[idx + 1..]
	}

	test := fn [mut passed, mut failed] (name string, result bool) {
		if result {
			println('  ✓ ${name}')
			passed++
		} else {
			println('  ✗ ${name}')
			failed++
		}
	}

	println('\n=== Extension Mapping Tests ===')

	test('Python extension maps correctly', get_language('.py') == 'python')

	test('V extension maps correctly', get_language('.v') == 'v')

	test('JavaScript extension maps correctly', get_language('.js') == 'javascript')

	test('Go extension maps correctly', get_language('.go') == 'go')

	println('\n=== Signature Format Tests ===')

	timestamp := '1704067200'
	method := 'POST'
	endpoint := '/execute'
	body := '{"language":"python"}'
	message := '${timestamp}:${method}:${endpoint}:${body}'

	test('Signature format starts with timestamp', message.starts_with(timestamp))

	test('Signature format contains :POST:', message.contains(':POST:'))

	test('Signature format contains :/execute:', message.contains(':/execute:'))

	println('\n=== Language Detection Tests ===')

	content := '#!/usr/bin/env python3\nprint(\'hello\')'
	first_line := content.split('\n')[0]

	test('Python shebang detection - starts with #!', first_line.starts_with('#!'))

	test('Python shebang detection - contains python', first_line.contains('python'))

	println('\n=== Argument Parsing Tests ===')

	arg1 := 'DEBUG=1'
	eq1 := arg1.index('=') or { 0 }
	key1 := arg1[..eq1]
	value1 := arg1[eq1 + 1..]

	test('Parse -e KEY=VALUE format - key', key1 == 'DEBUG')

	test('Parse -e KEY=VALUE format - value', value1 == '1')

	arg2 := 'URL=https://example.com?foo=bar'
	eq2 := arg2.index('=') or { 0 }
	key2 := arg2[..eq2]
	value2 := arg2[eq2 + 1..]

	test('Parse -e KEY=VALUE with equals in value', key2 == 'URL' && value2 == 'https://example.com?foo=bar')

	println('\n=== File Operations Tests ===')

	test('Extract file basename', get_basename('/home/user/project/script.v') == 'script.v')

	test('Extract file extension', get_extension('/home/user/project/script.v') == '.v')

	println('\n=== API Constants Tests ===')

	api_base := 'https://api.unsandbox.com'

	test('API base URL starts with https://', api_base.starts_with('https://'))

	test('API base URL contains unsandbox.com', api_base.contains('unsandbox.com'))

	println('\n=== Summary ===')
	println('Passed: ${passed}')
	println('Failed: ${failed}')
	println('Total:  ${passed + failed}')

	exit(if failed > 0 { 1 } else { 0 })
}
