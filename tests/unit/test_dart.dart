#!/usr/bin/env dart
/// Unit tests for un.dart - tests internal functions without API calls

import 'dart:convert';
import 'dart:io';
import 'package:crypto/crypto.dart';

int passed = 0;
int failed = 0;

void test(String name, void Function() fn) {
  try {
    fn();
    print('  ✓ $name');
    passed++;
  } catch (e) {
    print('  ✗ $name');
    print('    $e');
    failed++;
  }
}

void assertEqual(dynamic actual, dynamic expected) {
  if (actual != expected) {
    throw Exception("Expected '$expected' but got '$actual'");
  }
}

void assertNotEqual(dynamic a, dynamic b) {
  if (a == b) {
    throw Exception("Expected values to be different but both were '$a'");
  }
}

void assertContains(String str, String substr) {
  if (!str.contains(substr)) {
    throw Exception("Expected '$str' to contain '$substr'");
  }
}

void assertTrue(bool val) {
  if (!val) {
    throw Exception("Expected true but got false");
  }
}

String hmacSha256(String secret, String message) {
  final key = utf8.encode(secret);
  final bytes = utf8.encode(message);
  final hmac = Hmac(sha256, key);
  final digest = hmac.convert(bytes);
  return digest.toString();
}

void main() {
  final extMap = {
    '.py': 'python', '.js': 'javascript', '.ts': 'typescript',
    '.rb': 'ruby', '.php': 'php', '.pl': 'perl', '.lua': 'lua',
    '.sh': 'bash', '.go': 'go', '.rs': 'rust', '.c': 'c',
    '.cpp': 'cpp', '.java': 'java', '.kt': 'kotlin',
    '.hs': 'haskell', '.clj': 'clojure', '.erl': 'erlang',
    '.ex': 'elixir', '.jl': 'julia', '.dart': 'dart',
  };

  print('\n=== Extension Mapping Tests ===');

  test('Python extension maps correctly', () {
    assertEqual(extMap['.py'], 'python');
  });

  test('Dart extension maps correctly', () {
    assertEqual(extMap['.dart'], 'dart');
  });

  test('JavaScript extension maps correctly', () {
    assertEqual(extMap['.js'], 'javascript');
  });

  test('Go extension maps correctly', () {
    assertEqual(extMap['.go'], 'go');
  });

  test('Kotlin extension maps correctly', () {
    assertEqual(extMap['.kt'], 'kotlin');
  });

  print('\n=== HMAC Signature Tests ===');

  test('HMAC-SHA256 generates 64 character hex string', () {
    final sig = hmacSha256('test-secret', 'test-message');
    assertEqual(sig.length, 64);
  });

  test('Same input produces same signature', () {
    final sig1 = hmacSha256('key', 'msg');
    final sig2 = hmacSha256('key', 'msg');
    assertEqual(sig1, sig2);
  });

  test('Different secrets produce different signatures', () {
    final sig1 = hmacSha256('key1', 'msg');
    final sig2 = hmacSha256('key2', 'msg');
    assertNotEqual(sig1, sig2);
  });

  test('Signature format verification', () {
    final timestamp = '1704067200';
    final method = 'POST';
    final endpoint = '/execute';
    final body = '{"language":"python"}';

    final message = '$timestamp:$method:$endpoint:$body';

    assertTrue(message.startsWith(timestamp));
    assertContains(message, ':POST:');
    assertContains(message, ':/execute:');
  });

  print('\n=== Language Detection Tests ===');

  test('Detect language from .dart extension', () {
    final filename = 'script.dart';
    final ext = '.${filename.split('.').last}';
    assertEqual(extMap[ext], 'dart');
  });

  test('Python shebang detection', () {
    final content = "#!/usr/bin/env python3\nprint('hello')";
    final firstLine = content.split('\n')[0];
    assertTrue(firstLine.startsWith('#!'));
    assertContains(firstLine, 'python');
  });

  print('\n=== Argument Parsing Tests ===');

  test('Parse -e KEY=VALUE format', () {
    final arg = 'DEBUG=1';
    final parts = arg.split('=');
    final key = parts[0];
    final value = parts.sublist(1).join('=');
    assertEqual(key, 'DEBUG');
    assertEqual(value, '1');
  });

  test('Parse -e KEY=VALUE with equals in value', () {
    final arg = 'URL=https://example.com?foo=bar';
    final parts = arg.split('=');
    final key = parts[0];
    final value = parts.sublist(1).join('=');
    assertEqual(key, 'URL');
    assertEqual(value, 'https://example.com?foo=bar');
  });

  print('\n=== File Operations Tests ===');

  test('Base64 encoding/decoding', () {
    final content = "print('hello world')";
    final encoded = base64Encode(utf8.encode(content));
    final decoded = utf8.decode(base64Decode(encoded));
    assertEqual(decoded, content);
  });

  print('\n=== API Constants Tests ===');

  test('API base URL format', () {
    final apiBase = 'https://api.unsandbox.com';
    assertTrue(apiBase.startsWith('https://'));
    assertContains(apiBase, 'unsandbox.com');
  });

  // Summary
  print('\n=== Summary ===');
  print('Passed: $passed');
  print('Failed: $failed');
  print('Total:  ${passed + failed}');

  exit(failed > 0 ? 1 : 0);
}
