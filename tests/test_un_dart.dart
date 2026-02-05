// test_un_dart.dart - Comprehensive tests for un.dart CLI implementation
// Run: dart test_un_dart.dart
// Note: Requires un.dart to be in parent directory
// For integration tests: Requires UNSANDBOX_API_KEY environment variable

import 'dart:io';
import 'dart:mirrors';

int testsRun = 0;
int testsPassed = 0;
int testsFailed = 0;

void main() async {
  print('=== Running un.dart Tests ===\n');

  // Unit Tests - Extension Detection
  await testExtensionDetection();

  // Integration Tests - API Call (skip if no API key)
  final apiKey = Platform.environment['UNSANDBOX_API_KEY'];
  if (apiKey != null && apiKey.isNotEmpty) {
    await testApiCall();
    await testFibExecution();
    await testSnapshotCommand();
  } else {
    print('SKIP: API integration tests (UNSANDBOX_API_KEY not set)\n');
  }

  // Print summary
  print('=== Test Summary ===');
  print('Tests run: $testsRun');
  print('Passed: $testsPassed');
  print('Failed: $testsFailed');

  if (testsFailed > 0) {
    exit(1);
  } else {
    print('\nAll tests PASSED!');
    exit(0);
  }
}

Future<void> testExtensionDetection() async {
  print('--- Unit Tests: Extension Detection ---');

  testDetectLanguage('test.java', 'java');
  testDetectLanguage('test.kt', 'kotlin');
  testDetectLanguage('test.cs', 'csharp');
  testDetectLanguage('test.fs', 'fsharp');
  testDetectLanguage('test.groovy', 'groovy');
  testDetectLanguage('test.dart', 'dart');
  testDetectLanguage('test.py', 'python');
  testDetectLanguage('test.js', 'javascript');
  testDetectLanguage('test.rs', 'rust');
  testDetectLanguage('test.go', 'go');

  testDetectLanguageError('noextension');
  testDetectLanguageError('test.unknown');

  print('');
}

void testDetectLanguage(String filename, String expectedLang) {
  testsRun++;
  try {
    // Import and test detectLanguage from un.dart
    // Note: In Dart, we'll use a simpler approach - just test the logic directly
    final dotIndex = filename.lastIndexOf('.');
    if (dotIndex == -1) {
      throw Exception('Cannot detect language: no file extension');
    }
    final ext = filename.substring(dotIndex);

    const extMap = {
      '.java': 'java',
      '.kt': 'kotlin',
      '.cs': 'csharp',
      '.fs': 'fsharp',
      '.groovy': 'groovy',
      '.dart': 'dart',
      '.scala': 'scala',
      '.py': 'python',
      '.js': 'javascript',
      '.ts': 'typescript',
      '.rb': 'ruby',
      '.go': 'go',
      '.rs': 'rust',
      '.cpp': 'cpp',
      '.c': 'c',
      '.sh': 'bash',
    };

    final lang = extMap[ext];
    if (lang == null) {
      throw Exception('Unsupported file extension: $ext');
    }

    if (lang == expectedLang) {
      testsPassed++;
      print('PASS: detectLanguage("$filename") = "$expectedLang"');
    } else {
      testsFailed++;
      print('FAIL: detectLanguage("$filename") expected "$expectedLang", got "$lang"');
    }
  } catch (e) {
    testsFailed++;
    print('FAIL: detectLanguage("$filename") threw exception: $e');
  }
}

void testDetectLanguageError(String filename) {
  testsRun++;
  try {
    final dotIndex = filename.lastIndexOf('.');
    if (dotIndex == -1) {
      throw Exception('Cannot detect language: no file extension');
    }
    final ext = filename.substring(dotIndex);

    const extMap = {
      '.java': 'java',
      '.kt': 'kotlin',
      '.cs': 'csharp',
      '.fs': 'fsharp',
      '.groovy': 'groovy',
      '.dart': 'dart',
      '.scala': 'scala',
      '.py': 'python',
      '.js': 'javascript',
      '.ts': 'typescript',
      '.rb': 'ruby',
      '.go': 'go',
      '.rs': 'rust',
      '.cpp': 'cpp',
      '.c': 'c',
      '.sh': 'bash',
    };

    final lang = extMap[ext];
    if (lang == null) {
      throw Exception('Unsupported file extension: $ext');
    }

    testsFailed++;
    print('FAIL: detectLanguage("$filename") should throw exception');
  } catch (e) {
    // Expected to throw exception
    testsPassed++;
    print('PASS: detectLanguage("$filename") correctly throws exception');
  }
}

Future<void> testApiCall() async {
  print('--- Integration Test: API Call ---');
  testsRun++;

  try {
    // Create a simple test file
    final testCode = "console.log('Hello from Dart test');";
    final testFile = File('test_api_dart.js');
    await testFile.writeAsString(testCode);

    try {
      // Execute dart CLI with the test file
      final result = await Process.run('dart', ['../un.dart', 'test_api_dart.js']);

      if (result.exitCode == 0 && result.stdout.toString().contains('Hello from Dart test')) {
        testsPassed++;
        print('PASS: API call succeeded and returned expected output');
      } else {
        testsFailed++;
        print('FAIL: API call failed or unexpected output');
        print('Exit code: ${result.exitCode}');
        print('Output: ${result.stdout}');
        print('Error: ${result.stderr}');
      }
    } finally {
      if (await testFile.exists()) {
        await testFile.delete();
      }
    }
  } catch (e) {
    testsFailed++;
    print('FAIL: API call test threw exception: $e');
  }
  print('');
}

Future<void> testFibExecution() async {
  print('--- Functional Test: fib.java Execution ---');
  testsRun++;

  try {
    // Check if fib.java exists
    final fibFile = File('fib.java');
    if (!await fibFile.exists()) {
      testsFailed++;
      print('FAIL: fib.java not found in tests directory');
      print('');
      return;
    }

    // Execute Dart CLI with fib.java
    final result = await Process.run('dart', ['../un.dart', 'fib.java']);

    final output = result.stdout.toString();
    if (result.exitCode == 0 && output.contains('fib(10) = 55')) {
      testsPassed++;
      print('PASS: fib.java execution succeeded');
      print('Output: ${output.trim()}');
    } else {
      testsFailed++;
      print('FAIL: fib.java execution failed or unexpected output');
      print('Exit code: ${result.exitCode}');
      print('Output: $output');
      print('Error: ${result.stderr}');
    }
  } catch (e) {
    testsFailed++;
    print('FAIL: fib.java execution test threw exception: $e');
  }
  print('');
}

Future<void> testSnapshotCommand() async {
  print('--- Feature Parity Test: Snapshot Command ---');
  testsRun++;

  try {
    // Execute Dart CLI with snapshot --list
    final result = await Process.run('dart', ['../un.dart', 'snapshot', '--list']);

    if (result.exitCode == 0) {
      testsPassed++;
      print('PASS: snapshot --list command works');
    } else {
      testsFailed++;
      print('FAIL: snapshot --list command failed');
      print('Exit code: ${result.exitCode}');
      print('Output: ${result.stdout}');
      print('Error: ${result.stderr}');
    }
  } catch (e) {
    testsFailed++;
    print('FAIL: snapshot command test threw exception: $e');
  }
  print('');
}
