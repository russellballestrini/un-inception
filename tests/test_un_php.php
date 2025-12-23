#!/usr/bin/env php
<?php
/**
 * Test suite for UN CLI PHP implementation (un.php)
 * Tests extension detection, API calls, and end-to-end functionality
 */

// Test configuration
define('UN_SCRIPT', __DIR__ . '/../un.php');
define('FIB_PY', __DIR__ . '/../../test/fib.py');

class TestResults {
    public $passed = 0;
    public $failed = 0;
    public $skipped = 0;

    public function passTest($name) {
        echo "PASS: {$name}\n";
        $this->passed++;
    }

    public function failTest($name, $error) {
        echo "FAIL: {$name} - {$error}\n";
        $this->failed++;
    }

    public function skipTest($name, $reason) {
        echo "SKIP: {$name} - {$reason}\n";
        $this->skipped++;
    }
}

$results = new TestResults();

// Extension map for testing
const EXTENSION_MAP = [
    '.py' => 'python', '.js' => 'javascript', '.ts' => 'typescript', '.rb' => 'ruby',
    '.php' => 'php', '.pl' => 'perl', '.lua' => 'lua', '.sh' => 'bash',
    '.go' => 'go', '.rs' => 'rust', '.c' => 'c', '.cpp' => 'cpp', '.cc' => 'cpp',
    '.java' => 'java', '.kt' => 'kotlin', '.cs' => 'csharp', '.hs' => 'haskell',
    '.ml' => 'ocaml', '.clj' => 'clojure', '.ex' => 'elixir', '.erl' => 'erlang',
    '.swift' => 'swift', '.r' => 'r', '.jl' => 'julia', '.dart' => 'dart',
    '.scala' => 'scala', '.groovy' => 'groovy', '.nim' => 'nim', '.cr' => 'crystal',
    '.v' => 'vlang', '.zig' => 'zig', '.fs' => 'fsharp', '.vb' => 'vb',
    '.pas' => 'pascal', '.f90' => 'fortran', '.asm' => 'assembly', '.d' => 'd',
    '.rkt' => 'racket', '.scm' => 'scheme', '.lisp' => 'common_lisp',
    '.sol' => 'solidity', '.cob' => 'cobol', '.ada' => 'ada', '.tcl' => 'tcl',
];

function detectLanguage($filename) {
    $ext = strtolower(pathinfo($filename, PATHINFO_EXTENSION));
    $ext = '.' . $ext;
    return EXTENSION_MAP[$ext] ?? null;
}

// Test 1: Extension detection for Python
try {
    $lang = detectLanguage('test.py');
    if ($lang === 'python') {
        $results->passTest('Extension detection: .py -> python');
    } else {
        $results->failTest('Extension detection: .py -> python', "Got {$lang}");
    }
} catch (Exception $e) {
    $results->failTest('Extension detection: .py -> python', $e->getMessage());
}

// Test 2: Extension detection for JavaScript
try {
    $lang = detectLanguage('test.js');
    if ($lang === 'javascript') {
        $results->passTest('Extension detection: .js -> javascript');
    } else {
        $results->failTest('Extension detection: .js -> javascript', "Got {$lang}");
    }
} catch (Exception $e) {
    $results->failTest('Extension detection: .js -> javascript', $e->getMessage());
}

// Test 3: Extension detection for Ruby
try {
    $lang = detectLanguage('test.rb');
    if ($lang === 'ruby') {
        $results->passTest('Extension detection: .rb -> ruby');
    } else {
        $results->failTest('Extension detection: .rb -> ruby', "Got {$lang}");
    }
} catch (Exception $e) {
    $results->failTest('Extension detection: .rb -> ruby', $e->getMessage());
}

// Test 4: Extension detection for Go
try {
    $lang = detectLanguage('test.go');
    if ($lang === 'go') {
        $results->passTest('Extension detection: .go -> go');
    } else {
        $results->failTest('Extension detection: .go -> go', "Got {$lang}");
    }
} catch (Exception $e) {
    $results->failTest('Extension detection: .go -> go', $e->getMessage());
}

// Test 5: Extension detection for Rust
try {
    $lang = detectLanguage('test.rs');
    if ($lang === 'rust') {
        $results->passTest('Extension detection: .rs -> rust');
    } else {
        $results->failTest('Extension detection: .rs -> rust', "Got {$lang}");
    }
} catch (Exception $e) {
    $results->failTest('Extension detection: .rs -> rust', $e->getMessage());
}

// Test 6: Extension detection for unknown extension
try {
    $lang = detectLanguage('test.unknown');
    if ($lang === null) {
        $results->passTest('Extension detection: .unknown -> null');
    } else {
        $results->failTest('Extension detection: .unknown -> null', "Got {$lang}");
    }
} catch (Exception $e) {
    $results->failTest('Extension detection: .unknown -> null', $e->getMessage());
}

// Test 7: API call test (requires UNSANDBOX_API_KEY)
if (!getenv('UNSANDBOX_API_KEY')) {
    $results->skipTest('API call test', 'UNSANDBOX_API_KEY not set');
} else {
    try {
        $payload = json_encode([
            'language' => 'python',
            'code' => 'print("Hello from API")'
        ]);

        $ch = curl_init('https://api.unsandbox.com/execute');
        curl_setopt_array($ch, [
            CURLOPT_POST => true,
            CURLOPT_POSTFIELDS => $payload,
            CURLOPT_RETURNTRANSFER => true,
            CURLOPT_HTTPHEADER => [
                'Authorization: Bearer ' . getenv('UNSANDBOX_API_KEY'),
                'Content-Type: application/json'
            ]
        ]);

        $response = curl_exec($ch);
        $httpCode = curl_getinfo($ch, CURLINFO_HTTP_CODE);
        curl_close($ch);

        if ($httpCode === 200) {
            $result = json_decode($response, true);
            if (isset($result['stdout']) && strpos($result['stdout'], 'Hello from API') !== false) {
                $results->passTest('API call test');
            } else {
                $results->failTest('API call test', "Unexpected result: " . json_encode($result));
            }
        } else {
            $results->failTest('API call test', "HTTP {$httpCode}: {$response}");
        }
    } catch (Exception $e) {
        $results->failTest('API call test', $e->getMessage());
    }
}

// Test 8: End-to-end test with fib.py
if (!getenv('UNSANDBOX_API_KEY')) {
    $results->skipTest('End-to-end fib.py test', 'UNSANDBOX_API_KEY not set');
} elseif (!file_exists(FIB_PY)) {
    $results->skipTest('End-to-end fib.py test', 'fib.py not found at ' . FIB_PY);
} else {
    try {
        $output = [];
        $returnVar = 0;
        exec(UN_SCRIPT . ' ' . escapeshellarg(FIB_PY) . ' 2>&1', $output, $returnVar);
        $stdout = implode("\n", $output);

        if (strpos($stdout, 'fib(10) = 55') !== false) {
            $results->passTest('End-to-end fib.py test');
        } else {
            $results->failTest('End-to-end fib.py test',
                "Expected 'fib(10) = 55' in output, got: " . substr($stdout, 0, 200));
        }
    } catch (Exception $e) {
        $results->failTest('End-to-end fib.py test', $e->getMessage());
    }
}

// Print summary
echo "\n" . str_repeat("=", 50) . "\n";
echo "Test Summary:\n";
echo "  PASSED:  {$results->passed}\n";
echo "  FAILED:  {$results->failed}\n";
echo "  SKIPPED: {$results->skipped}\n";
echo "  TOTAL:   " . ($results->passed + $results->failed + $results->skipped) . "\n";
echo str_repeat("=", 50) . "\n";

// Exit with appropriate code
exit($results->failed === 0 ? 0 : 1);
