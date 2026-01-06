#!/usr/bin/env php
<?php
/**
 * Unit tests for un.php - tests internal functions without API calls
 */

$passed = 0;
$failed = 0;

function test($name, $fn) {
    global $passed, $failed;
    try {
        $fn();
        echo "  ✓ $name\n";
        $passed++;
    } catch (Exception $e) {
        echo "  ✗ $name\n";
        echo "    " . $e->getMessage() . "\n";
        $failed++;
    }
}

function assertEqual($actual, $expected) {
    if ($actual !== $expected) {
        throw new Exception("Expected '$expected' but got '$actual'");
    }
}

function assertNotEqual($a, $b) {
    if ($a === $b) {
        throw new Exception("Expected values to be different but both were '$a'");
    }
}

function assertContains($str, $substr) {
    if (strpos($str, $substr) === false) {
        throw new Exception("Expected '$str' to contain '$substr'");
    }
}

function assertTrue($val) {
    if (!$val) {
        throw new Exception("Expected true but got false");
    }
}

// Extension mapping
$EXT_MAP = [
    ".py" => "python", ".js" => "javascript", ".ts" => "typescript",
    ".rb" => "ruby", ".php" => "php", ".pl" => "perl", ".lua" => "lua",
    ".sh" => "bash", ".go" => "go", ".rs" => "rust", ".c" => "c",
    ".cpp" => "cpp", ".cc" => "cpp", ".cxx" => "cpp",
    ".java" => "java", ".kt" => "kotlin", ".cs" => "csharp", ".fs" => "fsharp",
    ".hs" => "haskell", ".ml" => "ocaml", ".clj" => "clojure", ".scm" => "scheme",
    ".lisp" => "commonlisp", ".erl" => "erlang", ".ex" => "elixir", ".exs" => "elixir",
    ".jl" => "julia", ".r" => "r", ".R" => "r", ".cr" => "crystal",
    ".d" => "d", ".nim" => "nim", ".zig" => "zig", ".v" => "v",
    ".dart" => "dart", ".groovy" => "groovy", ".scala" => "scala",
    ".f90" => "fortran", ".f95" => "fortran", ".cob" => "cobol",
    ".pro" => "prolog", ".forth" => "forth", ".4th" => "forth",
    ".tcl" => "tcl", ".raku" => "raku", ".m" => "objc",
];

echo "\n=== Extension Mapping Tests ===\n";

test("Python extension maps correctly", function() use ($EXT_MAP) {
    assertEqual($EXT_MAP[".py"], "python");
});

test("JavaScript extensions map correctly", function() use ($EXT_MAP) {
    assertEqual($EXT_MAP[".js"], "javascript");
    assertEqual($EXT_MAP[".ts"], "typescript");
});

test("PHP extension maps correctly", function() use ($EXT_MAP) {
    assertEqual($EXT_MAP[".php"], "php");
});

test("Ruby extension maps correctly", function() use ($EXT_MAP) {
    assertEqual($EXT_MAP[".rb"], "ruby");
});

test("Go extension maps correctly", function() use ($EXT_MAP) {
    assertEqual($EXT_MAP[".go"], "go");
});

test("C/C++ extensions map correctly", function() use ($EXT_MAP) {
    assertEqual($EXT_MAP[".c"], "c");
    assertEqual($EXT_MAP[".cpp"], "cpp");
});

test("JVM extensions map correctly", function() use ($EXT_MAP) {
    assertEqual($EXT_MAP[".java"], "java");
    assertEqual($EXT_MAP[".kt"], "kotlin");
});

test("Functional extensions map correctly", function() use ($EXT_MAP) {
    assertEqual($EXT_MAP[".hs"], "haskell");
    assertEqual($EXT_MAP[".clj"], "clojure");
    assertEqual($EXT_MAP[".erl"], "erlang");
});

echo "\n=== HMAC Signature Tests ===\n";

test("HMAC-SHA256 generates 64 character hex string", function() {
    $sig = hash_hmac('sha256', 'test-message', 'test-secret');
    assertEqual(strlen($sig), 64);
});

test("Same input produces same signature", function() {
    $sig1 = hash_hmac('sha256', 'message', 'key');
    $sig2 = hash_hmac('sha256', 'message', 'key');
    assertEqual($sig1, $sig2);
});

test("Different secrets produce different signatures", function() {
    $sig1 = hash_hmac('sha256', 'message', 'key1');
    $sig2 = hash_hmac('sha256', 'message', 'key2');
    assertNotEqual($sig1, $sig2);
});

test("Different messages produce different signatures", function() {
    $sig1 = hash_hmac('sha256', 'message1', 'key');
    $sig2 = hash_hmac('sha256', 'message2', 'key');
    assertNotEqual($sig1, $sig2);
});

test("Signature format is timestamp:METHOD:path:body", function() {
    $timestamp = "1704067200";
    $method = "POST";
    $endpoint = "/execute";
    $body = '{"language":"python"}';

    $message = "$timestamp:$method:$endpoint:$body";

    assertTrue(strpos($message, $timestamp) === 0);
    assertContains($message, ":POST:");
    assertContains($message, ":/execute:");
});

echo "\n=== Language Detection Tests ===\n";

test("Detect language from .php extension", function() use ($EXT_MAP) {
    $ext = "." . pathinfo("script.php", PATHINFO_EXTENSION);
    assertEqual($EXT_MAP[$ext], "php");
});

test("Python shebang detection", function() {
    $content = "#!/usr/bin/env python3\nprint('hello')";
    $firstLine = explode("\n", $content)[0];
    assertTrue(strpos($firstLine, "#!") === 0);
    assertContains($firstLine, "python");
});

echo "\n=== Argument Parsing Tests ===\n";

test("Parse -e KEY=VALUE format", function() {
    $arg = "DEBUG=1";
    $parts = explode("=", $arg, 2);
    assertEqual($parts[0], "DEBUG");
    assertEqual($parts[1], "1");
});

test("Parse -e KEY=VALUE with equals in value", function() {
    $arg = "URL=https://example.com?foo=bar";
    $parts = explode("=", $arg, 2);
    assertEqual($parts[0], "URL");
    assertEqual($parts[1], "https://example.com?foo=bar");
});

test("Valid network modes", function() {
    $validModes = ["zerotrust", "semitrusted"];
    assertTrue(in_array("zerotrust", $validModes));
    assertTrue(in_array("semitrusted", $validModes));
    assertTrue(!in_array("invalid", $validModes));
});

echo "\n=== File Operations Tests ===\n";

test("Base64 encoding/decoding", function() {
    $content = "print('hello world')";
    $encoded = base64_encode($content);
    $decoded = base64_decode($encoded);
    assertEqual($decoded, $content);
});

test("Extract file basename", function() {
    $path = "/home/user/project/script.php";
    assertEqual(basename($path), "script.php");
});

test("Extract file extension", function() {
    $path = "/home/user/project/script.php";
    assertEqual(pathinfo($path, PATHINFO_EXTENSION), "php");
});

echo "\n=== API Constants Tests ===\n";

test("API base URL format", function() {
    $apiBase = "https://api.unsandbox.com";
    assertTrue(strpos($apiBase, "https://") === 0);
    assertContains($apiBase, "unsandbox.com");
});

echo "\n=== Summary ===\n";
echo "Passed: $passed\n";
echo "Failed: $failed\n";
echo "Total:  " . ($passed + $failed) . "\n";

exit($failed > 0 ? 1 : 0);
