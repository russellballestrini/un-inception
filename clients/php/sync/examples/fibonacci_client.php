#!/usr/bin/env php
<?php
/**
 * Example: Execute JavaScript Fibonacci code using the unsandbox PHP SDK
 *
 * Prerequisites:
 *   - Set UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY environment variables
 *   - Or create ~/.unsandbox/accounts.csv with credentials
 *
 * Expected output (approximate):
 *   Executing JavaScript Fibonacci...
 *   Result:
 *   array(5) {
 *     ["status"]=> string(9) "completed"
 *     ["stdout"]=> string(...) "fib(10) = 55\nfib(20) = 6765\n"
 *     ["stderr"]=> string(0) ""
 *     ["exit_code"]=> int(0)
 *     ["runtime_ms"]=> int(...)
 *   }
 */

require_once __DIR__ . '/../src/un.php';

use Unsandbox\Unsandbox;
use Unsandbox\CredentialsException;
use Unsandbox\ApiException;

$jsCode = <<<'JS'
function fib(n) {
    if (n <= 1) return n;
    return fib(n - 1) + fib(n - 2);
}

console.log("fib(10) = " + fib(10));
console.log("fib(20) = " + fib(20));
JS;

echo "Executing JavaScript Fibonacci...\n";

try {
    $client = new Unsandbox();
    $result = $client->executeCode('javascript', $jsCode);

    echo "Result:\n";
    var_dump($result);
} catch (CredentialsException $e) {
    echo "Credentials error: " . $e->getMessage() . "\n";
    exit(1);
} catch (ApiException $e) {
    echo "API error: " . $e->getMessage() . " (code: " . $e->getCode() . ")\n";
    exit(1);
}
