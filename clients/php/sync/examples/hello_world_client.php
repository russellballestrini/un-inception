#!/usr/bin/env php
<?php
/**
 * Example: Execute Python code using the unsandbox PHP SDK
 *
 * Prerequisites:
 *   - Set UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY environment variables
 *   - Or create ~/.unsandbox/accounts.csv with credentials
 *
 * Expected output (approximate):
 *   Executing Python code...
 *   Result:
 *   array(5) {
 *     ["status"]=> string(9) "completed"
 *     ["stdout"]=> string(20) "Hello from Python!\n"
 *     ["stderr"]=> string(0) ""
 *     ["exit_code"]=> int(0)
 *     ["runtime_ms"]=> int(...)
 *   }
 */

require_once __DIR__ . '/../src/un.php';

use Unsandbox\Unsandbox;
use Unsandbox\CredentialsException;
use Unsandbox\ApiException;

echo "Executing Python code...\n";

try {
    $client = new Unsandbox();
    $result = $client->executeCode('python', 'print("Hello from Python!")');

    echo "Result:\n";
    var_dump($result);
} catch (CredentialsException $e) {
    echo "Credentials error: " . $e->getMessage() . "\n";
    exit(1);
} catch (ApiException $e) {
    echo "API error: " . $e->getMessage() . " (code: " . $e->getCode() . ")\n";
    exit(1);
}
