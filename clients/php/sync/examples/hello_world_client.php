#!/usr/bin/env php
<?php
/**
 * Hello World Client example - standalone version
 *
 * Demonstrates basic code execution patterns.
 * Shows how to execute code from a PHP program (simulated).
 *
 * To run:
 *   php hello_world_client.php
 *
 * Expected output:
 *   Executing Python code...
 *   Result status: completed
 *   Output: Hello from Python!
 */

echo "Executing Python code...\n";

// Simulated result (would normally call API)
$status = "completed";
$stdout = "Hello from Python!\n";

// Print result
echo "Result status: " . $status . "\n";
echo "Output: " . trim($stdout) . "\n";
