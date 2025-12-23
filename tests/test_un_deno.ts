#!/usr/bin/env -S deno run --allow-read --allow-env --allow-run
// Test suite for un_deno.ts (Deno TypeScript implementation)

const SCRIPT_DIR = new URL(".", import.meta.url).pathname;
const UN_DENO = `${SCRIPT_DIR}../un_deno.ts`;
const TEST_DIR = `${SCRIPT_DIR}../../test`;

// Colors
const RED = "\x1b[0;31m";
const GREEN = "\x1b[0;32m";
const YELLOW = "\x1b[1;33m";
const BLUE = "\x1b[0;34m";
const NC = "\x1b[0m"; // No Color

// Test counters
let testsRun = 0;
let testsPassed = 0;
let testsFailed = 0;

// Test result tracking
function testPassed(name: string) {
  testsPassed++;
  testsRun++;
  console.log(`${GREEN}✓ PASS${NC}: ${name}`);
}

function testFailed(name: string, error: string = "") {
  testsFailed++;
  testsRun++;
  console.log(`${RED}✗ FAIL${NC}: ${name}`);
  if (error) {
    console.log(`${RED}  Error: ${error}${NC}`);
  }
}

function testSkipped(name: string) {
  console.log(`${YELLOW}⊘ SKIP${NC}: ${name}`);
}

// Helper to run command and capture output
async function runCommand(cmd: string[]): Promise<{ exitCode: number; output: string }> {
  try {
    const process = new Deno.Command(cmd[0], {
      args: cmd.slice(1),
      stdout: "piped",
      stderr: "piped",
    });

    const { code, stdout, stderr } = await process.output();
    const output = new TextDecoder().decode(stdout) + new TextDecoder().decode(stderr);
    return { exitCode: code, output };
  } catch (error) {
    return { exitCode: 1, output: String(error) };
  }
}

// Unit Tests
console.log(`${BLUE}=== Unit Tests for un_deno.ts ===${NC}`);

// Test: Script exists and is executable
try {
  const stat = await Deno.stat(UN_DENO);
  if (stat.isFile && (stat.mode! & 0o111) !== 0) {
    testPassed("Script exists and is executable");
  } else {
    testFailed("Script exists and is executable", "File not executable");
  }
} catch {
  testFailed("Script exists and is executable", "File not found");
}

// Test: Usage message when no arguments
{
  const { exitCode, output } = await runCommand([UN_DENO]);
  if (exitCode !== 0 && output.includes("Usage:")) {
    testPassed("Shows usage message with no arguments");
  } else {
    testFailed("Shows usage message with no arguments", "Expected usage message");
  }
}

// Test: Error on non-existent file
{
  const { exitCode, output } = await runCommand([UN_DENO, "/tmp/nonexistent_file_12345.xyz"]);
  if (exitCode !== 0 && output.includes("not found")) {
    testPassed("Handles non-existent file");
  } else {
    testFailed("Handles non-existent file", "Expected 'not found' message");
  }
}

// Test: Error on unknown extension
{
  const unknownFile = `/tmp/test_unknown_ext_${Deno.pid}.unknownext`;
  await Deno.writeTextFile(unknownFile, "test");

  const { exitCode, output } = await runCommand([UN_DENO, unknownFile]);

  try {
    await Deno.remove(unknownFile);
  } catch {
    // Ignore cleanup errors
  }

  if (exitCode !== 0 && output.includes("Unknown file extension")) {
    testPassed("Handles unknown file extension");
  } else {
    testFailed("Handles unknown file extension", "Expected 'Unknown file extension' message");
  }
}

// Test: Error when API key not set
if (Deno.env.get("UNSANDBOX_API_KEY")) {
  const testFile = `${TEST_DIR}/fib.py`;
  try {
    await Deno.stat(testFile);

    // Temporarily unset API key
    const oldKey = Deno.env.get("UNSANDBOX_API_KEY");
    Deno.env.delete("UNSANDBOX_API_KEY");

    const { exitCode, output } = await runCommand([UN_DENO, testFile]);

    if (oldKey) {
      Deno.env.set("UNSANDBOX_API_KEY", oldKey);
    }

    if (exitCode !== 0 && output.includes("UNSANDBOX_API_KEY")) {
      testPassed("Requires API key");
    } else {
      testFailed("Requires API key", "Expected API key error message");
    }
  } catch {
    testSkipped("Requires API key (test file not found)");
  }
} else {
  testSkipped("Requires API key (API key already not set)");
}

// Integration Tests (require API key)
if (Deno.env.get("UNSANDBOX_API_KEY")) {
  console.log(`\n${BLUE}=== Integration Tests for un_deno.ts ===${NC}`);

  // Test: Can execute Python file
  {
    const fibPy = `${TEST_DIR}/fib.py`;
    try {
      await Deno.stat(fibPy);

      const { exitCode, output } = await runCommand([UN_DENO, fibPy]);

      if (exitCode === 0 && output.includes("fib(10)")) {
        testPassed("Executes Python file successfully");
      } else {
        testFailed("Executes Python file successfully", "Expected fibonacci output");
      }
    } catch {
      testSkipped("Executes Python file successfully (fib.py not found)");
    }
  }

  // Test: Can execute Bash file
  {
    const fibSh = `${TEST_DIR}/fib.sh`;
    try {
      await Deno.stat(fibSh);

      const { exitCode, output } = await runCommand([UN_DENO, fibSh]);

      if (exitCode === 0 && output.includes("fib(10)")) {
        testPassed("Executes Bash file successfully");
      } else {
        testFailed("Executes Bash file successfully", "Expected fibonacci output");
      }
    } catch {
      testSkipped("Executes Bash file successfully (fib.sh not found)");
    }
  }
} else {
  console.log(`\n${YELLOW}Skipping integration tests (UNSANDBOX_API_KEY not set)${NC}`);
}

// Summary
console.log(`\n${BLUE}=== Test Summary ===${NC}`);
console.log(`Total: ${testsRun} | Passed: ${testsPassed} | Failed: ${testsFailed}`);

if (testsFailed === 0) {
  console.log(`${GREEN}All tests passed!${NC}`);
  Deno.exit(0);
} else {
  console.log(`${RED}Some tests failed!${NC}`);
  Deno.exit(1);
}
