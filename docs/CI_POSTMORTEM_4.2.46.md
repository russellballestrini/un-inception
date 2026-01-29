# CI Pipeline Postmortem - Release 4.2.46 (2026-01-29)

## Summary

Release 4.2.46 triggered the full 42-language test matrix. **31 of 43 test jobs failed** (72% failure rate). Root cause analysis revealed three distinct failure categories, none of which indicate actual SDK bugs.

## Impact

- **12 languages passed**: python, ruby, bash, cpp, fortran, d, nim, v, crystal, fsharp, haskell, prolog
- **31 languages failed**: All others
- Pipeline status reported "success" because test jobs have `allow_failure: true`

## Root Causes

### 1. QR Test Missing Dependencies (23 failures)

**What**: QR code generation tests (`qr_generate`) require language-specific libraries (`qrcode` npm, python `qrcode`, etc.) that are not installed in the sandbox environment.

**Why it failed**: The test treated missing dependencies as a test FAIL rather than a SKIP. The QR test files (e.g., `test/qr.js`) do `require('qrcode')` which fails with `Cannot find module 'qrcode'` in the sandbox.

**Why python passed**: The `qrcode` pip package happens to be pre-installed in the sandbox image.

**Fix in 4.2.48**: QR test now detects missing dependency errors and reports SKIP instead of FAIL. This is honest - we can't test what we can't run.

### 2. Service Operations 5xx Errors (13 failures)

**What**: `service_destroy` and `service_exec` tests failed with HTTP 5xx errors from the API, even after retry exhaustion.

**Root cause - pool imbalance**: The cluster has two pool hosts:
- **Cammy** (`cammy-foxhop-net`): 32 vCPUs, load 37.96 (118% utilization), **21 services**
- **AI** (`ai-foxhop-net`): 32 vCPUs, load 9.26 (29% utilization), **0 services**

All 21 services were placed on Cammy while AI sat idle. The API's service placement algorithm appears to always select the first available pool rather than load-balancing across pools.

**Why `service_destroy` was especially bad**: A 5xx on destroy could mean the service was actually destroyed but the response was lost. The next retry would get "Service not found" which didn't match the success pattern `destroy|deleted|success`, and also didn't match any transient error pattern, so it fell into the "no-match" retry path (only 3 attempts, 2s delay).

**Fixes in 4.2.48**:
- `service_destroy` now accepts "not found" as success (service was already destroyed)
- Backoff cap increased from 10s to 30s
- No-match retry increased from 3 to 5 attempts with 3s delay
- Added nginx error messages to 5xx detection (bad gateway, service unavailable, gateway timeout)

**Remaining**: The pool imbalance is an API-side issue that needs a load-balancing fix in `api.unsandbox.com`.

### 3. Inline Code Format Issues (5 failures)

**What**: `exec_basic` failed for erlang, clojure, kotlin, objc, and deno. These languages can't run their "hello world" code via `build/un -s LANG 'code'`.

**Why**: The hello code format in `test-sdk.sh` doesn't match what the sandbox expects:
- **Erlang**: Module-based syntax (`-module(main).`) doesn't work for inline execution via escript
- **Kotlin**: `fun main() { println("test-ok") }` needs a `.kt` file, not inline
- **Deno**: "deno" isn't a valid language name for the API's `-s` flag
- **Clojure**: Inline format incompatible with sandbox's Clojure runner
- **Obj-C**: Requires compilation, can't run inline

**Status**: Not fixed in 4.2.48. These need per-language hello code adjustments or acceptance that inline execution isn't supported for all languages.

## Failure Output Improvement

**Problem**: When tests failed, the output showed only `head -5` of the output file, which often missed the actual error. HTTP status codes (500, 502, 503) were not displayed.

**Fix in 4.2.48**: Failure output now shows:
1. Matched HTTP status codes and error messages (via grep)
2. First 5 lines of raw output
3. Clear section markers for readability

## Change Detection Bug

**Problem**: The version bump for 4.2.46 changed 14 files across 8 language directories, but the main branch pipeline only ran 8 SDK tests instead of the full matrix. A version bump is a release-level change that should trigger full testing.

**Root cause**: `detect-changes.sh` line 140 checked for `tests?/|scripts/|\.gitlab-ci\.yml|clients/\{.*\}/` but did NOT check for `VERSION` or `Makefile` changes. The `clients/\{.*\}/` pattern with literal braces was also a bug - it would only match paths like `clients/{template}/` which don't exist.

**Fix in 4.2.48**: Changed to `^(VERSION|Makefile|tests?/|scripts/|\.gitlab-ci\.yml)` - VERSION and Makefile changes now trigger full matrix.

## Timeline

| Time | Event |
|------|-------|
| 20:20 | 4.2.46 tag pushed, pipelines start |
| 20:25 | Child pipeline 13574 starts full 42-language matrix |
| 20:28 | Python passes 20/20 (first to complete) |
| 20:35 | First failures appear (service 5xx, QR deps) |
| 20:45 | Second wave failures as Cammy becomes more overloaded |
| 21:45 | Pipeline 13574 completes: 12 pass, 31 fail |
| 21:50 | Root cause analysis complete |
| 22:00 | Fixes committed, 4.2.48 tagged |

## Lessons Learned

1. **QR tests assumed dependencies exist** - sandbox doesn't have every language's QR library. Tests should detect and SKIP, not FAIL.
2. **Service placement needs load balancing** - a single overloaded host causes cascading failures across all test languages.
3. **Retry logic needs to understand operation semantics** - "not found" after a destroy attempt is success, not failure.
4. **Core file changes should trigger full matrix** - VERSION bumps are release events.
5. **Failure output must include HTTP status codes** - without them, diagnosing API issues requires manual log reading.
