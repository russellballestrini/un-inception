# UN-Inception CI/CD Pipeline

## Overview

The UN-Inception pipeline tests 42+ SDK implementations across 8 languages without requiring any language runtimes on the build server. It achieves this through **inception testing** - using the compiled C CLI to execute other SDKs through the unsandbox API.

```
Build Server (C only) → unsandbox API → SDK (Python/JS/etc) → unsandbox API → Test Code
```

## Pipeline Stages

```
┌─────────┐   ┌─────────┐   ┌─────────┐   ┌─────────────┐   ┌──────────┐   ┌──────────┐   ┌────────┐
│   PRE   │ → │  BUILD  │ → │  TEST   │ → │   SCIENCE   │ → │ VALIDATE │ → │ DOCUMENT │ → │ REPORT │
└─────────┘   └─────────┘   └─────────┘   └─────────────┘   └──────────┘   └──────────┘   └────────┘
```

### Stage 1: PRE

**detect-changes**
- Analyzes git diff to identify which SDKs changed
- Outputs `changes.json` with list of affected languages
- Enables selective testing (only test what changed)

**generate-matrix**
- Reads `changes.json`
- Generates `test-matrix.yml` with parallel test jobs
- Each changed SDK gets its own test job

### Stage 2: BUILD

**build**
- Compiles the C CLI binary (`build/un`)
- Dependencies: `gcc`, `libcurl`, `libwebsockets`, `libssl`
- The C binary is the **only** compiled artifact needed
- All other SDKs are interpreted and tested remotely

```bash
make -C clients/c cli
cp clients/c/un build/un
```

### Stage 3: TEST (Dynamic Matrix)

Generated dynamically based on changed SDKs. Each job runs inception tests:

```bash
# Example: Testing Python SDK
build/un -n semitrusted \
    -e UNSANDBOX_PUBLIC_KEY=$KEY \
    -e UNSANDBOX_SECRET_KEY=$SECRET \
    clients/python/sync/src/un.py --help

build/un -n semitrusted \
    -e UNSANDBOX_PUBLIC_KEY=$KEY \
    -e UNSANDBOX_SECRET_KEY=$SECRET \
    clients/python/sync/src/un.py -s python 'print("test")'
```

**What's happening:**
1. `build/un` (C CLI on build server) calls unsandbox API
2. unsandbox runs `un.py` in a container with network access
3. `un.py` calls the unsandbox API to execute test code
4. Results bubble back through both layers

### Stage 4: SCIENCE

Parallel jobs for extended validation:

**science-validate-examples**
- Runs example code from all SDKs
- Burns API pool credits for real-world testing

**science-lint-sdks**
- Static analysis across SDK implementations
- Checks for consistency and code quality

**science-benchmark-clients**
- Performance testing of SDK implementations
- Measures execution time and resource usage

### Stage 5: VALIDATE

**validate-examples**
- Aggregates results from science jobs
- Generates summary statistics
- Identifies failures across SDK matrix

### Stage 6: DOCUMENT

**generate-documentation**
- Auto-generates docs from test results
- Creates verification timestamps
- Updates SDK status information

### Stage 7: REPORT

**report**
- Final aggregation of all results
- Generates JUnit XML for GitLab integration
- Produces human-readable summary

## The Inception Pattern

```
┌─────────────────────────────────────────────────────────────────────┐
│ Build Server                                                        │
│ ┌─────────────────────────────────────────────────────────────────┐ │
│ │ build/un (C binary)                                             │ │
│ │   └─→ POST api.unsandbox.com/execute                            │ │
│ └─────────────────────────────────────────────────────────────────┘ │
└───────────────────────────────┬─────────────────────────────────────┘
                                │
                                ▼
┌─────────────────────────────────────────────────────────────────────┐
│ Unsandbox Container (semitrusted network)                           │
│ ┌─────────────────────────────────────────────────────────────────┐ │
│ │ un.py / un.js / un.rb / etc                                     │ │
│ │   └─→ POST api.unsandbox.com/execute                            │ │
│ └─────────────────────────────────────────────────────────────────┘ │
└───────────────────────────────┬─────────────────────────────────────┘
                                │
                                ▼
┌─────────────────────────────────────────────────────────────────────┐
│ Unsandbox Container (isolated)                                      │
│ ┌─────────────────────────────────────────────────────────────────┐ │
│ │ Test code execution                                             │ │
│ │   print("inception-test-ok")                                    │ │
│ └─────────────────────────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────────────────────┘
```

## Build Server Requirements

Minimal dependencies - only what's needed to compile C:

```bash
apt install build-essential libcurl4-openssl-dev libwebsockets-dev libssl-dev
```

**Not required:** Python, Ruby, PHP, Go, Java, Rust, Node.js, etc.

All language runtimes exist in unsandbox containers, not on the build server.

## Environment Variables

Required in GitLab CI/CD Settings → Variables:

| Variable | Description | Protected | Masked |
|----------|-------------|-----------|--------|
| `UNSANDBOX_PUBLIC_KEY` | API public key | Yes | Yes |
| `UNSANDBOX_SECRET_KEY` | API secret key | Yes | Yes |

## Artifacts

Each stage produces artifacts passed to subsequent stages:

| Job | Artifact | Purpose |
|-----|----------|---------|
| detect-changes | `changes.json` | List of changed SDKs |
| generate-matrix | `test-matrix.yml` | Dynamic test job definitions |
| build | `build/un` | Compiled C CLI |
| test-* | `test-results-*/` | Per-SDK test results |
| science-* | `*-results/` | Extended validation data |
| report | `final-report.xml` | JUnit results for GitLab |

## Triggering

Pipeline runs on:
- Push to `main` branch
- Tags matching `v*.*.*`

## Local Testing

Test the inception pattern locally:

```bash
# Build C CLI
make -C clients/c cli

# Test Python SDK through inception
./clients/c/un -n semitrusted \
    -e UNSANDBOX_PUBLIC_KEY=$UNSANDBOX_PUBLIC_KEY \
    -e UNSANDBOX_SECRET_KEY=$UNSANDBOX_SECRET_KEY \
    clients/python/sync/src/un.py -s python 'print("hello")'
```

## Files

```
.gitlab-ci.yml          # Pipeline definition
test-matrix.yml         # Placeholder (overwritten by generate-matrix)
scripts/
├── detect-changes.sh   # Identifies changed SDKs
├── generate-matrix.sh  # Creates dynamic test jobs
├── build-clients.sh    # Compiles C CLI
├── test-sdk.sh         # Runs inception tests
├── validate-examples.sh
├── filter-results.sh
└── science/
    ├── lint-all-sdks.sh
    └── benchmark-clients.sh
```
