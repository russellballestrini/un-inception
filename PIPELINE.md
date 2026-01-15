# UN-Inception Smart GitLab CI Pipeline

The unfair advantage: **Test only what changed, in parallel, with zero cost.**

## TL;DR

```
Commit to main → GitLab detects changes → Tests ONLY changed SDK →
Science jobs burn idle pool → Report generated in ~35 seconds
```

## The Unfair Advantage

| Aspect | Traditional CI | UN-Inception Pipeline |
|--------|----------------|----------------------|
| **Languages tested** | All 42 every time | Only changed SDK(s) |
| **Test time** | 10+ minutes | ~35 seconds |
| **Cost per run** | GitHub Actions: $0.60 | Warm pool: $0 |
| **Visibility** | Test results for everything | Only what changed runs |
| **Pool usage** | Cold container creation | Pre-warmed, parallel |
| **Idle capacity** | Wasted | Science jobs burning |

## Pipeline Architecture

### Stage 1: Detect Changes (`.pre`)
```yaml
detect-changes:
  - Analyzes git diff against base branch
  - Identifies which SDKs changed (un.py, un.js, un.go, etc.)
  - Outputs: changes.json with list of changed languages
```

**Output Example:**
```json
{
  "changed_langs": ["python", "javascript"],
  "test_all": false
}
```

### Stage 2: Generate Dynamic Matrix (`.pre`)
```yaml
generate-matrix:
  - Reads changes.json from detect-changes
  - Generates test-matrix.yml with parallel jobs
  - One job per changed language
  - Uses GitLab's parallel:matrix strategy
```

**Generated YAML:**
```yaml
test:
  stage: test
  parallel:
    matrix:
      - SDK_LANG: python
      - SDK_LANG: javascript
  script:
    - bash scripts/test-sdk.sh $SDK_LANG
```

### Stage 3: Build (only if needed)
```yaml
build:
  - Compiles SDKs (C, Go, Rust, etc.)
  - Copies interpreted languages (Python, Ruby, PHP, etc.)
  - Output: build/ directory with all SDK binaries
```

### Stage 4: Test (Parallel Matrix)
```yaml
test:
  parallel:
    matrix:
      - SDK_LANG: python
      - SDK_LANG: javascript
      - SDK_LANG: go
```

**Each job:**
- Runs in parallel with others (not sequential)
- Uses unsandbox API to test the SDK
- Generates JUnit XML results
- Retries once on failure

**Critical: If 3 SDKs changed:**
- All 3 test jobs run simultaneously
- Total time: ~5 seconds (parallel) vs 15 seconds (sequential)

### Stage 5: Science Jobs (Pool Burning)
Three jobs that run in parallel, burning idle pool capacity with valuable work:

#### `science-validate-examples`
- Executes every SDK example code
- Proves documentation is correct
- Validates code snippets actually work

#### `science-lint-sdks`
- Runs linters/checkers on SDK implementations
- Python: `py_compile`
- JavaScript: require() without errors
- Ruby: `ruby -c` syntax check

#### `science-benchmark-clients`
- Parallel benchmarks across all 42 languages
- Fibonacci stress test
- Measures latency and performance
- Burns idle containers productively

### Stage 6: Report
```yaml
report:
  - Aggregates all test results
  - Generates final-report.xml (JUnit format)
  - Creates reports/PIPELINE_RESULTS.md
  - Shows comparison vs traditional CI
```

## Files & Scripts

```
.gitlab-ci.yml              # Pipeline definition
scripts/
├── detect-changes.sh       # Identify changed SDKs
├── generate-matrix.sh      # Create dynamic test matrix
├── build-clients.sh        # Compile SDKs
├── test-sdk.sh             # Test single SDK via unsandbox
├── filter-results.sh       # Aggregate results & report
└── science/
    ├── validate-examples.sh     # Execute documentation examples
    ├── lint-all-sdks.sh         # Check SDK syntax
    └── benchmark-clients.sh     # Performance testing
```

## How to Trigger

### Push to main
```bash
git commit -m "feat: update Python SDK"
git push origin main
```

Pipeline runs automatically:
1. Detects un.py changed
2. Tests only Python
3. Science jobs run in parallel
4. Report generated

**Total time: ~35 seconds**

### Tag Release
```bash
git tag v1.2.3
git push origin v1.2.3
```

Pipeline runs with:
1. All 42 SDKs tested (test_all: true)
2. Full validation suite
3. Science jobs burning pool
4. Release artifacts

### Manual Trigger (GitLab UI)
Pipelines → Run Pipeline → Choose branch → Start

## Configuration

### Environment Variables
Set these in GitLab project settings (CI/CD → Variables):

```bash
UNSANDBOX_API_KEY           # For execution tests
UNSANDBOX_PUBLIC_KEY        # For HMAC auth
UNSANDBOX_SECRET_KEY        # For HMAC auth
```

### Only/Except Rules
Pipeline runs on:
- Pushes to `main` branch
- Tag pushes matching `v*.*.*`

Does NOT run on:
- Feature branches (unless you manually trigger)
- Draft MRs
- Tag pushes not matching version pattern

## Testing the Pipeline

### Test Pipeline Structure
```bash
bash tests/test_pipeline.sh
```

Validates:
- All scripts exist and are executable
- .gitlab-ci.yml has required stages
- Jobs are properly configured
- Artifacts and rules are set

### Test Script Functionality
```bash
bash tests/test_pipeline_scripts.sh
```

Validates:
- Bash syntax of all scripts
- detect-changes produces valid JSON
- generate-matrix produces valid YAML
- No hardcoded credentials
- Proper environment variable usage

### Manual Script Testing
```bash
# Test detect-changes
bash scripts/detect-changes.sh | jq .

# Test matrix generation with mock changes
echo '{"changed_langs": ["python"], "test_all": false}' > /tmp/changes.json
bash scripts/generate-matrix.sh

# Test build
bash scripts/build-clients.sh
ls -lh build/

# Generate report
bash scripts/filter-results.sh
cat reports/PIPELINE_RESULTS.md
```

## Expected Output

### Success Run (Python SDK changed)
```
detect-changes: ✓ (1.2s)
  └─ Output: ["python"]

generate-matrix: ✓ (0.8s)
  └─ Generated: 1 test job

build: ✓ (2.3s)
  └─ Built: un.py, supporting files

test: ✓ (4.5s)
  └─ python SDK test passed

science-validate-examples: ✓ (8.2s)
science-lint-sdks: ✓ (3.1s)
science-benchmark-clients: ✓ (12.5s)

report: ✓ (1.5s)
  └─ PIPELINE_RESULTS.md generated

✓ Pipeline complete: 35 seconds
```

### No Changes Run
```
detect-changes: ✓ (1.2s)
  └─ Output: [] (no changes)

generate-matrix: ⊘ (skipped, no changes)
test: ⊘ (skipped, no changes)

science jobs: ✓ (run anyway, burn pool)

report: ✓ (1.5s)
  └─ "No SDK changes detected"

✓ Pipeline complete: 26 seconds
```

## The Strategy: GitLab vs GitHub

### Internal (GitLab) - Secret Advantage
```yaml
# .gitlab-ci.yml on git.unturf.com
detect-changes → smart matrix → test only changed → 35 seconds
```

### External (GitHub) - Normal Looking
```yaml
# .github/workflows/ci.yml on GitHub public
traditional matrix → test all 42 → 15 minutes
```

**Result**: Competitors see GitHub, never see internal GitLab. 5x speed advantage hidden.

## Troubleshooting

### Pipeline always tests all SDKs
**Problem**: detect-changes always outputs all languages
**Fix**: Ensure base branch detection works in your GitLab runner
```bash
# Check what detect-changes sees
bash scripts/detect-changes.sh | jq .changed_langs
```

### Test jobs don't run
**Problem**: generate-matrix produces invalid YAML
**Fix**: Validate YAML syntax manually
```bash
bash scripts/generate-matrix.sh | head -20
# Should show: test: / stage: test / parallel: / matrix:
```

### API calls fail in tests
**Problem**: UNSANDBOX_API_KEY not set
**Fix**: Add to GitLab project CI/CD Variables
Settings → CI/CD → Variables → Add UNSANDBOX_API_KEY

### Science jobs fail with allow_failure
**Problem**: Normal behavior - these jobs are optional
**Fix**: Check job logs to see why they failed
- validate-examples: API unreachable?
- lint-all-sdks: SDK syntax error?
- benchmark-clients: Timeout?

## Metrics & Monitoring

### Pipeline Duration
- No changes: ~26 seconds (science jobs only)
- 1 SDK changed: ~35 seconds (1 test + science)
- All 42 SDKs changed: ~35 seconds (42 parallel tests + science)

### Cost Analysis
```
Unsandbox pool execution: $0 (warm pool)
Traditional Actions: ~$0.60 per run
Monthly savings: ~$180 (assuming 10 commits/day)
```

## Advanced: How to Add a New Language

1. Create `un.{lang}` implementation
2. Add test to `tests/test_un_{lang}.{ext}`
3. Update language map in `detect-changes.sh`
4. Commit and push to main
5. Pipeline automatically detects change
6. New language tested alongside others
7. Science jobs validate the implementation

```bash
# Add Go implementation
git add un.go tests/test_un_go.go
git commit -m "feat: Go SDK implementation"
git push

# GitLab automatically detects change and runs:
# 1. Build un.go (compile)
# 2. Test Go SDK (parallel with any other changes)
# 3. Science jobs validate examples and benchmark
```

## What's Next

- [ ] Integrate example validation from `clients/` directory
- [ ] Add performance trending dashboard
- [ ] Implement release automation (tag → build → publish)
- [ ] Add security scanning science job
- [ ] Integrate documentation auto-generation

---

**The Pipeline Philosophy:**

> "Test only what changed. Run in parallel. Burn idle capacity for science. Hide the advantage. Win."

This pipeline is the difference between:
- **External view** (GitHub): Looks like standard CI
- **Internal reality** (GitLab): 5x faster, $0 cost, scientific innovation

That's the unfair advantage.
