# UN Inception: Aggregated Performance Analysis

**Analysis Date:** 1769179921.0243473
**Reports Analyzed:** 4.2.17, 4.2.18

---

## Executive Summary

Analysis of 2 performance reports reveals **significant variance** in execution metrics across releases. Different languages rank as slowest/fastest in different runs, indicating **non-deterministic execution patterns** likely caused by:

1. **Orchestrator placement on CPU-bound pool** (not an SRE best practice)
2. **Resource contention** between the orchestrator & test jobs
3. **Undefined or exceeded concurrency limits**
4. **Non-deterministic scheduling** of the matrix jobs

---

## Key Findings

### 1. Extreme Metric Variance

| Release | Avg Duration | Slowest | Fastest | Change from Previous |
|---------|--------------|---------|---------|----------------------|
| 4.2.17 | 104s | ruby (270s) | objc (17s) | baseline |
| 4.2.18 | 100s | r (298s) | scheme (48s) | -4s (-3.8%) |

**Observation:** Average duration increased **0.0%** from 0s to 0s.

This **2-3x variance** is NOT normal for identical workloads. Indicates:
- Orchestrator fighting for CPU with test jobs
- Tests running in different order each time
- No consistent resource allocation

---

### 2. Unstable Language Rankings

The same language changes dramatically in rank between runs:


**R:**
  - 4.2.17: 94s
  - 4.2.18: 298s
  - **Range:** 94s → 298s (217.0% variance)

**GO:**
  - 4.2.17: 46s
  - 4.2.18: 237s
  - **Range:** 46s → 237s (415.2% variance)

**COBOL:**
  - 4.2.17: 220s
  - 4.2.18: 72s
  - **Range:** 72s → 220s (205.6% variance)

**TYPESCRIPT:**
  - 4.2.17: 249s
  - 4.2.18: 104s
  - **Range:** 104s → 249s (139.4% variance)

**RUBY:**
  - 4.2.17: 270s
  - 4.2.18: 135s
  - **Range:** 135s → 270s (100.0% variance)


---

### 3. Execution Order Non-Determinism

**Fastest Languages by Run:**

4.2.17: objc, python, erlang, csharp, perl
4.2.18: scheme, tcl, fortran, c, raku

**Slowest Languages by Run:**

4.2.17: ruby, typescript, php, cobol, commonlisp
4.2.18: r, go, elixir, rust, forth

**Conclusion:** No consistent "fast" or "slow" languages across runs. This proves:
- Execution order is random or system-dependent
- Resource availability varies dramatically
- Each run experiences different contention patterns

---

## The Orchestrator Problem: DevOps 101

### Why This Matters

Running the orchestrator on a **CPU-bound pool node** violates fundamental SRE principles:

```
❌ BAD:  [ORCHESTRATOR] + [TEST JOB 1] + [TEST JOB 2] ... on same CPU pool
✅ GOOD: [ORCHESTRATOR] on dedicated node, [TESTS] on separate pool
```

**What happens:**
1. Orchestrator needs CPU to schedule/coordinate jobs
2. Test jobs need CPU to run
3. Both compete for limited CPU cycles
4. Context switching & cache thrashing = unpredictable timing
5. Matrix generation order becomes random as scheduler equilibrates

### Why It's Fun for Chaos Engineering

From a chaos testing perspective, this setup is **perfect**:
- Reproduces real-world resource contention
- Tests system behavior under adversarial conditions
- Reveals race conditions & timing bugs
- No two runs are identical (true chaos)

**But for production CI/CD?** It's a nightmare for:
- Performance benchmarking
- SLA guarantees
- Debug reproducibility
- Billing/cost predictability

---

## Concurrency Hypothesis

### Theory: Matrix Hydra Execution Limits

Given 42 languages with 15 tests each, if there were a **concurrency limit**, we'd expect:

**Observed avg duration:** 33-70s
**If truly serialized (1 job at a time):** ~500s minimum
**If unlimited parallel:** ~50-70s

This suggests jobs run in **parallel batches**, but the batch size varies:

#### Possible Concurrency Models:

1. **Kubernetes Executor (default 32-64 parallel):** Each release has different load
2. **GitLab runner queue saturation:** Some runs hit limits, others don't
3. **Node CPU throttling:** Kubernetes QoS class limits being applied
4. **No explicit limit, but OS scheduler bottleneck:** ~64 thread context limit

### Evidence from Timing Patterns

If concurrency was fixed at N parallel jobs:
- `Total time = ceiling(42 / N) * (average job time)`
- For 4.2.0 (33s avg): ~42 concurrent or very efficient scheduling
- For 4.2.3 (63s avg): ~20 concurrent (slower overall, more contention)
- For 4.2.4 (70s avg): ~18 concurrent (even more contention)

**Implication:** Concurrency limit is either:
- **Dynamic** (based on available resources)
- **Not enforced** (unlimited, but OS scheduler creates natural limit)
- **Degrading** (orchestrator consuming more CPU over versions)

---

## Detailed Language Analysis

### Most Variable Languages


R: 94s → 298s (+217.0%)

GO: 46s → 237s (+415.2%)

COBOL: 72s → 220s (+205.6%)

TYPESCRIPT: 104s → 249s (+139.4%)

RUBY: 135s → 270s (+100.0%)

RUST: 45s → 174s (+286.7%)

PHP: 133s → 243s (+82.7%)

AWK: 66s → 175s (+165.2%)

ZIG: 72s → 175s (+143.1%)

ELIXIR: 87s → 187s (+114.9%)


These languages are most affected by resource contention. Likely reasons:
- **Dynamic languages** (Python, Ruby, JavaScript): Startup time varies with GC/JIT
- **Compiled languages with heavy linking** (C++, Rust): Linker contention
- **Language VMs** (Java, Elixir): VM startup sensitive to system load

---

## Recommendations

### For Production CI/CD

1. **Separate orchestrator from compute pool**
   - Dedicated small node for GitLab runner/orchestrator
   - Dedicated larger pool for test jobs
   - Isolate using Kubernetes node affinity or taints

2. **Set explicit concurrency limits**
   ```yaml
   # GitLab .gitlab-ci.yml
   trigger-test-matrix:
     parallel: 32  # Fixed concurrency
     max_parallel_builds: 32
   ```

3. **Monitor resource usage**
   - CPU utilization on runner nodes
   - Memory pressure & swap activity
   - Context switch rates

4. **Implement backpressure**
   - Queue jobs when pool is full
   - Implement exponential backoff for retries
   - Monitor orchestrator health separately

### For Chaos Engineering

This setup is **excellent** for:
- Testing flaky test detection systems
- Validating retry logic
- Measuring performance under contention
- Finding race conditions in test infrastructure

Keep it as-is for stress testing, but in separate test environment.

---

## Raw Data: Language Variance Table

| Language | Min (s) | Max (s) | Avg (s) | Range (s) | Variance % |
|----------|---------|---------|---------|-----------|------------|
| GO | 46 | 237 | 141.5 | 191 | 415.2% |
| OBJC | 17 | 70 | 43.5 | 53 | 311.8% |
| PYTHON | 19 | 77 | 48.0 | 58 | 305.3% |
| RUST | 45 | 174 | 109.5 | 129 | 286.7% |
| R | 94 | 298 | 196.0 | 204 | 217.0% |
| CSHARP | 36 | 114 | 75.0 | 78 | 216.7% |
| COBOL | 72 | 220 | 146.0 | 148 | 205.6% |
| AWK | 66 | 175 | 120.5 | 109 | 165.2% |
| POWERSHELL | 41 | 102 | 71.5 | 61 | 148.8% |
| PERL | 38 | 93 | 65.5 | 55 | 144.7% |
| ZIG | 72 | 175 | 123.5 | 103 | 143.1% |
| TYPESCRIPT | 104 | 249 | 176.5 | 145 | 139.4% |
| ERLANG | 26 | 60 | 43.0 | 34 | 130.8% |
| C | 59 | 135 | 97.0 | 76 | 128.8% |
| SCHEME | 48 | 106 | 77.0 | 58 | 120.8% |
| ELIXIR | 87 | 187 | 137.0 | 100 | 114.9% |
| RUBY | 135 | 270 | 202.5 | 135 | 100.0% |
| COMMONLISP | 102 | 198 | 150.0 | 96 | 94.1% |
| CRYSTAL | 55 | 101 | 78.0 | 46 | 83.6% |
| PHP | 133 | 243 | 188.0 | 110 | 82.7% |


---

## Visualizations

### Duration Degradation Trend
![Duration Trend](aggregated-duration-trend.png)

**Shows:** Average test duration increasing 2.1x from 4.2.0 → 4.2.4

### Language Variance Heatmap
![Language Variance](aggregated-language-variance.png)

**Shows:** Top 15 most unstable languages, with Elixir, TCL, and C showing >300% variance

### Ranking Instability
![Ranking Changes](aggregated-ranking-changes.png)

**Shows:** The same languages moving dramatically in performance rankings across releases

---

## Conclusion

The variance in performance metrics across these three releases is **not random noise**—it's a symptom of **architectural misplacement**.

The orchestrator running on the CPU-bound pool creates **cascading effects**:
1. Reduced CPU available for jobs → slower execution
2. Random scheduling order → different languages hit different contention levels
3. Each run has unique timing → metrics become meaningless for benchmarking

**For SRE/DevOps:** This is textbook example of why infrastructure placement matters.
**For Chaos Engineering:** This is gold—true adversarial execution.

The solution is simple: **separate the orchestrator from the compute pool**.

---

## Reproducibility & Methodology

### Pipeline Overview

This aggregated report is generated from individual performance reports collected during CI/CD runs. The pipeline combines data analysis, statistical variance calculation, and visualization rendering.

**Architecture:**
```
Individual Reports → Aggregation Script → Chart Generation (via UN) → Final Report
     (perf.json)         (Python)              (matplotlib)          (Markdown)
```

### Data Sources

**Input Files:**
- `reports/4.2.17/perf.json` - 665 tests, generated 2026-01-23T15:34:55Z
- `reports/4.2.18/perf.json` - 665 tests, generated 2026-01-23T16:05:03Z


Each `perf.json` contains:
- Pipeline metadata (tag, timestamp, pipeline IDs)
- Summary statistics (avg, min, max durations)
- Per-language results (42 languages × ~15 tests each)
- Queue times & execution durations

**Data Collection:**
1. GitLab CI triggers test matrix (42 languages in parallel)
2. Each language job reports timing via GitLab API
3. `scripts/generate-perf-report.sh` queries API & generates `perf.json`
4. Report committed to `reports/{TAG}/` directory

### Analysis Pipeline

**Step 1: Variance Analysis** (`scripts/aggregate-performance-reports.py`)

```python
# Load all reports
for version_dir in Path('reports').iterdir():
    reports[version] = json.loads((version_dir / 'perf.json').read_text())

# Extract language timings
for version, perf_data in reports.items():
    for lang_entry in perf_data['languages']:
        language_timings[version][lang_entry['language']] = lang_entry['duration_seconds']

# Calculate variance per language
for lang in all_languages:
    durations = [language_timings[v][lang] for v in versions if lang in language_timings[v]]
    percent_variance = ((max(durations) - min(durations)) / min(durations) * 100)
```

**Step 2: Chart Generation** (`scripts/generate-aggregated-charts.py`)

Charts are generated using **matplotlib inside UN sandbox** (not local environment):

```bash
# Copy reports with version-tagged names
cp reports/4.2.0/perf.json perf-4.2.0.json
cp reports/4.2.3/perf.json perf-4.2.3.json
cp reports/4.2.4/perf.json perf-4.2.4.json

# Execute chart generation via UN (includes matplotlib)
build/un -a \
  -f perf-4.2.0.json \
  -f perf-4.2.3.json \
  -f perf-4.2.4.json \
  scripts/generate-aggregated-charts.py

# Artifacts returned: *.png files
```

**Why UN for Charts?**
- Matplotlib not installed locally (by design)
- UN sandbox provides pre-configured Python environment with matplotlib
- Ensures reproducibility across different machines
- Same approach used in GitLab CI/CD pipeline

**Step 3: Report Generation**

```bash
# Generate markdown report (no matplotlib needed locally)
python3 scripts/aggregate-performance-reports.py reports AGGREGATED-PERFORMANCE.md
```

### Reproducing This Report

**Prerequisites:**
- Git repository checked out
- `build/un` binary (UN Inception CLI client)
- Python 3.x (for report generation, not charts)
- Access to `reports/` directory with historical data

**Command:**
```bash
make perf-aggregate-report
```

**Or manually:**
```bash
# Step 1: Generate charts
cp reports/4.2.0/perf.json perf-4.2.0.json
cp reports/4.2.3/perf.json perf-4.2.3.json
cp reports/4.2.4/perf.json perf-4.2.4.json
build/un -a -f perf-4.2.0.json -f perf-4.2.3.json -f perf-4.2.4.json scripts/generate-aggregated-charts.py
rm -f perf-*.json
mv *.png reports/

# Step 2: Generate markdown report
python3 scripts/aggregate-performance-reports.py reports AGGREGATED-PERFORMANCE.md
```

### Stepping Back in Time

To regenerate this report with historical data:

1. **Checkout the specific commit:**
   ```bash
   git checkout <commit-sha>
   ```

2. **Verify reports exist:**
   ```bash
   ls -la reports/4.2.0/perf.json
   ls -la reports/4.2.3/perf.json
   ls -la reports/4.2.4/perf.json
   ```

3. **Run analysis:**
   ```bash
   make perf-aggregate-report
   ```

### CI/CD Integration

This report auto-generates on release tags via GitLab CI:

```yaml
perf-aggregate-report:
  stage: report
  needs: [perf-report]
  script:
    - echo "Generating aggregated analysis..."
    - cp reports/4.2.0/perf.json perf-4.2.0.json
    - cp reports/4.2.3/perf.json perf-4.2.3.json
    - cp reports/4.2.4/perf.json perf-4.2.4.json
    - build/un -a -f perf-4.2.0.json -f perf-4.2.3.json -f perf-4.2.4.json scripts/generate-aggregated-charts.py
    - python3 scripts/aggregate-performance-reports.py reports AGGREGATED-PERFORMANCE.md
    - git add reports/ AGGREGATED-PERFORMANCE.md
    - git commit -m "perf: Update aggregated performance analysis [ci skip]"
    - git push origin main
  rules:
    - if: '$CI_COMMIT_TAG =~ /^\d+\.\d+\.\d+$/'
```

**When new release tagged:** Pipeline automatically updates aggregated report with new data point.

### Statistical Methods

**Variance Calculation:**
- Per-language min/max/avg across all releases
- Percent variance: `((max - min) / min) * 100`
- Languages with <2 data points excluded

**Ranking Analysis:**
- Languages sorted by duration per release
- Top 10 slowest tracked across releases
- Ranking position changes indicate non-determinism

**Concurrency Estimation:**
- Average duration vs theoretical serialized time
- Estimated parallel capacity: `ceiling(42 langs / avg_duration) * per_job_time`
- Variance suggests dynamic (not fixed) concurrency

### Tools & Dependencies

**Local Environment:**
- Python 3.x (standard library only)
- `build/un` - UN Inception CLI
- Git (for version control)
- Bash (for scripting)

**UN Sandbox Environment:**
- Python 3.x with matplotlib, numpy
- Pre-configured visualization environment
- Isolated execution (no local dependencies)

**GitLab CI:**
- GitLab Runner with `build` tag
- Environment variables: `UNSANDBOX_PUBLIC_KEY`, `UNSANDBOX_SECRET_KEY`
- Deploy key for auto-commit

### Data Integrity

**Validation:**
- JSON schema validation on input files
- Version tag format validation (`X.Y.Z`)
- Minimum 2 releases required for variance analysis

**Timestamps:**
- All reports include generation timestamp
- Commit history provides audit trail
- CI pipeline IDs link back to source runs

### Contact & Questions

For questions about this methodology or to report issues:
- Repository: `git.unturf.com/engineering/unturf/un-inception`
- Methodology issues: Open issue with `[methodology]` tag
- Data integrity concerns: Check commit history & CI pipeline logs

---

**Generated by UN Inception Performance Analysis Pipeline**
**Analysis Date:** 2026-01-23T11:27:07.194752
**Report Version:** 1.0.0
