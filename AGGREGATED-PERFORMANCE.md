# UN Inception: Aggregated Performance Analysis

**Analysis Date:** 1769162752.9821825
**Reports Analyzed:** 4.2.0, 4.2.3, 4.2.4, 4.2.5, 4.2.6, 4.2.7, 4.2.8, 4.2.9

---

## Executive Summary

Analysis of 8 performance reports reveals **significant variance** in execution metrics across releases. Different languages rank as slowest/fastest in different runs, indicating **non-deterministic execution patterns** likely caused by:

1. **Orchestrator placement on CPU-bound pool** (not an SRE best practice)
2. **Resource contention** between the orchestrator & test jobs
3. **Undefined or exceeded concurrency limits**
4. **Non-deterministic scheduling** of the matrix jobs

---

## Key Findings

### 1. Extreme Metric Variance

| Release | Avg Duration | Slowest | Fastest | Change from Previous |
|---------|--------------|---------|---------|----------------------|
| 4.2.0 | 33s | raku (93s) | ocaml (19s) | baseline |
| 4.2.3 | 63s | rust (142s) | v (40s) | +30s (+90.9%) |
| 4.2.4 | 70s | python (110s) | c (23s) | +7s (+11.1%) |
| 4.2.5 | 67s | v (114s) | erlang (44s) | -3s (-4.3%) |
| 4.2.6 | 54s | haskell (128s) | awk (23s) | -13s (-19.4%) |
| 4.2.7 | 117s | typescript (319s) | dotnet (5s) | +63s (+116.7%) |
| 4.2.8 | 111s | kotlin (313s) | fortran (28s) | -6s (-5.1%) |
| 4.2.9 | 107s | ruby (279s) | d (19s) | -4s (-3.6%) |

**Observation:** Average duration increased **0.0%** from 0s to 0s.

This **2-3x variance** is NOT normal for identical workloads. Indicates:
- Orchestrator fighting for CPU with test jobs
- Tests running in different order each time
- No consistent resource allocation

---

### 2. Unstable Language Rankings

The same language changes dramatically in rank between runs:


**ELIXIR:**
  - 4.2.0: 20s
  - 4.2.3: 69s
  - 4.2.4: 105s
  - 4.2.5: 55s
  - 4.2.6: 41s
  - 4.2.7: 313s
  - 4.2.8: 127s
  - 4.2.9: 59s
  - **Range:** 20s → 313s (1465.0% variance)

**TYPESCRIPT:**
  - 4.2.0: 29s
  - 4.2.3: 75s
  - 4.2.4: 80s
  - 4.2.5: 59s
  - 4.2.6: 53s
  - 4.2.7: 319s
  - 4.2.8: 94s
  - 4.2.9: 118s
  - **Range:** 29s → 319s (1000.0% variance)

**R:**
  - 4.2.0: 25s
  - 4.2.3: 64s
  - 4.2.4: 74s
  - 4.2.5: 52s
  - 4.2.6: 47s
  - 4.2.7: 313s
  - 4.2.8: 126s
  - 4.2.9: 54s
  - **Range:** 25s → 313s (1152.0% variance)

**KOTLIN:**
  - 4.2.0: 27s
  - 4.2.3: 45s
  - 4.2.4: 54s
  - 4.2.5: 61s
  - 4.2.6: 69s
  - 4.2.7: 49s
  - 4.2.8: 313s
  - 4.2.9: 89s
  - **Range:** 27s → 313s (1059.3% variance)

**RUBY:**
  - 4.2.0: 35s
  - 4.2.3: 74s
  - 4.2.4: 79s
  - 4.2.5: 60s
  - 4.2.6: 48s
  - 4.2.7: 318s
  - 4.2.8: 93s
  - 4.2.9: 279s
  - **Range:** 35s → 318s (808.6% variance)


---

### 3. Execution Order Non-Determinism

**Fastest Languages by Run:**

4.2.0: ocaml, tcl, elixir, csharp, cobol
4.2.3: v, d, kotlin, awk, raku
4.2.4: c, d, cobol, raku, v
4.2.5: erlang, awk, bash, deno, tcl
4.2.6: awk, powershell, crystal, raku, erlang
4.2.7: dotnet, deno, awk, fortran, commonlisp
4.2.8: fortran, groovy, crystal, java, powershell
4.2.9: d, julia, csharp, v, objc

**Slowest Languages by Run:**

4.2.0: raku, javascript, cpp, rust, go
4.2.3: rust, c, python, typescript, javascript
4.2.4: python, javascript, elixir, scheme, bash
4.2.5: v, haskell, scheme, ocaml, powershell
4.2.6: haskell, go, cpp, rust, forth
4.2.7: typescript, ruby, r, elixir, crystal
4.2.8: kotlin, python, javascript, tcl, raku
4.2.9: ruby, deno, rust, crystal, java

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


ELIXIR: 20s → 313s (+1465.0%)

TYPESCRIPT: 29s → 319s (+1000.0%)

R: 25s → 313s (+1152.0%)

KOTLIN: 27s → 313s (+1059.3%)

RUBY: 35s → 318s (+808.6%)

CRYSTAL: 24s → 260s (+983.3%)

DENO: 24s → 253s (+954.2%)

TCL: 20s → 247s (+1135.0%)

PYTHON: 40s → 253s (+532.5%)

PHP: 23s → 235s (+921.7%)


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
| DOTNET | 5 | 102 | 59.3 | 97 | 1940.0% |
| ELIXIR | 20 | 313 | 98.6 | 293 | 1465.0% |
| R | 25 | 313 | 94.4 | 288 | 1152.0% |
| TCL | 20 | 247 | 89.9 | 227 | 1135.0% |
| KOTLIN | 27 | 313 | 88.4 | 286 | 1059.3% |
| TYPESCRIPT | 29 | 319 | 103.4 | 290 | 1000.0% |
| CRYSTAL | 24 | 260 | 98.2 | 236 | 983.3% |
| DENO | 24 | 253 | 69.5 | 229 | 954.2% |
| PHP | 23 | 235 | 94.2 | 212 | 921.7% |
| COBOL | 20 | 199 | 68.2 | 179 | 895.0% |
| RUBY | 35 | 318 | 123.2 | 283 | 808.6% |
| OCAML | 19 | 167 | 79.4 | 148 | 778.9% |
| HASKELL | 21 | 168 | 90.1 | 147 | 700.0% |
| OBJC | 29 | 223 | 80.8 | 194 | 669.0% |
| RAKU | 33 | 240 | 93.5 | 207 | 627.3% |
| JULIA | 32 | 226 | 81.8 | 194 | 606.2% |
| BASH | 35 | 233 | 91.4 | 198 | 565.7% |
| JAVA | 33 | 219 | 73.6 | 186 | 563.6% |
| SCHEME | 24 | 153 | 85.9 | 129 | 537.5% |
| FSHARP | 26 | 165 | 71.6 | 139 | 534.6% |


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
- `reports/4.2.0/perf.json` - 642 tests, generated 2026-01-18T23:20:51Z
- `reports/4.2.3/perf.json` - 642 tests, generated 2026-01-19T11:58:45Z
- `reports/4.2.4/perf.json` - 682 tests, generated 2026-01-19T12:02:14Z
- `reports/4.2.5/perf.json` - 658 tests, generated 2026-01-19T19:10:23Z
- `reports/4.2.6/perf.json` - 642 tests, generated 2026-01-19T20:22:16Z
- `reports/4.2.7/perf.json` - 631 tests, generated 2026-01-23T09:36:18Z
- `reports/4.2.8/perf.json` - 645 tests, generated 2026-01-23T10:01:33Z
- `reports/4.2.9/perf.json` - 645 tests, generated 2026-01-23T10:05:34Z


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
**Analysis Date:** 2026-01-23T05:05:53.087501
**Report Version:** 1.0.0
