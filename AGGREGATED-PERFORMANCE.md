# UN Inception: Aggregated Performance Analysis

**Analysis Date:** 1770551195.2485962
**Reports Analyzed:** 4.2.0, 4.2.10, 4.2.11, 4.2.12, 4.2.13, 4.2.14, 4.2.15, 4.2.16, 4.2.17, 4.2.18, 4.2.19, 4.2.20, 4.2.21, 4.2.22, 4.2.23, 4.2.24, 4.2.25, 4.2.26, 4.2.27, 4.2.28, 4.2.29, 4.2.3, 4.2.30, 4.2.31, 4.2.32, 4.2.36, 4.2.37, 4.2.38, 4.2.4, 4.2.46, 4.2.5, 4.2.50, 4.2.51, 4.2.52, 4.2.6, 4.2.7, 4.2.8, 4.2.9, 4.3.0, 4.3.1

---

## Executive Summary

Analysis of 40 performance reports reveals **significant variance** in execution metrics across releases. Different languages rank as slowest/fastest in different runs, indicating **non-deterministic execution patterns** likely caused by:

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
| 4.2.10 | 153s | scheme (320s) | bash (29s) | +120s (+363.6%) |
| 4.2.11 | 103s | deno (289s) | cpp (43s) | -50s (-32.7%) |
| 4.2.12 | 142s | go (406s) | erlang (21s) | +39s (+37.9%) |
| 4.2.13 | 126s | deno (290s) | haskell (23s) | -16s (-11.3%) |
| 4.2.14 | 97s | javascript (172s) | go (38s) | -29s (-23.0%) |
| 4.2.15 | 103s | d (203s) | cobol (49s) | +6s (+6.2%) |
| 4.2.16 | 98s | javascript (361s) | c (40s) | -5s (-4.9%) |
| 4.2.17 | 104s | ruby (270s) | objc (17s) | +6s (+6.1%) |
| 4.2.18 | 100s | r (298s) | scheme (48s) | -4s (-3.8%) |
| 4.2.19 | 151s | lua (472s) | dart (40s) | +51s (+51.0%) |
| 4.2.20 | 114s | java (272s) | crystal (49s) | -37s (-24.5%) |
| 4.2.21 | 102s | nim (215s) | erlang (22s) | -12s (-10.5%) |
| 4.2.22 | 300s | javascript (2173s) | clojure (14s) | +198s (+194.1%) |
| 4.2.23 | 373s | zig (1058s) | perl (8s) | +73s (+24.3%) |
| 4.2.24 | 129s | python (494s) | clojure (8s) | -244s (-65.4%) |
| 4.2.25 | 97s | fortran (151s) | bash (58s) | -32s (-24.8%) |
| 4.2.26 | 78s | cpp (130s) | python (25s) | -19s (-19.6%) |
| 4.2.27 | 119s | commonlisp (176s) | php (50s) | +41s (+52.6%) |
| 4.2.28 | 116s | commonlisp (145s) | awk (102s) | -3s (-2.5%) |
| 4.2.29 | 81s | go (144s) | groovy (38s) | -35s (-30.2%) |
| 4.2.3 | 63s | rust (142s) | v (40s) | -18s (-22.2%) |
| 4.2.30 | 161s | julia (232s) | ocaml (40s) | +98s (+155.6%) |
| 4.2.31 | 74s | go (106s) | erlang (43s) | -87s (-54.0%) |
| 4.2.32 | 99s | kotlin (159s) | cpp (32s) | +25s (+33.8%) |
| 4.2.36 | 1528s | scheme (1574s) | erlang (1236s) | +1429s (+1443.4%) |
| 4.2.37 | 267s | rust (721s) | powershell (41s) | -1261s (-82.5%) |
| 4.2.38 | 396s | php (1047s) | awk (25s) | +129s (+48.3%) |
| 4.2.4 | 70s | python (110s) | c (23s) | -326s (-82.3%) |
| 4.2.46 | 224s | raku (344s) | prolog (112s) | +154s (+220.0%) |
| 4.2.5 | 67s | v (114s) | erlang (44s) | -157s (-70.1%) |
| 4.2.50 | 183s | go (480s) | fortran (107s) | +116s (+173.1%) |
| 4.2.51 | 162s | go (425s) | powershell (50s) | -21s (-11.5%) |
| 4.2.52 | 136s | go (393s) | awk (48s) | -26s (-16.0%) |
| 4.2.6 | 54s | haskell (128s) | awk (23s) | -82s (-60.3%) |
| 4.2.7 | 117s | typescript (319s) | dotnet (5s) | +63s (+116.7%) |
| 4.2.8 | 111s | kotlin (313s) | fortran (28s) | -6s (-5.1%) |
| 4.2.9 | 107s | ruby (279s) | d (19s) | -4s (-3.6%) |
| 4.3.0 | 376s | typescript (829s) | c (47s) | +269s (+251.4%) |
| 4.3.1 | 238s | go (414s) | prolog (58s) | -138s (-36.7%) |

**Observation:** Average duration increased **0.0%** from 0s to 0s.

This **2-3x variance** is NOT normal for identical workloads. Indicates:
- Orchestrator fighting for CPU with test jobs
- Tests running in different order each time
- No consistent resource allocation

---

### 2. Unstable Language Rankings

The same language changes dramatically in rank between runs:


**JAVASCRIPT:**
  - 4.2.0: 90s
  - 4.2.10: 107s
  - 4.2.11: 60s
  - 4.2.12: 32s
  - 4.2.13: 173s
  - 4.2.14: 172s
  - 4.2.15: 130s
  - 4.2.16: 361s
  - 4.2.17: 127s
  - 4.2.18: 79s
  - 4.2.19: 68s
  - 4.2.20: 87s
  - 4.2.21: 42s
  - 4.2.22: 2173s
  - 4.2.23: 425s
  - 4.2.24: 242s
  - 4.2.25: 74s
  - 4.2.26: 114s
  - 4.2.27: 58s
  - 4.2.28: 113s
  - 4.2.29: 62s
  - 4.2.3: 75s
  - 4.2.30: 130s
  - 4.2.31: 64s
  - 4.2.32: 81s
  - 4.2.36: 1536s
  - 4.2.37: 504s
  - 4.2.38: 675s
  - 4.2.4: 109s
  - 4.2.46: 197s
  - 4.2.5: 60s
  - 4.2.50: 241s
  - 4.2.51: 70s
  - 4.2.52: 160s
  - 4.2.6: 50s
  - 4.2.7: 155s
  - 4.2.8: 253s
  - 4.2.9: 166s
  - 4.3.0: 196s
  - 4.3.1: 164s
  - **Range:** 32s → 2173s (6690.6% variance)

**R:**
  - 4.2.0: 25s
  - 4.2.10: 181s
  - 4.2.11: 95s
  - 4.2.12: 169s
  - 4.2.13: 107s
  - 4.2.14: 144s
  - 4.2.15: 164s
  - 4.2.16: 64s
  - 4.2.17: 94s
  - 4.2.18: 298s
  - 4.2.19: 106s
  - 4.2.20: 57s
  - 4.2.21: 24s
  - 4.2.22: 1834s
  - 4.2.23: 9s
  - 4.2.24: 434s
  - 4.2.25: 65s
  - 4.2.26: 66s
  - 4.2.27: 54s
  - 4.2.28: 137s
  - 4.2.29: 59s
  - 4.2.3: 64s
  - 4.2.30: 127s
  - 4.2.31: 61s
  - 4.2.32: 82s
  - 4.2.36: 1566s
  - 4.2.37: 48s
  - 4.2.38: 904s
  - 4.2.4: 74s
  - 4.2.46: 199s
  - 4.2.5: 52s
  - 4.2.50: 223s
  - 4.2.51: 250s
  - 4.2.52: 88s
  - 4.2.6: 47s
  - 4.2.7: 313s
  - 4.2.8: 126s
  - 4.2.9: 54s
  - 4.3.0: 453s
  - 4.3.1: 163s
  - **Range:** 9s → 1834s (20277.8% variance)

**SCHEME:**
  - 4.2.0: 24s
  - 4.2.10: 320s
  - 4.2.11: 92s
  - 4.2.12: 205s
  - 4.2.13: 128s
  - 4.2.14: 93s
  - 4.2.15: 77s
  - 4.2.16: 53s
  - 4.2.17: 106s
  - 4.2.18: 48s
  - 4.2.19: 122s
  - 4.2.20: 75s
  - 4.2.21: 79s
  - 4.2.22: 15s
  - 4.2.23: 285s
  - 4.2.24: 76s
  - 4.2.25: 74s
  - 4.2.26: 100s
  - 4.2.27: 145s
  - 4.2.28: 129s
  - 4.2.29: 68s
  - 4.2.3: 65s
  - 4.2.30: 206s
  - 4.2.31: 94s
  - 4.2.32: 98s
  - 4.2.36: 1574s
  - 4.2.37: 50s
  - 4.2.38: 581s
  - 4.2.4: 100s
  - 4.2.46: 242s
  - 4.2.5: 102s
  - 4.2.50: 157s
  - 4.2.51: 135s
  - 4.2.52: 158s
  - 4.2.6: 42s
  - 4.2.7: 146s
  - 4.2.8: 55s
  - 4.2.9: 153s
  - 4.3.0: 171s
  - 4.3.1: 276s
  - **Range:** 15s → 1574s (10393.3% variance)

**PYTHON:**
  - 4.2.0: 40s
  - 4.2.10: 56s
  - 4.2.11: 67s
  - 4.2.12: 29s
  - 4.2.13: 169s
  - 4.2.14: 170s
  - 4.2.15: 62s
  - 4.2.16: 130s
  - 4.2.17: 19s
  - 4.2.18: 77s
  - 4.2.19: 49s
  - 4.2.20: 88s
  - 4.2.21: 43s
  - 4.2.22: 108s
  - 4.2.23: 200s
  - 4.2.24: 494s
  - 4.2.25: 76s
  - 4.2.26: 25s
  - 4.2.27: 65s
  - 4.2.28: 114s
  - 4.2.29: 64s
  - 4.2.3: 78s
  - 4.2.30: 60s
  - 4.2.31: 70s
  - 4.2.32: 55s
  - 4.2.36: 1574s
  - 4.2.37: 88s
  - 4.2.38: 796s
  - 4.2.4: 110s
  - 4.2.46: 196s
  - 4.2.5: 61s
  - 4.2.50: 119s
  - 4.2.51: 81s
  - 4.2.52: 211s
  - 4.2.6: 52s
  - 4.2.7: 58s
  - 4.2.8: 253s
  - 4.2.9: 165s
  - 4.3.0: 671s
  - 4.3.1: 148s
  - **Range:** 19s → 1574s (8184.2% variance)

**TCL:**
  - 4.2.0: 20s
  - 4.2.10: 198s
  - 4.2.11: 100s
  - 4.2.12: 123s
  - 4.2.13: 46s
  - 4.2.14: 91s
  - 4.2.15: 138s
  - 4.2.16: 53s
  - 4.2.17: 67s
  - 4.2.18: 49s
  - 4.2.19: 83s
  - 4.2.20: 80s
  - 4.2.21: 98s
  - 4.2.22: 102s
  - 4.2.23: 712s
  - 4.2.24: 42s
  - 4.2.25: 82s
  - 4.2.26: 101s
  - 4.2.27: 148s
  - 4.2.28: 130s
  - 4.2.29: 97s
  - 4.2.3: 61s
  - 4.2.30: 213s
  - 4.2.31: 79s
  - 4.2.32: 99s
  - 4.2.36: 1572s
  - 4.2.37: 689s
  - 4.2.38: 396s
  - 4.2.4: 96s
  - 4.2.46: 117s
  - 4.2.5: 51s
  - 4.2.50: 152s
  - 4.2.51: 141s
  - 4.2.52: 158s
  - 4.2.6: 42s
  - 4.2.7: 148s
  - 4.2.8: 247s
  - 4.2.9: 54s
  - 4.3.0: 476s
  - 4.3.1: 132s
  - **Range:** 20s → 1572s (7760.0% variance)


---

### 3. Execution Order Non-Determinism

**Fastest Languages by Run:**

4.2.0: ocaml, tcl, elixir, csharp, cobol
4.2.10: bash, powershell, erlang, ruby, typescript
4.2.11: cpp, forth, lua, typescript, ruby
4.2.12: erlang, php, python, javascript, haskell
4.2.13: haskell, v, groovy, nim, kotlin
4.2.14: go, cpp, powershell, erlang, typescript
4.2.15: cobol, csharp, ocaml, objc, kotlin
4.2.16: cpp, c, raku, awk, groovy
4.2.17: objc, python, erlang, csharp, perl
4.2.18: scheme, tcl, fortran, c, raku
4.2.19: dart, python, typescript, javascript, dotnet
4.2.20: crystal, v, deno, r, csharp
4.2.21: erlang, r, ruby, awk, typescript
4.2.22: powershell, clojure, scheme, objc, v
4.2.23: perl, r, d, groovy, powershell
4.2.24: zig, nim, kotlin, fortran, forth
4.2.25: bash, powershell, forth, r, prolog
4.2.26: python, fsharp, ocaml, haskell, julia
4.2.27: php, lua, bash, perl, r
4.2.28: awk, zig, powershell, objc, nim
4.2.29: groovy, raku, erlang, forth, prolog
4.2.3: v, d, kotlin, awk, raku
4.2.30: ocaml, python, php, perl, lua
4.2.31: erlang, prolog, raku, dotnet, csharp
4.2.32: cpp, c, raku, go, python
4.2.36: erlang, awk, powershell, csharp, kotlin
4.2.37: powershell, erlang, cpp, r, scheme
4.2.38: awk, powershell, ruby, erlang, objc
4.2.4: c, d, cobol, raku, v
4.2.46: prolog, typescript, tcl, objc, clojure
4.2.5: erlang, awk, bash, deno, tcl
4.2.50: fortran, csharp, bash, ocaml, python
4.2.51: powershell, prolog, javascript, python, forth
4.2.52: awk, prolog, perl, objc, fortran
4.2.6: awk, powershell, crystal, raku, erlang
4.2.7: dotnet, deno, awk, fortran, commonlisp
4.2.8: fortran, groovy, crystal, java, powershell
4.2.9: d, julia, csharp, v, objc
4.3.0: c, bash, php, fortran, ruby
4.3.1: prolog, awk, powershell, typescript, dart

**Slowest Languages by Run:**

4.2.0: raku, javascript, cpp, rust, go
4.2.10: scheme, clojure, deno, c, julia
4.2.11: deno, awk, erlang, elixir, clojure
4.2.12: go, crystal, groovy, deno, awk
4.2.13: deno, raku, awk, cpp, java
4.2.14: javascript, python, php, bash, elixir
4.2.15: d, cpp, ruby, bash, lua
4.2.16: javascript, clojure, crystal, lua, fsharp
4.2.17: ruby, typescript, php, cobol, commonlisp
4.2.18: r, go, elixir, rust, forth
4.2.19: lua, perl, java, ruby, powershell
4.2.20: java, zig, cobol, perl, haskell
4.2.21: nim, dart, java, cpp, rust
4.2.22: javascript, r, nim, zig, lua
4.2.23: zig, v, commonlisp, deno, elixir
4.2.24: python, php, r, elixir, deno
4.2.25: fortran, crystal, perl, awk, cpp
4.2.26: cpp, raku, cobol, javascript, ruby
4.2.27: commonlisp, fortran, d, zig, powershell
4.2.28: commonlisp, perl, lua, r, bash
4.2.29: go, crystal, deno, cpp, java
4.2.3: rust, c, python, typescript, javascript
4.2.30: julia, haskell, fsharp, dart, tcl
4.2.31: go, rust, forth, scheme, groovy
4.2.32: kotlin, cobol, fortran, d, zig
4.2.36: scheme, python, tcl, elixir, r
4.2.37: rust, ruby, php, dotnet, tcl
4.2.38: php, raku, r, bash, clojure
4.2.4: python, javascript, elixir, scheme, bash
4.2.46: raku, powershell, rust, commonlisp, lua
4.2.5: v, haskell, scheme, ocaml, powershell
4.2.50: go, groovy, awk, javascript, erlang
4.2.51: go, php, clojure, lua, perl
4.2.52: go, python, ruby, typescript, rust
4.2.6: haskell, go, cpp, rust, forth
4.2.7: typescript, ruby, r, elixir, crystal
4.2.8: kotlin, python, javascript, tcl, raku
4.2.9: ruby, deno, rust, crystal, java
4.3.0: typescript, go, python, java, objc
4.3.1: go, groovy, perl, deno, objc

**Conclusion:** No consistent "fast" or "slow" languages across runs. This proves:
- Execution order is random or system-dependent
- Resource availability varies dramatically
- Each run experiences different contention patterns

---

### 4. API Health Trends

**Overall API Health:** 6.9/100 (avg across 9 releases)
**Trend:** STABLE
**Total Retries (all releases):** 4293

| Release | Health Score | Total Retries | 429 (Rate Limit) | 5xx (Server) | Timeout | Connection |
|---------|--------------|---------------|------------------|--------------|---------|------------|
| 4.2.36 | 0/100 | 2122 | 0 | 2122 | 0 | 0 |
| 4.2.37 | 0/100 | 151 | 0 | 60 | 0 | 0 |
| 4.2.38 | 0/100 | 222 | 0 | 125 | 0 | 0 |
| 4.2.46 | 0/100 | 146 | 0 | 146 | 0 | 0 |
| 4.2.50 | 0/100 | 58 | 0 | 58 | 0 | 0 |
| 4.2.51 | 28/100 | 36 | 0 | 36 | 0 | 0 |
| 4.2.52 | 34/100 | 33 | 0 | 33 | 0 | 0 |
| 4.3.0 | 0/100 | 876 | 839 | 27 | 10 | 0 |
| 4.3.1 | 0/100 | 649 | 634 | 5 | 10 | 0 |

**Interpretation:**
- **Score 95-100:** API healthy, tests pass on first attempt
- **Score 80-94:** Some transient errors, tests recovered via retry
- **Score < 80:** Significant API instability affecting test reliability

**Scientific Integrity Note:** Prior to 4.2.34, tests used "soft passes" that masked failures.
Now tests retry transient errors and fail honestly if they can't verify results.

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


JAVASCRIPT: 32s → 2173s (+6690.6%)

R: 9s → 1834s (+20277.8%)

SCHEME: 15s → 1574s (+10393.3%)

PYTHON: 19s → 1574s (+8184.2%)

TCL: 20s → 1572s (+7760.0%)

ELIXIR: 20s → 1570s (+7750.0%)

NIM: 8s → 1557s (+19362.5%)

OBJC: 17s → 1559s (+9070.6%)

CLOJURE: 8s → 1549s (+19262.5%)

V: 21s → 1562s (+7338.1%)


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
| DOTNET | 5 | 1539 | 173.6 | 1534 | 30680.0% |
| R | 9 | 1834 | 227.0 | 1825 | 20277.8% |
| NIM | 8 | 1557 | 175.9 | 1549 | 19362.5% |
| CLOJURE | 8 | 1549 | 188.4 | 1541 | 19262.5% |
| FORTRAN | 8 | 1547 | 139.2 | 1539 | 19237.5% |
| PERL | 8 | 1546 | 182.1 | 1538 | 19225.0% |
| D | 8 | 1545 | 141.9 | 1537 | 19212.5% |
| ZIG | 8 | 1542 | 218.8 | 1534 | 19175.0% |
| FORTH | 8 | 1540 | 172.7 | 1532 | 19150.0% |
| KOTLIN | 8 | 1536 | 161.9 | 1528 | 19100.0% |
| CSHARP | 8 | 1534 | 162.4 | 1526 | 19075.0% |
| LUA | 9 | 1548 | 209.3 | 1539 | 17100.0% |
| PROLOG | 9 | 1540 | 124.2 | 1531 | 17011.1% |
| RUST | 9 | 1537 | 191.5 | 1528 | 16977.8% |
| SCHEME | 15 | 1574 | 168.0 | 1559 | 10393.3% |
| OBJC | 17 | 1559 | 168.4 | 1542 | 9070.6% |
| POWERSHELL | 14 | 1269 | 128.0 | 1255 | 8964.3% |
| PYTHON | 19 | 1574 | 174.8 | 1555 | 8184.2% |
| OCAML | 19 | 1537 | 168.4 | 1518 | 7989.5% |
| TCL | 20 | 1572 | 187.1 | 1552 | 7760.0% |


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
- `reports/4.2.10/perf.json` - 673 tests, generated 2026-01-23T11:46:18Z
- `reports/4.2.11/perf.json` - 669 tests, generated 2026-01-23T12:14:08Z
- `reports/4.2.12/perf.json` - 665 tests, generated 2026-01-23T13:30:32Z
- `reports/4.2.13/perf.json` - 673 tests, generated 2026-01-23T14:19:49Z
- `reports/4.2.14/perf.json` - 661 tests, generated 2026-01-23T14:48:26Z
- `reports/4.2.15/perf.json` - 657 tests, generated 2026-01-23T15:14:36Z
- `reports/4.2.16/perf.json` - 665 tests, generated 2026-01-23T15:25:53Z
- `reports/4.2.17/perf.json` - 665 tests, generated 2026-01-23T15:34:55Z
- `reports/4.2.18/perf.json` - 665 tests, generated 2026-01-23T16:05:03Z
- `reports/4.2.19/perf.json` - 701 tests, generated 2026-01-23T20:20:06Z
- `reports/4.2.20/perf.json` - 685 tests, generated 2026-01-23T20:41:23Z
- `reports/4.2.21/perf.json` - 661 tests, generated 2026-01-23T21:16:07Z
- `reports/4.2.22/perf.json` - 697 tests, generated 2026-01-24T17:57:56Z
- `reports/4.2.23/perf.json` - 713 tests, generated 2026-01-24T19:14:09Z
- `reports/4.2.24/perf.json` - 665 tests, generated 2026-01-24T19:13:51Z
- `reports/4.2.25/perf.json` - 681 tests, generated 2026-01-24T21:04:06Z
- `reports/4.2.26/perf.json` - 661 tests, generated 2026-01-24T21:08:03Z
- `reports/4.2.27/perf.json` - 653 tests, generated 2026-01-24T23:27:26Z
- `reports/4.2.28/perf.json` - 645 tests, generated 2026-01-24T23:31:17Z
- `reports/4.2.29/perf.json` - 724 tests, generated 2026-01-28T20:40:04Z
- `reports/4.2.3/perf.json` - 642 tests, generated 2026-01-19T11:58:45Z
- `reports/4.2.30/perf.json` - 840 tests, generated 2026-01-28T22:16:15Z
- `reports/4.2.31/perf.json` - 704 tests, generated 2026-01-28T22:17:41Z
- `reports/4.2.32/perf.json` - 776 tests, generated 2026-01-28T22:23:28Z
- `reports/4.2.36/perf.json` - 860 tests, generated 2026-01-29T02:03:51Z
- `reports/4.2.37/perf.json` - 812 tests, generated 2026-01-29T13:55:52Z
- `reports/4.2.38/perf.json` - 832 tests, generated 2026-01-29T15:57:07Z
- `reports/4.2.4/perf.json` - 682 tests, generated 2026-01-19T12:02:14Z
- `reports/4.2.46/perf.json` - 860 tests, generated 2026-01-29T20:46:47Z
- `reports/4.2.5/perf.json` - 658 tests, generated 2026-01-19T19:10:23Z
- `reports/4.2.50/perf.json` - 860 tests, generated 2026-01-30T00:20:38Z
- `reports/4.2.51/perf.json` - 860 tests, generated 2026-01-31T17:11:41Z
- `reports/4.2.52/perf.json` - 860 tests, generated 2026-01-31T20:22:30Z
- `reports/4.2.6/perf.json` - 642 tests, generated 2026-01-19T20:22:16Z
- `reports/4.2.7/perf.json` - 631 tests, generated 2026-01-23T09:36:18Z
- `reports/4.2.8/perf.json` - 645 tests, generated 2026-01-23T10:01:33Z
- `reports/4.2.9/perf.json` - 645 tests, generated 2026-01-23T10:05:34Z
- `reports/4.3.0/perf.json` - 820 tests, generated 2026-02-06T15:25:06Z
- `reports/4.3.1/perf.json` - 824 tests, generated 2026-02-08T11:44:58Z


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
**Analysis Date:** 2026-02-08T06:46:35.375632
**Report Version:** 1.0.0
