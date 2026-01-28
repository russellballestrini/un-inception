# UN Inception: Aggregated Performance Analysis

<<<<<<< Updated upstream
<<<<<<< Updated upstream
**Analysis Date:** 1769632836.9671485
**Reports Analyzed:** 4.2.0, 4.2.10, 4.2.11, 4.2.12, 4.2.13, 4.2.14, 4.2.15, 4.2.16, 4.2.17, 4.2.18, 4.2.19, 4.2.20, 4.2.21, 4.2.22, 4.2.23, 4.2.24, 4.2.25, 4.2.26, 4.2.27, 4.2.28, 4.2.29, 4.2.3, 4.2.4, 4.2.5, 4.2.6, 4.2.7, 4.2.8, 4.2.9
=======
**Analysis Date:** 1769638616.3642986
**Reports Analyzed:** 4.2.0, 4.2.10, 4.2.11, 4.2.12, 4.2.13, 4.2.14, 4.2.15, 4.2.16, 4.2.17, 4.2.18, 4.2.19, 4.2.20, 4.2.21, 4.2.22, 4.2.23, 4.2.24, 4.2.25, 4.2.26, 4.2.27, 4.2.28, 4.2.29, 4.2.3, 4.2.30, 4.2.4, 4.2.5, 4.2.6, 4.2.7, 4.2.8, 4.2.9
>>>>>>> Stashed changes
=======
**Analysis Date:** 1769638690.0262566
**Reports Analyzed:** 4.2.0, 4.2.10, 4.2.11, 4.2.12, 4.2.13, 4.2.14, 4.2.15, 4.2.16, 4.2.17, 4.2.18, 4.2.19, 4.2.20, 4.2.21, 4.2.22, 4.2.23, 4.2.24, 4.2.25, 4.2.26, 4.2.27, 4.2.28, 4.2.29, 4.2.3, 4.2.30, 4.2.31, 4.2.4, 4.2.5, 4.2.6, 4.2.7, 4.2.8, 4.2.9
>>>>>>> Stashed changes

---

## Executive Summary

<<<<<<< Updated upstream
<<<<<<< Updated upstream
Analysis of 28 performance reports reveals **significant variance** in execution metrics across releases. Different languages rank as slowest/fastest in different runs, indicating **non-deterministic execution patterns** likely caused by:
=======
Analysis of 29 performance reports reveals **significant variance** in execution metrics across releases. Different languages rank as slowest/fastest in different runs, indicating **non-deterministic execution patterns** likely caused by:
>>>>>>> Stashed changes
=======
Analysis of 30 performance reports reveals **significant variance** in execution metrics across releases. Different languages rank as slowest/fastest in different runs, indicating **non-deterministic execution patterns** likely caused by:
>>>>>>> Stashed changes

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
<<<<<<< Updated upstream
<<<<<<< Updated upstream
| 4.2.4 | 70s | python (110s) | c (23s) | +7s (+11.1%) |
=======
| 4.2.30 | 161s | julia (232s) | ocaml (40s) | +98s (+155.6%) |
| 4.2.4 | 70s | python (110s) | c (23s) | -91s (-56.5%) |
>>>>>>> Stashed changes
=======
| 4.2.30 | 161s | julia (232s) | ocaml (40s) | +98s (+155.6%) |
| 4.2.31 | 74s | go (106s) | erlang (43s) | -87s (-54.0%) |
| 4.2.4 | 70s | python (110s) | c (23s) | -4s (-5.4%) |
>>>>>>> Stashed changes
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
<<<<<<< Updated upstream
=======
  - 4.2.31: 64s
>>>>>>> Stashed changes
  - 4.2.4: 109s
  - 4.2.5: 60s
  - 4.2.6: 50s
  - 4.2.7: 155s
  - 4.2.8: 253s
  - 4.2.9: 166s
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
<<<<<<< Updated upstream
=======
  - 4.2.31: 61s
>>>>>>> Stashed changes
  - 4.2.4: 74s
  - 4.2.5: 52s
  - 4.2.6: 47s
  - 4.2.7: 313s
  - 4.2.8: 126s
  - 4.2.9: 54s
  - **Range:** 9s → 1834s (20277.8% variance)

**NIM:**
  - 4.2.0: 31s
  - 4.2.10: 78s
  - 4.2.11: 68s
  - 4.2.12: 36s
  - 4.2.13: 41s
  - 4.2.14: 97s
  - 4.2.15: 99s
  - 4.2.16: 80s
  - 4.2.17: 98s
  - 4.2.18: 71s
  - 4.2.19: 102s
  - 4.2.20: 103s
  - 4.2.21: 215s
  - 4.2.22: 1217s
  - 4.2.23: 286s
  - 4.2.24: 8s
  - 4.2.25: 100s
  - 4.2.26: 53s
  - 4.2.27: 139s
  - 4.2.28: 103s
  - 4.2.29: 110s
  - 4.2.3: 52s
  - 4.2.30: 181s
<<<<<<< Updated upstream
=======
  - 4.2.31: 63s
>>>>>>> Stashed changes
  - 4.2.4: 76s
  - 4.2.5: 58s
  - 4.2.6: 58s
  - 4.2.7: 77s
  - 4.2.8: 93s
  - 4.2.9: 42s
  - **Range:** 8s → 1217s (15112.5% variance)

**ZIG:**
  - 4.2.0: 32s
  - 4.2.10: 185s
  - 4.2.11: 68s
  - 4.2.12: 199s
  - 4.2.13: 145s
  - 4.2.14: 61s
  - 4.2.15: 151s
  - 4.2.16: 131s
  - 4.2.17: 175s
  - 4.2.18: 72s
  - 4.2.19: 90s
  - 4.2.20: 224s
  - 4.2.21: 192s
  - 4.2.22: 1014s
  - 4.2.23: 1058s
  - 4.2.24: 8s
  - 4.2.25: 108s
  - 4.2.26: 55s
  - 4.2.27: 169s
  - 4.2.28: 103s
  - 4.2.29: 119s
  - 4.2.3: 61s
  - 4.2.30: 182s
<<<<<<< Updated upstream
=======
  - 4.2.31: 89s
>>>>>>> Stashed changes
  - 4.2.4: 60s
  - 4.2.5: 59s
  - 4.2.6: 59s
  - 4.2.7: 79s
  - 4.2.8: 187s
  - 4.2.9: 153s
  - **Range:** 8s → 1058s (13125.0% variance)

**V:**
  - 4.2.0: 22s
  - 4.2.10: 78s
  - 4.2.11: 67s
  - 4.2.12: 100s
  - 4.2.13: 31s
  - 4.2.14: 97s
  - 4.2.15: 63s
  - 4.2.16: 78s
  - 4.2.17: 167s
  - 4.2.18: 115s
  - 4.2.19: 86s
  - 4.2.20: 56s
  - 4.2.21: 121s
  - 4.2.22: 21s
  - 4.2.23: 1040s
  - 4.2.24: 101s
  - 4.2.25: 97s
  - 4.2.26: 54s
  - 4.2.27: 133s
  - 4.2.28: 104s
  - 4.2.29: 108s
  - 4.2.3: 40s
  - 4.2.30: 188s
<<<<<<< Updated upstream
=======
  - 4.2.31: 63s
>>>>>>> Stashed changes
  - 4.2.4: 49s
  - 4.2.5: 114s
  - 4.2.6: 58s
  - 4.2.7: 76s
  - 4.2.8: 109s
  - 4.2.9: 42s
  - **Range:** 21s → 1040s (4852.4% variance)


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
<<<<<<< Updated upstream
=======
4.2.31: erlang, prolog, raku, dotnet, csharp
>>>>>>> Stashed changes
4.2.4: c, d, cobol, raku, v
4.2.5: erlang, awk, bash, deno, tcl
4.2.6: awk, powershell, crystal, raku, erlang
4.2.7: dotnet, deno, awk, fortran, commonlisp
4.2.8: fortran, groovy, crystal, java, powershell
4.2.9: d, julia, csharp, v, objc

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
<<<<<<< Updated upstream
=======
4.2.31: go, rust, forth, scheme, groovy
>>>>>>> Stashed changes
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


JAVASCRIPT: 32s → 2173s (+6690.6%)

R: 9s → 1834s (+20277.8%)

NIM: 8s → 1217s (+15112.5%)

ZIG: 8s → 1058s (+13125.0%)

V: 21s → 1040s (+4852.4%)

LUA: 9s → 975s (+10733.3%)

COMMONLISP: 27s → 956s (+3440.7%)

DENO: 24s → 926s (+3758.3%)

ELIXIR: 20s → 917s (+4485.0%)

PHP: 23s → 824s (+3482.6%)


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
<<<<<<< Updated upstream
<<<<<<< Updated upstream
| R | 9 | 1834 | 175.6 | 1825 | 20277.8% |
| NIM | 8 | 1217 | 128.2 | 1209 | 15112.5% |
| ZIG | 8 | 1058 | 179.2 | 1050 | 13125.0% |
| LUA | 9 | 975 | 145.8 | 966 | 10733.3% |
| FORTH | 8 | 628 | 109.9 | 620 | 7750.0% |
| DOTNET | 5 | 360 | 93.3 | 355 | 7100.0% |
| JAVASCRIPT | 32 | 2173 | 202.0 | 2141 | 6690.6% |
| PERL | 8 | 468 | 107.4 | 460 | 5750.0% |
| V | 21 | 1040 | 115.2 | 1019 | 4852.4% |
| CSHARP | 8 | 385 | 99.1 | 377 | 4712.5% |
| ELIXIR | 20 | 917 | 151.3 | 897 | 4485.0% |
| OBJC | 17 | 693 | 108.3 | 676 | 3976.5% |
| KOTLIN | 8 | 313 | 97.7 | 305 | 3812.5% |
| CLOJURE | 8 | 310 | 108.2 | 302 | 3775.0% |
| DENO | 24 | 926 | 155.8 | 902 | 3758.3% |
| COBOL | 20 | 759 | 137.9 | 739 | 3695.0% |
| HASKELL | 21 | 754 | 124.6 | 733 | 3490.5% |
| PHP | 23 | 824 | 140.3 | 801 | 3482.6% |
| TCL | 20 | 712 | 116.4 | 692 | 3460.0% |
| COMMONLISP | 27 | 956 | 122.5 | 929 | 3440.7% |
=======
| R | 9 | 1834 | 173.9 | 1825 | 20277.8% |
| NIM | 8 | 1217 | 130.1 | 1209 | 15112.5% |
| ZIG | 8 | 1058 | 179.3 | 1050 | 13125.0% |
| LUA | 9 | 975 | 144.7 | 966 | 10733.3% |
| FORTH | 8 | 628 | 112.0 | 620 | 7750.0% |
| DOTNET | 5 | 360 | 94.4 | 355 | 7100.0% |
| JAVASCRIPT | 32 | 2173 | 199.6 | 2141 | 6690.6% |
| PERL | 8 | 468 | 107.0 | 460 | 5750.0% |
| V | 21 | 1040 | 117.8 | 1019 | 4852.4% |
| CSHARP | 8 | 385 | 102.3 | 377 | 4712.5% |
| ELIXIR | 20 | 917 | 150.8 | 897 | 4485.0% |
| OBJC | 17 | 693 | 111.6 | 676 | 3976.5% |
| KOTLIN | 8 | 313 | 100.2 | 305 | 3812.5% |
| CLOJURE | 8 | 310 | 111.0 | 302 | 3775.0% |
| DENO | 24 | 926 | 156.6 | 902 | 3758.3% |
| COBOL | 20 | 759 | 138.7 | 739 | 3695.0% |
| HASKELL | 21 | 754 | 128.1 | 733 | 3490.5% |
| PHP | 23 | 824 | 138.4 | 801 | 3482.6% |
| TCL | 20 | 712 | 119.7 | 692 | 3460.0% |
| COMMONLISP | 27 | 956 | 124.9 | 929 | 3440.7% |
>>>>>>> Stashed changes
=======
| R | 9 | 1834 | 170.1 | 1825 | 20277.8% |
| NIM | 8 | 1217 | 127.8 | 1209 | 15112.5% |
| ZIG | 8 | 1058 | 176.3 | 1050 | 13125.0% |
| LUA | 9 | 975 | 141.9 | 966 | 10733.3% |
| FORTH | 8 | 628 | 111.7 | 620 | 7750.0% |
| DOTNET | 5 | 360 | 92.8 | 355 | 7100.0% |
| JAVASCRIPT | 32 | 2173 | 195.0 | 2141 | 6690.6% |
| PERL | 8 | 468 | 105.4 | 460 | 5750.0% |
| V | 21 | 1040 | 115.9 | 1019 | 4852.4% |
| CSHARP | 8 | 385 | 100.7 | 377 | 4712.5% |
| ELIXIR | 20 | 917 | 147.9 | 897 | 4485.0% |
| OBJC | 17 | 693 | 109.9 | 676 | 3976.5% |
| KOTLIN | 8 | 313 | 99.5 | 305 | 3812.5% |
| CLOJURE | 8 | 310 | 110.3 | 302 | 3775.0% |
| DENO | 24 | 926 | 154.3 | 902 | 3758.3% |
| COBOL | 20 | 759 | 136.9 | 739 | 3695.0% |
| HASKELL | 21 | 754 | 126.1 | 733 | 3490.5% |
| PHP | 23 | 824 | 136.0 | 801 | 3482.6% |
| TCL | 20 | 712 | 118.4 | 692 | 3460.0% |
| COMMONLISP | 27 | 956 | 123.7 | 929 | 3440.7% |
>>>>>>> Stashed changes


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
<<<<<<< Updated upstream
=======
- `reports/4.2.31/perf.json` - 704 tests, generated 2026-01-28T22:17:41Z
>>>>>>> Stashed changes
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
<<<<<<< Updated upstream
<<<<<<< Updated upstream
**Analysis Date:** 2026-01-28T15:40:37.091730
=======
**Analysis Date:** 2026-01-28T17:16:56.482158
>>>>>>> Stashed changes
=======
**Analysis Date:** 2026-01-28T17:18:10.143038
>>>>>>> Stashed changes
**Report Version:** 1.0.0
