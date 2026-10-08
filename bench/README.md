# Performance tooling

Everything needed to measure the interpreter, so that a change can be judged
against a baseline on several axes instead of by feel.

- **Wall-clock time** (tasty-bench `Mean`, `2*Stdev`): what users feel.
  Noisy: about ±2% run to run on an idle machine.
- **Allocation** (`Allocated`, needs `-T`): **deterministic** for a given
  binary (±0.0% run to run), so the best regression signal. Allocation rate
  is also the first-order cost driver in this interpreter.
- **GC copying** (`Copied`): a proxy for live data retained. Space leaks
  show up here.
- **Peak memory** (`Peak Memory`): a process-wide high-water mark, **not
  per-benchmark** (see the caveat below).
- **Pipeline stage**: the `Scanner`, `Parser`, `Resolver` and `Interpreter`
  groups tell you which stage a regression is in.
- **Workload shape**: the `Interpreter` benchmarks cover calls, global vs
  local loops, closures, classes, strings, scope push/pop and deep
  recursion. A change can win on one and lose on another.
- **Where the time goes**: `scripts/profile.sh` gives a cost-centre profile,
  a heap profile and a GC summary.

## Benchmarks

`bench/Main.hs` (tasty-bench) with workloads defined in `bench/Workloads.hs`.
Each interpreter workload is a Lox program targeting one area (see the module
header). The benchmark **fails** if the program raises an interpreter error, so
a broken workload cannot be silently measured as "very fast".

The `fib(N)` benchmark names are stable on purpose: CI tracks their history.

```sh
cabal bench hox-bench                      # run everything (~30 s)
cabal bench hox-bench --benchmark-options '-p closures'   # one group / name
```

`-T` is enabled in the benchmark's RTS options, so the allocation columns are
always present. The benchmark executable is intentionally **not** `-threaded`
and has no `-N`: the interpreter is single-threaded and parallel GC only adds
noise (on `fib(27)` the `hox` executable's `-N` costs ~25% versus `-N1`).

### Baselines and comparison

```sh
scripts/bench.sh save before          # run and store baseline "before"
# ...make a change...
scripts/bench.sh compare before       # run again and diff against it
scripts/bench.sh list
```

`compare` prints time / allocation / GC-copied side by side with percentage
deltas, and exits non-zero when time regresses by more than `TIME_THRESHOLD`
(default 20%) or allocation by more than `ALLOC_THRESHOLD` (default 2%). Both
are overridable through the environment. Pass tasty options after `--`:

```sh
scripts/bench.sh compare before -- -p '/Interpreter/'
```

Baselines are stored in `dist-newstyle/perf/baselines/` (git-ignored). Timings
are only comparable on the same machine, so save a baseline on the machine you
will compare on. Allocation numbers are portable across machines for the same
GHC version, which is why they have the tighter threshold.

CI (`.github/workflows/test.yaml`) runs the same suite on every push and tracks
history with `benchmark-action/github-action-benchmark`.

#### Peak memory caveat

tasty-bench reads the RTS's maximum-memory-in-use counter, which only ever
grows during the process. After the first memory-hungry benchmark every later
one reports the same value. To get the peak of one benchmark, run it alone:

```sh
dist-newstyle/.../hox-bench -p '$NF == "fib(30)"' --csv /tmp/one.csv
```

For residency over time use `scripts/profile.sh heap` instead.

## Profiling

```sh
# cost centres: time and allocation (hox.prof)
scripts/profile.sh time fibo.lox
# live heap by closure type over time (hox.hp / hox.ps)
scripts/profile.sh heap fibo.lox
# +RTS -s summary: allocation, copying, max residency
scripts/profile.sh gc fibo.lox
# profile the benchmark suite itself (argument is a tasty -p pattern)
scripts/profile.sh bench closures
```

Output lands in `dist-newstyle/perf/profiles/<name>/`. Builds use
`cabal.project.perf` (`-O2`) instead of the default project, so a personal
`cabal.project.local` cannot change the results.

A few things worth knowing when reading profiles:

- **Cost-centre placement matters.** The default is `--profiling-detail=late`
  (`-fprof-late`): cost centres are added *after* optimisation, so the profile
  describes the optimised program. `-fprof-auto` (`PROF_DETAIL=all`) puts a
  cost centre on every binding, which stops GHC inlining and specialising and
  badly distorts this code: on `fib(27)` it inflated allocation about 3.5x and
  runtime about 7x, and attributes cost to the wrong places. Use it only for
  fine-grained attribution, never for absolute numbers.
- Profiled builds are slower (~2.5x with `late`); compare percentages, not
  seconds. Use `scripts/profile.sh gc` or the benchmarks for real timings.
- `heap` uses `-hT` (by closure type) on a non-profiled build, so it does not
  perturb allocation. That is the quickest way to spot a space leak: residency
  growing with the number of loop iterations is a leak.

## Suggested workflow for a performance change

1. `scripts/bench.sh save before` on a clean checkout.
2. Find the cost: `scripts/profile.sh time <program>` (and `heap` if memory
   looks off).
3. Make **one** change.
4. `scripts/bench.sh compare before`: look at allocation first (deterministic),
   then time. Check the other workloads for regressions, not only the one you
   targeted.
5. Run the test suite and the Crafting Interpreters tests
   (`nix run .#hox-crafting-interpreters-tests`) before committing.
