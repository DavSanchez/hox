#!/usr/bin/env bash
# Run the hox benchmark suite, optionally saving it as / comparing it to a named baseline.
#
#   scripts/bench.sh                         run everything, print the results
#   scripts/bench.sh save NAME               run and store as baseline NAME
#   scripts/bench.sh compare NAME            run and compare against baseline NAME
#   scripts/bench.sh list                    list stored baselines
#
# Extra arguments after `--` go to the benchmark binary (tasty options), e.g.
#
#   scripts/bench.sh save before -- -p '/fib/'          only the fib benchmarks
#   scripts/bench.sh compare before -- -p '/fib/'
#
# Environment:
#   TIME_THRESHOLD   % slowdown counted as a regression by `compare` (default 20)
#   ALLOC_THRESHOLD  % allocation growth counted as a regression (default 2)
#
# Builds use cabal.project.perf (-O2, no profiling) instead of the default project, so a
# local cabal.project.local cannot skew the numbers.
#
# Baselines live in dist-newstyle/perf/baselines (git-ignored): timings are only
# comparable on the same machine, so they are not meant to be committed.
set -euo pipefail

root="$(cd "$(dirname "$0")/.." && pwd)"
cd "$root"
perf_dir="dist-newstyle/perf"
baselines="$perf_dir/baselines"
mkdir -p "$baselines"

cmd="run"
name=""
case "${1:-}" in
  save | compare) cmd="$1"; name="${2:?usage: bench.sh $1 NAME [-- tasty options]}"; shift 2 ;;
  list) ls -1 "$baselines" 2>/dev/null | sed 's/\.csv$//'; exit 0 ;;
  "" | --) ;;
  *) echo "unknown command: $1" >&2; exit 64 ;;
esac
[ "${1:-}" = "--" ] && shift

out="$perf_dir/latest.csv"
[ "$cmd" = "save" ] && out="$baselines/$name.csv"
if [ "$cmd" = "compare" ] && [ ! -f "$baselines/$name.csv" ]; then
  echo "no baseline named '$name' (see: scripts/bench.sh list)" >&2
  exit 66
fi

# Separate build dir: keeps the optimised, non-profiled benchmark binary
# independent of whatever profiling configuration the main dist-newstyle has.
cabal build hox-bench --project-file=cabal.project.perf --enable-benchmarks --disable-profiling --builddir="$perf_dir/build" >&2
bin="$(cabal list-bin hox-bench --project-file=cabal.project.perf --enable-benchmarks --disable-profiling --builddir="$perf_dir/build")"

"$bin" --csv "$out" "$@"

if [ "$cmd" = "compare" ]; then
  echo
  python3 scripts/bench_compare.py "$baselines/$name.csv" "$out" \
    --time-threshold "${TIME_THRESHOLD:-20}" --alloc-threshold "${ALLOC_THRESHOLD:-2}"
elif [ "$cmd" = "save" ]; then
  echo "saved baseline '$name' -> $out"
fi
