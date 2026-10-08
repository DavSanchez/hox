#!/usr/bin/env bash
# Profile the interpreter. Results go to dist-newstyle/perf/profiles/<name>/ (git-ignored).
#
#   scripts/profile.sh time  FILE.lox   time + allocation by cost centre (.prof)
#   scripts/profile.sh heap  FILE.lox   heap residency over time, by closure type (.hp -> .svg)
#   scripts/profile.sh gc    FILE.lox   GC / allocation / residency summary (+RTS -s), no profiling build
#   scripts/profile.sh bench PATTERN    time profile of the benchmark suite; PATTERN is a tasty -p
#                                       pattern, e.g. 'closures' or '$NF == "fib(25)"'
#
# Examples:
#   scripts/profile.sh time fibo.lox
#   scripts/profile.sh bench 'closures'
#   scripts/profile.sh bench '$NF == "fib(25)"'
#
# Environment:
#   PROF_DETAIL   cost-centre placement: late (default), toplevel, all
#                 `late` adds cost centres *after* optimisation, so the profile
#                 reflects the optimised program. `all` (-fprof-auto) gives
#                 finer granularity but distorts what GHC can inline/specialise:
#                 on this code it inflates allocation ~3.5x and runtime ~7x, so
#                 treat its absolute numbers with care.
#   RTS_ARGS      extra RTS flags (default: -N1 for the hox executable, which is
#                 -threaded; none for the non-threaded benchmark binary)
set -euo pipefail

root="$(cd "$(dirname "$0")/.." && pwd)"
cd "$root"
mode="${1:?usage: profile.sh time|heap|gc FILE.lox | profile.sh bench PATTERN}"
target="${2:?missing FILE.lox / PATTERN}"
detail="${PROF_DETAIL:-late}"
perf_dir="$root/dist-newstyle/perf"

build() { # build <exe|bench> <profiling: yes|no>
  local what="$1" prof="$2" args=(--project-file=cabal.project.perf --builddir="$perf_dir/build-$2")
  [ "$what" = bench ] && args+=(--enable-benchmarks) && what=hox-bench || what=exe:hox
  if [ "$prof" = yes ]; then
    args+=(--enable-profiling --profiling-detail="$detail" --ghc-options=-rtsopts)
  else
    args+=(--disable-profiling)
  fi
  (cd "$root" && cabal build "$what" "${args[@]}" >&2 && cabal list-bin "$what" "${args[@]}")
}

if [ "$mode" = bench ]; then
  rts="${RTS_ARGS:-}"
  name="bench-$(echo "$target" | tr -c 'A-Za-z0-9\n' _)"
  out="$perf_dir/profiles/$name"; mkdir -p "$out"; cd "$out"
  bin="$(build bench yes)"
  "$bin" -p "$target" +RTS -p -s $rts -RTS | tee run.txt
  echo "profile: $out/hox-bench.prof"
  exit 0
fi

rts="${RTS_ARGS:--N1}"
file="$(cd "$(dirname "$target")" && pwd)/$(basename "$target")"
name="$(basename "$target" .lox)-$mode"
out="$perf_dir/profiles/$name"; mkdir -p "$out"; cd "$out"

case "$mode" in
  time)
    bin="$(build exe yes)"
    "$bin" "$file" +RTS -p -s $rts -RTS 2>&1 | tee run.txt
    echo "profile: $out/hox.prof"
    ;;
  heap)
    bin="$(build exe no)"
    "$bin" "$file" +RTS -hT -i0.05 -s $rts -RTS 2>&1 | tee run.txt
    hp2ps -c hox.hp >/dev/null 2>&1 && echo "graph:   $out/hox.ps" || echo "profile: $out/hox.hp"
    ;;
  gc)
    bin="$(build exe no)"
    "$bin" "$file" +RTS -s $rts -RTS 2>&1 | tee run.txt
    ;;
  *) echo "unknown mode: $mode" >&2; exit 64 ;;
esac
