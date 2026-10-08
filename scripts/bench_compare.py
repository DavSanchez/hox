#!/usr/bin/env python3
"""Compare two tasty-bench CSV files on every axis they contain.

    bench_compare.py OLD.csv NEW.csv [--time-threshold PCT] [--alloc-threshold PCT]

Axes: wall-clock time (Mean), allocated bytes and bytes copied by the GC.
Allocation is deterministic for a given binary, so it is a far more reliable
regression signal than time, particularly on noisy machines: the default
threshold for it is therefore much tighter. Bytes copied is a proxy for how
much live data the program keeps around (space leaks show up here); it is
informational only because it depends on GC timing.

"Peak Memory" is deliberately not compared: tasty-bench reports a process-wide
high-water mark, so every benchmark that runs after the most memory-hungry one
shows the same number. To measure the peak of a single benchmark, run it alone
(`scripts/bench.sh -- -p '$NF == "fib(30)"'`) and read the CSV.

Exits with status 1 if any benchmark regressed beyond the thresholds.
"""
import argparse
import csv
import sys

AXES = [
    # (column, label, kind)
    ("Mean (ps)", "time", "time"),
    ("Allocated", "alloc", "alloc"),
    ("Copied", "copied", "info"),
]


def load(path):
    with open(path, newline="") as f:
        return {row["Name"]: row for row in csv.DictReader(f)}


def fmt_bytes(n):
    for unit in ("B", "KB", "MB", "GB"):
        if abs(n) < 1024 or unit == "GB":
            return f"{n:.0f} {unit}" if unit == "B" else f"{n:.1f} {unit}"
        n /= 1024


def fmt_time(ps):
    ns = ps / 1000
    for unit, scale in (("ns", 1), ("μs", 1e3), ("ms", 1e6), ("s", 1e9)):
        if ns < scale * 1000 or unit == "s":
            return f"{ns / scale:.2f} {unit}"


def pct(old, new):
    return float("inf") if old == 0 and new else (0.0 if old == 0 else (new - old) / old * 100)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("old")
    ap.add_argument("new")
    ap.add_argument("--time-threshold", type=float, default=20.0, help="%% slowdown that counts as a regression")
    ap.add_argument("--alloc-threshold", type=float, default=2.0, help="%% allocation increase that counts as a regression")
    args = ap.parse_args()

    old, new = load(args.old), load(args.new)
    regressions = []
    name_w = max((len(n) for n in new), default=10)

    header = f"{'benchmark':<{name_w}}  {'time':>22}  {'allocated':>22}  {'GC copied':>22}"
    print(header)
    print("-" * len(header))
    for name, row in new.items():
        if name not in old:
            print(f"{name:<{name_w}}  (new, no baseline)")
            continue
        cells = []
        for col, label, kind in AXES:
            if col not in row or col not in old[name]:
                cells.append(None)
                continue
            o, n = float(old[name][col]), float(row[col])
            d = pct(o, n)
            cells.append((o, n, d))
            limit = {"time": args.time_threshold, "alloc": args.alloc_threshold}.get(kind)
            if limit is not None and d > limit:
                regressions.append((name, label, d))

        def cell(c, f, width):
            if c is None:
                return " " * width
            _, n, d = c
            mark = " " if abs(d) < 1 else ("▲" if d > 0 else "▼")
            return f"{f(n):>{width - 9}} {d:+6.1f}%{mark}"

        t, a, c = cells
        print(f"{name:<{name_w}}  {cell(t, fmt_time, 22)}  {cell(a, fmt_bytes, 22)}  {cell(c, fmt_bytes, 22)}")

    for name in old:
        if name not in new:
            print(f"{name:<{name_w}}  (in baseline only)")

    print()
    if regressions:
        print("REGRESSIONS:")
        for name, label, d in regressions:
            print(f"  {name}: {label} {d:+.1f}%")
        return 1
    print("No regressions beyond thresholds "
          f"(time > {args.time_threshold}%, alloc > {args.alloc_threshold}%).")
    return 0


if __name__ == "__main__":
    sys.exit(main())
