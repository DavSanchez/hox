# `hox`: A tree-walk Lox implementation written in Haskell

[![built with nix](https://builtwithnix.org/badge.svg)](https://builtwithnix.org)

[![❄️ Hox package checks](https://github.com/DavSanchez/hox/actions/workflows/test.yaml/badge.svg)](https://github.com/DavSanchez/hox/actions/workflows/test.yaml)

This is my implementation of a tree-walk interpreter of Lox, written in Haskell
and following (where possible) the Java original from the book
[Crafting Interpreters](https://www.craftinginterpreters.com) by Robert Nystrom.

## Completed chapters

- [x] Chapter 04: Scanning
- [x] Chapter 05: Representing Code
- [x] Chapter 06: Parsing Expressions
- [x] Chapter 07: Evaluating Expressions
- [x] Chapter 08: Statements and State
- [x] Chapter 09: Control Flow
- [x] Chapter 10: Functions
- [x] Chapter 11: Resolving and Binding
- [x] Chapter 12: Classes
- [x] Chapter 13: Inheritance

## Benchmark evolution

Performance has room for improvements, though somewhat expected for tree-walk
interpreters. Will be revisiting this to add improvements.
I have minimal benchmarks set up, which I expect to expand upon and improve.
See [per-commit performance charts](http://davsanchez.github.io/hox/dev/bench) to
check how it's going.
