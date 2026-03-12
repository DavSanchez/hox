# Performance Analysis

This document describes known performance bottlenecks in `hox` and practical
improvement strategies, ordered roughly by expected impact.

---

## How to profile

### Quick benchmark (no profiling)

```sh
cabal bench
```

Runs the `fib(20)`, `fib(25)`, and `fib(30)` interpreter benchmarks via
`tasty-bench`.

### Time profiling

```sh
cabal build --enable-profiling
cabal bench --enable-profiling -- +RTS -p -RTS
# Produces hox-bench.prof
```

### Heap profiling

```sh
cabal bench --enable-profiling -- +RTS -hc -p -RTS
hp2ps -c hox-bench.hp
# Produces hox-bench.ps (open with evince / any PS viewer)
```

To profile by type constructor instead of cost centre use `-hy` instead of
`-hc`.

### GC statistics

```sh
cabal bench -- +RTS -s -RTS
```

Prints allocation totals, GC pause times, and live-data stats after the run.
High allocation rates or long GC pauses are a strong signal that intermediate
structures should be eliminated.

---

## Identified bottlenecks

### 1. Scanner — O(n) keyword lookup
**File:** `src/Language/Scanner/Naive.hs:30-48,125`

```haskell
keywords :: [(String, TokenType)]
keywords = [("and", AND), ("class", CLASS), ...]

tokenType = fromMaybe (IDENTIFIER (pack identifier)) (lookup identifier keywords)
```

`lookup` on a list is O(n). For the 16 keywords, every identifier token
requires up to 16 string comparisons.  A top-level `Map` (or even `HashMap`)
gives O(log n) / O(1) average:

```haskell
import qualified Data.Map.Strict as M

keywordMap :: M.Map String TokenType
keywordMap = M.fromList keywords

-- then:
tokenType = fromMaybe (IDENTIFIER (pack identifier)) (M.lookup identifier keywordMap)
```

---

### 2. Scanner — three passes over every identifier, number, and string
**File:** `src/Language/Scanner/Naive.hs:97-126`

The scanner calls `takeWhile`, then `length` on the result, then `drop` on the
original input — three separate O(k) passes for every multi-character token.
Example (identifiers, line 124-126):

```haskell
let identifier = c : takeWhile isAlphaNum ss       -- pass 1 (build list)
    tokenType  = ...lookup identifier...
in naiveScanTokens (drop (length identifier) input) -- pass 2 (length) + pass 3 (drop)
```

Replace with `span` / `break`, which splits in a single pass and directly
returns the remainder:

```haskell
let (rest, remaining) = span isAlphaNum ss
    identifier = c : rest
in naiveScanTokens remaining l (validToken tokenType l : tt)
```

The same pattern appears for numbers (integer part, decimal check) and strings
(`takeWhile (/= '"')` + `drop (length ... + 1)`).

---

### 3. Scanner — full list reversal on every scan
**File:** `src/Language/Scanner.hs:22`

```haskell
scanTokens s = NE.reverse $ naiveScanTokens s 1 []
```

Tokens are accumulated in reverse order (each new token is prepended), so the
entire `NonEmpty` list is reversed at the end.  For a large source file this is
a gratuitous O(n) allocation.

Options:
- Build the list front-to-back by appending (use `Data.Sequence` or a
  difference list internally, convert at the end).
- Alternatively accept reverse order and adapt callers — the parser already
  processes tokens sequentially so consuming them in a `Seq` or reversed
  `Vector` is fine.

---

### 4. Scanner — `String` (linked list) as input type
**File:** `src/Language/Scanner/Naive.hs:51-128`

Haskell's `String = [Char]` stores each character as a heap-allocated cons
cell.  For any non-trivial source file this means poor cache locality and high
GC pressure.  Switching the scanner to consume `Data.Text` (or `Data.Text.Lazy`
for streaming) would reduce allocations substantially and open the door to
`Data.Text.Internal` slicing (O(1) substring views) instead of `takeWhile`
copies.

This is the largest structural change but likely the highest single-item
payoff for the scanner.

---

### 5. Interpreter — repeated `get` inside parameter-binding loop
**File:** `src/Runtime/Interpreter.hs:518-525`

```haskell
mapM_
  ( \((paramName, _), argValue) -> do
      s <- get          -- StateT get on every iteration
      declare paramName argValue s
  )
  paramWithArgs
```

`get` threads the full `ProgramState` through the `StateT` on every loop
iteration, even though the state is not changing between iterations (only the
top `IORef` frame is mutated).  Hoist the `get` out of the loop:

```haskell
s <- get
mapM_ (\((paramName, _), argValue) -> declare paramName argValue s) paramWithArgs
```

---

### 6. Interpreter — `assignInFrame` reads the `IORef` twice
**File:** `src/Runtime/Environment.hs:37-42`

```haskell
assignInFrame name val frame = do
  m <- liftIO $ readIORef frame           -- read 1
  if M.member name m
    then liftIO (modifyIORef' frame (M.insert name val)) $> True  -- read 2 (inside modifyIORef')
    else pure False
```

`modifyIORef'` internally reads the ref a second time.  Use
`Data.Map.Strict.insertLookupWithKey` to do the membership check and update
atomically in one Map traversal, then write back once:

```haskell
assignInFrame name val frame = liftIO $ do
  m <- readIORef frame
  case M.lookup name m of
    Nothing -> pure False
    Just _  -> writeIORef frame (M.insert name val m) >> pure True
```

Or more concisely with `alterF`.

---

### 7. Interpreter — `arity` re-traverses the method table on every call
**File:** `src/Runtime/Value.hs:120-126`

```haskell
arity (Callable (ClassConstructor cls _)) =
  case lookupMethod "init" cls of
    Just (func, _) -> length (funcParams func)
    Nothing        -> 0
```

`callCallable` (`Interpreter.hs:506-511`) calls `arity` before every
invocation.  For `ClassConstructor` this walks the method map (`lookupMethod`)
and then counts parameters (`length funcParams`) every single call.

Cache the arity once at class-construction time by storing it in
`ClassConstructor`:

```haskell
data CallableType
  = ...
  | ClassConstructor LoxClass (Maybe SuperClass) Int  -- cached arity
```

---

### 8. Interpreter — full `ProgramState` record allocation on every call
**File:** `src/Runtime/Interpreter.hs:514-529`

```haskell
call (Callable (UserDefinedFunction func closure isInit)) args = do
  state    <- get
  newState <- pushClosureScope closure state
  put newState
  ...
  modify (\ps -> ps {environment = environment state})
```

Each function call allocates a new `ProgramState` record (`pushClosureScope`
returns a new record) and restores the old one on return.  Since `ProgramState`
only has two fields (`environment` and `globals`), the allocation is small, but
it happens on every call — including every recursive Fibonacci step.

A `ReaderT`/`IORef`-based design where the environment pointer is stored in an
`IORef` (rather than in `StateT`) avoids the record copy; however this is a
larger refactor.  As a smaller step, storing the previous environment on the
call-stack and restoring it directly (without re-constructing the record) can
reduce the overhead.

---

### 9. Environment — `IORef (Map Text Value)` per frame
**File:** `src/Runtime/Environment.hs:22-24`

```haskell
type Frame a = IORef (M.Map Text a)
type Environment a = [Frame a]
```

Each scope frame is a heap-allocated `IORef` pointing to a heap-allocated
balanced-BST.  Variable access at depth `d` therefore requires:

1. O(d) list traversal to reach the right frame (`getAtDistance`).
2. One `readIORef` (memory barrier / pointer dereference).
3. One `Map.lookup` — O(log k) where k is the number of variables in that
   frame.

For typical function frames (few variables, shallow nesting) the practical cost
is dominated by the `IORef` dereference and allocation of the `Map` node on
insert.

Possible improvements:
- Replace `Map Text` with `HashMap Text` (O(1) average lookup on `Text` keys).
- For frames with a statically known set of variables (available from the
  resolver), a flat `Vector` indexed by variable slot number would give O(1)
  access with no hashing.
- Use `STRef` / unboxed arrays when the interpreter can be run in `ST` rather
  than `IO`.

---

### 10. Missing `-O2` on library
**File:** `hox.cabal:150` (fixed in this branch)

The library stanza previously had no explicit `-O` flag.  Cabal defaults to
`-O1`; adding `-O2` enables more aggressive inlining and specialisation, which
matters especially for the `StateT`/`ExceptT` monad stack in the interpreter.
The `cabal.project` added in this branch also sets `optimization: 2` globally.

Expected improvement: free speedup of 10–30% on interpreter-heavy benchmarks
with no code changes.

---

## Summary table

| # | Location | Issue | Effort | Expected impact |
|---|----------|-------|--------|----------------|
| 1 | `Scanner/Naive.hs:125` | O(n) list keyword lookup | Low | Small–medium |
| 2 | `Scanner/Naive.hs:97-126` | 3× passes per token | Low | Medium |
| 3 | `Scanner.hs:22` | Full list reversal | Low | Small–medium |
| 4 | `Scanner/Naive.hs` | `String` input type | High | High |
| 5 | `Interpreter.hs:518-525` | `get` inside param loop | Trivial | Small |
| 6 | `Environment.hs:37-42` | Double IORef read in assign | Low | Small |
| 7 | `Value.hs:120-126` | `arity` re-traversal per call | Low | Small–medium |
| 8 | `Interpreter.hs:514-529` | `ProgramState` alloc per call | Medium | Medium |
| 9 | `Environment.hs:22-24` | `IORef (Map Text)` per frame | High | High |
| 10 | `hox.cabal` | No `-O2` on library | Trivial | Medium |
