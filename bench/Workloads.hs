-- | Lox programs used by the benchmark suite.
--
-- Each runtime workload targets a different part of the interpreter so that a
-- regression (or improvement) can be traced to the area that caused it:
--
-- * 'fib': call overhead, argument passing, local lookups, binary operators.
-- * 'loopGlobal': global variable reads/writes, @while@, arithmetic.
-- * 'loopLocal': the same, but through local scopes (@for@ desugars to blocks).
-- * 'closures': closure creation, upvalue (non-local) access and assignment.
-- * 'classes': instantiation, field get/set, method binding, @super@.
-- * 'strings': text concatenation.
-- * 'blocks': scope push/pop cost (nested blocks).
-- * 'deepRecursion': stack depth / memory behaviour of non-tail recursion.
--
-- Programs never @print@ (it would flood the benchmark output) and instead
-- store their result in a global, so the work cannot be optimised away.
module Workloads
  ( fib,
    loopGlobal,
    loopLocal,
    closures,
    classes,
    strings,
    blocks,
    deepRecursion,
    frontEndSource,
  )
where

fib :: Int -> String
fib n =
  unlines
    [ "fun fib(n) {",
      "  if (n < 2) return n;",
      "  return fib(n - 1) + fib(n - 2);",
      "}",
      "var result = fib(" ++ show n ++ ");"
    ]

loopGlobal :: Int -> String
loopGlobal n =
  unlines
    [ "var i = 0;",
      "var sum = 0;",
      "while (i < " ++ show n ++ ") { sum = sum + i; i = i + 1; }",
      "var result = sum;"
    ]

loopLocal :: Int -> String
loopLocal n =
  unlines
    [ "fun run() {",
      "  var sum = 0;",
      "  for (var i = 0; i < " ++ show n ++ "; i = i + 1) { sum = sum + i; }",
      "  return sum;",
      "}",
      "var result = run();"
    ]

closures :: Int -> String
closures n =
  unlines
    [ "fun makeCounter() {",
      "  var c = 0;",
      "  fun inc() { c = c + 1; return c; }",
      "  return inc;",
      "}",
      "fun run() {",
      "  var f = makeCounter();",
      "  var i = 0;",
      "  while (i < " ++ show n ++ ") { f(); i = i + 1; }",
      "  return f();",
      "}",
      "var result = run();"
    ]

classes :: Int -> String
classes n =
  unlines
    [ "class Vec {",
      "  init(x, y) { this.x = x; this.y = y; }",
      "  add(o) { return Vec(this.x + o.x, this.y + o.y); }",
      "  len2() { return this.x * this.x + this.y * this.y; }",
      "}",
      "class Vec3 < Vec {",
      "  init(x, y, z) { super.init(x, y); this.z = z; }",
      "  len2() { return super.len2() + this.z * this.z; }",
      "}",
      "fun run() {",
      "  var v = Vec(0, 0);",
      "  var d = Vec(1, 2);",
      "  var w = Vec3(1, 2, 3);",
      "  var total = 0;",
      "  for (var i = 0; i < " ++ show n ++ "; i = i + 1) {",
      "    v = v.add(d);",
      "    total = total + w.len2();",
      "  }",
      "  return v.len2() + total;",
      "}",
      "var result = run();"
    ]

strings :: Int -> String
strings n =
  unlines
    [ "fun run() {",
      "  var s = \"\";",
      "  for (var i = 0; i < " ++ show n ++ "; i = i + 1) { s = s + \"x\"; }",
      "  return s;",
      "}",
      "var result = run();"
    ]

blocks :: Int -> String
blocks n =
  unlines
    [ "fun run() {",
      "  var acc = 0;",
      "  for (var i = 0; i < " ++ show n ++ "; i = i + 1) {",
      "    { var a = i; { var b = a; { acc = acc + b; } } }",
      "  }",
      "  return acc;",
      "}",
      "var result = run();"
    ]

deepRecursion :: Int -> String
deepRecursion n =
  unlines
    [ "fun depth(n) { if (n == 0) return 0; return 1 + depth(n - 1); }",
      "var result = depth(" ++ show n ++ ");"
    ]

-- | A large (several thousand lines) program for benchmarking the scanner,
-- parser and resolver in isolation: all the workloads above, repeated.
--
-- Redeclaring globals is legal in Lox, so repeating the snippets is valid.
frontEndSource :: String
frontEndSource =
  concat . replicate 40 . concat $
    [ fib 20,
      loopGlobal 1000,
      loopLocal 1000,
      closures 1000,
      classes 1000,
      strings 1000,
      blocks 1000,
      deepRecursion 1000
    ]
