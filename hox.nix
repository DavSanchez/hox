{ mkDerivation, base, lib, mtl, tasty, tasty-bench, tasty-hunit
, tasty-quickcheck, text, time, unordered-containers
}:
mkDerivation {
  pname = "hox";
  version = "1.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base mtl text time unordered-containers
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base tasty tasty-hunit tasty-quickcheck ];
  benchmarkHaskellDepends = [ base tasty-bench ];
  homepage = "https://github.com/DavSanchez/hox";
  description = "An implementation of a tree-walk interpreter for Lox, the language explored in the book Crafting Interpreters by Robert Nystrom";
  license = lib.licenses.bsd3;
  mainProgram = "hox";
}
