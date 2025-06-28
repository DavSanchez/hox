{ mkDerivation, base, lib, megaparsec, tasty, tasty-hunit
, tasty-quickcheck
}:
mkDerivation {
  pname = "hox";
  version = "0.1.6.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base megaparsec ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base tasty tasty-hunit tasty-quickcheck ];
  homepage = "https://github.com/DavSanchez/hox";
  description = "An implementation of a tree-walk interpreter for Lox, the language explored in the book Crafting Interpreters by Robert Nystrom";
  license = lib.licenses.bsd3;
  mainProgram = "hox";
}
