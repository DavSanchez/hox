{ mkDerivation, base, lib }:
mkDerivation {
  pname = "hox";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base ];
  homepage = "https://github.com/DavSanchez/hox";
  description = "An implementation of a tree-walk interpreter for Lox, the language explored in the book Crafting Interpreters by Robert Nystrom";
  license = lib.licenses.bsd3;
  mainProgram = "hox";
}