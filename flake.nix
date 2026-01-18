{
  description = "My implementations of Lox, a programming language from the book 'Crafting Interpreters' by Robert Nystrom";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    oldDartNixpkgs.url = "github:nixos/nixpkgs/8cad3dbe48029cb9def5cdb2409a6c80d3acfe2e"; # Dart 2.19.6
    flake-parts.url = "github:hercules-ci/flake-parts";
    git-hooks = {
      url = "github:cachix/git-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    crafting-interpreters = {
      url = "github:munificent/craftinginterpreters";
      flake = false;
    };
  };

  outputs =
    inputs@{ flake-parts, ... }:
    # https://flake.parts/module-arguments.html
    flake-parts.lib.mkFlake { inherit inputs; } (
      # top@{ config, withSystem, moduleWithSystem, ... }: # Unused for now
      _: {
        imports = with inputs; [
          git-hooks.flakeModule
          treefmt-nix.flakeModule
        ];
        systems = [
          "x86_64-linux"
          "aarch64-linux"
          "x86_64-darwin"
          "aarch64-darwin"
          # ...
        ];
        perSystem =
          {
            config,
            pkgs,
            system,
            self',
            ...
          }:
          let
            dart2 = inputs.oldDartNixpkgs.legacyPackages.${system}.dart;
            crafting-interpreters-script =
              interpreter:
              pkgs.writers.writeHaskellBin "crafting-interpreters-script"
                {
                  libraries = [
                    pkgs.haskellPackages.shh
                    pkgs.haskellPackages.temporary
                  ];
                }
                ''
                  {-# LANGUAGE TemplateHaskell #-}

                  import Shh
                  import System.IO.Temp (withSystemTempDirectory)
                  import System.Environment (getArgs)
                  import Data.List (sort)
                  import Control.Monad (forM_)
                  import System.Exit (exitFailure)

                  -- Load binaries from Nix packages. The dependencies will be captured
                  -- in the closure.
                  loadFromBins ["${dart2}", "${pkgs.gnumake}", "${pkgs.uutils-coreutils-noprefix}"]

                  chapterMap :: [(String, String)]
                  chapterMap =
                    [ ("4", "chap04_scanning"), ("04", "chap04_scanning")
                    , ("6", "chap06_parsing"), ("06", "chap06_parsing")
                    , ("7", "chap07_evaluating"), ("07", "chap07_evaluating")
                    , ("8", "chap08_statements"), ("08", "chap08_statements")
                    , ("9", "chap09_control"), ("09", "chap09_control")
                    , ("10", "chap10_functions")
                    , ("11", "chap11_resolving")
                    , ("12", "chap12_classes")
                    , ("13", "chap13_inheritance")
                    ]

                  orderedChapters :: [String]
                  orderedChapters =
                    [ "chap04_scanning"
                    , "chap06_parsing"
                    , "chap07_evaluating"
                    , "chap08_statements"
                    , "chap09_control"
                    , "chap10_functions"
                    , "chap11_resolving"
                    , "chap12_classes"
                    , "chap13_inheritance"
                    ]

                  getChapter :: String -> Maybe String
                  getChapter arg = lookup arg chapterMap

                  main :: IO ()
                  main = withSystemTempDirectory "crafting-interpreters-tests" $ \tmpDir -> do
                    args <- getArgs
                    targets <- if null args
                               then pure orderedChapters
                               else case mapM getChapter args of
                                      Just ts -> pure ts
                                      Nothing -> do
                                        echo "Error: Invalid chapter number provided."
                                        exitFailure

                    -- Copy repository to temporary directory
                    echo $ "Copying ${inputs.crafting-interpreters} to temporary directory " <> tmpDir <> "..."
                    cp "--no-preserve=all" "-r" "${inputs.crafting-interpreters}/." tmpDir
                    -- Change to it
                    cd tmpDir
                    cd "./tool"
                    -- Pull dependencies
                    dart "pub" "get"
                    cd tmpDir
                    -- Run the tests!
                    forM_ targets $ \target -> do
                      echo "--------------------------------------------------------------------------------"
                      echo ("Running " ++ target ++ "...")
                      echo "--------------------------------------------------------------------------------"
                      dart "tool/bin/test.dart" target "--interpreter" "${pkgs.lib.getExe interpreter}" "--arguments" ("--" ++ target)
                '';
            haskellPackages = pkgs.haskell.packages.ghc9122; # GHC 9.12.2
            hoxPkg = haskellPackages.callPackage ./hox/hox.nix { };
            ghcWithDeps = haskellPackages.ghcWithPackages (p: hoxPkg.propagatedBuildInputs);
            doctestScript = pkgs.writeShellApplication {
              name = "hox-doctest";
              runtimeInputs = [
                ghcWithDeps
                haskellPackages.doctest
              ];
              text = ''
                cd hox
                export NIX_GHC=${ghcWithDeps}/bin/ghc
                doctest -XGHC2024 -isrc src/ -w -Wdefault --verbose
              '';
            };
            weederScript = pkgs.writeShellApplication {
              name = "hox-weeder";
              runtimeInputs = [
                ghcWithDeps
                haskellPackages.weeder
              ];
              text = ''
                rm -rf hox/.hie/* hox/dist-newstyle/temp/*
                mkdir -p hox/.hie hox/dist-newstyle/temp
                cd hox
                ghc -XGHC2024 -isrc -fwrite-ide-info -hiedir=.hie -odir=dist-newstyle/temp -hidir=dist-newstyle/temp -o dist-newstyle/temp/Main --make app/Main.hs
                weeder --hie-directory=.hie
              '';
            };
          in
          {
            apps = {
              hox-crafting-interpreters-tests = {
                type = "app";
                program = "${crafting-interpreters-script self'.packages.hox}/bin/crafting-interpreters-script";
                meta.description = "Run the Crafting Interpreters test suite. Usage: nix run .#tests -- [chapters...]";
              };
              hoxDoctest = {
                type = "app";
                program = "${pkgs.lib.getExe doctestScript}";
                meta.description = "Run Hox's doctests from Nix.";
              };
              hoxWeeder = {
                type = "app";
                program = "${pkgs.lib.getExe weederScript}";
                meta.description = "Run Hox's weeder unused code detector from Nix.";
              };
            };

            devShells = rec {
              default = hox;
              hox = pkgs.mkShell {
                shellHook = ''
                  ${config.pre-commit.installationScript}
                '';
                packages =
                  (with haskellPackages; [
                    haskell-language-server
                    cabal-install
                    doctest
                    weeder
                  ])
                  ++ [
                    pkgs.just
                    pkgs.gnumake
                    dart2
                  ];
              };
            };

            packages = {
              default = hoxPkg;
              hox = hoxPkg;
              hox-test = pkgs.haskell.lib.overrideCabal hoxPkg (drv: {
                # Tests are enabled by default
                # Copy test binary to output bin directory
                postInstall = (drv.postInstall or "") + ''
                  install -D dist/build/hox-test/hox-test $out/bin/hox-test
                '';
                mainProgram = "hox-test";
              });
              hox-bench = pkgs.haskell.lib.overrideCabal hoxPkg (drv: {
                # Configure to build benchmarks
                doBenchmark = true;
                # Copy benchmark binary to output bin directory
                postInstall = (drv.postInstall or "") + ''
                  install -D dist/build/hox-bench/hox-bench $out/bin/hox-bench
                '';
                mainProgram = "hox-bench";
              });
            };

            # Git hooks
            pre-commit = {
              settings = {
                hooks = {
                  # Formatters
                  treefmt = {
                    enable = true;
                    packageOverrides.treefmt = config.treefmt.build.wrapper;
                  };
                  actionlint.enable = true; # GitHub Actions
                  convco.enable = true; # Conventional commits
                  gitlint.enable = true; # Git commit messages
                  check-merge-conflicts.enable = true; # Check for merge conflicts
                  hlint = {
                    enable = true; # Haskell linter
                    package = haskellPackages.hlint;
                  };
                  cabal2nix = {
                    enable = true; # Cabal to Nix package definition
                    settings.outputFilename = "hox/hox.nix";
                    extraPackages = [ haskellPackages.cabal2nix ];
                  };
                  markdownlint.enable = true; # Markdown
                  # Custom
                  # Haskell doctests
                  doctest = {
                    enable = true;
                    name = "doctest";
                    entry = "${self'.apps.hoxDoctest.program}";
                    pass_filenames = false;
                  };
                  # Haskell unused code detector
                  weeder = {
                    enable = true;
                    name = "weeder";
                    entry = "${self'.apps.hoxWeeder.program}";
                    pass_filenames = false;
                  };
                };
              };
            };

            # Formatters
            treefmt = {
              programs = {
                nixfmt = {
                  enable = true; # Nix
                  excludes = [ "hox/hox.nix" ]; # Generated by cabal2nix
                };
                cabal-gild = {
                  enable = true; # Cabal
                  package = haskellPackages.cabal-gild;
                };
                ormolu = {
                  enable = true; # Haskell
                  package = haskellPackages.ormolu;
                };
              };
            };
          };
      }
    );
}
