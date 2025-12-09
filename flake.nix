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
              testCase: interpreter:
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

                  -- Load binaries from Nix packages. The dependencies will be captured
                  -- in the closure.
                  loadFromBins ["${dart2}", "${pkgs.gnumake}", "${pkgs.uutils-coreutils-noprefix}"]

                  main :: IO ()
                  main = withSystemTempDirectory "crafting-interpreters-tests" $ \tmpDir -> do
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
                    dart "tool/bin/test.dart" "${testCase}" "--interpreter" "${pkgs.lib.getExe interpreter}" "--arguments" "--${testCase}"
                '';
            mkTestApp = testCase: interpreter: {
              type = "app";
              program = "${crafting-interpreters-script testCase interpreter}/bin/crafting-interpreters-script";
              meta.description = "Run the Crafting Interpreters test suite for ${testCase}, using the '${interpreter.meta.mainProgram}' interpreter.";
            };
            haskellPackages = pkgs.haskell.packages.ghc9122; # GHC 9.12.2
            hoxPkg = haskellPackages.callPackage ./hox.nix { };
            ghcWithDeps = haskellPackages.ghcWithPackages (p: hoxPkg.propagatedBuildInputs);
            doctestScript = pkgs.writeShellApplication {
              name = "hox-doctest";
              runtimeInputs = [
                ghcWithDeps
                haskellPackages.doctest
              ];
              text = "doctest -XGHC2024 -isrc src/ -w -Wdefault --verbose";
            };
            weederScript = pkgs.writeShellApplication {
              name = "hox-weeder";
              runtimeInputs = [
                ghcWithDeps
                haskellPackages.weeder
              ];
              text = ''
                mkdir -p .hie dist-newstyle/temp
                ghc -XGHC2024 -isrc -fwrite-ide-info -hiedir=.hie -odir=dist-newstyle/temp -hidir=dist-newstyle/temp -o dist-newstyle/temp/Main --make app/Main.hs
                weeder --hie-directory=.hie
              '';
            };
          in
          {
            apps = {
              test-chapter04 = mkTestApp "chap04_scanning" self'.packages.hox;
              test-chapter06 = mkTestApp "chap06_parsing" self'.packages.hox;
              test-chapter07 = mkTestApp "chap07_evaluating" self'.packages.hox;
              test-chapter08 = mkTestApp "chap08_statements" self'.packages.hox;
              test-chapter09 = mkTestApp "chap09_control" self'.packages.hox;
              test-chapter10 = mkTestApp "chap10_functions" self'.packages.hox;
              test-chapter11 = mkTestApp "chap11_resolving" self'.packages.hox;
              test-chapter12 = mkTestApp "chap12_classes" self'.packages.hox;
              # test-chapter13 = mkTestApp "chap13_inheritance" self'.packages.hox;

              hoxDoctest = {
                type = "app";
                program = "${doctestScript}/bin/hox-doctest";
                meta.description = "Run Hox's doctests from Nix.";
              };
              hoxWeeder = {
                type = "app";
                program = "${weederScript}/bin/hox-weeder";
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
                    pkgs.gnumake
                    dart2
                  ];
              };
            };

            packages = rec {
              default = hox;
              hox = hoxPkg;
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
                    enable = true; # Cabal to Nix pacakge definition
                    settings.outputFilename = "hox.nix";
                    extraPackages = [ haskellPackages.cabal2nix ];
                  };
                  markdownlint.enable = true; # Markdown
                  # Custom
                  # Haskell doctests
                  doctest = {
                    enable = true;
                    name = "doctest";
                    entry = "${pkgs.lib.getExe doctestScript}";
                    pass_filenames = false;
                  };
                  # Haskell unused code detector
                  weeder = {
                    enable = true;
                    name = "weeder";
                    entry = "${pkgs.lib.getExe weederScript}";
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
                  excludes = [ "hox.nix" ]; # Generated by cabal2nix
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
