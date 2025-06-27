{
  description = "My implementations of Lox, a programming language from the book 'Crafting Interpreters' by Robert Nystrom";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.05";
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
              testCase:
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
                    cp "--no-preserve=all" "-r" "${inputs.crafting-interpreters}/." tmpDir
                    -- Change to it
                    cd tmpDir
                    pwd
                    cd "./tool"
                    -- Pull dependencies
                    dart "pub" "get"
                    cd tmpDir
                    -- Run the tests!
                    dart "tool/bin/test.dart" "${testCase}" "--interpreter" "${self'.packages.hox}/bin/hox"
                '';
            mkTestApp = testCase: {
              type = "app";
              program = "${crafting-interpreters-script testCase}/bin/crafting-interpreters-script";
              meta.description = "Run the Crafting Interpreters test suite for ${testCase}";
            };
            haskellPackages = pkgs.haskell.packages.ghc9101; # GHC 9.10.1
          in
          {
            apps = {
              test-chapter04 = mkTestApp "chap04_scanning";
            };

            devShells.default = pkgs.mkShell {
              shellHook = ''
                ${config.pre-commit.installationScript}
              '';
              packages =
                (with haskellPackages; [
                  haskell-language-server
                  cabal-install
                  doctest
                ])
                ++ [
                  pkgs.gnumake
                  dart2
                ];
            };

            packages = rec {
              hox = haskellPackages.callPackage ./hox.nix { };
              default = hox;
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
                  flake-checker.enable = true; # Nix
                  hlint.enable = true; # Haskell linter
                  cabal2nix = {
                    enable = true; # Cabal to Nix pacakge definition
                    settings.outputFilename = "hox.nix";
                    extraPackages = [ haskellPackages.cabal2nix ];
                  };
                  markdownlint.enable = true; # Markdown
                };
              };
            };

            # Formatters
            treefmt = {
              programs = {
                nixfmt = {
                  enable = true; # Nix
                  excludes = [ "hox.nix" ]; # Autogenerated by cabal2nix
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
