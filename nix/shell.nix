{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

with nixpkgs;

pkgs.haskell.lib.dontCheck
    (pkgs.haskell.lib.justStaticExecutables
        pkgs.haskellPackages.callPackage (import ./default.nix) {})
