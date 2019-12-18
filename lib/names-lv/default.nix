{ nixpkgs ? import <nixpkgs> {} }:

with nixpkgs;

callPackage ./names.nix {}
