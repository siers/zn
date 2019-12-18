let
  pkgs = import <nixpkgs> {};
in
  (import nix/default.nix { nixpkgs = pkgs; }).env
