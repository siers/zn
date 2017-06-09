{ pkgs ? import <nixpkgs> {} }:

let main =
    pkgs.haskell.lib.justStaticExecutables
        (pkgs.haskellPackages.callPackage (import ./default.nix) {});

in pkgs.dockerTools.buildImage {
    name = "zn";
    tag = "latest";

    config = {
        Cmd = ["${main}/bin/zn"];
    };
}
