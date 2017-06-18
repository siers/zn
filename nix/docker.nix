{ pkgs ? import <nixpkgs> {} }:

let main =
    pkgs.haskell.lib.dontCheck
        (pkgs.haskell.lib.justStaticExecutables
            (pkgs.haskellPackages.callPackage (import ./default.nix) {}));

in pkgs.dockerTools.buildImage {
    name = "zn";
    tag = "latest";

    contents = [
        main
        pkgs.coreutils
        pkgs.bash
    ];

    config = {
        Cmd = ["${main}/bin/zn"];
        WorkingDir = "/work";

        Environment = {
            "LANG" = "en_US.UTF-8";
        };

        Volumes = {
            "/work" = {};
        };
    };
}
