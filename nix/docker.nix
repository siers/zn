{ pkgs ? import <nixpkgs> {} }:

let main =
    pkgs.haskell.lib.dontCheck
        (pkgs.haskell.lib.justStaticExecutables
            (pkgs.haskellPackages.callPackage (import ./default.nix) {}));

in pkgs.dockerTools.buildImage {
    name = "zn";
    tag = "latest";

    contents = [
        main pkgs.coreutils pkgs.bash pkgs.gnugrep pkgs.glibcLocales
    ];

    config = {
        Cmd = ["${main}/bin/zn"];
        WorkingDir = "/work";

        Env = [
            "LANG=en_US.UTF-8"
            "LOCALE_ARCHIVE=${pkgs.glibcLocales}/lib/locale/locale-archive"
        ];

        Volumes = {
            "/work" = {};
        };
    };
}
