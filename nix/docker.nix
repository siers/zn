{ pkgs ? import <nixpkgs> {} }:

let
  system =
    (import <nixpkgs/nixos/lib/eval-config.nix> {
      modules = [
        {
          imports = [ <nixpkgs/nixos/modules/profiles/docker-container.nix> ];
        }
      ];
    }).config.system;

  main = import ./default.nix { inherit pkgs; };

in
  pkgs.dockerTools.buildImage {
    name = "zn";
    tag = "latest";

    contents = pkgs.symlinkJoin {
      name = "zn-contents";
      paths = [
        system.build.etc
        system.path
        pkgs.bash
        pkgs.coreutils
        main
      ];
    };

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
