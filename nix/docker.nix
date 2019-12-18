{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let
  system =
    (import <nixpkgs/nixos/lib/eval-config.nix> {
      modules = [
        {
          imports = [ <nixpkgs/nixos/modules/profiles/docker-container.nix> ];
        }
      ];
    }).config.system;

  mini-system =
    runCommand "mini-system" {} ''
      mkdir -p $out
      cd ${system.build.etc}
      ${rsync}/bin/rsync -aR etc/{services,protocols,ssl,nsswitch.conf} $out

      mkdir -p $out/bin
      ln -s ${bash}/bin/sh $out/bin/sh
    '';

  main = import ./default.nix { nixpkgs = pkgs; };

in
  dockerTools.buildImage {
    name = "zn";
    tag = "latest";

    contents = symlinkJoin {
      name = "zn-contents";
      paths = [
        mini-system
        main
      ];
    };

    config = {
      Cmd = ["${main}/bin/zn"];
      WorkingDir = "/work";

      Env = [
        "LANG=en_US.UTF-8"
        "LOCALE_ARCHIVE=${glibcLocales.override { allLocales = false; }}/lib/locale/locale-archive"
      ];

      Volumes = {
        "/work" = {};
      };
    };
  }
