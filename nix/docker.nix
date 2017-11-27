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
    runCommand "mini-system" {} "
      mkdir -p $out
      cd ${system.build.etc}
      ${rsync}/bin/rsync -aR etc/{services,protocols,ssl,nsswitch.conf} $out
    ";

  main = import ./default.nix { inherit pkgs; };

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
