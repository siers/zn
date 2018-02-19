# nix-prefetch-git https://github.com/nixos/nixpkgs-channels.git refs/heads/nixos-unstable > nixpkgs-version.json
{ pkgs ? import <nixpkgs> {} }:

let
  pinned = json:
    let
      version = pkgs.lib.importJSON (./nixpkgs- + json + ".json");
      source = pkgs.fetchFromGitHub {
        owner = "NixOS";
        repo = "nixpkgs-channels";
        inherit (version) rev sha256;
      };
    in import source {};

  pkgs_17_09 = pinned "17.09";
  pkgs_unstable = pinned "unstable";
in

let
  haskellPackages' = with pkgs_17_09; haskellPackages.override {
    overrides = self: super: with haskell.lib;
      {
        servant = dontCheck (self.callHackage "servant" "0.10" {});
        servant-client = dontCheck (self.callHackage "servant-client" "0.10" {});
        irc-client = dontCheck (super.callHackage "irc-client" "1.0.0.1" {});
      };
  };

  zn = { aeson, array, async, base, base64-bytestring, binary, bytestring
    , case-insensitive, conduit, conduit-combinators, conduit-extra
    , connection, containers, cryptonite, data-default, either, exceptions
    , extra, filepath, http-client, http-client-tls, http-types, ini
    , irc-client, irc-ctcp, irc-conduit, lens, megaparsec, mtl, network
    , network-simple, process, regex-tdfa, regex-tdfa-text, retry, safe, split
    , stdenv, stm, stm-chans, streaming-commons, strict, strptime, tagged
    , tagsoup, telegram-api, template-haskell, text, text-format, text-icu
    , text-regex-replace, time, tls, transformers, uglymemo, unix, unix-time
    , unordered-containers, x509-system, xml-conduit, hspec, hpack
    , mkDerivation
    }@args:

    let
      hsDeps = builtins.attrValues (removeAttrs args [ "mkDerivation" ]);
      cliDeps = with pkgs_17_09; [ git coreutils ];
      deps = hsDeps ++ cliDeps;
    in
      mkDerivation {
        pname = "zn";
        version = "0.1.0.0";
        src = ./..;
        isLibrary = false;
        isExecutable = true;
        enableSharedExecutables = false;
        enableSharedLibraries   = false;
        executableHaskellDepends = deps;
        testHaskellDepends = hsDeps;
        description = "IRC bot";
        license = stdenv.lib.licenses.free;
      };

in
  with pkgs_17_09;
  with haskell.lib;
  with haskellPackages;

  dontCheck
    (justStaticExecutables
      (callPackage zn {
        inherit (haskellPackages') irc-client telegram-api;
      }))
