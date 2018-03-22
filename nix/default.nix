# d() { nix-prefetch-git https://github.com/nixos/nixpkgs-channels.git refs/heads/nixos-$1 $1 nix/nixpkgs-$1.json; }; d "18.03"
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
  haskellPackages' = pkgs: with pkgs; haskellPackages.override {
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
    , network-simple, process, random, regex-tdfa, regex-tdfa-text, retry, safe
    , split, stdenv, stm, stm-chans, streaming-commons, strptime, tagged
    , tagsoup, telegram-api, template-haskell, text, text-format, text-icu
    , text-regex-replace, time, tls, transformers, uglymemo, unix, unix-time
    , unordered-containers, x509-system, xml-conduit, hspec, hpack
    , groundhog, groundhog-th, groundhog-sqlite, monad-control, transformers-base
    , mkDerivation, cliDeps
    }@args:

    let
      # This little cute idea that I could just only have the things I need
      # to create the version string:
      # > gitHead = lib.removePrefix "ref: " (lib.fileContents ../.git/HEAD);
      # > gitCommit = lib.fileContents ((toString ../.git) + "/" + gitHead);
      # > matches name "\.git/(HEAD|${gitHead}|/objects/${gitCommit})$"
      # …The problem is that it's just too much code and it won't be shipped
      # in the container, so why bother. Also, it's an optimization that
      # be wrecked in case .git's only got a packfile or there's some
      # other implementation detail that I shouldn't bother to optimize around.
      ### This should work, but the object path must be changed a little.

      prefix = "^" + toString ../. + "/";
      matches = name: pattern: (builtins.match (prefix + pattern) name) != null;
      source = builtins.filterSource (name: type: # true means keep
          matches name "(\.git|src|test|lib|[^/]+\.cabal$)($|/.*)"
        ) ../.;

      hsDeps = builtins.attrValues (removeAttrs args [
        "mkDerivation" "cliDeps"
      ]);
    in
      mkDerivation {
        pname = "zn";
        version = "0.1.0.0";
        src = source;
        isLibrary = false;
        isExecutable = true;
        enableSharedExecutables = false;
        enableSharedLibraries   = false;
        executableHaskellDepends = hsDeps ++ cliDeps;
        testHaskellDepends = hsDeps;
        description = "IRC bot";
        license = stdenv.lib.licenses.free;
      };

in
  with pkgs_17_09;
  with haskell.lib;

  let
    cliDeps = [ git coreutils ];
  in

  with haskellPackages;

  dontCheck
    (justStaticExecutables
      (callPackage zn {
        inherit (haskellPackages' pkgs_17_09) irc-client telegram-api;
        inherit cliDeps;
      }))
