{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let
  hackagePackages' = haskellPackages.override {
    overrides = self: super: let
      in {
        servant = haskell.lib.dontCheck (self.callHackage "servant" "0.10" {});
        servant-client = haskell.lib.dontCheck (self.callHackage "servant-client" "0.10" {});
      }; };

  irc-client = haskellPackages.callPackage (import ./irc-client.nix) {};
  telegram-api = hackagePackages'.telegram-api;

  zn = { mkDerivation, aeson, array, async, base, base64-bytestring, binary
    , bytestring, case-insensitive, conduit, conduit-combinators
    , conduit-extra, connection, containers, cryptonite, data-default, either, exceptions
    , extra, filepath, http-client, http-client-tls, http-types, ini, irc-client, irc-ctcp
    , irc-conduit, lens, megaparsec, mtl, network, network-simple, process, regex-tdfa
    , regex-tdfa-text, retry, safe, split, stdenv, stm, stm-chans
    , streaming-commons, strict, strptime, tagged, tagsoup, telegram-api
    , template-haskell, text, text-format, text-icu, text-regex-replace, time, tls, transformers
    , uglymemo, unix, unix-time, unordered-containers, x509-system, xml-conduit, hspec
    , pkgs
    }:

    let
      deps = [
        aeson array async base base64-bytestring binary
        bytestring case-insensitive conduit conduit-combinators
        conduit-extra connection containers cryptonite data-default either exceptions
        extra filepath http-client http-client-tls http-types ini irc-client irc-ctcp
        irc-conduit lens megaparsec mtl network network-simple process regex-tdfa
        regex-tdfa-text retry safe split stdenv stm stm-chans
        streaming-commons strict strptime tagged tagsoup telegram-api
        template-haskell text text-format text-icu text-regex-replace time tls transformers
        uglymemo unix unix-time unordered-containers x509-system xml-conduit hspec
        git coreutils
      ];
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
        testHaskellDepends = deps;
        description = "IRC bot";
        license = stdenv.lib.licenses.free;
      };

in
  haskell.lib.dontCheck
    (haskell.lib.justStaticExecutables
        (haskellPackages.callPackage zn { inherit irc-client telegram-api; }))
