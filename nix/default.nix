# d() { nix-prefetch-git https://github.com/nixos/nixpkgs-channels.git refs/heads/nixos-$1 > nix/nixpkgs-$1.json; }; d "18.03"
{ nixpkgs ? import <nixpkgs> {} }:

let
  pkgs = nixpkgs;
  gitignore = nixpkgs.nix-gitignore;

  irc-client = pkgs.haskellPackages.callPackage
    ({ mkDerivation, base, bytestring, conduit, connection, containers
     , contravariant, exceptions, irc-conduit, irc-ctcp, mtl
     , network-conduit-tls, old-locale, profunctors, stm, stm-chans
     , text, time, tls, transformers, x509, x509-store, x509-validation
     }:
     mkDerivation {
       pname = "irc-client";
       version = "1.1.2.1";
       sha256 = "1zaa8na730m96flgiyzcwq95v2ianvflsw3abvdavf7xpq4s71ld";
       libraryHaskellDepends = [
         base bytestring conduit connection containers contravariant
         exceptions irc-conduit irc-ctcp mtl network-conduit-tls old-locale
         profunctors stm stm-chans text time tls transformers x509
         x509-store x509-validation
       ];
       description = "An IRC client library";
       license = pkgs.lib.licenses.mit;
     }) {};
in

let
  zn = { aeson, array, async, base, base64-bytestring, binary, bytestring
    , case-insensitive, conduit, conduit-combinators, conduit-extra
    , connection, containers, cryptonite, data-default, either, exceptions
    , extra, filepath, http-client, http-client-tls, http-types, ini
    , irc-client, irc-ctcp, irc-conduit, lens, magic, megaparsec, mtl, network
    , network-simple, parser-combinators, process, random, regex-tdfa-text, retry, safe
    , split, stdenv, stm, stm-chans, streaming-commons, strptime, tagged
    , tagsoup, template-haskell, text, text-format, text-icu
    , text-regex-replace, time, tls, transformers, uglymemo, unix, unix-time
    , unordered-containers, x509-system, hspec, xml-conduit
    , groundhog, groundhog-th, groundhog-sqlite, monad-control, transformers-base
    , mkDerivation, buildDeps, runtimeDeps
    }@args:

    let
      source = gitignore.gitignoreSourcePure [
        ../.gitignore
        ''
          data/
          nix/
          *.nix
          !.git
        ''
        (if builtins.pathExists ~/.gitignore then builtins.readFile ~/.gitignore else "")
      ] ../.;

      hsDeps = builtins.attrValues (removeAttrs args [
        "mkDerivation" "buildDeps" "runtimeDeps"
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
        executableHaskellDepends = hsDeps ++ buildDeps;
        testHaskellDepends = hsDeps;
        description = "IRC bot";
        license = stdenv.lib.licenses.free;
        postInstall = ''
          wrapProgram $out/bin/zn --prefix PATH : ${pkgs.lib.makeBinPath runtimeDeps}
        '';
      };

in
  with pkgs;
  with haskell.lib;

  let
    names = callPackage ../lib/names-lv/names.nix {};
    runtimeDeps = [ coreutils names units ];
    buildDeps = runtimeDeps ++ [ makeWrapper cabal-install git sqlite ]; # also for nix-shell
  in

  with haskellPackages;

  dontCheck
    (justStaticExecutables
      (callPackage zn {
        inherit buildDeps runtimeDeps irc-client;
      }))
