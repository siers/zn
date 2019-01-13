{ fetchgit }:

let
  source = fetchgit {
    name = "telegram-api";
    url = "git://github.com/siers/haskell-telegram-api";
    rev = "706a0f02fb495d2938f69e99a0a1e0076701918c";
    sha256 = "0wwq5jjn7vrbrmi1b7zjmms2398vz7r66shi0y5kfh9xy3w7f05n";
  };
in
  { mkDerivation, aeson, ansi-wl-pprint, base, bytestring, containers
  , filepath, hjpath, hspec, http-api-data, http-client
  , http-client-tls, http-media, http-types, mime-types, mtl
  , optparse-applicative, random, servant, servant-client
  , servant-client-core, stdenv, string-conversions, text
  , transformers, utf8-string
  }:
  mkDerivation {
    pname = "telegram-api";
    version = "0.7.1.1";
    src = source;
    enableSeparateDataOutput = true;
    libraryHaskellDepends = [
      aeson base bytestring containers http-api-data http-client
      http-media http-types mime-types mtl servant servant-client
      servant-client-core string-conversions text transformers
    ];
    testHaskellDepends = [
      aeson ansi-wl-pprint base filepath hjpath hspec http-client
      http-client-tls http-types optparse-applicative random servant
      servant-client servant-client-core text transformers utf8-string
    ];
    homepage = "http://github.com/klappvisor/haskell-telegram-api#readme";
    description = "Telegram Bot API bindings";
    license = stdenv.lib.licenses.bsd3;
  }
