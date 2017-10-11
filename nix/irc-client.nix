{ mkDerivation, base, bytestring, conduit, connection, containers
, contravariant, exceptions, irc-conduit, irc-ctcp, mtl
, network-conduit-tls, old-locale, profunctors, stm, stm-conduit
, text, time, tls, transformers, x509, x509-store, x509-validation
, callPackage, stdenv
}:

callPackage
  ({ mkDerivation, base, bytestring, conduit, connection, containers
    , contravariant, exceptions, irc-conduit, irc-ctcp, mtl
    , network-conduit-tls, old-locale, profunctors, stm, stm-conduit
    , text, time, tls, transformers, x509, x509-store, x509-validation
    }:
    mkDerivation {
      pname = "irc-client";
      version = "1.0.0.1";
      sha256 = "0qg4bvl82wwm7jlrxsmc4aw51rfdygq8qzm6x7j4121av5wbk095";
      libraryHaskellDepends = [
        base bytestring conduit connection containers contravariant
        exceptions irc-conduit irc-ctcp mtl network-conduit-tls old-locale
        profunctors stm stm-conduit text time tls transformers x509
        x509-store x509-validation
      ];
      homepage = "https://github.com/barrucadu/irc-client";
      description = "An IRC client library";
      license = stdenv.lib.licenses.mit;
      hydraPlatforms = stdenv.lib.platforms.none;
    }) {}
