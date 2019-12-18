{ stdenv, bundlerEnv }:

let env = bundlerEnv {
  name = "names-env";
  gemdir = ./.;
};

in stdenv.mkDerivation {
  name = "names";
  buildInputs = [ env.wrappedRuby ];

  src = ./.;
  installPhase = ''
    cp -r $src $out
  '';
}
