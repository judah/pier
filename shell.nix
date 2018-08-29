with import <nixpkgs> {};

stdenv.mkDerivation rec {
  name = "env";

  env = buildEnv {
    name  = name;
    paths = buildInputs;
  };

  buildInputs = [
    ghc
    zlib
  ];

  shellHook = ''
    export LD_LIBRARY_PATH="${zlib}/lib/"
  '';
}
