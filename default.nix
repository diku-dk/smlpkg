{ pkgs ? import <nixpkgs> {},
}:

pkgs.stdenv.mkDerivation rec {
  name = "smlpkg";

  src = ./.;

  nativeBuildInputs = [ pkgs.mlton ];

  checkInputs = [ pkgs.unzip ];

  enableParallelBuilding = true;

  doCheck = true;

  # Set as an environment variable in all the phase scripts.
  MLCOMP = "mlton";

  buildPhase = ''
    make all
  '';

  installPhase = ''
    make install prefix=$out
  '';

  # We cannot run the pkgtests, as Nix does not allow network
  # connections.
  checkPhase = ''
    make -C src test
  '';

}
