{
    pkgs ? import (builtins.fetchTarball "https://github.com/nixos/nixpkgs/archive/3d8a93602bc54ece7a4e689d9aea1a574e2bbc24.tar.gz") { },
    compiler ? "ghc927"
}:
let
    haskellPackages = pkgs.haskell.packages."${compiler}";
    ghc = haskellPackages.ghcWithPackages (p: [
        p.JuicyPixels
        p.random
    ]);
in
pkgs.stdenv.mkDerivation {
    name = "lazycollage";
    src = ./.;
    buildPhase = "${ghc}/bin/ghc -Wall -O2 -o lazycollage 2023-07-14-lazycollage.lhs";
    installPhase = "cp lazycollage $out";
}
