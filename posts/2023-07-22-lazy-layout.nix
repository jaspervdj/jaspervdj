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
    name = "lazy-layout";
    src = ./.;
    buildPhase = "${ghc}/bin/ghc -Wall -O2 -o lazy-layout 2023-07-22-lazy-layout.lhs";
    installPhase = "cp lazy-layout $out";
}
