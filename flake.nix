{
  description = "jaspervdj";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = inputs :
    inputs.flake-utils.lib.eachDefaultSystem (system:
     let pkgs = inputs.nixpkgs.legacyPackages.${system};
         haskell = pkgs.haskell.packages.ghc98;

     in
  {
    packages = {
      default = (haskell.callCabal2nix "jaspervdj" ./. {}).overrideAttrs (old: {
        nativeBuildInputs = (old.nativeBuildInputs or []) ++ [
          pkgs.makeWrapper
        ];

        postFixup = (old.postFixup or "") + ''
          wrapProgram $out/bin/jaspervdj \
            --prefix PATH : ${pkgs.lib.makeBinPath [
              pkgs.imagemagick
              pkgs.ghostscript
            ]}
        '';
      });
    };
    devShells = {
      default = pkgs.mkShell {
        packages = [
          haskell.ghc
          pkgs.imagemagick
          pkgs.cabal-install
          pkgs.ffmpeg
          pkgs.stylish-haskell
          pkgs.zlib
        ];
      };
    };
  });
}
