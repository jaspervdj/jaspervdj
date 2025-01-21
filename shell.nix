{ pkgs ? import <nixpkgs> {} }:
with pkgs;
mkShell rec {
  buildInputs = [
    cabal-install
    graphviz
    haskell.compiler.ghc96
    imagemagick
    openssh
    pkg-config
    rsync
    texlive.combined.scheme-full  # Just for `standalone.cls`, urgh!
    ghostscript  # For tex as well.
    zlib.dev
  ] ++ lib.optionals stdenv.isDarwin (with darwin.apple_sdk.frameworks; [
    Cocoa
    CoreServices
  ]);
}
