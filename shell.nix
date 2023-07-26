{ pkgs ? import <nixpkgs> {} }:
with pkgs;
mkShell rec {
  buildInputs = [
    cabal-install
    graphviz
    haskell.compiler.ghc925
    imagemagick
    openssh
    pkgconfig
    rsync
    texlive.combined.scheme-full  # Just for `standalone.cls`, urgh!
    zlib
  ] ++ lib.optionals stdenv.isDarwin (with darwin.apple_sdk.frameworks; [
    Cocoa
    CoreServices
  ]);

  # Ensure that libz.so and other libraries are available to TH
  # splices, cabal repl, etc.
  LD_LIBRARY_PATH = lib.makeLibraryPath buildInputs;
}
