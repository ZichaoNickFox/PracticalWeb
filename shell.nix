{ pkgs ? import <nixpkgs> {} }:
let
  ghc = pkgs.haskell.packages.ghc963.ghcWithPackages (p: [
    p.base
    p.hspec
    p.text
    p.lens
    p.lens-aeson
    p.aeson
    p.aeson-qq
    p.pcre-heavy
    p.pcre-light
    p.safe
    p.classy-prelude
    p.mtl
  ]);
in
pkgs.mkShell {
  buildInputs = [ ghc ];
}