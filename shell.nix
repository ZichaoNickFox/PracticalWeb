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
    p.mtl
    p.containers
    p.data-default
    p.data-has
    p.string-random
    p.transformers
    p.katip
  ]);
in
pkgs.mkShell {
  buildInputs = [ ghc ];
}