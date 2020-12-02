{ nixpkgs ? import <nixos-unstable> {} }:
let
  inherit (nixpkgs) pkgs;
  inherit (pkgs) haskellPackages;

  haskellDeps = ps: with ps; [
    split
  ];

  ghc = haskellPackages.ghcWithPackages haskellDeps;

  nixPackages = [
    ghc
    pkgs.gdb
    haskellPackages.cabal-install
  ];
in
pkgs.stdenv.mkDerivation {
  name = "env";
  buildInputs = nixPackages;
}
