{ pkgs ? import <nixpkgs> { } }:

let
  haskellPackages = pkgs.haskellPackages;
  serialport-unix = haskellPackages.callCabal2nix "serialport-unix" ./. { };
in
haskellPackages.shellFor {
  packages = p: [ serialport-unix ];
  nativeBuildInputs = with haskellPackages; [
    cabal-install
    ghc
  ];
}
