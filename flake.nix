{
  description = "Cross platform serial port library for Unix";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        haskellPackages = pkgs.haskellPackages;
        serialport-unix = haskellPackages.callCabal2nix "serialport-unix" ./. { };
      in
      {
        packages.default = serialport-unix;

        devShells.default = haskellPackages.shellFor {
          packages = p: [ serialport-unix ];
          nativeBuildInputs = with haskellPackages; [
            cabal-install
            ghc
          ];
        };
      });
}
