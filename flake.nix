{
  description = "PNG distance-map generator in Haskell";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
        };
        hsPkgs = pkgs.haskellPackages;
        package = hsPkgs.callCabal2nix "png-distance-map" self { };
      in
      {
        packages.default = package;

        apps.default = {
          type = "app";
          program = "${package}/bin/png-distance-map";
        };

        devShells.default = hsPkgs.shellFor {
          packages = p: [ package ];
          buildInputs = with pkgs; [ cabal-install ];
        };
      }
    );
}
