{
  inputs.nixpkgs.url = github:NixOS/nixpkgs/nixos-25.05;
  inputs.flake-utils.url = github:numtide/flake-utils;
  outputs = { self, flake-utils, nixpkgs }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        our-place = (pkgs.haskellPackages.callPackage ./our-place.nix { });
      in
        rec {
          packages.default = our-place;
          devShells.default = our-place.env;
          devShells.full =
            pkgs.haskellPackages.shellFor {
              packages = hpkgs: [ our-place ];
              nativeBuildInputs = [
                pkgs.cabal-install
                pkgs.haskell-language-server
                pkgs.cabal2nix
                pkgs.gnumake
              ];
            };
        });
}
