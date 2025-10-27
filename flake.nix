{
  inputs.nixpkgs.url = github:NixOS/nixpkgs/nixos-25.05;
  inputs.flake-utils.url = github:numtide/flake-utils;
  outputs = { self, flake-utils, nixpkgs }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        jfdm-hakyll = (pkgs.haskellPackages.callPackage ./jfdm-hakyll.nix { });
      in
        rec {
          packages.default = jfdm-hakyll;
          devShells.default = jfdm-hakyll.env;
          devShells.full =
            pkgs.haskellPackages.shellFor {
              packages = hpkgs: [ jfdm-hakyll ];
              nativeBuildInputs = [
                pkgs.cabal-install
                pkgs.haskell-language-server
                pkgs.cabal2nix
                pkgs.gnumake
              ];
            };
        });
}
