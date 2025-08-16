{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs    = nixpkgs.legacyPackages.${system};
        overlay = final: prev: {
          hello = prev.callCabal2nix "hello" ./. { };
        };
        haskellPackages = pkgs.haskellPackages.extend overlay;
      in {
        # nix build
        packages.default = haskellPackages.hello;

        # nix develop
        devShells.default = haskellPackages.shellFor {
          packages = p: [ p.hello ];
          buildInputs = with haskellPackages; [
            cabal-install
            haskell-language-server
          ];
        };
      }
    );
}
