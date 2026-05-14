{
    description = "A very basic Haskell dev env flake";

    inputs = {
        nixpkgs-stable.url   = "github:nixos/nixpkgs/nixos-25.11";
        nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
        flake-utils.url      = "github:numtide/flake-utils";
    };

    outputs = { self, nixpkgs-stable, nixpkgs-unstable, flake-utils }:
        flake-utils.lib.eachDefaultSystem (system:
        let
            pkgs-stable   = import nixpkgs-stable   { inherit system; };
            pkgs-unstable = import nixpkgs-unstable { inherit system; };
        in
        {
            devShells.default = pkgs-unstable.mkShell {
                packages = [
                    pkgs-unstable.haskellPackages.haskell-language-server
                    pkgs-unstable.cabal-install
                    pkgs-unstable.haskell.compiler.ghc910
                    pkgs-unstable.zlib
                    pkgs-unstable.haskellPackages.hoogle
                    pkgs-unstable.stylish-haskell
                ];
            };
        }
    );
}
