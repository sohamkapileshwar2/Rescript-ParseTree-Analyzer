{
  description = "Rescript Parsetree";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      system = "aarch64-darwin";
      pkgs = nixpkgs.legacyPackages.${system};
    in
    {
      devShell = pkgs.mkShell {
        buildInputs = with pkgs; [

          # Haskell packages
          haskell.compiler.ghc925
          cabal-install

          # OCaml packages
        ];
      };
      packages.${system}.default = self.devShell;
      devShells.x86_64-darwin.default = self.devShell;
    };
}