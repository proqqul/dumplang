{
  outputs = { self, nixpkgs, ... }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
    in {
      devShells.${system}.default = pkgs.haskellPackages.shellFor {
        packages = hpkgs: [
          (hpkgs.callCabal2nix "dumplang" ./compiler {})
        ];
        nativeBuildInputs = with pkgs; [
          zig
          zls
          cabal-install
          haskell-language-server
          hlint
          cabal2nix
          haskellPackages.implicit-hie
          xxd # because we are 1337 binary haxxorz
        ];
      };
    };
}
