{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    zig = {
      url = "github:mitchellh/zig-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    zls = {
      url = "github:zigtools/zls";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        zig-overlay.follows = "zig";
      };
    };
  };
  outputs = { self, nixpkgs, zig, zls, ... }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
    in {
      devShells.${system}.default = pkgs.haskellPackages.shellFor {
        packages = hpkgs: [
          (hpkgs.callCabal2nix "dumplang" ./compiler {})
        ];
        nativeBuildInputs = with pkgs; [
          zig.packages.${system}.master
          zls.packages.${system}.zls
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
