{
  description = "Onyx programming language flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
  };

  outputs = {
    self,
    nixpkgs,
  }: let
    forAllSys = nixpkgs.lib.genAttrs nixpkgs.lib.platforms.all;
  in {
    packages = forAllSys (system: let
      pkgs = import nixpkgs {inherit system;};

      onyx = pkgs.callPackage ./default.nix {};
    in {
      default = onyx;
    });
  };
}
