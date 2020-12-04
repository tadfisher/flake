{
  description = "Tad's Nix configurations";

  inputs = {
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixos-hardware.url = "github:NixOS/nixos-hardware";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nur.url = "github:nix-community/NUR";
  };

  outputs = { self, ... }@inputs:
    let
      systems = [ "x86_64-linux" ];

    in {
      nixosConfigurations = import ./nixos/hosts inputs;
      nixosModules = import ./nixos/modules inputs;
      overlay = import ./pkgs;
    };
}
