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

      mkOverlays = system: [
        (final: prev: self.overlay final prev)
        (final: prev: inputs.nur.overlay final prev)
        (final: prev: inputs.emacs-overlay.overlay final prev)
      ];

      mkPackages = system: import inputs.nixpkgs {
        inherit system;
        overlays = self.overlays.${system};
        config = { allowUnfree = true; };
      };

    in {
      inherit (inputs.nixpkgs) lib;
      nixosConfigurations = import ./nixos/hosts inputs;
      nixosModules = import ./nixos/modules inputs;
      overlay = final: prev: import ./pkgs final prev;
      overlays = self.lib.genAttrs systems mkOverlays;
      packages = self.lib.genAttrs systems mkPackages;
    };
}
