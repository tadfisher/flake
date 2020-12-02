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

  outputs = { self, emacs-overlay, home-manager, nixos-hardware, nixpkgs, nur }:
    let
      systems = [ "x86_64-linux" ];

      mkSystemOutput = system:
        let
          pkgs = import nixpkgs {
            inherit system;
            overlays = [ self.overlay ];
            config = { allowUnfree = true; };
          };
        in {
          packages."${system}" = self.overlay pkgs pkgs;
        };

    in {
      overlay = import ./pkgs;
    };
}
