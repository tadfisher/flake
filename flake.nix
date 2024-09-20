{
  description = "PIA VPN";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-unstable";
    flake-compat.url = "https://flakehub.com/f/edolstra/flake-compat/1.tar.gz";
  };

  outputs = { ... }: {
    nixosModules = {
      default = ./nixos/modules/pia-vpn.nix;
    };
  };
}
