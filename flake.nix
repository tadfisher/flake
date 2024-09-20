{
  description = "PIA VPN";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-unstable";
  };

  outputs = { ... }: {
    nixosModules = {
      default = ./nixos/modules/pia-vpn.nix;
    };
  };
}
