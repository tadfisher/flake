{ pkgs, lib, ... }:

with lib;

{
  imports = [
    ../profiles/core.nix
  ];

  boot = {
    kernelPackages = pkgs.linuxPackages_latest;
    supportedFilesystems = mkForce [ "btrfs" "vfat" "f2fs" "xfs" "ntfs" "cifs" ];
  };

  environment = {
    etc."nixos/flake" = {
      source = cleanSource ../../.;
    };

    systemPackages = with pkgs; [
      sedutil-fork
      sed-opal-unlocker
    ];
  };

  services.openssh.enable = true;
}
