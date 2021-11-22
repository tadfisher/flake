{ pkgs, lib, ... }:

with lib;

{
  imports = [
    ../profiles/core.nix
  ];

  boot.supportedFilesystems = mkForce [ "btrfs" "reiserfs" "vfat" "f2fs" "xfs" "ntfs" "cifs" ];

  environment.systemPackages = with pkgs; [
    sedutil-fork
    sed-opal-unlocker
  ];

  services.openssh.enable = true;
}
