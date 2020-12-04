{ config, lib, pkgs, ... }:

{
  users.extraUsers.tad = {
    isNormalUser = true;
    home = "/home/tad";
    description = "Tad Fisher";
    uid = 1000;
    extraGroups =
      [ "wheel" "networkmanager" "docker" "libvirtd" "video" "adbusers" ];
  };
}
