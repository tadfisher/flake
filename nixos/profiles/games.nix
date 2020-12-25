{ config, pkgs, ... }:
let
  steamCfg = config.users.users.steam;
in
{
  imports = [
    ./users/steam.nix
  ];

  hardware.steam-hardware.enable = true;

  home-manager.users.steam = { config, lib, pkgs, ... }: {
    home = {
      homeDirectory = steamCfg.home;
      packages = with pkgs; [
        libarchive
        steam
        winetricks
      ];
    };
  };

  services = {
    udev.extraRules = ''
      ${builtins.readFile ../../data/udev/dualshock3.rules}
    '';

    xserver.displayManager.sessionPackages = [ pkgs.steamos-compositor-plus ];
  };
}
