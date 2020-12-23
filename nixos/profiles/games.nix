{ pkgs, ... }:

{
  imports = [
    ./users/steam.nix
  ];

  hardware.steam-hardware.enable = true;

  services = {
    udev.extraRules = ''
      ${builtins.readFile ../../data/udev/dualshock3.rules}
    '';

    xserver.displayManager.sessionPackages = [ pkgs.steamos-compositor-plus ];
  };
}
