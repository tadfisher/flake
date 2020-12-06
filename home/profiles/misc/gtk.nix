{ config, lib, pkgs, ... }:

with lib;

{
  gtk = { enable = true; };

  home.packages = [
    # paper-icon-theme doesn’t propagate these: https://github.com/NixOS/nixpkgs/issues/84983
    pkgs.gnome3.adwaita-icon-theme
    pkgs.gnome3.gnome-themes-extra
  ];

  qt = {
    enable = mkDefault true;
    platformTheme = mkDefault "gnome";
  };
}
