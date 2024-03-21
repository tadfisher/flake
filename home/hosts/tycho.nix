{ config, lib, pkgs, ... }:

{
  imports = [
    ../profiles/core.nix
    ../profiles/games.nix
    ../profiles/gnome.nix
    ../profiles/development/android.nix
    ../profiles/development/hardware.nix
    ../profiles/development/go.nix
    ../profiles/development/jvm.nix
    ../profiles/development/nix.nix
    ../profiles/development/python.nix
    ../profiles/services/gpg-agent.nix
    ../profiles/services/kbfs.nix
    ../profiles/services/mopidy.nix
  ];

  accounts.email.accounts."tadfisher@gmail.com".primary = true;

  home.packages = with pkgs; [
    brasero
    celestia
    gamescope
    transmission-remote-gtk
  ];
}
