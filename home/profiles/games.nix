{ pkgs, ... }:

{
  home.packages = with pkgs; [
    gnome3.gnome-chess
    enyo-doom
    gzdoom
    openmw
    portmod
    protontricks
    retroarch
    steam-run
    stockfish
    vkquake
    yquake2
    yquake2-all-games
  ];
}
