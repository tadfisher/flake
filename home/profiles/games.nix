{ pkgs, ... }:

{
  home = {
    packages = with pkgs; [
      enyo-doom
      gnome-chess
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

    sessionVariables = {
      # https://gitlab.com/OpenMW/openmw/-/issues/6185
      OSG_VERTEX_BUFFER_HINT = "VERTEX_BUFFER_OBJECT";
    };
  };
}
