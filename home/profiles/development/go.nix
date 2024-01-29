{ config, pkgs, ... }:

{
  home.packages = with pkgs; [ godef gopkgs gopls gotests gotools ];

  programs.go = {
    enable = true;
    goPath = "${config.xdg.dataHome}/go";
  };
}
