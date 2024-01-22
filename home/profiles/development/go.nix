{ config, pkgs, ... }:

{
  home.packages = with pkgs; [ gocode godef gopkgs gotests gotools ];

  programs.go = {
    enable = true;
    goPath = "${config.xdg.dataHome}/go";
  };
}
