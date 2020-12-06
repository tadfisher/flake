{ config, pkgs, ... }:

{
  home.packages = with pkgs; [ dep go2nix gocode godef gopkgs gotests gotools ];

  programs.go = {
    enable = true;
    goPath = "${config.xdg.dataHome}/go";
  };
}
