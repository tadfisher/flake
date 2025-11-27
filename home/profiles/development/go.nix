{ config, pkgs, ... }:

{
  home.packages = with pkgs; [ godef gopkgs gopls gotests gotools ];

  programs.go = {
    enable = true;
    env.GOPATH = "${config.xdg.dataHome}/go";
  };
}
