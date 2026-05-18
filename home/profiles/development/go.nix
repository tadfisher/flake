{ config, pkgs, ... }:

{
  home.packages = with pkgs; [ godef gopkgs gotests gotools ];

  programs.go = {
    enable = true;
    env.GOPATH = "${config.xdg.dataHome}/go";
  };
}
