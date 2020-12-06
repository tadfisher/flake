{ config, lib, pkgs, ... }:

with lib;

{
  programs.gnome-terminal = {
    enable = true;
    showMenubar = false;
    profile = {
      "4b808fa9-e65c-40f5-9b6a-101ceb39d9c4" = {
        default = true;
        visibleName = "plata";
        colors = {
          foregroundColor = "#f5f5f5";
          backgroundColor = "#111111";
          boldColor = null;
          palette = [
            "#111111"
            "#f44336"
            "#c3e88d"
            "#ffcb6b"
            "#52b2ff"
            "#8796ed"
            "#1de9b6"
            "#f5f5f5"
            "#8c8c8c"
            "#f44336"
            "#c3e88d"
            "#ffcb6b"
            "#52b2ff"
            "#8796ed"
            "#1de9b6"
            "#ffffff"
          ];
          cursor = {
            foreground = "#111111";
            background = "#f44336";
          };
          highlight = {
            foreground = "#111111";
            background = "#8796ed";
          };
        };
        cursorShape = "ibeam";
        allowBold = true;
        scrollOnOutput = false;
      };
    };
  };
}
