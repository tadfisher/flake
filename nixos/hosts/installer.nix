{ pkgs, ... }:

{
  imports = [
    ../profiles/core.nix
  ];

  environment.systemPackages = with pkgs; [
    sedutil
  ];
}
