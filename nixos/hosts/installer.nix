{ pkgs, ... }:

{
  imports = [
    ../profiles/core.nix
  ];

  environment.systemPackages = with pkgs; [
    sedutil
    sed-opal-unlocker
  ];

  services.openssh.enable = true;
}
