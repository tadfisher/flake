{ pkgs, ... }:

{
  imports = [
    ../profiles/core.nix
  ];

  environment.systemPackages = with pkgs; [
    sedutil-fork
    sed-opal-unlocker
  ];

  services.openssh.enable = true;
}
