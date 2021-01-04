{ config, lib, pkgs, ... }:

with lib;

{
  programs.mosh = {
    enable = true;
    withUtempter = true;
  };

  services.openssh = {
    enable = true;
    passwordAuthentication = false;
    permitRootLogin = "no";
    startWhenNeeded = true;
    extraConfig = ''
      StreamLocalBindUnlink yes
    '';
  };
}
