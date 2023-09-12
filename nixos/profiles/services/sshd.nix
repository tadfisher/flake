{ config, lib, pkgs, ... }:

with lib;

{
  programs.mosh = {
    enable = true;
    withUtempter = true;
  };

  services.openssh = {
    enable = true;
    startWhenNeeded = true;
    settings = {
      PasswordAuthentication = mkDefault false;
      PermitRootLogin = "no";
      StreamLocalBindUnlink = true;
    };
  };
}
