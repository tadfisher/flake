{ config, lib, pkgs, ... }:

{
  boot.loader = {
    systemd-boot.enable = true;
    timeout = 0;
  };
}
