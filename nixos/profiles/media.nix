{ config, lib, pkgs, ... }:

with lib;

{
  environment.systemPackages = with pkgs; [
    ffmpeg
    vaapiIntel
    zlib
  ];

  networking.firewall = {
    allowedTCPPorts = [
      # Jellyfin
      8096
      8920
    ];

    allowedUDPPorts = [
      # Jellyfin
      1900
      7359
    ];
  };

  services = {
    jackett = {
      enable = true;
      openFirewall = true;
    };

    jellyfin = {
      enable = true;
      package = pkgs.jellyfin;
    };

    plex = {
      enable = true;
      openFirewall = true;
      package = pkgs.plex-plexpass;
    };

    radarr = {
      enable = true;
      openFirewall = true;
    };

    sonarr = {
      enable = true;
      openFirewall = true;
    };
  };

  users.users = {
    jellyfin.extraGroups = [ "media" "video" ];
    plex.extraGroups = [ "media" "video" ];
    radarr.extraGroups = [ "media" ];
    sonarr.extraGroups = [ "media" ];
  };
}
