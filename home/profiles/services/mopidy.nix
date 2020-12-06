{ config, lib, pkgs, ... }:

with lib;

{
  services.mopidy = {
    enable = true;

    extensionPackages = with pkgs; [
      mopidy-mpd
      mopidy-mpris
      mopidy-somafm
      mopidy-youtube
    ];

    configuration = generators.toINI { } {
      core.restore_state = true;
      audio.mixer = "none";
      mpd = {
        enabled = true;
        hostname = "127.0.0.1";
      };
      youtube = {
        api_enabled = true;
        autoplay_enabled = true;
        youtube_api_key = (import ../../../secrets).google.apiKey;
      };
    };
  };
}
