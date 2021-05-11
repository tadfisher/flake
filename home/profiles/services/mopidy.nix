{ config, lib, pkgs, ... }:

with lib;

let
  secrets = (import ../../../secrets).mopidy;

in
{
  services.mopidy = {
    enable = true;

    extensionPackages = with pkgs; [
      mopidy-mpd
      mopidy-mpris
      mopidy-somafm
      mopidy-spotify
      mopidy-youtube
    ];

    settings = {
      core.restore_state = true;
      audio.mixer = "none";
      mpd = {
        enabled = true;
        hostname = "127.0.0.1";
      };
      spotify = {
        client_id = secrets.spotify.clientId;
        client_secret = secrets.spotify.clientSecret;
      };
      youtube = {
        api_enabled = true;
        autoplay_enabled = true;
        youtube_api_key = secrets.youtube.apiKey;
      };
    };
  };
}
