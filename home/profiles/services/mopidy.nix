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
      mopidy-ytmusic
    ];

    settings = {
      core.restore_state = true;
      audio.mixer = "none";
      mpd = {
        enabled = true;
        hostname = "127.0.0.1";
      };
      ytmusic = {
        auth_json = "${../../../secrets/mopidy-ytmusic.auth.json}";
      };
    };
  };
}
