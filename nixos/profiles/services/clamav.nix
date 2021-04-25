{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.clamav;

in
{
  networking.firewall.allowedTCPPorts = [ 3310 ];

  services.clamav = {
    daemon.enable = true;
    updater = {
      enable = true;
      frequency = 1;
      interval = "daily";
    };
  };

  systemd = {
    services.clamav-daemon = {
      requires = [ "clamav-daemon.socket" ];

      unitConfig.ConditionPathExistsGlob = [
        "${cfg.daemon.settings.DatabaseDirectory}/main.{c[vl]d,inc}"
        "${cfg.daemon.settings.DatabaseDirectory}/daily.{c[vl]d,inc}"
      ];

      serviceConfig = {
        ExecStart = mkForce "${pkgs.clamav}/sbin/clamd --foreground";
        PrivateNetwork = mkForce "no";
      };
    };

    sockets.clamav-daemon = {
      description = "Socket for ClamAV daemon";
      documentation = [
        "man:clamd(8)"
        "man:clamd.conf(5)"
        "https://www.clamav.net/documents/"
      ];

      unitConfig.ConditionPathExistsGlob = [
        "${cfg.daemon.settings.DatabaseDirectory}/main.{c[vl]d,inc}"
        "${cfg.daemon.settings.DatabaseDirectory}/daily.{c[vl]d,inc}"
      ];

      listenStreams = [
        cfg.daemon.settings.LocalSocket
        "0.0.0.0:3310"
      ];

      socketConfig = {
        SocketUser = "clamav";
        SocketGroup = "clamav";
        RemoveOnStop = "true";
      };
    };
  };
}
