{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.gnirehtet;

  args = "-d '${concatStringsSep "," cfg.dns}' " + "-p '${toString cfg.port}' "
    + "-r '${concatStringsSep "," cfg.routes}'";

in {
  options = {
    services.gnirehtet = {
      enable = mkEnableOption "gnirehtet reverse-tethering daemon";

      port = mkOption {
        type = types.ints.between 1024 65535;
        default = 31416;
        description = ''
          Port on which the relay server should listen.
        '';
      };

      dns = mkOption {
        type = types.listOf types.str;
        example = literalExample ''
          [ "1.1.1.1" "1.0.0.1" ]
        '';
        description = ''
          Make connected Android devices use these DNS servers.

          If not set, Google public DNS is used (8.8.8.8).
        '';
      };

      routes = mkOption {
        type = types.listOf types.str;
        example = literalExample ''
          [ "10.0.0.0/8" ]
        '';
        description = ''
          Only reverse tether these routes.

          If not set, redirect all traffic (0.0.0.0/0).
        '';
      };
    };
  };

  config = mkIf cfg.enable {

    services.gnirehtet = {
      dns = mkDefault [ "8.8.8.8" ];
      routes = mkDefault [ "0.0.0.0/0" ];
    };

    systemd.user.targets."adb@" = {
      Unit = {
        Description = "Android ADB Device %I";
        BindsTo = [ "dev-adb-%i.device" ];
        After = [ "dev-adb-%i.device" "adb-wait-device@%i.service" ];
        Wants = [ "adb-wait-device@%i.service" "gnirehtet-client@%i.service" ];
        StopWhenUnneeded = true;
      };
    };

    systemd.user.services."adb-wait-device@" = {
      Unit = { Description = "Wait for ADB device %I to be connected"; };

      Service = {
        Type = "oneshot";
        RemainAfterExit = true;
        ExecStart =
          "${pkgs.coreutils}/bin/timeout 30 ${pkgs.androidenv.androidPkgs_9_0.platform-tools}/bin/adb -s %I wait-for-device";
      };
    };

    systemd.user.services.gnirehtet-relay = {
      Unit = {
        Description = "gnirehtet reverse-tethering relay server";
        StopWhenUnneeded = true;
      };

      Service = {
        ExecStart =
          "${pkgs.gnirehtet}/bin/gnirehtet relay -p ${toString cfg.port}";
      };
    };

    systemd.user.services."gnirehtet-client@" = {
      Unit = {
        Description = "gnirehtet reverse-tethering client";
        Wants = [ "gnirehtet-relay.service" ];
        BindsTo = [ "adb@%i.target" ];
        After = [ "adb@%i.target" ];
      };

      Service = {
        Type = "oneshot";
        RemainAfterExit = true;
        ExecStartPre =
          "${pkgs.coreutils}/bin/timeout 20 ${pkgs.androidenv.androidPkgs_9_0.platform-tools}/bin/adb -s %I wait-for-device";
        ExecStart = "${pkgs.gnirehtet}/bin/gnirehtet start %I ${args}";
        ExecStop = "${pkgs.gnirehtet}/bin/gnirehtet stop %I";
        ExecReload = "${pkgs.gnirehtet}/bin/gnirehtet restart %I ${args}";
      };

      Install = { WantedBy = [ "adb.target" ]; };
    };
  };
}
