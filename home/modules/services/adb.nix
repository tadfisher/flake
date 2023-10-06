{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.adb;

in
{
  options.services.adb = {
    enable = mkEnableOption "Android Debug Bridge";

    package = mkOption {
      type = types.package;
      default = pkgs.androidSdkPackages.platform-tools;
      description = ''
        SDK platform-tools package to use.
      '';
    };

    port = mkOption {
      type = types.ints.between 1025 65535;
      default = 5037;
      description = ''
        Port to which the ADB service should bind.
      '';
    };

    mdns = mkOption {
      type = types.nullOr (types.enum [ "dns-sd" "openscreen" ]);
      default = "dns-sd";
      description = lib.mdDoc ''
        mDNS client implementation to use.

        Options are:
          - `null`: Disable mDNS device discovery.
          - `"dns-sd"` _(default)_: Use the system DNS-SD service (Avahi, Bonjour).
          - `"openscreen"`: Use the built-in Open Screen mDNS client.
      '';
    };

    logLevel = mkOption {
      type = types.enum [ "verbose" "debug" "info" "warning" "fatal" "silent" ];
      default = "info";
      description = lib.mdDoc ''
        Minimum log severity.
      '';
    };
  };

  config = mkIf (cfg.enable) {
    home.sessionVariables.ADB_MDNS_OPENSCREEN = "1";

    systemd.user = {
      services.adb = {
        Unit = {
          Description = "Android Debug Bridge";
          After = [ "adb.socket" ];
          Requires = [ "adb.socket" ];
        };

        Service = {
          Type = "simple";
          Environment =
            [(if cfg.mdns == "dns-sd" then "ADB_MDNS_OPENSCREEN=0"
              else if cfg.mdns == "openscreen" then "ADB_MDNS_OPENSCREEN=1"
              else "ADB_MDNS=0")]
            ++ ["ANDROID_LOG_TAGS=${builtins.substring 0 1 cfg.logLevel}"];

          ExecStart = "${cfg.package}/adb server nodaemon -L acceptfd:3";
        };
      };

      sockets.adb = {
        Unit = {
          Description = "Android Debug Bridge";
          PartOf = [ "adb.service" ];
        };

        Socket = {
          ListenStream = "127.0.0.1:${toString cfg.port}";
          Accept = "no";
        };

        Install.WantedBy = [ "sockets.target" ];
      };
    };
  };
}
