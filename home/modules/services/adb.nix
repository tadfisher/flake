{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.adb;

  mdnsEnv =
    if cfg.mdnsBackend == "dns-sd" then "ADB_MDNS_OPENSCREEN=0"
    else if cfg.mdnsBackend == "openscreen" then "ADB_MDNS_OPENSCREEN=1"
    else "ADB_MDNS=0";

  usbEnv =
    if cfg.usbBackend == "builtin" then "ADB_LIBUSB=0"
    else if cfg.usbBackend == "libusb" then "ADB_LIBUSB=1"
    else "ADB_USB=0";

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
        Port on which the ADB service should listen.
      '';
    };

    mdnsBackend = mkOption {
      type = types.nullOr (types.enum [ "dns-sd" "openscreen" ]);
      default = "dns-sd";
      description = lib.mdDoc ''
        mDNS transport backend to use.

        Options are:
          - `null`: Disable the mDNS transport.
          - `"dns-sd"` _(default)_: Use the system DNS-SD service (Avahi, Bonjour).
          - `"openscreen"`: Use the built-in Open Screen mDNS client.
      '';
    };

    logLevel = mkOption {
      type = types.enum [ "verbose" "debug" "info" "warning" "fatal" "silent" ];
      default = "info";
      description = ''
        Minimum log severity.
      '';
    };

    usbBackend = mkOption {
      type = types.nullOr (types.enum [ "builtin" "libusb" ]);
      default = "builtin";
      description = lib.mdDoc ''
        USB transport backend to use.

        Options are:
          - `null`: Disable the USB transport.
          - `"builtin"` (_default_): Use the built-in USB backend.
          - `"libusb"`: Use the libusb backend.
      '';
    };
  };

  config = mkIf (cfg.enable) {

    systemd.user = {
      services.adb = {
        Unit = {
          Description = "Android Debug Bridge";
          After = [ "adb.socket" ];
          Requires = [ "adb.socket" ];
        };

        Service = {
          Type = "simple";
          Environment = [
            mdnsEnv
            usbEnv
            "ANDROID_LOG_TAGS=${builtins.substring 0 1 cfg.logLevel}"
          ];
          ExecStart = "${cfg.package}/adb server nodaemon -L acceptfd:3";
        };
      };

      sockets.adb = {
        Unit = {
          Description = "Android Debug Bridge";
          PartOf = [ "adb.service" ];
        };

        Socket.ListenStream = "127.0.0.1:${toString cfg.port}";

        Install.WantedBy = [ "sockets.target" ];
      };
    };
  };
}
