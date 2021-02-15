# TODO https://github.com/nix-community/home-manager/pull/1797
{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.xdg.systemDirs;

  configDirs = concatStringsSep ":" cfg.config;

  dataDirs = concatStringsSep ":" cfg.data;

in
{
  meta.maintainers = with maintainers; [ tadfisher ];

  options.xdg.systemDirs = {
    config = mkOption {
      type = types.listOf types.str;
      default = [ ];
      example = [ "/etc/xdg" ];
      description = ''
        Directory names to add to <envar>XDG_CONFIG_DIRS</envar>
        in the user session.
      '';
    };

    data = mkOption {
      type = types.listOf types.str;
      default = [ ];
      example = [ "/usr/share" "/usr/local/share" ];
      description = ''
        Directory names to add to <envar>XDG_DATA_DIRS</envar>
        in the user session.
      '';
    };
  };

  config = mkMerge [
    (mkIf (cfg.config != [ ]) {
      systemd.user.sessionVariables.XDG_CONFIG_DIRS =
        "${configDirs}\${XDG_CONFIG_DIRS:+:$XDG_CONFIG_DIRS}";
    })

    (mkIf (cfg.data != [ ]) {
      systemd.user.sessionVariables.XDG_DATA_DIRS =
        "${dataDirs}\${XDG_DATA_DIRS:+:$XDG_DATA_DIRS}";
    })

    {
      home.sessionVariablesExtra = optionalString (config.systemd.user.sessionVariables != { }) ''
        export $(${pkgs.systemd}/lib/systemd/user-environment-generators/30-systemd-environment-d-generator)
      '';
    }
  ];
}
