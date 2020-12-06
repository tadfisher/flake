{ config, lib, pkgs, ... }:

with pkgs;
with lib;
let
  cfg = config.services.mopidy;

  mopidyConf = writeText "mopidy.conf" cfg.configuration;

  mopidyEnv = buildEnv {
    name = "mopidy-with-extensions-${mopidy.version}";
    paths = closePropagation cfg.extensionPackages;
    pathsToLink = [ "/${mopidyPackages.python.sitePackages}" ];
    buildInputs = [ makeWrapper ];
    postBuild = ''
      makeWrapper ${mopidy}/bin/mopidy $out/bin/mopidy \
        --prefix PYTHONPATH : $out/${mopidyPackages.python.sitePackages} \
        --add-flags "--config ${
          concatStringsSep ":" ([ mopidyConf ] ++ cfg.extraConfigFiles)
        }"
    '';
  };

in
{

  meta.maintainers = [ maintainers.tadfisher ];

  options.services.mopidy = {

    enable = mkEnableOption "Mopidy music player daemon";

    dataDir = mkOption {
      defaultText = "~/.local/share/mopidy";
      type = types.str;
      description = ''
        The directory where Mopidy stores its state.
      '';
    };

    extensionPackages = mkOption {
      default = [ ];
      type = types.listOf types.package;
      example = literalExample "[ pkgs.mopidy-spotify ]";
      description = ''
        Mopidy extensions that should be loaded by the service.
      '';
    };

    configuration = mkOption {
      default = "";
      type = types.lines;
      description = ''
        The configuration that Mopidy should use.
      '';
    };

    extraConfigFiles = mkOption {
      default = [ ];
      type = types.listOf types.str;
      description = ''
        Extra config file read by Mopidy when the service starts.
        Later files in the list overrides earlier configuration.
      '';
    };
  };

  config = mkIf cfg.enable {
    home.packages = [ mopidyEnv ];

    services.mopidy.dataDir = mkDefault "${config.xdg.dataHome}/mopidy";

    systemd.user.services.mopidy = {
      Unit = { Description = "Mopidy music server"; };
      Service = { ExecStart = "${mopidyEnv}/bin/mopidy"; };
      Install = { WantedBy = [ "default.target" ]; };
    };

    systemd.user.services.mopidy-scan = {
      Unit = { Description = "Mopidy local file scanner"; };
      Service = {
        ExecStart = "${mopidyEnv}/bin/mopidy local scan";
        Type = "oneshot";
      };
    };
  };
}
