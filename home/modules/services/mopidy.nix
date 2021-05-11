{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.services.mopidy;

  mopidyEnv = with pkgs; buildEnv {
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

  settingsFormat = pkgs.formats.ini {
    listToValue = concatMapStringsSep "," (generators.mkValueStringDefault {});
  };

  mopidyConf = settingsFormat.generate "mopidy.conf" cfg.settings;

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

    settings = mkOption {
      default = { };
      type = types.submodule {
        freeformType = settingsFormat.type;
      };
      description = ''
        The configuration that Mopidy should use.
        </para><para>
        See <link xlink:href="https://docs.mopidy.com/en/latest/config/" />
        for supported values.
      '';
    };

    extraConfigFiles = mkOption {
      default = [ ];
      type = types.listOf types.str;
      description = ''
        Extra config file read by Mopidy when the service starts.
        Values from later files in the list override values from earlier files.
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
