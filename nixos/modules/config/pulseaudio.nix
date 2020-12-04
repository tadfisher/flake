{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.hardware.pulseaudio;

  enabledModules = filter (m: m.enable) (attrValues cfg.modules);

  toModule = name: arguments:
    let
      toVal = v:
        if builtins.isString v then ''"${v}"''
        else if builtins.isBool v then if v then "1" else "0"
        else if builtins.isList v then concatMapStringsSep "," toVal v
        else if builtins.isAttrs v then ''"${toConf v}"''
        else toString v;
      toConf = v:
        if builtins.isAttrs v then concatStringsSep " " (mapAttrsToList (k: v:
          k + optionalString (v != null) "=${toVal v}"
        ) v)
        else toVal v;
    in
      ''load-module ${name}${optionalString (arguments != {}) " ${toConf arguments}"}'';

  moduleOpts = { name, config, ... }: {
    options = {
      enable = mkEnableOption "${name} module." // { default = true; };

      name = mkOption {
        type = types.str;
        readOnly = true;
        description = ''
          Name of the Pulseaudio module. This is set to the
          attribute name of the module configuration.
        '';
      };

      package = mkOption {
        type = types.nullOr types.package;
        default = null;
        example = literalExample "pkgs.pulseaudio-modules-bt";
        description = ''
          Package providing this module.
        '';
      };

      arguments = mkOption {
        type = types.attrs;
        default = {};
        example = literalExample ''
          {
            use_master_format = true;
            aec_method = "webrtc";
            aec_args = {
              analog_gain_control = false;
              digital_gain_control = true;
              beamforming = true;
              mic_geometry = [ (-0.03) 0.0 0.0 0.03 0.0 0.0 ];
              noise_suppression = true;
            };
            source_name = "echoCancel_source";
            sink_name = "echoCancel_sink";
          }
        '';
        description = ''
          Attribute set of arguments to provide when loading the module.
        '';
      };

      finalConfig = mkOption {
        type = types.lines;
        internal = true;
        readOnly = true;
        description = ''
          Configuration lines to add to <varname>pulseaudio.extraConfig</varname>
          for this module.
        '';
      };
    };

    config = {
      name = name;
      finalConfig = toModule name config.arguments;
    };
  };

in {
  options.hardware.pulseaudio.modules = mkOption {
    type = types.attrsOf (types.submodule moduleOpts);
    default = {};
  };

  config.hardware.pulseaudio = {
    extraModules =
      filter (p: p != null) (catAttrs "package" enabledModules);

    extraConfig =
      concatStringsSep "\n" (catAttrs "finalConfig" enabledModules);
  };
}
