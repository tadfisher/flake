{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.programs.pass;

  homeDir = config.home.homeDirectory;

  storeOpts = { name, config, ... }: {
    options = {
      path = mkOption {
        type = types.str;
        readOnly = true;
        default = name;
        description = ''
          Path to the directory for this store, relative to
          <varname>home.homeDirectory</varname>. This is set
          to the attribute name of the store configuration.
        '';
      };

      primary = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether this is the primary store. Only one store may
          be set as primary. The <command>pass</command> will
          operate on this store by default.
        '';
      };

      alias = mkOption {
        type = types.nullOr types.str;
        default = null;
        description = "Shell alias for conveniently accessing this store.";
      };

      absPath = mkOption {
        type = types.path;
        readOnly = true;
        internal = true;
        default = "${homeDir}/${config.path}";
        description = ''
          A convenience option whose value is the absolute path of
          this store.
        '';
      };

      command = mkOption {
        type = types.str;
        readOnly = true;
        internal = true;
        default =
          ''PASSWORD_STORE_DIR="${config.absPath}" ${cfg.package}/bin/pass'';
        description = ''
          A convenience option whose value is the shell command to
          use <command>pass</command> with this store.
        '';
      };
    };
  };

in {
  options = {
    programs.pass = {
      enable = mkEnableOption "zx2c4 password store";

      package = mkOption {
        type = types.package;
        default = pkgs.pass;
        defaultText = literalExample "pkgs.pass";
        example = literalExample
          "pkgs.pass.withExtensions (e: [ e.pass-otp e.pass-audit ])";
        description = "The pass package to use.";
      };

      stores = mkOption {
        type = types.attrsOf (types.submodule [ storeOpts ]);
        default = mkDefault { ".password-store".primary = true; };
        example = literalExample ''
          {
            ".password-store-personal" = {
              primary = true;
              alias = "pp";
            };
            ".password-store-work" = {
              alias = "pw";
            };
          }
        '';
        description = ''
          Set of password stores, keyed by path. The store path is
          relative to <varname>home.homeDirectory</varname>.
        '';
      };

      primaryStore = mkOption {
        type = types.attrs;
        internal = true;
        readOnly = true;
        default = findFirst (s: s.primary) null (attrValues cfg.stores);
      };

      settings = mkOption rec {
        type = with types; attrsOf str;
        apply = mergeAttrs default;
        default = { PASSWORD_STORE_DIR = cfg.primaryStore.absPath; };
        defaultText = literalExample ''
          { PASSWORD_STORE_DIR = "$XDG_DATA_HOME/password-store"; }
        '';
        example = literalExample ''
          {
            PASSWORD_STORE_DIR = "/some/directory";
            PASSWORD_STORE_KEY = "12345678";
            PASSWORD_STORE_CLIP_TIME = "60";
          }
        '';
        description = ''
          The <literal>pass</literal> environment variables dictionary.
          </para><para>
          See the "Environment variables" section of
          <citerefentry>
            <refentrytitle>pass</refentrytitle>
            <manvolnum>1</manvolnum>
          </citerefentry>
          and the extension man pages for more information about the
          available keys.
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    assertions = [
      (let
        primaries =
          catAttrs "path" (filter (s: s.primary) (attrValues cfg.stores));
      in {
        assertion = cfg.stores == { } || length primaries == 1;
        message = "pass: Must have exactly one primary store but found "
          + toString (length primaries) + optionalString (length primaries > 1)
          (", namely " + concatStringsSep ", " primaries);
      })
    ];

    home.packages = [ cfg.package ];

    home.sessionVariables = cfg.settings;

    programs.bash.shellAliases =
      mapAttrs' (path: store: nameValuePair store.alias store.command)
      (filterAttrs (path: store: store.alias != null) cfg.stores);
  };
}
