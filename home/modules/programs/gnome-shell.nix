{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.programs.gnome-shell;

  extensionOpts = {
    options = {
      id = mkOption {
        type = types.nullOr types.str;
        default = null;
        example = "user-theme@gnome-shell-extensions.gcampax.github.com";
        description = ''
          ID of the gnome-shell extension. If not provided, it
          will be obtained from <varname>package.uuid</varname>.
        '';
      };

      package = mkOption {
        type = types.nullOr types.str;
        default = null;
        example = "pkgs.gnome3.gnome-shell-extensions";
        description = ''
          Package providing a gnome-shell extension with id <varname>id</varname>.
        '';
      };
    };

    config = { id = mkDefault package.uuid or null; };
  };

  themeOpts = {
    options = {
      name = mkOption {
        type = types.str;
        example = "Plata-Noir";
        description = ''
          Name of the gnome-shell theme.
        '';
      };
      package = mkOption {
        type = types.nullOr types.package;
        default = null;
        example = literalExample "pkgs.plata-theme";
        description = ''
          Package providing a gnome-shell theme named <varname>name</varname>.
        '';
      };
    };
  };

in
{
  options.desktops.gnome = {
    enable = mkEnableOption "gnome desktop";

    extensions = {
      packages = mkOption {
        type = types.listOf (types.submodule extensionOpts);
        default = [ ];
        example = literalExample ''
          [
            { package = pkgs.gnomeExtensions.dash-to-panel; }
            {
              id = "user-theme@gnome-shell-extensions.gcampax.github.com";
              package = pkgs.gnome3.gnome-shell-extensions;
            }
          ]
        '';
        description = ''
          List of gnome-shell extensions.
        '';
      };

      ids = mkOption {
        type = types.listOf types.str;
        default = [ ];
        example = literalExample ''
          [
            "screenshot-window-sizer@gnome-shell-extensions.gcampax.github.com"
            "user-theme@gnome-shell-extensions.gcampax.github.com"
            "windowsNavigator@gnome-shell-extensions.gcampax.github.com"
            "workspace-indicator@gnome-shell-extensions.gcampax.github.com"
          ]
        '';
        description = ''
          List of Gnome extensions to enable by ID.
        '';
      };
    };

    theme = mkOption {
      type = types.nullOr (types.submodule themeOpts);
      default = null;
      example = literalExample ''
        {
          name = "Plata-Noir";
          package = [ pkgs.plata-theme ];
        }
      '';
      description = ''
        Theme to use for gnome-shell.
      '';
    };
  };

  config = mkIf cfg.enable mkMerge [
    (mkIf (cfg.extensions != { }) {
      dconf.settings."org/gnome/shell".enabled-extensions =
        catAttrs "id" cfg.extensions;
      home.packages = filter (p: p != null) (catAttrs "package" cfg.extensions);
    })

    (mkIf (cfg.theme != null) {
      dconf.settings."org/gnome/shell/extensions/user-theme".name =
        cfg.theme.name;
      home.packages = optional (cfg.theme.package != null) cfg.theme.package;
    })
  ];
}
