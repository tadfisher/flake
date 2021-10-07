{ config, pkgs, lib, ... }:

with lib;

let
  cfg = config.programs.cardboard;

  cardboardPackage = cfg.package.overrideAttrs (attrs: {
    passthru = (attrs.passthru or { }) // {
      providedSessions = [ "cardboard" ];
    };
  });

in {
  options.programs.cardboard = {
    enable = mkEnableOption "Cardboard Wayland compositor";

    package = mkOption {
      type = types.package;
      default = pkgs.cardboard;
      description = ''
        Package to use for cardboard.
      '';
    };

    extraSessionCommands = mkOption {
      type = types.lines;
      default = "";
      example = ''
        # SDL:
        export SDL_VIDEODRIVER=wayland
        # QT (needs qt5.qtwayland in systemPackages):
        export QT_QPA_PLATFORM=wayland-egl
        export QT_WAYLAND_DISABLE_WINDOWDECORATION="1"
        # Fix for some Java AWT applications (e.g. Android Studio),
        # use this if they aren't displayed properly:
        export _JAVA_AWT_WM_NONREPARENTING=1
      '';
      description = ''
        Shell commands executed just before Cardboard is started. See
        <link xlink:href="https://github.com/swaywm/sway/wiki/Running-programs-natively-under-wayland" />
        and <link xlink:href="https://github.com/swaywm/wlroots/blob/master/docs/env_vars.md" />
        for some useful environment variables.
      '';
    };

    extraPackages = mkOption {
      type = with types; listOf package;
      default = [ ];
      defaultText = literalExpression ''
        with pkgs; [ swaylock swayidle alacritty dmenu ];
      '';
      example = literalExpression ''
        with pkgs; [
          i3status i3status-rust
          termite rofi light
        ]
      '';
      description = ''
        Extra packages to be installed system wide. See
        <link xlink:href="https://github.com/swaywm/sway/wiki/Useful-add-ons-for-sway" /> and
        <link xlink:href="https://github.com/swaywm/sway/wiki/i3-Migration-Guide#common-x11-apps-used-on-i3-with-wayland-alternatives" />
        for a list of useful software.
      '';
    };
  };

  config = mkIf cfg.enable {
    environment.systemPackages = [ cardboardPackage ] ++ cfg.extraPackages;
    hardware.opengl.enable = mkDefault true;
    fonts.enableDefaultFonts = mkDefault true;
    programs.dconf.enable = mkDefault true;
    # To make a Cardboard session available if a display manager like SDDM is enabled:
    services.xserver.displayManager.sessionPackages = [ cardboardPackage ];
    programs.xwayland.enable = mkDefault true;
    # For screen sharing (this option only has an effect with xdg.portal.enable):
    xdg.portal.extraPortals = [ pkgs.xdg-desktop-portal-wlr ];
  };

  meta.maintainers = with lib.maintainers; [ tadfisher ];
}
