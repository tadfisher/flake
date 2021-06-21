{ config, lib, pkgs, ... }:

with lib;

{
  imports = [
    ./desktop.nix
    ./programs/gnome-terminal.nix
  ];

  dconf.settings = {
    "org/gnome/desktop/background" = {
      picture-uri = "file://${../../data}/Abstract.jpg";
      picture-options = "zoom";
    };

    "org/gnome/desktop/input-sources" = {
      xkb-options = [ "ctrl:nocaps" "compose:prsc" ];
    };

    "org/gnome/desktop/interface" = {
      clock-format = "12h";
      cursor-blink = false;
      cursor-size = 16;
      cursor-theme = "Paper";
      document-font-name = "Noto Sans 9.75";
      gtk-im-module = "xim";
      gtk-key-theme = "Emacs";
      monospace-font-name = "JetBrains Mono Regular 9.75";
      scaling-factor = 1;
    };

    "org/gnome/desktop/lockdown" = { disable-lock-screen = false; };

    "org/gnome/desktop/screensaver" = {
      picture-uri =
        "file://${../../data}/Seattle%20Museum%20of%20Pop%20Culture.jpg";
      picture-options = "zoom";
    };

    "org/gnome/desktop/wm/keybindings" = {
      activate-window-menu = [ "" ];
      minimize = [ "" ];
      move-to-corner-ne = [ "" ];
      move-to-corner-nw = [ "" ];
      move-to-corner-se = [ "" ];
      move-to-corner-sw = [ "" ];
      move-to-side-e = [ "" ];
      move-to-side-n = [ "" ];
      move-to-side-s = [ "" ];
      move-to-side-w = [ "" ];
      move-to-workspace-1 = [ "" ];
      move-to-workspace-2 = [ "" ];
      move-to-workspace-3 = [ "" ];
      move-to-workspace-4 = [ "" ];
      move-to-workspace-5 = [ "" ];
      move-to-workspace-6 = [ "" ];
      move-to-workspace-7 = [ "" ];
      move-to-workspace-8 = [ "" ];
      move-to-workspace-9 = [ "" ];
      move-to-workspace-10 = [ "" ];
      move-to-workspace-down = [ "" ];
      move-to-workspace-left = [ "" ];
      move-to-workspace-right = [ "" ];
      move-to-workspace-up = [ "" ];
      switch-applications = [ "<Alt>Tab" ];
      switch-applications-backward = [ "<Primary><Alt>Tab" ];
      switch-group = [ "<Alt>Above_Tab" ];
      switch-group-backward = [ "<Primary><Alt>Above_Tab" ];
      switch-to-workspace-1 = [ "" ];
      switch-to-workspace-2 = [ "" ];
      switch-to-workspace-3 = [ "" ];
      switch-to-workspace-4 = [ "" ];
      switch-to-workspace-5 = [ "" ];
      switch-to-workspace-6 = [ "" ];
      switch-to-workspace-7 = [ "" ];
      switch-to-workspace-8 = [ "" ];
      switch-to-workspace-9 = [ "" ];
      switch-to-workspace-10 = [ "" ];
      switch-to-workspace-down = [ "" ];
      switch-to-workspace-left = [ "" ];
      switch-to-workspace-right = [ "" ];
      switch-to-workspace-up = [ "" ];
    };

    "org/gnome/desktop/wm/preferences" = { resize-with-right-button = true; };

    "org/gnome/mutter" = {
      auto-maximize = false;
      experimental-features = [ "autostart-xwayland" "rt-scheduler" "scale-monitor-framebuffer" ];
    };

    "org/gnome/mutter/keybindings" = {
      switch-monitor = [ ];
      toggle-tiled-left = [ ];
      toggle-tiled-right = [ ];
    };

    "org/gnome/settings-daemon/plugins/media-keys" = {
      screensaver = [ "<Primary><Alt>BackSpace" ];
    };

    "org/gnome/settings-daemon/peripherals/mouse" = { locate-pointer = true; };

    "org/gnome/shell" = {
      always-show-log-out = true;
      disable-user-extensions = false;
      enabled-extensions = with pkgs; [
        dash-to-panel.uuid
        paperwm.uuid
        vertical-overview.uuid
        "user-theme@gnome-shell-extensions.gcampax.github.com"
      ];
    };

    "org/gnome/shell/overrides" = {
      attach-modal-dialogs = false;
      edge-tiling = false;
      workspaces-only-on-primary = false;
    };

    "org/gnome/shell/extensions/paperwm" = {
      horizontal-margin = 0;
      use-default-background = true;
      vertical-margin = 0;
      vertical-margin-bottom = 0;
      window-gap = 0;
    };

    "org/gnome/shell/extensions/paperwm/keybindings" = {
      close-window = [ "<Super>BackSpace" ];
      move-down = [ "<Shift><Super>k" ];
      move-down-workspace = [ "<Shift><Super>n" ];
      move-left = [ "<Shift><Super>j" ];
      move-monitor-left = [ "<Shift><Super>h" ];
      move-monitor-right = [ "<Super>colon" ];
      move-previous-workspace = [ "<Shift><Super>Above_Tab" ];
      move-previous-workspace-backward = [ "<Primary><Shift><Super>Above_Tab" ];
      move-right = [ "<Shift><Super>l" ];
      move-up = [ "<Shift><Super>i" ];
      move-up-workspace = [ "<Shift><Super>p" ];
      new-window = [ "<Super>Return" ];
      previous-workspace = [ "<Super>Above_Tab" ];
      previous-workspace-backward = [ "<Primary><Super>Above_Tab" ];
      slurp-in = [ "<Super>u" ];
      switch-down = [ "<Super>k" ];
      switch-down-workspace = [ "<Super>n" ];
      switch-first = [ "<Primary><Super>j" ];
      switch-last = [ "<Primary><Super>k" ];
      switch-left = [ "<Super>j" ];
      switch-monitor-left = [ "<Super>h" ];
      switch-monitor-right = [ "<Super>semicolon" ];
      switch-right = [ "<Super>l" ];
      switch-up = [ "<Super>i" ];
      switch-up-workspace = [ "<Super>p" ];
      toggle-scratch = [ "<Shift><Super>s" ];
      toggle-scratch-layer = [ "<Super>s" ];
    };

    "org/gnome/shell/extensions/user-theme".name = "Plata-Noir";

    "org/gnome/shell/keybindings" = {
      focus-active-notification = [ "" ];
      toggle-overview = [ "" ];
    };
  };

  home.packages = with pkgs; [
    dash-to-panel
    gnome3.dconf-editor
    gnome3.gnome-shell-extensions
    gnome3.gnome-tweaks
    paperwm
    roboto
    vertical-overview
    virtmanager
  ];

  # programs.gnome-shell = {
  #   enable = true;
  #   extensions = [
  #     { package = pkgs.dash-to-panel; }
  #     { package = pkgs.paperwm; }
  #     { package = pkgs.vertical-overview; }
  #   ];
  #   theme = {
  #     name = "Plata-Noir";
  #     package = pkgs.plata-theme;
  #   };
  # };

  # Prevent clobbering SSH_AUTH_SOCK
  pam.sessionVariables = { GSM_SKIP_SSH_AGENT_WORKAROUND = "1"; };

  programs.chromium.extensions = [
    "gphhapmejobijbbhgpjhcjognlahblep" # GNOME Shell integration
    # "jfnifeihccihocjbfcfhicmmgpjicaec" # GSConnect
  ];

  programs.gnome-terminal.enable = true;

  # Disable gnome-keyring ssh-agent
  xdg.configFile = {
    "autostart/gnome-keyring-ssh.desktop".text = ''
      ${lib.fileContents
      "${pkgs.gnome3.gnome-keyring}/etc/xdg/autostart/gnome-keyring-ssh.desktop"}
      Hidden=true
    '';
  };
}
