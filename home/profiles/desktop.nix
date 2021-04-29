{ config, lib, pkgs, ... }:

with lib;

{
  imports = [
    ./misc/gnome-paths.nix
    ./services/emacs.nix
  ];

  fonts.fontconfig.enable = true;

  gtk = {
    enable = true;
    font = {
      name = "Roboto 9.75";
      package = pkgs.roboto;
    };
    iconTheme = {
      name = "Paper";
      package = pkgs.paper-icon-theme;
    };
    theme = {
      name = "Plata-Noir";
      package = pkgs.plata-theme;
    };
    gtk2.extraConfig = ''
      gtk-cursor-blink = 0
      gtk-im-module = "xim"
      gtk-key-theme-name = "Emacs"
    '';
    gtk3.extraConfig = {
      gtk-application-prefer-dark-theme = 1;
      gtk-cursor-blink = false;
      gtk-im-module = "xim";
      gtk-key-theme-name = "Emacs";
    };
  };

  home = {
    file = {
      ".Xcompose".text = ''
        include "${pkgs.xcompose}/share/dotXCompose"
        include "${pkgs.xcompose}/share/emoji.compose"
        include "${pkgs.xcompose}/share/modletters.compose"
        include "${pkgs.xcompose}/share/tags.compose"
        include "${pkgs.xcompose}/share/maths.compose"
      '';

      ".xprofile".text = ''
        . "$HOME/.nix-profile/etc/profile.d/hm-session-vars.sh"
        if [[ -e "$HOME/.profile" ]]; then
          . "$HOME/.profile"
        fi
      '';
    };

    keyboard.options = [ "ctrl:nocaps" "compose:prsc" ];

    packages = with pkgs; [
      emacs-all-the-icons-fonts
      gksu
      gparted
      gnome3.gnome-themes-extra
      jetbrains-mono
      keybase
      libreoffice
      material-icons
      noto-fonts
      noto-fonts-cjk
      noto-fonts-emoji
      qt5.qtwayland
      roboto
      roboto-mono
      slack
      xorg.xhost
    ];

    sessionVariables = {
      QT_QPA_PLATFORM = "wayland";
      MOZ_DBUS_REMOTE = "1";
      MOZ_ENABLE_WAYLAND = "1";
    };
  };

  programs = {

    browserpass = {
      enable = true;
      browsers = [ "chromium" "firefox" ];
    };

    chromium = {
      enable = true;
      package = pkgs.chromium.override {
        commandLineArgs = [
          "--enable-features=OverlayScrollbar"
          "--enable-gpu-rasterization"
          "--enable-oop-rasterization"
          "--ignore-gpu-blacklist"
        ];
        pulseSupport = true;
      };
      extensions = [
        "naepdomgkenhinolocfifgehidddafch" # browserpass-ce
        "afjjoildnccgmjbblnklbohcbjehjaph" # Browserpass OTP
        "ghbmnnjooekpmoecnnnilnnbdlolhkhi" # Google Docs Offline
        "pkehgijcmpdhfbdbbnkijodmdjhbjlgp" # Privacy Badger
        "eimadpbcbfnmbkopoojfekhnkhdbieeh" # Dark Reader
      ];
    };

    emacs.package = pkgs.emacsPgtkGcc;

    firefox = {
      enable = true;
      profiles.default = {
        settings = {
          "browser.tabs.drawInTitlebar" = true;
          "browser.uidensity" = 0;
          "extensions.pocket.enabled" = false;
          "gfx.webrender.compositor" = true;
          "svg.context-properties.content.enabled" = true;
          "toolkit.cosmeticAnimations.enabled" = false;
          "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
          "ui.key.menuAccessKey" = 0; # Hide access key underlining
        };

        # TODO package this
        userChrome = ''
          @import "/home/tad/proj/firefox-plata-theme/result/plata-theme.css";
          @import "/home/tad/proj/firefox-plata-theme/result/hide-single-tab.css";
          @import "/home/tad/proj/firefox-plata-theme/result/system-icons.css";
          @import "/home/tad/proj/firefox-plata-theme/result/drag-window-headerbar-buttons.css";
        '';
      };
    };
  };

  qt = {
    enable = true;
    platformTheme = "gnome";
    style = {
      name = "adwaita-dark";
      package = pkgs.adwaita-qt;
    };
  };

  services.gpg-agent.pinentryFlavor = "gnome3";
}
