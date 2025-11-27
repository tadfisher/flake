{ pkgs, ... }:

{
  imports = [
    ./misc/gnome-paths.nix
    ./programs/zed-editor.nix
    ./services/emacs.nix
  ];

  gtk = {
    enable = true;
    colorScheme = "dark";
    font = {
      name = "Roboto";
      size = 9.75;
      package = pkgs.roboto;
    };
    theme = {
      name = "adw-gtk3-dark";
      package = pkgs.adw-gtk3;
    };
    gtk2.extraConfig = ''
      gtk-cursor-blink = 0
      gtk-im-module = "xim"
      gtk-key-theme-name = "Emacs"
    '';
    gtk3 = {
      extraCss = ''
        .background .titlebar:backdrop, .background .titlebar { border-radius: 0; }
        decoration {
          border-radius: 0;
        }
      '';
      extraConfig = {
        gtk-application-prefer-dark-theme = 1;
        gtk-cursor-blink = false;
        gtk-im-module = "xim";
        gtk-key-theme-name = "Emacs";
      };
    };
    gtk4.extraCss = ''
      window.csd {
        border-radius: 0;
      }
    '';
  };

  dconf.settings."org/gnu/emacs/defaults-by-name/emacs" = {
    fullscreen = "fullheight";
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
      d-spy
      emacs-all-the-icons-fonts
      gparted
      gnome-themes-extra
      keybase
      qt5.qtwayland
      signal-desktop
      simple-scan
      slack
      xorg.xhost
    ];

    sessionVariables = {
      NIXOS_OZONE_WL = "1";
      MOZ_DBUS_REMOTE = "1";
      MOZ_ENABLE_WAYLAND = "1";
      QT_QPA_PLATFORM = "wayland";
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
          "--oauth2-client-id=77185425430.apps.googleusercontent.com"
          "--oauth2-client-secret=OTJgUOQcT7lO7GsGZq2G4IlT"
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

    emacs = {
      package = pkgs.emacs30-pgtk;
      extraPackages = (epkgs: with epkgs; [
        treesit-grammars.with-all-grammars
      ]);
    };

    firefox = {
      enable = true;
      package = pkgs.firefox;
      profiles.default = {
        settings = {
          "extensions.pocket.enabled" = false;
          "gfx.webrender.all" = true;
          "media.ffmpeg.vaapi.enabled" = true;
          "media.ffvpx.enabled" = false;
          "media.navigator.mediadataencoder_vpx_enabled" = true;
          "media.rdd-ffmpeg.enabled" = true;
          "ui.key.menuAccessKey" = 0; # Hide access key underlining
          "ui.titlebarRadius" = 0;
        };
      };
    };

    inkscape = {
      enable = true;
      package = pkgs.inkscape-with-extensions;
    };

    password-store.package = pkgs.pass-wayland.withExtensions (e: with e; [
      # BUG https://github.com/NixOS/nixpkgs/pull/335757
      # pass-audit
      pass-otp
    ]);

    rbw = {
      enable = true;
      settings = {
        email = "tadfisher@gmail.com";
        base_url = "https://vault.orion.tad.codes";
        lock_timeout = 86400;
        pinentry = pkgs.pinentry-gnome3;
      };
    };
  };

  qt = {
    enable = true;
    # platformTheme.name = "adwaita";
    # style = {
    #   name = "adwaita-dark";
    #   package = pkgs.adwaita-qt;
    # };
  };

  services = {
    gpg-agent = {
      pinentry.package = pkgs.pinentry-gnome3;
      extraConfig = ''
        allow-emacs-pinentry
      '';
    };
  };

  xdg.configFile."gtk-4.0/settings.ini".source = (pkgs.formats.ini { }).generate "settings.ini" {
    Settings = {
      gtk-application-prefer-dark-theme = 1;
      gtk-cursor-blink = false;
      gtk-font-name = "Roboto 9.75";
    };
  };

  xdg.dataFile."icc/Lenovo P32u-10 (DCI-P3 D65, 48%).icm".source = ../../data/icc + "/Lenovo P32u-10 (DCI-P3 D65, 48%).icm";
  xdg.dataFile."icc/Thinkpad P14s OLED (D65, 50%).icc".source = ../../data/icc + "/Thinkpad P14s OLED (D65, 50%).icc";
}
