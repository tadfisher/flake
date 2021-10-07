{ config, lib, pkgs, ... }:

with lib;

mkMerge [
  {
    boot = {
      consoleLogLevel = 0;
      initrd.verbose = false;
      kernelParams = [ "quiet" "udev.log_priority=3" ];
      plymouth.enable = true;
    };

    environment.systemPackages = with pkgs; [
      gnome3.adwaita-icon-theme
      gnome3.gnome-themes-extra
      paper-icon-theme
      plata-theme
    ];

    fonts.fonts = with pkgs; [
      jetbrains-mono
      noto-fonts
      noto-fonts-cjk
      noto-fonts-emoji
      roboto
      roboto-mono
    ];

    hardware = {
      bluetooth = {
        enable = true;
        package = pkgs.bluezFull;
      };

      opengl = {
        enable = true;
        driSupport32Bit = true;
      };

      pulseaudio.enable = false;

      sane = {
        enable = true;
        brscan4 = {
          enable = true;
          netDevices.brother = {
            model = "MFC-9130CW";
            nodename = "brother";
          };
        };
      };
    };

    networking.networkmanager = {
      enable = true;
      extraConfig = ''
        [connection]
        connection.mdns=2
      '';
      wifi = {
        backend = "iwd";
        powersave = true;
      };
    };

    nix = {
      daemonNiceLevel = 10;
      daemonIONiceLevel = 7;
    };

    powerManagement = {
      enable = true;
      scsiLinkPolicy = "med_power_with_dipm";
    };

    programs.seahorse.enable = false;

    security.rtkit.enable = true;

    services = {
      avahi.enable = false;

      dbus.packages = [ pkgs.gcr ];

      flatpak.enable = true;

      fwupd.enable = true;

      gnome = {
        chrome-gnome-shell.enable = true;
        experimental-features.realtime-scheduling = true;
      };

      pcscd.enable = true;

      pipewire = {
        enable = true;
        alsa.enable = true;
        alsa.support32Bit = true;
        pulse.enable = true;
        media-session.enable = true;
      };

      printing = {
        enable = true;
        drivers = with pkgs; [ mfc9130cwlpr mfc9130cw-cupswrapper ];
      };

      samba = {
        enable = true;
        nsswins = true;
      };

      saned.enable = true;

      tlp.enable = false;

      udev = {
        # TODO Package these explicitly
        extraRules = ''
          ${builtins.readFile ../../data/udev/jlink.rules}
          ${builtins.readFile ../../data/udev/particle.rules}
        '';
        packages = with pkgs; [
          android-udev-rules
          openocd
          yubikey-personalization
        ];
      };

      xserver = {
        enable = true;
        desktopManager.gnome.enable = true;
        displayManager.gdm.enable = true;
        enableCtrlAltBackspace = true;
        libinput.enable = true;
        videoDrivers = [ "modesetting" ];
        xkbOptions = "ctrl:nocaps";
      };
    };

    xdg.portal = {
      enable = true;
      gtkUsePortal = true;
    };
  }

  (mkIf config.virtualisation.libvirtd.enable {
    environment.systemPackages = with pkgs; [
      gnome3.gnome-boxes
    ];
  })

  # TODO https://github.com/NixOS/nixpkgs/issues/121121
  {
    security.polkit.extraConfig = ''
      polkit.addRule(function(action, subject) {
        if (action.id == "org.debian.pcsc-lite.access_card" &&
            subject.isInGroup("wheel")) {
          return polkit.Result.YES;
        }
      });

      polkit.addRule(function(action, subject) {
        if (action.id == "org.debian.pcsc-lite.access_pcsc" &&
            subject.isInGroup("wheel")) {
          return polkit.Result.YES;
        }
      });
    '';
  }
]
