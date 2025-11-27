{ config, lib, pkgs, ... }:

with lib;

mkMerge [
  {
    boot = {
      consoleLogLevel = 0;
      initrd = {
        verbose = false;
      };
      kernelParams = [
        "quiet"
        "boot.shell_on_fail"
        "rd.systemd.show_status=false"
        "rd.udev.log_priority=3"
        "udev.log_priority=3"
      ];
      plymouth.enable = mkDefault true;
    };

    environment = {
      systemPackages = with pkgs; [
        adw-gtk3
        adwaita-icon-theme
        gnome-themes-extra
        gst_all_1.gst-libav
        gst_all_1.gst-vaapi
        paper-icon-theme
        pcscliteWithPolkit.out
      ];
    };

    fonts = {
      packages = with pkgs; [
        inter
        jetbrains-mono
        roboto
      ];

      enableDefaultPackages = true;
    };

    hardware = {
      bluetooth = {
        enable = true;
        package = pkgs.bluez;
      };

      gpgSmartcards.enable = true;

      graphics.enable = true;

      sane = {
        enable = true;
        brscan4 = {
          enable = true;
          netDevices.brother = {
            model = "MFC-9130CW";
            ip = "192.168.0.17";
          };
        };
      };
    };

    i18n.inputMethod = {
      enable = true;
      type = "ibus";
      ibus.engines = with pkgs.ibus-engines; [ typing-booster ];
    };

    networking.networkmanager = {
      enable = true;
      settings.connection."connection.mdns" = 2;
      wifi = {
        backend = "iwd";
        powersave = true;
      };
    };

    nix = {
      daemonCPUSchedPolicy = "batch";
      daemonIOSchedPriority = 7;
    };

    powerManagement = {
      enable = true;
      scsiLinkPolicy = "med_power_with_dipm";
    };

    programs = {
      dconf.enable = true;
      evolution.enable = false;
      gdk-pixbuf.modulePackages = [ pkgs.webp-pixbuf-loader ];
      seahorse.enable = false;
    };

    security = {
      rtkit.enable = true;

      tpm2 = {
        enable = true;
        abrmd.enable = true;
        tctiEnvironment = {
          enable = true;
          interface = "tabrmd";
        };
      };
    };

    services = {
      avahi.enable = false;

      colord.enable = true;

      dbus.packages = [ pkgs.gcr ];

      desktopManager.gnome = {
        enable = true;
        sessionPath = [ pkgs.argyllcms ];
      };

      displayManager.gdm.enable = true;

      flatpak.enable = true;

      fwupd.enable = true;

      gnome.gnome-browser-connector.enable = true;

      libinput.enable = true;

      logind = {
        settings.Login = {
          HandleLidSwitch = "suspend-then-hibernate";
          HandleLidSwitchDocked = "ignore";
          HandleLidSwitchExternalPower = "lock";
          KillUserProcesses = true;
        };
      };

      pcscd.enable = true;

      pipewire = {
        enable = true;
        alsa.enable = true;
        alsa.support32Bit = true;
        jack.enable = true;
        pulse.enable = true;
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
          ${builtins.readFile ../../data/udev/spyderx.rules}
        '';
        packages = with pkgs; [
          yubikey-personalization
        ];
      };

      xserver = {
        enable = true;
        enableCtrlAltBackspace = true;
        videoDrivers = [ "modesetting" ];
        xkb.options = "ctrl:nocaps";
      };
    };

    systemd = {
      network.wait-online.enable = false; # We use NetworkManager for workstations
      oomd = {
        enableRootSlice = true;
        enableUserSlices = true;
      };
      services = {
        bluetooth.serviceConfig.ExecStart = mkForce [
          ""
          "${config.hardware.bluetooth.package}/libexec/bluetooth/bluetoothd -f /etc/bluetooth/main.conf --experimental"
        ];
        # BUG: https://github.com/NixOS/nixpkgs/issues/180175
        NetworkManager-wait-online.enable = mkForce false;
      };
      sleep.extraConfig = ''
        HibernateDelaySec=1h
      '';
    };

    xdg.portal = {
      enable = true;
      extraPortals = [ pkgs.kdePackages.xdg-desktop-portal-kde ];
      xdgOpenUsePortal = true;
    };
  }

  (mkIf config.virtualisation.libvirtd.enable {
    environment.systemPackages = with pkgs; [
      gnome-boxes
    ];
  })
]
