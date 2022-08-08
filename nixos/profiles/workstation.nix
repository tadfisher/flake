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
      enableDebugInfo = true;
      etc."systemd/oomd.conf".text = ''
        [OOM]
        DefaultMemoryPressureDurationSec=20s
      '';
      systemPackages = with pkgs; [
        adw-gtk3
        gnome3.adwaita-icon-theme
        gnome3.gnome-themes-extra
        gst_all_1.gst-libav
        gst_all_1.gst-vaapi
        paper-icon-theme
      ];
    };

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

      opengl.enable = true;

      pulseaudio.enable = false;

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
      enabled = "ibus";
      ibus.engines = with pkgs.ibus-engines; [ typing-booster ];
    };

    networking.networkmanager = {
      enable = true;
      extraConfig = ''
        [connection]
        connection.mdns=2
      '';
      wifi = {
        # backend = "iwd";
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
      evolution.enable = true;
      seahorse.enable = false;
    };

    security = {
      rtkit.enable = true;

      tpm2 = {
        enable = true;
        abrmd.enable = true;
        pkcs11.enable = true;
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

      flatpak.enable = true;

      fwupd.enable = true;

      gnome = {
        chrome-gnome-shell.enable = true;
      };

      logind = {
        killUserProcesses = true;
        lidSwitch = "suspend-then-hibernate";
        lidSwitchDocked = "ignore";
        lidSwitchExternalPower = "lock";
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
          android-udev-rules
          yubikey-personalization
        ];
      };

      xserver = {
        enable = true;
        desktopManager.gnome = {
          enable = true;
          sessionPath = [ pkgs.argyllcms ];
        };
        displayManager.gdm.enable = true;
        enableCtrlAltBackspace = true;
        gdk-pixbuf.modulePackages = [ pkgs.webp-pixbuf-loader ];
        libinput.enable = true;
        videoDrivers = [ "modesetting" ];
        xkbOptions = "ctrl:nocaps";
      };
    };

    systemd = {
      package = pkgs.systemd.override { withOomd = true; };
      oomd = {
        enable = true;
        enableRootSlice = true;
        enableUserServices = true;
      };
      services = {
        bluetooth.serviceConfig.ExecStart = mkForce [
          ""
          "${config.hardware.bluetooth.package}/libexec/bluetooth/bluetoothd -f /etc/bluetooth/main.conf --experimental"
        ];
        systemd-networkd-wait-online.enable = false;
      };
      sleep.extraConfig = ''
        HibernateDelaySec=1h
      '';
    };

    users = {
      groups.systemd-oom.gid = 666;
      users.systemd-oom = {
        uid = 666;
        group = "systemd-oom";
        isSystemUser = true;
      };
    };

    xdg.portal = {
      enable = true;
    };
  }

  (mkIf config.virtualisation.libvirtd.enable {
    environment.systemPackages = with pkgs; [
      gnome3.gnome-boxes
    ];
  })
]
