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

    environment = {
      etc."systemd/oom.conf".text = ''
        [OOM]
        DefaultMemoryPressureDurationSec=20s
      '';
      systemPackages = with pkgs; [
        gnome3.adwaita-icon-theme
        gnome3.gnome-themes-extra
        paper-icon-theme
        plata-theme
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
            nodename = "brother";
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
        experimental-features.realtime-scheduling = true;
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
          # TODO https://github.com/NixOS/nixpkgs/pull/153501
          libmtp.out
          openocd
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
      additionalUpstreamSystemUnits = [ "systemd-oomd.service" ];
      extraConfig = ''
        DefaultMemoryAccounting=yes
        DefaultTasksAccounting=yes
      '';
      services = {
        systemd-oomd = {
          wantedBy = lib.mkIf (config.swapDevices != [ ]) [ "multi-user.target" ];
          after = [ "swap.target" ];
          aliases = [ "dbus-org.freedesktop.oom1.service" ];
        };
        "user@".serviceConfig = {
          ManagedOOMMemoryPressure = "kill";
          ManagedOOMMemoryPressureLimit = "50%";
        };
      };
      slices."-".sliceConfig = {
        ManagedOOMSwap = "kill";
      };
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
