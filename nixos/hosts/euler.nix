{ config, lib, pkgs, ... }:
with lib;
let
  secrets = import ../../secrets;

in
{
  imports = [
    ../profiles/core.nix
    ../profiles/users/tad.nix
    ../profiles/uefi.nix
    ../profiles/virt-amd.nix
    ../profiles/workstation.nix
  ];

  boot = {
    initrd = {
      availableKernelModules = [
        "nvme"
        "sd_mod"
        "usbhid"
        "usb_storage"
        "xhci_pci"
      ];
      opal = {
        drives.system = {
          opalDevice = "/dev/nvme0";
          blockDevice = "/dev/nvme0n1";
        };
        sedutilPackage = pkgs.sedutil-fork;
      };
    };
    kernelModules = [
      "kvm-amd"
    ];
    kernelParams = [
      "mitigations=off"
    ];
  };

  environment.etc."NetworkManager/system-connections/mercury.nmconnection" = {
    mode = "0600";
    source = pkgs.substituteAll {
      src = ../../secrets/euler/vpn/mercury.nmconnection.in;
      ca = ../../secrets/euler/vpn/mercury-ca.pem;
      cert = ../../secrets/euler/vpn/mercury-cert.pem;
      key = ../../secrets/euler/vpn/mercury-key.pem;
      ta = ../../secrets/euler/vpn/mercury-tls-auth.pem;
    };
  };

  fileSystems = {
    "/boot" = {
      # device = "/dev/disk/by-label/boot";
      device = "/dev/disk/by-uuid/87E0-37B4";
      fsType = "vfat";
    };
    "/" = {
      device = "/dev/disk/by-label/pool";
      fsType = "btrfs";
      options = [ "subvol=root" "discard=async" "compress-force=zstd" ];
    };
    "/home" = {
      device = "/dev/disk/by-label/pool";
      fsType = "btrfs";
      options = [ "subvol=home" "discard=async" "compress-force=zstd" ];
    };
    "/mnt/pool" = {
      device = "/dev/disk/by-label/pool";
      fsType = "btrfs";
      options = [ "discard=async" "compress-force=zstd" ];
    };
    "/mnt/snap" = {
      device = "/dev/disk/by-label/pool";
      fsType = "btrfs";
      options = [ "subvol=snap" "discard=async" "compress-force=zstd" ];
    };
  };

  nix = {
    binaryCaches = [ "https://cache.mercury.com/" ];
    binaryCachePublicKeys = [ "cache.mercury.com:yhfFlgvqtv0cAxzflJ0aZW3mbulx4+5EOZm6k3oML+I=" ];
    buildCores = 4;
    maxJobs = 4;
  };

  powerManagement = {
    cpuFreqGovernor = "schedutil";
    powerUpCommands = ''
      ${pkgs.sed-opal-unlocker}/bin/sed-opal-unlocker s3save /dev/nvme0n1 ${../../secrets/euler/pool.hash}
      # Limit charging thresholds to 40-50%
      echo 40 > /sys/class/power_supply/BAT0/charge_control_start_threshold
      echo 50 > /sys/class/power_supply/BAT0/charge_control_end_threshold
    '';
  };

  programs.cardboard.enable = true;

  security.pki.certificateFiles = [ ../../secrets/euler/mercury.ca.crt ];

  services = {
    btrfs.autoScrub = {
      enable = true;
      fileSystems = [ "/dev/nvme0n1" ];
    };

    postgresql = {
      enable = true;
      package = pkgs.postgresql_12;
      enableTCPIP = false;
      authentication = ''
        local all all trust
        host all all 127.0.0.1/32 trust
        host all all ::1/128 trust
      '';
      settings = {
        timezone = "UTC";
        shared_buffers = 128;
        fsync = false;
        synchronous_commit = false;
        full_page_writes = false;
      };
    };

    udev.extraRules = ''
      # Enable systemd device units for android devices
      ENV{adb_user}=="yes", SUBSYSTEM=="usb", ENV{DEVTYPE}=="usb_device", ACTION=="add", TAG+="systemd", SYMLINK="android adb/%s{serial}", ENV{SYSTEMD_USER_WANTS}+="adb@%s{serial}.target"
      ENV{adb_user}=="yes", SUBSYSTEM=="usb", ENV{DEVTYPE}=="usb_device", ACTION=="remove", TAG+="systemd"
    '';

    xserver.libinput.touchpad = {
      disableWhileTyping = true;
      naturalScrolling = true;
    };
  };

  # swapDevices = [{ label = "swap"; }];

  system.stateVersion = "21.03";
}
