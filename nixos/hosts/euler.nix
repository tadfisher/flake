{ config, lib, pkgs, ... }:
with lib;
let
  secrets = import ../../secrets;

in
{
  imports = [
    ../profiles/core.nix
    # ../profiles/networks/euler.nix
    ../profiles/services/sshd.nix
    ../profiles/users/tad.nix
    ../profiles/uefi.nix
    ../profiles/virt-amd.nix
    ../profiles/workstation.nix
  ];

  boot = {
    extraModulePackages = [
      config.boot.kernelPackages.v4l2loopback.out
    ];
    initrd = {
      availableKernelModules = [
        "nvme"
        "ehci_pci"
        "xhci_pci"
        "usbhid"
        "rtsx_pci_sdmmc"
        "sd_mod"
        "usb_storage"
      ];
      opal.sedutilPackage = pkgs.sedutil-fork;
    };
    kernelModules = [
      "kvm-amd"
    ];
    # https://patchwork.freedesktop.org/patch/537553/
    kernelPackages = pkgs.linuxPackages_latest;
    kernelParams = [
      "mitigations=off"
    ];
    resumeDevice = "/dev/disk/by-label/swap";
  };

  environment = {
    etc."NetworkManager/system-connections/mercury.nmconnection" = {
      mode = "0600";
      source = pkgs.substituteAll {
        src = ../../secrets/mercury/vpn/mercury.nmconnection.in;
        ca = ../../secrets/mercury/vpn/mercury-ca.pem;
        cert = ../../secrets/mercury/vpn/mercury-cert.pem;
        key = ../../secrets/mercury/vpn/mercury-key.pem;
        ta = ../../secrets/mercury/vpn/mercury-tls-auth.pem;
      };
    };
  };

  fileSystems = {
    "/boot" = {
      device = "/dev/disk/by-label/boot";
      fsType = "vfat";
    };
    "/" = {
      device = "/dev/disk/by-label/pool";
      fsType = "btrfs";
      opalDevice = "/dev/nvme0";
      options = [ "subvol=root" "discard=async" "compress-force=zstd" ];
    };
    "/home" = {
      device = "/dev/disk/by-label/pool";
      fsType = "btrfs";
      opalDevice = "/dev/nvme0";
      options = [ "subvol=home" "discard=async" "compress-force=zstd" ];
    };
    "/mnt/pool" = {
      device = "/dev/disk/by-label/pool";
      fsType = "btrfs";
      opalDevice = "/dev/nvme0";
      options = [ "discard=async" "compress-force=zstd" ];
    };
    "/mnt/snap" = {
      device = "/dev/disk/by-label/pool";
      fsType = "btrfs";
      opalDevice = "/dev/nvme0";
      options = [ "subvol=snap" "discard=async" "compress-force=zstd" ];
    };
  };

  nix.settings = {
    cores = 8;
    max-jobs = 4;
    substituters = mkAfter [ "https://cache.mercury.com/" ];
    trusted-public-keys = mkAfter [ "cache.mercury.com:yhfFlgvqtv0cAxzflJ0aZW3mbulx4+5EOZm6k3oML+I=" ];
  };

  powerManagement = {
    powerUpCommands = ''
      ${pkgs.sed-opal-unlocker}/bin/sed-opal-unlocker s3save /dev/nvme0n1 ${../../secrets/euler/pool.hash}
    '';
  };

  security.pki.certificateFiles = [ ../../secrets/mercury/mercury.ca.crt ];

  services = {
    btrfs.autoScrub = {
      enable = true;
      fileSystems = [ "/dev/nvme0n1" ];
    };

    libinput.touchpad = {
      disableWhileTyping = true;
      naturalScrolling = true;
    };

    openssh.settings.PasswordAuthentication = true;

    postgresql =
      let
        package = pkgs.postgresql_13;
      in
      {
        enable = true;
        inherit package;
        enableTCPIP = false;
        authentication = ''
          local all all trust
          host all all 127.0.0.1/32 trust
          host all all ::1/128 trust
        '';
        extensions = [ package.pkgs.postgis ];
        settings = {
          timezone = "UTC";
          shared_buffers = 128;
          fsync = false;
          synchronous_commit = false;
          full_page_writes = false;
        };
      };

    tailscale = {
      enable = true;
      useRoutingFeatures = "client";
    };

    udev.extraRules = ''
      # Enable systemd device units for android devices
      ENV{adb_user}=="yes", SUBSYSTEM=="usb", ENV{DEVTYPE}=="usb_device", ACTION=="add", TAG+="systemd", SYMLINK="android adb/%s{serial}", ENV{SYSTEMD_USER_WANTS}+="adb@%s{serial}.target"
      ENV{adb_user}=="yes", SUBSYSTEM=="usb", ENV{DEVTYPE}=="usb_device", ACTION=="remove", TAG+="systemd"
    '';
  };

  swapDevices = [{ label = "swap"; }];

  system.stateVersion = "21.03";

  virtualisation.libvirtd.qemu = {
    ovmf.packages = [ pkgs.OVMFFull ];
    swtpm.enable = true;
  };
}
