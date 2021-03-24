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
      device = "/dev/disk/by-label/boot";
      fsType = "vfat";
    };
    "/" = {
      device = "/dev/disk/by-label/pool";
      fsType = "btrfs";
      options = [ "subvol=root" "discard=async" "compress=zstd" ];
    };
    "/home" = {
      device = "/dev/disk/by-label/pool";
      fsType = "btrfs";
      options = [ "subvol=home" "discard=async" "compress=zstd" ];
    };
    "/mnt/pool" = {
      device = "/dev/disk/by-label/pool";
      fsType = "btrfs";
      options = [ "discard=async" "compress=zstd" ];
    };
    "/mnt/snap" = {
      device = "/dev/disk/by-label/pool";
      fsType = "btrfs";
      options = [ "subvol=snap" "discard=async" "compress=zstd" ];
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
      # '';
  };

  security.pki.certificateFiles = [ ../../secrets/euler/mercury.ca.crt ];

  services = {
    btrfs.autoScrub = {
      enable = true;
      fileSystems = [ "/dev/nvme0n1" ];
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

  swapDevices = [{ label = "swap"; }];

  system.stateVersion = "21.03";
}
