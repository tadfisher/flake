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
    ../profiles/workstation.nix
  ];

  boot = {
    initrd = {
      availableKernelModules = [
        "nvme"
        "sd_mod"
        "usb_storage"
        "xhci_pci"
      ];
    };
    kernelModules = [
      "coretemp"
      "kvm-intel"
    ];
    kernelParams = [
      "i915.fastboot=1"
      "mitigations=off"
      "snd_hda_intel.power_save=1"
      "snd_hda_intel.power_save_controller=Y"
    ];
  };

  fileSystems = {
    "/boot" = {
      device = "/dev/disk/by-label/boot";
      fsType = "vfat";
    };
    "/" = {
      device = "/dev/disk/by-label/pool";
      fsType = "btrfs";
      options = [ "subvol=root,discard=async,compress-force=zstd" ];
    };
    "/home" = {
      device = "/dev/disk/by-label/pool";
      fsType = "btrfs";
      options = [ "subvol=home,discard=async,compress-force=zstd" ];
    };
    "/mnt/pool" = {
      device = "/dev/disk/by-label/pool";
      fsType = "btrfs";
      options = [ "discard=async,compress-force=zstd" ];
    };
    "/mnt/snap" = {
      device = "/dev/disk/by-label/pool";
      fsType = "btrfs";
      options = [ "subvol=snap,discard=async,compress-force=zstd" ];
    };
  };

  nix = {
    buildCores = 4;
    maxJobs = 2;
  };

  powerManagement.cpuFreqGovernor = "conservative";

  services = {
    btrfs.autoScrub = {
      enable = true;
      fileSystems = [ "/dev/nvme0n1p1" ];
    };

    udev.extraRules = ''
      # Runtime PM for I2C Adapter i2c-0 (SMBus I801 adapter at efa0)
      # Runtime PM for I2C Adapter i2c-1 (i915 gmbus dpc)
      # Runtime PM for I2C Adapter i2c-2 (i915 gmbus dpb)
      # Runtime PM for I2C Adapter i2c-3 (i915 gmbus dpd)
      SUBSYSTEM=="i2c", KERNEL=="i2c-[0-4]", ATTR{device/power/control}="auto"

      # Autosuspend for Intel Corp. bluetooth adapter
      SUBSYSTEM=="usb", ATTR{idVendor}=="8087", ATTR{idProduct}=="0a2b", ATTR{power/control}="auto"

      # Autosuspend for USB device Chicony Electronics Co., Ltd Integrated Camera
      SUBSYSTEM=="usb", ATTR{idVendor}=="04f2", ATTR{idProduct}=="b604", ATTR{power/control}="auto"

      # Autosuspend for Synaptics fingerprint sensor
      SUBSYSTEM=="usb", ATTR{idVendor}=="06cb", ATTR{idProduct}=="009a", ATTR{power/control}="auto"

      # Runtime PM for PCI Device *
      SUBSYSTEM=="pci", ATTR{power/control}="auto"

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
