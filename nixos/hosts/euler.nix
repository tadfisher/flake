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
    initrd.availableKernelModules = [
      "nvme"
      "sd_mod"
      "usbhid"
      "usb_storage"
      "xhci_pci"
    ];
    kernelModules = [
      "kvm-amd"
    ];
    kernelParams = [
      "mitigations=off"
    ];
  };

  fileSystems = {
    "/" = {
      device = "/dev/nvme0n1";
      fsType = "btrfs";
      options = [ "subvol=root,discard=async,compress=zstd" ];
    };
    "/boot" = {
      device = "/dev/disk/by-label/EFI";
      fsType = "vfat";
    };
    "/home" = {
      device = "/dev/nvme0n1";
      fsType = "btrfs";
      options = [ "subvol=home,discard=async,compress=zstd" ];
    };
  };

  nix = {
    buildCores = 8;
    maxJobs = 2;
  };

  powerManagement = {
    cpuFreqGovernor = "conservative";
    # powerUpCommands = ''
    #   ${pkgs.sed-opal-unlocker}/bin/sed-opal-unlocker s3save /dev/nvme0 ${../../secrets/euler/sedhash}
    # '';
  };

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

  system.stateVersion = "21.03";
}
