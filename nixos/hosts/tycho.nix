{ config, lib, pkgs, ... }:
with lib;

{
  imports = [
    ../profiles/core.nix
    ../profiles/games.nix
    ../profiles/users/tad.nix
    ../profiles/uefi.nix
    ../profiles/workstation.nix
  ];

  boot = {
    initrd.availableKernelModules = [
      "ahci"
      "nvme"
      "sd_mod"
      "usbhid"
      "usb_storage"
      "xhci_pci"
    ];
    kernelModules = [
      "kvm-amd"
      "lm92"
    ];
    kernelParams = [
      "mitigations=off"
    ];
  };

  environment.enableDebugInfo = true;

  fileSystems = {
    "/" = {
      device = "/dev/disk/by-uuid/4584fda1-e195-414e-9557-12a749848132";
      fsType = "btrfs";
      options = [ "subvol=root" ];
    };
    "/boot" = {
      device = "/dev/disk/by-uuid/87E0-37B4";
      fsType = "vfat";
    };
    "/home" = {
      device = "/dev/disk/by-uuid/4584fda1-e195-414e-9557-12a749848132";
      fsType = "btrfs";
      options = [ "subvol=home" ];
    };
    "/mnt/pool" = {
      device = "/dev/disk/by-uuid/4584fda1-e195-414e-9557-12a749848132";
      fsType = "btrfs";
    };
    "/var/local/steam" = {
      device = "/dev/disk/by-uuid/4584fda1-e195-414e-9557-12a749848132";
      fsType = "btrfs";
      options = [ "subvol=steam" ];
    };
  };

  hardware = {
    opengl.extraPackages = with pkgs; [
      libvdpau-va-gl
      vaapiVdpau
    ];
  };

  nix = {
    maxJobs = 16;
  };

  services = {
    openssh.enable = true;

    udev.extraHwdb = ''
      # Elecom HUGE TrackBall
      mouse:usb:v056ep010d:name:ELECOM TrackBall Mouse HUGE TrackBall:
      ID_INPUT_TRACKBALL=1
      MOUSE_DPI=500@125 *1000@125 1500@125
    '';

    # Issues with amdgpu reset
    xserver.displayManager.gdm.autoSuspend = false;
  };

  systemd.network.networks."40-enp4s0" = {
    name = "enp4s0";
    DHCP = "ipv4";
    networkConfig = {
      DNSSEC = "allow-downgrade";
      EmitLLDP = "nearest-bridge";
      LLMNR = true;
      MulticastDNS = true;
    };
    dhcpConfig.UseDomains = true;
  };

  users.groups = {
    games.gid = 1001;
    # TODO Fix kepler NFS permissions
    media.gid = 2000;
  };

  virtualisation.libvirtd.enable = true;

  system.stateVersion = "21.03";
}
