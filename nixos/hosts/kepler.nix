{ config, lib, pkgs, ... }:
with lib;

{
  imports = [
    ../profiles/core.nix
    ../profiles/media.nix
    ../profiles/seedbox.nix
    ../profiles/services/sshd.nix
    ../profiles/users/tad.nix
    ../profiles/uefi.nix
  ];

  boot = {
    initrd.availableKernelModules = [
      "ahci"
      "ehci_pci"
      "nvme"
      "sd_mod"
      "usbhid"
      "usb_storage"
      "xhci_pci"
    ];
    kernelModules = [
      "kvm-intel"
      "ip6_tables"
    ];
    kernelParams = [
      "mitigations=off"
    ];
  };

  environment.systemPackages = with pkgs; [
    bind
    bridge-utils
    iptables
    psmisc
    wget
  ];

  fileSystems = {
    "/" = {
      device = "/dev/disk/by-label/system";
      fsType = "btrfs";
      options = [ "subvol=nixos" ];
    };

    "/mnt/system" = {
      device = "/dev/disk/by-label/system";
      fsType = "btrfs";
    };

    "/boot" = {
      device = "/dev/disk/by-partlabel/efi-nixos";
      fsType = "vfat";
    };

    "/mnt/data" = {
      device = "/dev/sda";
      fsType = "btrfs";
    };

    "/srv/backup" = {
      device = "/dev/sda";
      fsType = "btrfs";
      options = [ "subvol=backup" ];
    };

    "/srv/mail" = {
      device = "/dev/sda";
      fsType = "btrfs";
      options = [ "subvol=mail" ];
    };

    "/srv/media" = {
      device = "/dev/sda";
      fsType = "btrfs";
      options = [ "subvol=media" ];
    };

    "/srv/torrents" = {
      device = "/dev/sda";
      fsType = "btrfs";
      options = [ "subvol=torrents" ];
    };

    "/srv/steam" = {
      device = "/dev/sda";
      fsType = "btrfs";
      options = [ "subvol=steam" ];
    };
  };

  nix = {
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 7d";
    };
    maxJobs = 4;
    optimise.automatic = true;
  };

  services = {
    btrfs.autoScrub = {
      enable = true;
      fileSystems = [
        "/mnt/data"
        "/mnt/system"
      ];
    };

    iperf3.enable = true;

    transmission.settings = {
      download-dir = "/srv/media/.incoming";
      watch-dir = "/srv/media/.incoming/";
    };

    unifi = {
      enable = true;
      openPorts = true;
      unifiPackage = pkgs.unifi6;
    };
  };

  systemd.network = {
    networks = {
      "40-enp3s0" = {
        name = "enp3s0";
        DHCP = "ipv4";
        networkConfig = {
          DNSSEC = "allow-downgrade";
          EmitLLDP = "nearest-bridge";
          LLMNR = true;
          MulticastDNS = true;
        };
        dhcpConfig.UseDomains = true;
      };
      "40-enp4s0" = {
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
    };
  };

  users.groups = {
    games.gid = 1001;
    # TODO Fix kepler NFS permissions
    media.gid = 2000;
  };

  virtualisation.libvirtd.enable = true;

  system.stateVersion = "21.03";
}
