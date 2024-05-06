{ lib, pkgs, ... }:
with lib;
{
  imports = [
    ../profiles/core.nix
    ../profiles/services/sshd.nix
    ../profiles/users/tad.nix
    ../profiles/uefi.nix
    ../profiles/virt-amd.nix
    ../profiles/workstation.nix
  ];

  boot = {
    initrd = {
      availableKernelModules = [
        "nvme"
        "xhci_pci"
	      "thunderbolt"
        "usbhid"
        "sd_mod"
        "usb_storage"
      ];
      luks.devices."crypt".device = "/dev/disk/by-partlabel/crypt";
    };
    kernelModules = [
      "kvm-amd"
    ];
    kernelPackages = pkgs.linuxPackages_testing;
    kernelParams = [
      "mitigations=off"
      "resume_offset=533760"
    ];
    resumeDevice = "/dev/disk/by-label/pool";
  };

  hardware.firmware = mkBefore [ pkgs.ath11k-firmware ];

  environment.etc = {
    "NetworkManager/system-connections/mercury.nmconnection" = {
      mode = "0600";
      source = pkgs.substituteAll {
        src = ../../secrets/mercury/vpn/mercury.nmconnection.in;
        ca = ../../secrets/mercury/vpn/mercury-ca.pem;
        cert = ../../secrets/mercury/vpn/mercury-cert.pem;
        key = ../../secrets/mercury/vpn/mercury-key.pem;
        ta = ../../secrets/mercury/vpn/mercury-tls-auth.pem;
      };
    };
    "kolide-k2/secret" = {
      mode = "0600";
      source = ../../secrets/mercury/kolide;
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
      options = [ "subvol=root" "discard=async" "compress-force=zstd" ];
    };
    "/home" = {
      device = "/dev/disk/by-label/pool";
      fsType = "btrfs";
      options = [ "subvol=home" "discard=async" "compress-force=zstd" ];
    };
    "/var/swap" = {
      device = "/dev/disk/by-label/pool";
      options = [ "subvol=swap" ];
    };

    # "/mnt/pool" = {
    #   device = "/dev/disk/by-label/pool";
    #   fsType = "btrfs";
    #   options = [ "discard=async" "compress-force=zstd" ];
    # };
    # "/mnt/snap" = {
    #   device = "/dev/disk/by-label/pool";
    #   fsType = "btrfs";
    #   opalDevice = "/dev/nvme0";
    #   options = [ "subvol=snap" "discard=async" "compress-force=zstd" ];
    # };
  };

  nix.settings = {
    cores = 8;
    max-jobs = 4;
    substituters = mkAfter [ "https://cache.mercury.com/" ];
    trusted-public-keys = mkAfter [ "cache.mercury.com:yhfFlgvqtv0cAxzflJ0aZW3mbulx4+5EOZm6k3oML+I=" ];
  };

  security.pki.certificateFiles = [ ../../secrets/mercury/mercury.ca.crt ];

  services = {
    btrfs.autoScrub = {
      enable = true;
      fileSystems = [ "/dev/nvme0n1" ];
    };

    fwupd.extraRemotes = [ "lvfs-testing" ];

    homed.enable = true;

    kolide-launcher.enable = true;

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
        extraPlugins = [ package.pkgs.postgis ];
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

  swapDevices = [{ device = "/var/swap/swapfile"; }];

  system.stateVersion = "23.11";

  virtualisation.libvirtd.qemu = {
    ovmf.packages = [ pkgs.OVMFFull ];
    swtpm.enable = true;
  };
}
