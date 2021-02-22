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
      opal.drives.system = {
        opalDevice = "/dev/nvme1";
        blockDevice = "/dev/nvme1n1";
      };
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
    ] ++ (optional
      (versionAtLeast config.boot.kernelPackages.kernel.version "5.9")
      "msr.allow_writes=on");
    plymouth.theme = "breeze-text";
  };

  environment.etc = {
    "NetworkManager/system-connections/simple-vpn.nmconnection" = {
      mode = "0600";

      text = generators.toINI { } {
        connection = {
          id = "simple-vpn";
          uuid = "7c486dc2-a980-4c2b-a476-ef222725cb52";
          type = "vpn";
          permissions = "user:tad:;";
          secondaries = "";
        };

        vpn = {
          inherit (secrets.simple.vpn) ca cert key ta;
          auth = "SHA256";
          cert-pass-flags = 0;
          cipher = "AES-256-CBC";
          connection-type = "password-tls";
          dev-type = "tun";
          dev = "tun";
          password-flags = 1;
          persistent = "yes";
          ping-restart = 3600;
          ping = 10;
          remote = "openvpn.banksimple.com:1194:udp";
          reneg-seconds = 0;
          service-type = "org.freedesktop.NetworkManager.openvpn";
          ta-dir = 1;
          tun-ipv6 = "no";
          username = "tad";
        };

        ipv4 = { method = "auto"; };
      };
    };

    "NetworkManager/system-connections/simple-wifi.nmconnection" = {
      mode = "0600";

      text = generators.toINI { } {
        connection = {
          id = "simple-wifi";
          uuid = "28ab5a0d-fde7-4c0c-8814-9496c302c95f";
          type = "wifi";
          permissions = "user:tad:;";
        };

        wifi = {
          mac-address-blacklist = "";
          mode = "infrastructure";
          ssid = "Simple";
        };

        wifi-security = {
          auth-alg = "open";
          key-mgmt = "wpa-eap";
        };

        "802-1x" = with (secrets.simple.wifi); {
          eap = "ttls";
          identity = user;
          password = pass;
          phase2-auth = "mschapv2";
        };

        ipv4 = {
          dns-search = "";
          method = "auto";
        };

        ipv6 = {
          addr-gen-mode = "stable-privacy";
          dns-search = "";
          method = "auto";
        };
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
      options = [ "subvol=root,discard=async,compress=zstd" ];
    };
    "/home" = {
      device = "/dev/disk/by-label/pool";
      fsType = "btrfs";
      options = [ "subvol=home,discard=async,compress=zstd" ];
    };
    "/mnt/pool" = {
      device = "/dev/disk/by-label/pool";
      fsType = "btrfs";
      options = [ "discard=async,compress=zstd" ];
    };
    "/mnt/snap" = {
      device = "/dev/disk/by-label/pool";
      fsType = "btrfs";
      options = [ "subvol=snap,discard=async,compress=zstd" ];
    };
  };

  hardware.pulseaudio = {
    modules.module-echo-cancel = {
      package = pkgs.pulseaudio-modules-bt;
      arguments = {
        use_master_format = true;
        aec_method = "webrtc";
        aec_args = {
          analog_gain_control = false;
          digital_gain_control = true;
          beamforming = true;
          mic_geometry = [ (-3.0e-2) 0.0 0.0 3.0e-2 0.0 0.0 ];
          noise_suppression = true;
        };
        source_name = "echoCancel_source";
        sink_name = "echoCancel_sink";
      };
    };
    extraConfig = ''
      set-default-source echoCancel_source
      set-default-sink echoCancel_sink
    '';
  };

  nix = {
    buildCores = 4;
    maxJobs = 2;
  };

  powerManagement = {
    cpuFreqGovernor = "conservative";
    powerUpCommands = ''
      ${pkgs.sed-opal-unlocker}/bin/sed-opal-unlocker s3save /dev/nvme1 ${../../secrets/dirac/pool.hash}
    '';
  };

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
