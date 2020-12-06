{ self, nixos-hardware, ... }@inputs:
let
  modules = [
    nixos-hardware.nixosModules.common-cpu-intel-kaby-lake
    nixos-hardware.nixosModules.common-pc-laptop-ssd
    nixos-hardware.nixosModules.lenovo-thinkpad-t480s
  ];

  config = { config, lib, pkgs, ... }:
    with lib; {
      imports = [
        ../../profiles/core
        ../../profiles/users/tad
        ../../profiles/workstation
        ../../profiles/x86_64
        ./networks.nix
      ];

      boot = {
        initrd.availableKernelModules =
          [ "nvme" "sd_mod" "usb_storage" "xhci_pci" ];
        kernelModules = [ "coretemp" "kvm-intel" ];
        kernelParams = [
          "i915.fastboot=1"
          "mitigations=off"
          "snd_hda_intel.power_save=1"
          "snd_hda_intel.power_save_controller=Y"
        ] ++ (optional
          (versionAtLeast config.boot.kernelPackages.kernel.version "5.9")
          "msr.allow_writes=on");
      };

      fileSystems = {
        "/" = {
          device = "/dev/nvme0n1p1";
          fsType = "btrfs";
          options = [ "subvol=root,discard=async,compress=zstd" ];
        };
        "/boot" = {
          device = "/dev/disk/by-uuid/6922-B344";
          fsType = "vfat";
        };
        "/home" = {
          device = "/dev/nvme0n1p1";
          fsType = "btrfs";
          options = [ "subvol=home,discard=async,compress=zstd" ];
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
          ${pkgs.sed-opal-unlocker}/bin/sed-opal-unlocker s3save /dev/nvme0n1 ${
            ../../../secrets/dirac/sedhash
          }
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

        xserver.libinput = {
          disableWhileTyping = true;
          naturalScrolling = true;
        };
      };

      system.stateVersion = "19.09";
    };

in
modules ++ [ config ]
