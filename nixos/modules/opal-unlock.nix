{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.boot.initrd.opal;

  consoleFunctions = ''
    opal_msg() {
      echo "$@"
    }

    opal_die() {
      msg "$@" >&2
      exit 1
    }

    opal_askpass() {
      echo -n "$@: " >&2
      IFS= read -rs passphrase
      echo -n "$passphrase"
    }
  '';

  plymouthFunctions = ''
    opal_msg() {
      plymouth display-message --text="$@"
    }

    opal_die() {
      msg "$@"
      plymouth watch-keystroke
      exit 1
    }

    opal_askpass() {
      plymouth ask-for-password --prompt="$@"
    }
  '';

  commonFunctions = ''
    opal_wait_for_device() {
      local name="$1"
      local target="$2"
      local secs="''${3:-10}"

      if ! [ -e $target ]; then
        opal_msg "Waiting $secs seconds for $name to appear..."
        for try in $(seq $secs); do
          sleep 1
          if [ -e $target ]; then
            success=true
            break
          fi
        done
        if [ $success == true ]; then
          return 0
        else
          return 1
        fi
      fi
      return 0
    }

    opal_device_is_locked() {
      sedutil-cli --query "$1" | grep -q 'Locked = Y'
    }

    opal_device_is_shadowed() {
      sedutil-cli --query "$1" | grep -q 'MBRDone = N'
    }

    opal_unlock_device() {
      local name="$1"
      local target="$2"
      local locked=false
      local shadowed=false
      local pass=

      if ! opal_wait_for_device "$name" "$target"; then
        opal_msg "$name failed to appear."
        return 1
      fi

      opal_device_is_locked "$target" && locked=true
      opal_device_is_shadowed "$target" && shadowed=true

      [ $locked != true ] && [ $shadowed != true ] && return 0

      for i in $(seq 1 3); do
        pass="$(opal_askpass "Enter passphrase for $name ($i/3)")"
        [ $locked == true ] && sedutil-cli --setLockingRange 0 RW "$pass" "$target" && locked=false
        [ $shadowed == true ] && sedutil-cli --setMBRDone on "$pass" "$target" && shadowed=false
        [ $locked != true ] && [ $shadowed != true ] && return 0
      done

      return 1
    }
  '';

  unlockCommand = name: { opalDevice, blockDevice }: ''
    if opal_unlock_device "${name}" "${opalDevice}"; then
      blockdev --rereadpt "${blockDevice}"
    else
      opal_die "Failed to unlock ${name}. Cycle power to try again."
    fi
  '';

  driveOpts = { name, config, ... }: {
    options = {
      opalDevice = mkOption {
        type = types.path;
        example = "/dev/nvme0";
        description = ''
          Self-encrypted device to unlock.
        '';
      };

      blockDevice = mkOption {
        type = types.path;
        example = "/dev/nvme0n1";
        description = ''
          Block device containing the partition table.
        '';
      };
    };

    config = {
      opalDevice = mkDefault name;
      blockDevice = mkDefault config.opalDevice;
    };
  };

in
{
  options = {
    boot.initrd.opal = {
      enable = mkEnableOption "OPAL drive unlocking";

      drives = mkOption {
        type = types.attrsOf (types.submodule driveOpts);
        default = { };
        example = {
          "/dev/sda" = { };
          "/dev/nvme0".blockDevice = "/dev/nvme0n1";
          root = {
            opalDevice = "/dev/nvme1";
            blockDevice = "/dev/nvme1n1";
          };
        };
        description = ''
          Drives which may be self-encrypted using TGC OPAL 2
          and must be unlocked during the boot process.
        '';
      };

      sedutilPackage = mkOption {
        type = types.package;
        default = pkgs.sedutil;
        description = ''
          Sedutil package to use. This should match the sedutil package
          used to initialize the drive.
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    assertions = [
      {
        assertion = config.boot.initrd.enable;
        message = "Option 'boot.initrd.opal.drives' requires 'boot.initrd.enable'.";
      }
    ];

    boot.initrd = {
      extraUtilsCommands = ''
        copy_bin_and_libs ${pkgs.util-linux}/bin/blockdev
        copy_bin_and_libs ${cfg.sedutilPackage}/bin/sedutil-cli
      '';

      extraUtilsCommandsTest = ''
        $out/bin/blockdev --version
        $out/bin/sedutil-cli --help 2>&1 | grep -q "sedutil-cli"
      '';

      # We need to unlock drives before LVM but after plymouth is set up.
      preLVMCommands = mkOrder 2000 ''
        ${if config.boot.plymouth.enable then plymouthFunctions else consoleFunctions}
        ${commonFunctions}
        ${concatStringsSep "\n" (mapAttrsToList unlockCommand cfg.drives)}
      '';
    };

    environment.systemPackages = [ cfg.sedutilPackage ];
  };
}
