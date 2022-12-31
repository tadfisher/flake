{ config, lib, utils, pkgs, ... }:

with lib;

let
  inherit (utils) escapeSystemdPath;

  cfg = config.boot.initrd.opal;

  opalFilesystems = filter (x: x.opalDevice != null) config.system.build.fileSystems;

  stage1Filesystems = filter utils.fsNeededForBoot opalFilesystems;

  stage1OpalDevices = unique (catAttrs "opalDevice" stage1Filesystems);

  stage2Filesystems = filter (x: !(utils.fsNeededForBoot x)) opalFilesystems;

  stage2OpalDevices = filter (x: !(builtins.elem x stage1OpalDevices))
    (unique (catAttrs "opalDevice" stage2Filesystems));

  verboseFlag =
    if cfg.verbosity >= 1 && cfg.verbosity <= 5
    then "-" + concatStringsSep "" (genList (_: "v") cfg.verbosity)
    else "";

  consoleFunctions = ''
    opal_msg() {
      info "$@"
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

    opal_askpass() {
      plymouth ask-for-password --prompt="$@"
    }
  '';

  commonFunctions = ''
    opal_die() {
      info "$@"
      fail
    }

    opal_device_exists() {
      local target="$1"
      if [ -e $target ]; then
        return 0
      else
        local uuid=$(echo -n $target | sed -e 's,UUID=\(.*\),\1,g')
        blkid --uuid $uuid >/dev/null
        return $?
      fi
    }

    opal_wait_for_device() {
      local name="$1"
      local target="$2"
      local secs="''${3:-10}"

      if ! opal_device_exists $target; then
        opal_msg "Waiting $secs seconds for $name to appear..."
        local success=false
        for try in $(seq $secs); do
          sleep 1
          if opal_device_exists $target; then
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

    opal_sedutil() {
      sedutil-cli ${verboseFlag} "$@"
    }

    opal_device_is_locked() {
      opal_sedutil --query "$1" | grep -q 'Locked = Y'
    }

    opal_device_is_shadowed() {
      opal_sedutil --query "$1" | grep -q 'MBRDone = N'
    }

    opal_unlock_device() {
      local name="$1"
      local target="$2"
      local lockingRange="$3"
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
        [ $locked == true ] && opal_sedutil --setLockingRange $lockingRange RW "$pass" "$target" && locked=false
        [ $shadowed == true ] && opal_sedutil --setMBRDone on "$pass" "$target" && shadowed=false
        [ $locked != true ] && [ $shadowed != true ] && return 0
      done

      return 1
    }
  '';

  unlockCommand = opalDevice: ''
    if opal_unlock_device "${opalDevice}" "${opalDevice}" 0; then
      echo "Unlocked ${opalDevice}"
      # blockdev --rereadpt "${device}"
    else
      opal_die "Failed to unlock ${name}. Cycle power to try again."
    fi
  '';

  driveOpts = { name, config, ... }: {
    options = {
      opalDevice = mkOption {
        type = types.nullOr types.path;
        default = null;
        example = "/dev/nvme0";
        description = ''
          Self-encrypted device to unlock.
        '';
      };

      opalLockingRange = mkOption {
        type = types.ints.between 0 9;
        default = 0;
        description = ''
          Locking range to unlock.
        '';
      };
    };
  };

  hibernateResumeService = optional (config.boot.resumeDevice != "")
    "systemd-hibernate-resume@${escapeSystemdPath config.boot.resumeDevice}.service";

  getFilesystems = filesystems: opalDevice:
    filter (x: x.opalDevice == opalDevice) filesystems;

  getMounts = filesystems: prefix: opalDevice:
    let
      # Remove the "/" suffix because even though most mountpoints
      # won't have it, the "/" mountpoint will, and we can't have the
      # trailing slash in "/sysroot/" in stage 1.
      mountPoint = fs: escapeSystemdPath (prefix + (lib.removeSuffix "/" fs.mountPoint));
    in
    map (x: "${mountPoint x}.mount") (getFilesystems filesystems opalDevice);

  serviceName = opalDevice: "opal-unlock-${escapeSystemdPath opalDevice}";

  unlockService = { opalDevice, filesystems, systemd, prefix ? "" }:
    nameValuePair (serviceName opalDevice) {
      description = "Opal Unlock for ${opalDevice}";

      unitConfig = {
        DefaultDependencies = false;
        IgnoreOnIsolate = true;
      };

      after = [
        "${escapeSystemdPath opalDevice}.device"
        "systemd-udevd-kernel.socket"
      ];
      requires = [ "${escapeSystemdPath opalDevice}.device" ];
      wantedBy = (getMounts filesystems prefix opalDevice) ++ [ "local-fs.target" ];
      before = (getMounts filesystems prefix opalDevice) ++ [ "local-fs.target" ];

      script = ''
        opal_sedutil() {
          ${cfg.sedutilPackage}/bin/sedutil-cli ${verboseFlag} "$@"
        }

        opal_device_max_authentications() {
          opal_sedutil --query "$1" | ${pkgs.gnused}/bin/sed -r -e '/MaxAuthentications/ { s/.*MaxAuthentications *= *([0-9]+).*/\1/; p }' -e 'd'
        }

        opal_device_is_locked() {
          opal_sedutil --query "$1" | ${pkgs.gnugrep}/bin/grep -q 'Locked = Y'
        }

        opal_device_is_shadowed() {
          opal_sedutil --query "$1" | ${pkgs.gnugrep}/bin/grep -q 'MBRDone = N'
        }

        locked=false
        shadowed=false
        pass=
        max=

        opal_device_is_locked "${opalDevice}" && locked=true
        opal_device_is_shadowed "${opalDevice}" && shadowed=true

        [ $locked != true ] && [ $shadowed != true ] && return 0

        max="$(opal_device_max_authentications "${opalDevice}")"
        [ -z "$max" ] && max=3

        for i in $(seq 1 $max); do
          pass="$(${systemd}/bin/systemd-ask-password --timeout=0 --id="opal-unlock:${opalDevice}" "Enter passphrase for ${opalDevice} ($i/$max)")"
          [ $locked == true ] && opal_sedutil --setLockingRange 0 RW "$pass" "${opalDevice}" && locked=false
          [ $shadowed == true ] && opal_sedutil --setMBRDone on "$pass" "${opalDevice}" && shadowed=false
          if [ $locked != true ] && [ $shadowed != true ]; then
            ${systemd}/bin/udevadm trigger -v -s block
            exit 0
          fi
        done

        exit 1
      '';

      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
        TimeoutSec = 0;
        KeyringMode = "shared";
      };
    };

in
{
  options = {
    fileSystems = mkOption {
      type = types.attrsOf (types.submodule driveOpts);
    };

    boot.initrd.opal = {
      sedutilPackage = mkOption {
        type = types.package;
        default = pkgs.sedutil;
        description = ''
          Sedutil package to use. This should match the sedutil package
          used to initialize the drive.
        '';
      };

      verbosity = mkOption {
        type = types.ints.between 0 5;
        default = 0;
        description = ''
          Verbosity level between 1 and 5, or 0 to disable.
        '';
      };
    };
  };

  config = mkIf (opalFilesystems != { }) {
    boot.initrd = mkIf (stage1Filesystems != { }) {
      availableKernelModules = [ "dm_mod" "input_leds" ];

      extraUtilsCommands = mkIf (!config.boot.initrd.systemd.enable) ''
        copy_bin_and_libs ${pkgs.util-linux}/bin/blockdev
        copy_bin_and_libs ${cfg.sedutilPackage}/bin/sedutil-cli
      '';

      extraUtilsCommandsTest = mkIf (!config.boot.initrd.systemd.enable) ''
        $out/bin/blockdev --version
        $out/bin/sedutil-cli --help 2>&1 | grep -q "sedutil-cli"
      '';

      # We need to unlock drives before LVM but after plymouth is set up.
      preLVMCommands = mkIf (!config.boot.initrd.systemd.enable) (mkOrder 2000 ''
        ${if config.boot.plymouth.enable then plymouthFunctions else consoleFunctions}
        ${commonFunctions}
        ${concatMapStringsSep "\n" unlockCommand stage1OpalDevices}
      '');

      services.udev.rules = ''
        SUBSYSTEM=="nvme", ACTION=="add", TAG+="systemd"
      '';

      systemd = {
        services = mkMerge [
          (listToAttrs (map
            (opalDevice: (unlockService {
              inherit opalDevice;
              filesystems = opalFilesystems;
              systemd = config.boot.initrd.systemd.package;
              prefix = "/sysroot";
            }))
            stage1OpalDevices))

          (listToAttrs (map
            (opalDevice: nameValuePair (serviceName opalDevice) {
              requiredBy = hibernateResumeService;
              before = hibernateResumeService;
            })
            stage1OpalDevices))
        ];
        extraBin = {
          grep = "${pkgs.gnugrep}/bin/grep";
          sed = "${pkgs.gnused}/bin/sed";
          sedutil-cli = "${cfg.sedutilPackage}/bin/sedutil-cli";
        };
      };
    };

    services.udev.extraRules = mkIf (stage2Filesystems != { }) ''
      SUBSYSTEM=="nvme", ACTION=="add", TAG+="systemd"
    '';

    systemd = mkIf (stage2Filesystems != { }) {
      services = listToAttrs (map
        (opalDevice: (unlockService {
          inherit opalDevice;
          filesystems = stage2Filesystems;
          systemd = config.systemd.package;
        }) // { restartIfChanged = false; })
        stage2OpalDevices);

      targets.opal-disks =
        let
          services = map (opalDevice: "${serviceName opalDevice}.service") stage2OpalDevices;
        in
        {
          requires = services;
          after = services;
          wantedBy = [ "multi-user.target" ];
        };
    };

    environment.systemPackages = [ cfg.sedutilPackage ];
  };
}
