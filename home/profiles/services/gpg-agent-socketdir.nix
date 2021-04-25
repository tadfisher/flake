{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.gpg-agent;

in
{
  # Ensure socket directory is created for remote forwarding.
  systemd.user.services.gpg-agent-socketdir = {
    Unit = {
      Description = "Create GnuPG socket directory";
      Documentation = "man:gpgconf(1)";
    };

    Service = {
      Type = "oneshot";
      ExecStart = "${pkgs.gnupg}/bin/gpgconf --create-socketdir"
        + optionalString cfg.verbose " --verbose";
      RemainAfterExit = true;
    };

    Install = {
      WantedBy = [ "default.target" ];
    };
  };
}
