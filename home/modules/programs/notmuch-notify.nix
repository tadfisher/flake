{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.programs.notmuch-notify;

  configFile = (pkgs.formats.toml { }).generate "notmuch-notify.toml" {
    inherit (cfg) query command interval maildir;
  };

in
{
  options.programs.notmuch-notify = {
    enable = mkEnableOption "notmuch-notify";

    package = mkOption {
      type = types.package;
      default = pkgs.notmuch-notify;
      description = ''
        Package to use for notmuch-notify.
      '';
    };

    query = mkOption {
      type = types.str;
      default = "tag:unread NOT (tag:spam OR tag:deleted)";
      description = ''
        Notmuch query for which new mail is checked.
      '';
    };

    command = mkOption {
      type = types.str;
      default = "${pkgs.coreutils}/bin/true";
      example = "astroid";
      description = ''
        Command to open a mail client.
      '';
    };

    interval = mkOption {
      type = types.ints.positive;
      default = 86400;
      defaultText = "24 hours";
      description = ''
        Interval in seconds to consider a message as unseen.
      '';
    };

    maildir = mkOption {
      type = types.str;
      default = config.accounts.email.maildirBasePath;
      description = ''
        Maildir path to check for new messages. The default is obtained
        from <option>accounts.email.maildirBasePath</option>.
      '';
    };
  };

  config = mkIf cfg.enable {
    home.packages = [ cfg.package ];

    xdg.configFile."notmuch-notify.toml".source = configFile;
  };
}
