{ config, lib, pkgs, ... }:

with lib;
{
  accounts.email.accounts."tad@mercury.com" = {
    address = "tad@mercury.com";
    flavor = "gmail.com";
    folders.drafts = "drafts";
    gpg = {
      key = "tad@mercury.com";
      signByDefault = true;
    };
    imapnotify = {
      enable = false;
      boxes = [ "Inbox" ];
      onNotify = ''
        ${config.programs.lieer.package}/bin/gmi sync -C ${
          config.accounts.email.accounts."tad@mercury.com".maildir.absPath
        }
      '';
      onNotifyPost = ''
        ${config.programs.notmuch-notify.package}/bin/notmuch-notify
      '';
    };
    lieer = {
      enable = true;
      settings.drop_non_existing_label = true;
      sync.enable = true;
    };
    msmtp.enable = true;
    notmuch.enable = true;
    passwordCommand = "${config.programs.password-store.package}/bin/pass show mail.google.com/tad@mercury.com";
    realName = "Tad Fisher";
    userName = "tad@mercury.com";
  };

  dconf.settings."org/gnome/shell".enabled-extensions = [
    pkgs.gnomeExtensions.tailscale-status.extensionUuid
  ];

  home.packages = with pkgs; [
    devenv
    gnomeExtensions.tailscale-status
    zoom-us
  ];

  programs = {
    git = {
      includes = [
        {
          condition = "gitdir:${config.home.homeDirectory}/work/";
          contents.user = {
            email = "tad@mercury.com";
            signingKey = "tad@mercury.com";
          };
        }
      ];
    };

    notmuch.extraConfig.query = {
      "personal" = ''path:"tadfisher@gmail.com/**"'';
      "work" = ''path:"tad@mercury.com/**"'';
    };

    ssh.matchBlocks = {
      "*.internal.mercury.com".forwardAgent = true;
      "ohm.local".forwardAgent = true;
    };
  };
}
