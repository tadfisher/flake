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
      enable = true;
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

  home.packages = with pkgs; [
    zoom-us
  ];

  programs.git = {
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
}
