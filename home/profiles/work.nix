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
    lieer = {
      enable = true;
      dropNonExistingLabels = true;
      sync.enable = true;
    };
    msmtp.enable = true;
    notmuch.enable = true;
    passwordCommand = "pass show mail.google.com/tad@mercury.com | head -n 1";
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
