{ config, lib, pkgs, ... }:

with lib;

{
  imports = [
    ./misc/mail.nix
    ./programs/emacs.nix
    ./programs/git.nix
  ];

  home = {
    homeDirectory = "/home/tad";

    packages = with pkgs; [
      bash-completion
      curl
      dosfstools
      file
      gnupg
      inetutils
      jq
      lm_sensors
      p7zip
      ripgrep
      rw
      trash-cli
      tree
      unrar
      unzip
    ];

    stateVersion = "21.03";
  };

  programs = {
    bash = {
      enable = true;
      shellAliases = {
        l = "ls -l --group-directories-first";
        ll = "ls -alh --group-directories-first";
      };
    };
    direnv = {
      enable = true;
      nix-direnv.enable = true;
    };
    gpg = {
      enable = true;
      # scdaemonSettings = {
      #   reader-port = "Yubico YubiKey OTP+FIDO+CCID 00 00";
      #   disable-application = "piv";
      # };
    };
    home-manager.enable = true;
    mercurial = {
      enable = true;
      userName = "Tad Fisher";
      userEmail = "tadfisher@gmail.com";
    };
    nix-index.enable = true;
    password-store = {
      enable = true;
      package = mkDefault (pkgs.pass.withExtensions (e: with e; [
        # BUG https://github.com/NixOS/nixpkgs/pull/335757
        # pass-audit
        pass-otp
      ]));
    };
    ssh = {
      enable = true;
      compression = true;
      controlMaster = "auto";
      controlPersist = "10m";
    };
  };

  systemd.user.startServices = "sd-switch";

  xdg.userDirs = {
    enable = true;
    desktop = "${config.home.homeDirectory}";
    documents = "${config.home.homeDirectory}/doc";
    download = "${config.home.homeDirectory}/download";
    music = "${config.home.homeDirectory}/media/music";
    pictures = "${config.home.homeDirectory}/media/image";
    publicShare = "${config.home.homeDirectory}/public";
    templates = "${config.home.homeDirectory}/templates";
    videos = "${config.home.homeDirectory}/media/video";
  };
}
