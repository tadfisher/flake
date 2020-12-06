{ config, lib, pkgs, ... }:

{
  imports = [
    ./misc/mail.nix
    ./programs/emacs.nix
    ./programs/git.nix
    ./programs/texlive.nix
  ];

  home.packages = with pkgs; [
    bash-completion
    curl
    dosfstools
    file
    gnupg
    jq
    lm_sensors
    ripgrep
    rw
    telnet
    trash-cli
    tree
    unrar
    unzip
  ];

  nixpkgs = {
    config = {
      allowUnfree = true;
      allowBroken = true;
      android_sdk.accept_license = true;
      wine.build = "wineWow";
    };
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
      enableNixDirenvIntegration = true;
      config = {
        whitelist = {
          prefix = [
            "${config.home.homeDirectory}/proj"
            "${config.home.homeDirectory}/simple"
            "${config.home.homeDirectory}/src"
          ];
        };
      };
    };
    git.enable = true;
    home-manager.enable = true;
    mercurial = {
      enable = true;
      userName = "Tad Fisher";
      userEmail = "tadfisher@gmail.com";
    };
    pass = {
      enable = true;
      package = pkgs.pass.withExtensions (e: [ e.pass-audit e.pass-otp ]);
      stores.".local/share/pass/personal".alias = "pp";
    };
    ssh = {
      enable = true;
      controlMaster = "auto";
      controlPersist = "10m";
    };
  };

  services = {
    gpg-agent = {
      enable = true;
      defaultCacheTtl = 3600;
      defaultCacheTtlSsh = 3600;
      enableExtraSocket = true;
      enableSshSupport = true;
      grabKeyboardAndMouse = false;
    };
    kbfs = {
      enable = true;
      extraFlags = [ "-label kbfs" "-mount-type normal" ];
      mountPoint = ".local/keybase";
    };
  };

  xdg.userDirs = {
    enable = true;
    desktop = "$HOME";
    documents = "$HOME/doc";
    download = "$HOME/download";
    music = "$HOME/media/music";
    pictures = "$HOME/media/image";
    publicShare = "$HOME/public";
    templates = "$HOME/templates";
    videos = "$HOME/media/video";
  };

  systemd.user.startServices = true;
}
