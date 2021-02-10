{ config, lib, pkgs, ... }:

{
  imports = [
    ./misc/mail.nix
    ./programs/emacs.nix
    ./programs/git.nix
    ./programs/texlive.nix
  ];

  home = {
    # Conflicts with pcscd
    file.".gnupg/scdaemon.conf".text = ''
      disable-ccid
    '';

    homeDirectory = "/home/tad";

    packages = with pkgs; [
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
    git = {
      enable = true;
      extraConfig.init.defaultBranch = "main";
    };
    gpg.enable = true;
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
      compression = true;
      controlMaster = "auto";
      controlPersist = "10m";
      matchBlocks."kepler" = {
        hostname = "kepler.lan";
        user = "tad";
        extraOptions = {
          RemoteForward = "/run/user/1000/gnupg/S.gpg-agent /run/user/1000/gnupg/S.gpg-agent.extra";
        };
      };
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
