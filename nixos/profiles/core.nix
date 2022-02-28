{ config, lib, pkgs, ... }:

with lib;

{
  boot = {
    cleanTmpDir = true;
    kernelPackages = pkgs.linuxPackages_latest;
    supportedFilesystems = [ "exfat" ];
  };

  console = {
    earlySetup = true;
    font = "Lat2-Terminus16";
    keyMap = "emacs2";
  };

  environment.systemPackages = with pkgs; [
    btrfs-progs
    cacert
    curl
    dnsutils
    efibootmgr
    emacs
    gitAndTools.gitFull
    gitAndTools.git-crypt
    gnupg
    htop
    nvme-cli
    pciutils
    psmisc
    usbutils
    vim
    wget
    zlib
  ];

  i18n.defaultLocale = "en_US.UTF-8";

  hardware.enableRedistributableFirmware = true;

  networking = {
    # Needed for mDNS
    firewall.enable = false;
    firewall.allowedUDPPorts = [ 5353 ];
    useDHCP = false;
    useNetworkd = true;
  };

  nix.settings = {
    substituters = [
      "https://nix-community.cachix.org/"
    ];
    trusted-public-keys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];
    trusted-substituters = [
      "https://hydra.nixos.org/"
    ];
    trusted-users = [ "@wheel" ];
    sandbox = true;
  };

  nixpkgs.config.allowUnfree = true;

  programs.bash.enableCompletion = true;

  security = {
    pam = {
      enableSSHAgentAuth = true;
      services.ssh.sshAgentAuth = true;
    };
    sudo.wheelNeedsPassword = false;
  };

  services.resolved = {
    enable = true;
    extraConfig = ''
      MulticastDNS=true
      DNSOverTLS=opportunistic
    '';
    llmnr = "false";
  };

  systemd.network.enable = true;

  users = {
    mutableUsers = false;
    users.root.hashedPassword = "$6$cva/GQP1SG8fA5g0$QQyEV5vqZgX4TDYcjCq.XosXgP/QCpCUBmMlAy9GRFbLJMxzpWrpqRGo9Wag65JJoLHPkRQeDgiSM4EEPeNv1/";
  };

  time.timeZone = "America/Los_Angeles";

  zramSwap.enable = true;
}
