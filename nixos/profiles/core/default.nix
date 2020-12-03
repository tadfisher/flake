{ config, lib, pkgs, ... }:

with lib;

{
  boot = {
    cleanTmpDir = true;
    kernelPackages = pkgs.linuxPackages_latest;
    kernelParams = [
      "quiet"
    ];
    loader = {
      systemd-boot.enable = true;
      timeout = 0;
    };
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
    useDHCP = false;
    useNetworkd = true;
  };

  nix = {
    package = pkgs.nixFlakes;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
    trustedUsers = [ "@wheel" ];
    useSandbox = true;
  };

  nixpkgs.config.allowUnfree = true;

  programs = {
    bash.enableCompletion = true;
  };

  services = {
    resolved = {
      enable = true;
      dnssec = "false";
      extraConfig = ''
        MulticastDNS=true
        DNSOverTLS=opportunistic
      '';
    };
  };

  systemd.network.enable = true;

  time.timeZone = "America/Los_Angeles";
}
