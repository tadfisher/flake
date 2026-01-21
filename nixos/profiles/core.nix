{ config, lib, pkgs, ... }:

with lib;

{
  boot = {
    kernelPackages = mkDefault pkgs.linuxPackages_latest;
    supportedFilesystems = [ "exfat" ];
    initrd.systemd.enable = mkDefault true;
    tmp.cleanOnBoot = true;
  };

  console = {
    earlySetup = true;
    # BUG https://github.com/NixOS/nixpkgs/issues/257904
    # font = "Lat2-Terminus16";
    keyMap = "emacs2";
  };

  documentation.nixos.enable = false;

  # BUG https://github.com/NixOS/nixpkgs/pull/308884
  environment.sessionVariables.LD_LIBRARY_PATH = [
    (makeLibraryPath [ pkgs.pcscliteWithPolkit ])
  ];

  environment.systemPackages = with pkgs; [
    btrfs-progs
    cacert
    curl
    dnsutils
    efibootmgr
    emacs
    git-crypt
    gitFull
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

  programs = {
    bash.completion.enable = true;
    command-not-found.enable = false;
    nix-ld.enable = true;
  };

  security = {
    pam = {
      sshAgentAuth = {
        enable = true;
        authorizedKeysFiles = [ "/etc/ssh/authorized_keys.d/%u" ];
      };
      services.ssh.sshAgentAuth = true;
    };
    sudo.wheelNeedsPassword = false;
  };

  services = {
    resolved = {
      enable = true;
      settings.Resolve = {
        DNSOverTLS = "opportunistic";
        LLMNR = true;
        MulticastDNS = true;
        ResolveUnicastSingleLabel = true;
      };
    };

    userdbd.silenceHighSystemUsers = true;
  };

  systemd.network.enable = true;

  users = {
    mutableUsers = false;
    users.root.hashedPassword = "$6$cva/GQP1SG8fA5g0$QQyEV5vqZgX4TDYcjCq.XosXgP/QCpCUBmMlAy9GRFbLJMxzpWrpqRGo9Wag65JJoLHPkRQeDgiSM4EEPeNv1/";
  };

  time.timeZone = "America/Los_Angeles";

  zramSwap.enable = true;
}
