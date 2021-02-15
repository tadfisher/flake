{ config, lib, pkgs, ... }:

{
  boot.plymouth.enable = true;

  environment.systemPackages = with pkgs; [
    gnome3.adwaita-icon-theme
    gnome3.gnome-themes-extra
    paper-icon-theme
    plata-theme
  ];

  fonts.fonts = with pkgs; [
    jetbrains-mono
    noto-fonts
    noto-fonts-cjk
    noto-fonts-emoji
    roboto
    roboto-mono
  ];

  hardware = {
    bluetooth = {
      enable = true;
      package = pkgs.bluezFull;
    };

    opengl = {
      enable = true;
      driSupport32Bit = true;
    };

    pulseaudio.enable = false;
  };

  networking.networkmanager = {
    enable = true;
    extraConfig = ''
      [connection]
      connection.mdns=2
    '';
    wifi = {
      backend = "iwd";
      powersave = true;
    };
  };

  nix = {
    daemonNiceLevel = 10;
    daemonIONiceLevel = 7;
  };

  powerManagement = {
    enable = true;
    scsiLinkPolicy = "med_power_with_dipm";
  };

  programs.seahorse.enable = false;

  security.rtkit.enable = true;

  services = {
    avahi.enable = false;

    dbus.packages = [ pkgs.gcr ];

    fwupd.enable = true;

    gnome3 = {
      chrome-gnome-shell.enable = true;
      experimental-features.realtime-scheduling = true;
    };

    pcscd.enable = true;

    pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
      media-session.enable = true;
    };

    printing.enable = true;

    samba = {
      enable = true;
      nsswins = true;
    };

    tlp.enable = false;

    udev = {
      # TODO Package these explicitly
      extraRules = ''
        ${builtins.readFile ../../data/udev/jlink.rules}
        ${builtins.readFile ../../data/udev/particle.rules}
      '';
      packages = with pkgs; [
        android-udev-rules
        openocd
      ];
    };

    xserver = {
      enable = true;
      desktopManager.gnome3.enable = true;
      displayManager.gdm.enable = true;
      enableCtrlAltBackspace = true;
      libinput.enable = true;
      videoDrivers = [ "modesetting" ];
      xkbOptions = "ctrl:nocaps";
    };
  };
}
