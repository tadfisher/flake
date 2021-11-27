{ config, lib, pkgs, ... }:

{
  imports = [
    ../profiles/core.nix
    ../profiles/gnome.nix
    ../profiles/development/android.nix
    ../profiles/development/go.nix
    ../profiles/development/jvm.nix
    ../profiles/development/nix.nix
    ../profiles/development/python.nix
    ../profiles/services/gpg-agent.nix
    ../profiles/services/kbfs.nix
    ../profiles/services/mopidy.nix
  ];

  accounts.email.accounts."tadfisher@gmail.com".primary = true;

  home.packages = with pkgs; [ inkscape gimp ];

  android-sdk.packages = sdk: with sdk; [
    build-tools-30-0-2
    cmdline-tools-latest
    emulator
    platforms-android-30
    platform-tools
    skiaparser-1
    sources-android-30
    system-images-android-30-google-apis-playstore-x86
  ];

  programs = {
    devhelp = {
      enable = true;
      packages = with pkgs.gnome; [
        atk
        glib
        gobject-introspection
      ];
    };
    lieer.enable = true;
    ssh.extraConfig = ''
      Host tycho,tycho.lan
        Match user tad
          ForwardAgent yes
        Match user nix-ssh
          IdentitiesOnly yes
          IdentityFile nix-ssh@tycho.lan
    '';
  };

  services = {
    gnirehtet.enable = true;
    lieer.enable = true;
    mopidy.enable = true;
  };
}
