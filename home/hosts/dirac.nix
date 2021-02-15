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
    ../profiles/services/mopidy.nix
  ];

  accounts.email.accounts."tadfisher@gmail.com".primary = true;

  home.packages = with pkgs; [ inkscape gimp ];

  android-sdk.packages = sdk: with sdk; [
    system-images-android-23-google-apis-x86
    system-images-android-24-google-apis-x86
    system-images-android-27-google-apis-playstore-x86
  ];

  programs = {
    lieer.enable = true;
    pass.stores.".local/share/pass/personal".primary = true;
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
