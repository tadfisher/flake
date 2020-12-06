{ ... }@inputs:

{ config, lib, pkgs, ... }:

{
  imports = [ ../../profiles/core.nix ../../profiles/gnome.nix ];

  accounts.email.accounts."tad@simple.com".primary = true;

  home.packages = with pkgs; [ inkscape gimp ];

  profiles = {
    dev = {
      enable = true;

      android = {
        enable = true;
        sdk = {
          channel = "canary";
          packages = sdk:
            with sdk; [
              build-tools-29-0-3
              build-tools-30-0-2
              cmdline-tools-latest
              emulator
              platform-tools
              platforms.android-29
              platforms.android-30
              skiaparser-1
              sources.android-28
              sources.android-29
              sources.android-30
              system-images.android-23.google-apis.x86
              system-images.android-24.google-apis.x86
              system-images.android-27.google-apis-playstore.x86
              system-images.android-29.google-apis-playstore.x86
            ];
        };
      };

      go.enable = true;
      jvm.enable = true;
      nix.enable = true;
      python.enable = true;
      rust.enable = true;
    };
    nixos.enable = true;
    work.enable = true;
  };

  programs = {
    # home-manager.path = "<home-manager>";
    lieer.enable = true;
    ssh.extraConfig = ''
      Host tycho,tycho.lan
        Match user tad
          ForwardAgent yes
        Match user nix-ssh
          IdentitiesOnly yes
          IdentityFile nix-ssh@tycho.lan
    '';
    texlive.enable = true;
  };

  services = {
    gnirehtet.enable = true;
    lieer.enable = true;
    mopidy.enable = true;
  };
}
