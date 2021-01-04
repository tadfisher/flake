{ pkgs ? import <nixpkgs> { } }:

with pkgs;

{
  gamescope = callPackage ./gamescope { };

  libcapsule = callPackage ./libcapsule { };

  libcapsule-i686 = pkgsi686Linux.callPackage ./libcapsule { };

  plex-plexpass = callPackage ./plex-plexpass { };
  plexRaw-plexpass = callPackage ./plex-plexpass/raw.nix { };

  python-ips = python3.pkgs.callPackage ./python-ips { };

  sed-opal-unlocker = callPackage ./sed-opal-unlocker { };

  steam-runtime-tools = callPackage ./steam-runtime-tools {
    stdenv = multiStdenv;
  };

  steam-runtime-tools-i686 = callPackage ./steam-runtime-tools {
    stdenv = multiStdenv;
    steam-runtime-tools-i686 = null;
  };

  steamos-compositor-plus = callPackage ./steamos-compositor-plus { };

  steamos-modeswitch-inhibitor = callPackage ./steamos-modeswitch-inhibitor { };

  steamos-modeswitch-inhibitor-i686 = pkgsi686Linux.callPackage ./steamos-modeswitch-inhibitor { };

  unifi-beta = callPackage ./unifi-beta { };

  # Testing.
  waffle-test = enableDebugging (waffle.overrideAttrs (attrs: {
    dontStrip = true;
  }));

  xcompose = callPackage ./xcompose { };

  zephyr-toolchain = callPackage ./zephyr-toolchain { };
}
