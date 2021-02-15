{ pkgs ? import <nixpkgs> { } }:

with pkgs;

{
  dart-sass = callPackage ./dart-sass { };

  gamescope = callPackage ./gamescope { };

  libcapsule = callPackage ./libcapsule { };

  libcapsule-i686 = pkgsi686Linux.callPackage ./libcapsule { };

  libliftoff = callPackage ./libliftoff { };

  pipewire = callPackage ./pipewire { };

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

  xcompose = callPackage ./xcompose { };

  zephyr-toolchain = callPackage ./zephyr-toolchain { };
}
