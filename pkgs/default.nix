{ inputs
, pkgs
}:

with pkgs;

{
  dart-sass = callPackage ./dart-sass { };

  dash-to-panel = callPackage ./dash-to-panel { src = inputs.dash-to-panel; };

  gamescope = callPackage ./gamescope { };

  # gimp = callPackage ./gimp {
  #   gegl = gegl_0_4;
  #   lcms = lcms2;
  #   inherit (darwin.apple_sdk.frameworks) AppKit Cocoa;
  # };

  maxflow = callPackage ./maxflow { };

  mfc9130cw-cupswrapper = callPackage ./mfc9130cwcupswrapper { };
  mfc9130cwlpr = pkgsi686Linux.callPackage ./mfc9130cwlpr { };

  libcapsule = callPackage ./libcapsule { };

  libcapsule-i686 = pkgsi686Linux.callPackage ./libcapsule { };

  libliftoff = callPackage ./libliftoff { };

  paperwm = callPackage ./paperwm { src = inputs.paperwm; };

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

  vertical-overview = callPackage ./vertical-overview { src = inputs.vertical-overview; };

  zephyr-toolchain = callPackage ./zephyr-toolchain { };
}
