{ inputs
, pkgs
}:

with pkgs;

let
  naersk-lib = inputs.naersk.lib.${stdenv.hostPlatform.system};

in
{
  cleaner-overview = callPackage ./cleaner-overview { };

  dart-sass = callPackage ./dart-sass { };

  dash-to-panel = callPackage ./dash-to-panel { src = inputs.dash-to-panel; };

  kotlin-native-unwrapped = callPackage ./kotlin-native {
    jdk = openjdk_headless;
    llvmPackages = llvmPackages_8;
  };

  kotlin-native = callPackage ./kotlin-native/wrapper.nix { };

  gamescope = callPackage ./gamescope { src = inputs.gamescope; };

  # gimp = callPackage ./gimp {
  #   gegl = gegl_0_4;
  #   lcms = lcms2;
  #   inherit (darwin.apple_sdk.frameworks) AppKit Cocoa;
  # };

  instant-workspace-switcher = callPackage ./instant-workspace-switcher {
    src = inputs.instant-workspace-switcher;
  };

  maxflow = callPackage ./maxflow { };

  mfc9130cw-cupswrapper = callPackage ./mfc9130cwcupswrapper { };
  mfc9130cwlpr = pkgsi686Linux.callPackage ./mfc9130cwlpr { };

  libcapsule = callPackage ./libcapsule { };

  libcapsule-i686 = pkgsi686Linux.callPackage ./libcapsule { };

  paperwm = callPackage ./paperwm { src = inputs.paperwm; };

  plex-plexpass = callPackage ./plex-plexpass { };
  plexRaw-plexpass = callPackage ./plex-plexpass/raw.nix { };

  portmod = callPackage ./portmod {
    inherit naersk-lib;
    src = inputs.portmod;
  };

  python-ips = python3.pkgs.callPackage ./python-ips { };

  sed-opal-unlocker = callPackage ./sed-opal-unlocker { };

  steamos-compositor-plus = callPackage ./steamos-compositor-plus { };

  steamos-modeswitch-inhibitor = callPackage ./steamos-modeswitch-inhibitor { };

  steamos-modeswitch-inhibitor-i686 = pkgsi686Linux.callPackage ./steamos-modeswitch-inhibitor { };

  xcompose = callPackage ./xcompose { };

  vertical-overview = callPackage ./vertical-overview { src = inputs.vertical-overview; };

  webp-pixbuf-loader = callPackage ./webp-pixbuf-loader { };

  zephyr-toolchain = callPackage ./zephyr-toolchain { };
}
