{ inputs
, pkgs
}:

with pkgs;

{
  adw-gtk3 = callPackage ./adw-gtk3 { src = inputs.adw-gtk3; };

  cleaner-overview = callPackage ./cleaner-overview { };

  emacsCustom = emacsPgtkGcc.override {
    withXwidgets = true;
    withXinput2 = true;
  };

  dart-sass = callPackage ./dart-sass { };

  dash-to-panel = callPackage ./dash-to-panel { src = inputs.dash-to-panel; };

  firefox-gnome-theme = callPackage ./firefox-gnome-theme { src = inputs.firefox-gnome-theme; };

  gamescope = callPackage ./gamescope { src = inputs.gamescope; };

  # gimp = callPackage ./gimp {
  #   gegl = gegl_0_4;
  #   lcms = lcms2;
  #   inherit (darwin.apple_sdk.frameworks) AppKit Cocoa;
  # };

  instant-workspace-switcher = callPackage ./instant-workspace-switcher {
    src = inputs.instant-workspace-switcher;
  };

  jetbrains-jdk17 = (openjdk17.overrideAttrs (attrs: {
    pname = "jetbrains-jdk";
    version = "unstable-17-0-1";
    src = inputs.jetbrains-jdk;
    patches = [ ];
    meta = attrs.meta // {
      longDescription = ''
        JetBrains Runtime is a runtime environment for running IntelliJ Platform
        based products on Windows, Mac OS X, and Linux. JetBrains Runtime is
        based on OpenJDK project with some modifications. These modifications
        include: Subpixel Anti-Aliasing, enhanced font rendering on Linux, HiDPI
        support, ligatures, some fixes for native crashes not presented in
        official build, and other small enhancements.

        JetBrains Runtime is not a certified build of OpenJDK. Please, use at
        your own risk.
      '';
      homepage = "https://confluence.jetbrains.com/display/JBR/JetBrains+Runtime";
    };
    passthru = attrs.passthru // {
      home = "${jetbrains-jdk17}/lib/openjdk";
    };
  })).override {
    enableJavaFX = false;
  };

  kotlin-native-unwrapped = callPackage ./kotlin-native {
    jdk = openjdk_headless;
    llvmPackages = llvmPackages_8;
  };

  kotlin-native = callPackage ./kotlin-native/wrapper.nix { };

  maxflow = callPackage ./maxflow { };

  mfc9130cw-cupswrapper = callPackage ./mfc9130cwcupswrapper { };
  mfc9130cwlpr = pkgsi686Linux.callPackage ./mfc9130cwlpr { };

  notmuch-notify = callPackage ./notmuch-notify {
    src = inputs.notmuch-notify;
  };

  libcapsule = callPackage ./libcapsule { };

  libcapsule-i686 = pkgsi686Linux.callPackage ./libcapsule { };

  paperwm = callPackage ./paperwm { src = inputs.paperwm; };

  plex-plexpass = callPackage ./plex-plexpass { };
  plexRaw-plexpass = callPackage ./plex-plexpass/raw.nix { };

  portmod = callPackage ./portmod { src = inputs.portmod; };

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
