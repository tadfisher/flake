{ inputs
, pkgs
}:

let
  inherit (pkgs) callPackage pkgsi686Linux python3 system;
in
{
  ath11k-firmware = callPackage ./ath11k-firmware { };

  cleaner-overview = callPackage ./cleaner-overview { };

  dart-sass = callPackage ./dart-sass { };

  inherit (inputs.emacs-overlay.packages.${system}) emacs-pgtk emacs-unstable-pgtk;

  firefox-gnome-theme = callPackage ./firefox-gnome-theme { src = inputs.firefox-gnome-theme; };

  fleet = callPackage ./fleet { };

  mfc9130cw-cupswrapper = callPackage ./mfc9130cwcupswrapper { };
  mfc9130cwlpr = pkgsi686Linux.callPackage ./mfc9130cwlpr { };

  notmuch-notify = callPackage ./notmuch-notify {
    src = inputs.notmuch-notify;
  };

  libcapsule = callPackage ./libcapsule { };

  libcapsule-i686 = pkgsi686Linux.callPackage ./libcapsule { };

  openjdk21-wakefield = pkgs.openjdk21.overrideAttrs (prev: {
    src = inputs.openjdk-wakefield;
    buildInputs = prev.buildInputs ++ [
      pkgs.shaderc
      pkgs.wayland
    ];
    configureFlags = prev.configureFlags ++ [
      "--with-libffi-include=${pkgs.libffi.dev}/include"
      "--with-libffi-lib=${pkgs.libffi.out}/lib"
      "--with-wayland-include=${pkgs.wayland.dev}/include"
      "--with-wayland-lib=${pkgs.wayland.out}/lib"
      "--with-vulkan-include=${pkgs.vulkan-headers}/include"
      "--with-vulkan-shader-compiler=glslc"
    ];
  });

  paperwm = callPackage ./paperwm { src = inputs.paperwm; };

  plex-plexpass = callPackage ./plex-plexpass { };
  plexRaw-plexpass = callPackage ./plex-plexpass/raw.nix { };

  portmod = callPackage ./portmod { src = inputs.portmod; };

  python-ips = python3.pkgs.callPackage ./python-ips { };

  sed-opal-unlocker = callPackage ./sed-opal-unlocker { };

  sedcli = callPackage ./sedcli { src = inputs.sedcli; };

  steamos-compositor-plus = callPackage ./steamos-compositor-plus { };

  steamos-modeswitch-inhibitor = callPackage ./steamos-modeswitch-inhibitor { };

  steamos-modeswitch-inhibitor-i686 = pkgsi686Linux.callPackage ./steamos-modeswitch-inhibitor { };

  xcompose = callPackage ./xcompose { };

  vertical-overview = callPackage ./vertical-overview { src = inputs.vertical-overview; };

  zephyr-toolchain = callPackage ./zephyr-toolchain { };

  zbus = callPackage ./zbus { };
}
