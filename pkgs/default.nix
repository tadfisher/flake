{ pkgs ? import <nixpkgs> { } }:

with pkgs;

{
  gamescope = callPackage ./gamescope { };

  sed-opal-unlocker = callPackage ./sed-opal-unlocker { };

  steamos-compositor-plus = callPackage ./steamos-compositor-plus { };

  steamos-modeswitch-inhibitor = callPackage ./steamos-modeswitch-inhibitor { };

  steamos-modeswitch-inhibitor-i686 = pkgsi686Linux.callPackage ./steamos-modeswitch-inhibitor { };

  xcompose = callPackage ./xcompose { };
}
