{ pkgs ? import <nixpkgs> { } }:

with pkgs;

{
  sed-opal-unlocker = callPackage ./tools/security/sed-opal-unlocker { };

  xcompose = callPackage ./data/misc/xcompose { };
}
