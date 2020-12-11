{ pkgs ? import <nixpkgs> { } }:

with pkgs;

{
  sed-opal-unlocker = callPackage ./sed-opal-unlocker { };

  xcompose = callPackage ./xcompose { };
}
