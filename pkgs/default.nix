{ pkgs ? import <nixpkgs> {},
  super ? import <nixpkgs> {}
}:

with pkgs;

rec {
  sed-opal-unlocker = callPackage ./tools/security/sed-opal-unlocker { };
}
