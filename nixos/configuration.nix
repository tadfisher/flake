# this file is an impure recreation of the flake profile currently deployed
# based on the systems hostname. The purpose is so tools which do not yet have
# flake support (e.g `nixos-option`), can work as expected.
{ lib, ... }:
let
  inherit (builtins) attrNames readDir;

  hostname = lib.fileContents /etc/hostname;
  host = "${toString ./.}/nixos/hosts/${hostname}/default.nix";
  config = lib.optional (builtins.pathExists host) host;
in {
  imports = (import ./modules/list.nix) ++ [
    "${
      builtins.fetchTarball
      "https://github.com/nixos-community/home-manager/archive/master.tar.gz"
    }/nixos"
    /etc/nixos/profiles/core.nix
  ] ++ config;

  networking.hostName = hostname;
  nix.nixPath = [
    "nixpkgs=${<nixpkgs>}"
    "nixos-config=${toString ./.}/configuration.nix"
    # "nixpkgs-overlays=${toString ../.}/overlays"
  ];

  nixpkgs.overlays = [ (import ../pkgs) ];
}
