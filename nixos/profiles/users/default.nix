{ config, lib, pkgs, ... }:

with lib;

{
  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
  };
}
