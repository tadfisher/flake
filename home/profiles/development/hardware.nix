{ pkgs, ... }:

{
  home.packages = with pkgs; [
    acpica-tools
    binutils
    dmidecode
    eagle
    fwts
    horizon-eda
    # TODO https://github.com/NixOS/nixpkgs/pull/165630
    # kicad
    librepcb
    openscad
  ];
}
