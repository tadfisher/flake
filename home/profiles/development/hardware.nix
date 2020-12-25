{ pkgs, ... }:

{
  home.packages = with pkgs; [
    acpica-tools
    binutils
    dmidecode
    eagle
    freecad
    fwts
    horizon-eda
    kicad
    librepcb
    openscad
  ];
}
