{ pkgs, ... }:

{
  home.packages = with pkgs; [
    acpica-tools
    binutils-unwrapped
    dmidecode
    eagle
    fwts
    horizon-eda
    kicad
    librepcb
    openscad
  ];
}
