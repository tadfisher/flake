{ pkgs, ... }:

{
  home.packages = with pkgs; [
    acpica-tools
    binutils
    dmidecode
    fwts
    horizon-eda
  ];
}
