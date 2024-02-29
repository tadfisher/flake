{ pkgs, ... }:

{
  home.packages = with pkgs; [
    acpica-tools
    binutils
    dmidecode
    eagle
    fwts
    horizon-eda
  ];
}
