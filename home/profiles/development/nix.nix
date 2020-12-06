{ pkgs, ... }:

{
  home.pkgs = with pkgs; [
    binutils
    nix-index
    nix-prefetch-github
    nix-prefetch-scripts
    nox
    patchelf
  ];
}
