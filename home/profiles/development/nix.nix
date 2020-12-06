{ pkgs, ... }:

{
  home.packages = with pkgs; [
    binutils
    nix-index
    nix-prefetch-github
    nix-prefetch-scripts
    nox
    patchelf
  ];
}
