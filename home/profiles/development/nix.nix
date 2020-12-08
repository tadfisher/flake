{ pkgs, ... }:

{
  home.packages = with pkgs; [
    binutils
    nix-index
    # TODO broken: nix-prefetch-github
    # TODO broken: nix-prefetch-scripts
    nox
    patchelf
  ];
}
