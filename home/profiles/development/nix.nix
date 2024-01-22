{ pkgs, ... }:

{
  home.packages = with pkgs; [
    binutils
    # TODO broken: nix-prefetch-github
    # TODO broken: nix-prefetch-scripts
    nox
    patchelf
  ];
}
