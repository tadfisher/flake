{ pkgs, ... }:

{
  programs.zed-editor = {
    extensions = [
      "adwaita"
      "haskell"
      "nix"
      "typespec"
    ];

    extraPackages = with pkgs; [
      haskell-language-server
      nixd
    ];
  };
}
