{ ... }:

{
  programs = {
    emacs-lsp = import ./programs/emacs-lsp.nix;
    firefox = import ./programs/firefox.nix;
    gnome-shell = import ./programs/gnome-shell.nix;
    pass = import ./programs/pass.nix;
  };

  services = {
    gnirehtet = import ./services/gnirehtet.nix;
    mopidy = import ./services/mopidy.nix;
  };
}
