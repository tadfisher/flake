{ callPackage, ... }:

{
  base16-plata-theme = callPackage ./applications/editors/emacs-modes/base16-plata-theme { };

  gnome-shell-mode = callPackage ./applications/editors/emacs-modes/gnome-shell-mode { };

  ligature = callPackage ./applications/editors/emacs-modes/ligature { };

  org-cv = callPackage ./applications/editors/emacs-modes/org-cv { };

  pretty-tabs = callPackage ./applications/editors/emacs-modes/pretty-tabs { };
}
