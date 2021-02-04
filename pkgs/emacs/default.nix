emacsPackages: with emacsPackages;

{
  base16-plata-theme = callPackage ./base16-plata-theme { };

  company-gnome-shell = callPackage ./gnome-shell-mode/company-gnome-shell.nix { };

  counsel-flymake = callPackage ./counsel-flymake { };

  gnome-shell-mode = callPackage ./gnome-shell-mode { };

  ligature = callPackage ./ligature { };

  org-cv = callPackage ./org-cv { };

  pretty-tabs = callPackage ./pretty-tabs { };
}
