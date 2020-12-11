emacsPackages: with emacsPackages;

{
  base16-plata-theme = callPackage ./base16-plata-theme { };

  company-gnome-shell = callPackage ./gnome-shell-mode/company-gnome-shell.nix { };

  gnome-shell-mode = callPackage ./gnome-shell-mode { };

  ligature = callPackage ./ligature { };

  org-cv = callPackage ./org-cv { };

  pretty-tabs = callPackage ./pretty-tabs { };
}
