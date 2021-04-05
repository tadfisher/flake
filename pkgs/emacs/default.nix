{ fetchgit }:

final: prev:

with final;

{
  auctex = prev.auctex.override {
    elpaBuild = args: final.elpaBuild (args // {
      version = "13.0.6";
      src = fetchgit {
        url = "https://git.savannah.gnu.org/git/emacs/elpa.git";
        rev = "83403e9f98924a07f0d4f27032ea024a28dfc86f";
        sha256 = "08iimsqd52idvk4k5yxfql3pk7xvmdql99kkvzvajhsdx4mpzfjk";
      };
    });
  };

  base16-plata-theme = callPackage ./base16-plata-theme { };

  company-gnome-shell = callPackage ./gnome-shell-mode/company-gnome-shell.nix { };

  counsel-flymake = callPackage ./counsel-flymake { };

  gnome-shell-mode = callPackage ./gnome-shell-mode { };

  ligature = callPackage ./ligature { };

  org-cv = callPackage ./org-cv { };

  pretty-tabs = callPackage ./pretty-tabs { };
}
