{ inputs
, lib
, fetchgit
, rustPlatform
}:

final: prev:

with final;

{
  base16-plata-theme = callPackage ./base16-plata-theme { };

  company-gnome-shell = callPackage ./gnome-shell-mode/company-gnome-shell.nix { };

  counsel-flymake = callPackage ./counsel-flymake { };

  gnome-shell-mode = callPackage ./gnome-shell-mode { };

  ligature = callPackage ./ligature { src = inputs.ligature-el; };

  org-cv = callPackage ./org-cv { };

  pretty-tabs = callPackage ./pretty-tabs { };

  # yaml = prev.yaml.overrideAttrs (attrs: rec {
  #   # native-comp hangs for this package.
  #   postInstall = "";
  # });
}
