{ inputs
, lib
, fetchgit
, rustPlatform
}:

final: prev:

with final;

{
  adw-themes = callPackage ./adw-themes { };

  base16-plata-theme = callPackage ./base16-plata-theme { };

  ligature = callPackage ./ligature { src = inputs.ligature-el; };

  org-cv = callPackage ./org-cv { };

  pretty-tabs = callPackage ./pretty-tabs { };
}
