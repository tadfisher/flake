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

  password-store-otp = prev.password-store-otp.overrideAttrs (attrs: {
    src = inputs.password-store-otp-el;
  });

  pretty-tabs = callPackage ./pretty-tabs { };
}
