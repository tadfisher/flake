{ lib, trivialBuild, base16-theme, pretty-tabs }:

trivialBuild rec {
  pname = "base16-plata-theme";
  version = "0.1";

  src = ./.;

  packageRequires = [ base16-theme ];

  meta = {
    description = "Plata themes for Emacs";
    license = lib.licenses.gpl3;
  };
}
