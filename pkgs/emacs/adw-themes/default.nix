{ lib, trivialBuild, doom-themes }:

trivialBuild rec {
  pname = "adw-themes";
  version = "0.1";

  src = ./.;

  packageRequires = [ doom-themes ];

  meta = {
    description = "Adwaita themes for Emacs";
    license = lib.licenses.gpl3;
  };
}
