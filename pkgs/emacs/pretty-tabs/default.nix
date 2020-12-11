{ lib, trivialBuild, all-the-icons }:

trivialBuild rec {
  pname = "pretty-tabs";
  version = "0.1";

  src = ./.;

  packageRequires = [ all-the-icons ];

  meta = {
    description = "Prettier tabs in tab-bar-mode";
    license = lib.licenses.gpl3;
  };
}
