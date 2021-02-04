{ lib, trivialBuild, counsel }:

trivialBuild rec {
  pname = "counsel-flymake";
  version = "0.1";

  src = ./.;

  packageRequires = [ counsel ];

  meta = {
    description = "Flymake integration for Counsel/Ivy";
    license = lib.licenses.gpl3;
  };
}
