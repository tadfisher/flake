{ lib, stdenv, fetchFromGitHub, fetchpatch, src }:

stdenv.mkDerivation rec {
  pname = "paperwm-unstable";
  version = "40.0";

  inherit src;

  patches = [
    (fetchpatch {
      url = "https://github.com/tadfisher/PaperWM/commit/1c2fabc3c738bbd78d22c3c878ed6919fee9b723.patch";
      sha256 = "sha256-BoG9aIWcgbSUZZ5AI+xlWyogy+OrgiHrqEJnv0eElTA=";
    })
  ];


  uuid = "paperwm@hedning:matrix.org";

  dontBuild = true;

  installPhase = ''
    runHook preInstall
    mkdir -p $out/share/gnome-shell/extensions/${uuid}
    cp -r . $out/share/gnome-shell/extensions/${uuid}
    runHook postInstall
  '';

  meta = with lib; {
    description = "Tiled scrollable window management for Gnome Shell";
    homepage = "https://github.com/paperwm/PaperWM";
    license = licenses.gpl3;
    maintainers = with maintainers; [ hedning ];
  };
}
