{ lib
, stdenv
, src
}:

stdenv.mkDerivation rec {
  pname = "gnome-shell-vertical-overview";
  version = "5";

  inherit src;

  uuid = "vertical-overview@RensAlthuis.github.com";

  dontBuild = true;

  installPhase = ''
    runHook preInstall
    mkdir -p $out/share/gnome-shell/extensions/${uuid}
    cp -r . $out/share/gnome-shell/extensions/${uuid}
    runHook postInstall
  '';

  meta = with lib; {
    description = "Bringing back vertically stacked workspaces";
    license = licenses.gpl3Plus;
    maintainers = with maintainers; [ tadfisher ];
    homepage = "https://github.com/RensAlthuis/vertical-overview";
  };
}
