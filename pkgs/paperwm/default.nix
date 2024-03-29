{ lib
, stdenv
, src
}:

let
   uuid = "paperwm@paperwm.github.com";
in
stdenv.mkDerivation {
  pname = "gnome-shell-extension-paperwm";
  version = "unstable";

  inherit src;

  dontConfigure = true;
  dontBuild = true;

  installPhase = ''
    runHook preInstall
    mkdir -p "$out/share/gnome-shell/extensions/${uuid}"
    cp -r . "$out/share/gnome-shell/extensions/${uuid}"
    runHook postInstall
  '';

  meta = with lib; {
    homepage = "https://github.com/paperwm/PaperWM";
    description = "Tiled scrollable window management for Gnome Shell";
    license = licenses.gpl3Plus;
    maintainers = with maintainers; [ hedning AndersonTorres ];
    platforms = platforms.all;
  };

  passthru.extensionUuid = uuid;
}
