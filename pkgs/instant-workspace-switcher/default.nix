{ lib
, stdenv
, src
}:

stdenv.mkDerivation rec {
  pname = "gnome-shell-extension-instant-workspace-switcher";
  version = "1";

  inherit src;

  uuid = "instantworkspaceswitcher@amalantony.net";

  dontBuild = true;

  installPhase = ''
    runHook preInstall
    mkdir -p $out/share/gnome-shell/extensions/${uuid}
    cp -r ${uuid}/* $out/share/gnome-shell/extensions/${uuid}
    runHook postInstall
  '';

  meta = with lib; {
    description = "Disable the workspace switch animation";
    license = licenses.gpl2Plus;
    maintainers = with maintainers; [ tadfisher ];
    homepage = "https://github.com/amalantony/gnome-shell-extension-instant-workspace-switcher";
  };
}
