{ lib
, stdenv
}:

stdenv.mkDerivation rec {
  pname = "gnome-shell-cleaner-overview";
  version = "2";

  uuid = "overview_cleaner@gonza.com";

  dontBuild = true;

  src = ./.;

  installPhase = ''
    runHook preInstall
    mkdir -p $out/share/gnome-shell/extensions/${uuid}
    cp metadata.json extension.js $out/share/gnome-shell/extensions/${uuid}
    runHook postInstall
  '';

  meta = with lib; {
    description = "Makes all the windows in the overview the same height and orders them by last recent used";
    license = licenses.gpl3Plus;
    maintainers = with maintainers; [ tadfisher ];
    homepage = "https://github.com/gonzaarcr/touchpad-window-switcher-gnome-ext/blob/master/touchpad_window_switcher%40gonza.com/overviewCleaner.js";
  };
}
