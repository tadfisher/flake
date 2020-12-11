{ lib, trivialBuild, fetchFromGitHub, company, js2-mode, flycheck }:
let
  source = import ./source.nix { inherit fetchFromGitHub; };
in
trivialBuild {
  pname = "gnome-shell-mode";
  inherit (source) version;

  src = "${source.src}/local/gnome-shell-mode";

  packageRequires = [ js2-mode flycheck ];

  postInstall = ''
    install -D gnome-shell-mode@hedning:matrix.org/* -t $out/share/emacs/site-lisp
    install bootstrap.js session.sh $out/share/emacs/site-lisp
  '';

  meta = with lib; {
    description = "Tight integration of emacs with gnome-shell";
    homepage = "https://github.com/paperwm/gnome-shell-mode";
    maintainers = [ maintainers.tadfisher ];
    license = licenses.gpl2;
  };
}
