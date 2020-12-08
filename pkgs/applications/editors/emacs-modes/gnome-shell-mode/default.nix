{ lib, trivialBuild, fetchFromGitHub, company, js2-mode, flycheck }:
let
  version = "2020-03-15";

  source = fetchFromGitHub {
    owner = "paperwm";
    repo = "gnome-shell-mode";
    rev = "a780fced2487188759b7b320cbf8884d4dcebe93";
    sha256 = "0gqr9asajwd9fkpfsqr2l7fk124nzjdn5ql8li8h19wack87cc1j";
  };

in
rec {
  gnome-shell-mode = trivialBuild {
    pname = "gnome-shell-mode";
    inherit version;

    src = "${source}/local/gnome-shell-mode";

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
  };

  company-gnome-shell = trivialBuild {
    pname = "company-gnome-shell";
    inherit version;

    src = "${source}/local/company-gnome-shell";

    packageRequires = [ company gnome-shell-mode ];

    meta = with lib; {
      description = "Gnome Shell runtime js completion";
      homepage = "https://github.com/paperwm/gnome-shell-mode";
      maintainers = [ maintainers.tadfisher ];
      license = licenses.gpl2;
    };
  };
}
