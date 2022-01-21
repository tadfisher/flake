{ lib
, stdenv
, src
, trivialBuild
}:

trivialBuild {
  pname = "ligature";
  version = "unstable";

  inherit src;

  meta = with lib; {
    description = "Typographic ligatures in Emacs";
    homepage = "https://github.com/mickeynp/ligature.el";
    license = licenses.gpl3;
    maintainers = [ maintainers.tadfisher ];
  };
}
