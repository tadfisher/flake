{ lib
, stdenv
, src
, trivialBuild
}:

trivialBuild {
  pname = "eglot-booster";
  version = "unstable";

  inherit src;

  meta = with lib; {
    description = "Boost eglot using lsp-booster";
    homepage = "https://github.com/jdtsmith/eglot-booster";
    license = licenses.gpl3;
    maintainers = [ maintainers.tadfisher ];
  };
}
