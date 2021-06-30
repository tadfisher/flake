{ lib
, stdenv
, naersk-lib
, src
, bubblewrap
, cacert
, git
, imagemagick
, python3Packages
, tr-patcher
, tes3cmd
}:

let
  portmod-rust = naersk-lib.buildPackage {
    inherit src;
    buildInputs = [ python3Packages.python ];
    copyLibs = true;
  };

  bin-program = [
    bubblewrap
    git
    python3Packages.virtualenv
    tr-patcher
    tes3cmd
    imagemagick
  ];

in
python3Packages.buildPythonApplication rec {
  inherit src;
  pname = "portmod";

  version = "git"; # TODO: dynamically find the version

  prePatch = ''
    substituteInPlace setup.py \
      --replace "from setuptools_rust import Binding, RustExtension" "" \
      --replace "RustExtension(\"portmodlib.portmod\", binding=Binding.PyO3, strip=True)" ""
  '';

  SETUPTOOLS_SCM_PRETEND_VERSION = version;

  propagatedBuildInputs = with python3Packages; [
    GitPython
    appdirs
    chardet
    colorama
    fasteners
    packaging
    patool
    progressbar2
    python-sat
    redbaron
    requests
    restrictedpython
    setuptools
    setuptools_scm
  ];

  nativeBuildInputs = bin-program ++ (with python3Packages; [ pytest black ]);

  doCheck = false; # Tests require network access

  postInstall = ''
    cp ${portmod-rust}/lib/libportmod.so $(echo $out/lib/python*/*/portmod)/portmod.so
    for script in $out/bin/*
    do
      wrapProgram $script \
        --prefix PATH : ${lib.makeBinPath bin-program} \
        --prefix GIT_SSL_CAINFO : ${cacert}/etc/ssl/certs/ca-bundle.crt
    done
  '';

  shellHook = ''
    cp ${portmod-rust}/lib/libportmod.so portmod/portmod.so
    chmod +w portmod/portmod.so
  '';
}
