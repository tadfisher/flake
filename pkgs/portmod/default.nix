{ lib
, src
, callPackage
, python3Packages
, fetchFromGitLab
, cacert
, cargo,
 rustc
, rustPlatform
, bubblewrap
, git
, perlPackages
, imagemagick
, fetchurl
, fetchzip
, jre
, makeWrapper
, tr-patcher
, tes3cmd
, openmw
}:

let
  version = "2.5.0";

  bin-programs = [
    bubblewrap
    git
    python3Packages.virtualenv
    tr-patcher
    tes3cmd
    imagemagick
    openmw
  ];

in
python3Packages.buildPythonApplication {
  inherit src version;

  pname = "portmod";

  SETUPTOOLS_SCM_PRETEND_VERSION = version;

  cargoDeps = rustPlatform.fetchCargoTarball {
    inherit src;
    name = "portmod-cargo-deps-${version}";
    hash = "sha256-9lo7xVD0lIV4O8yNWXhcIvb+PusJVZ2msRl8/vMPrhM=";
  };

  nativeBuildInputs = [
    cargo
    rustc
  ] ++ (with python3Packages; [
    setuptools-rust
    setuptools-scm
    setuptools
  ]) ++ (with rustPlatform; [
    cargoSetupHook
  ]);

  propagatedBuildInputs = with python3Packages; [
    requests
    chardet
    colorama
    restrictedpython
    GitPython
    progressbar2
    python-sat
    redbaron
    patool
    packaging
    fasteners
  ];

  doCheck = false;

  checkInputs = with python3Packages; [
    pytestCheckHook
  ] ++ bin-programs;

  preCheck = ''
    export HOME=$(mktemp -d)
  '';

  # some test require network access
  disabledTests = [
    "test_masters_esp"
    "test_logging"
    "test_execute_network_permissions"
    "test_execute_permissions_bleed"
    "test_git"
    "test_sync"
    "test_manifest"
    "test_add_repo"
  ];

  postInstall = ''
    makeWrapperArgs+=("--prefix" "GIT_SSL_CAINFO" ":" "${cacert}/etc/ssl/certs/ca-bundle.crt" \
      "--prefix" "PATH" ":" "${lib.makeBinPath bin-programs }")
  '';

  meta = {
    description = "mod manager for openMW based on portage";
    homepage = "https://gitlab.com/portmod/portmod";
    license = lib.licenses.gpl3;
    maintainers = with lib.maintainers; [ marius851000 ];
  };
}
