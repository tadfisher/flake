{ stdenv
, lib
, src
, runCommand
, jdk19
, gradle
, libclang
}:

let
  llvmHome = runCommand "jextract-llvm-home" { } ''
    mkdir -p $out/lib
    ln -sf ${libclang.lib}/lib/* $out/lib/
    ln -sf ${libclang.libllvm.lib}/lib/* $out/lib/
  '';

in
stdenv.mkDerivation rec {
  pname = "jextract";
  version = "unstable";

  inherit src;

  nativeBuildInputs = [
    gradle
  ];

  buildInputs = [
    llvmHome
  ];

  buildPhase = ''
    gradle -Pjdk19_home=${jdk19.home} -Pllvm_home=${llvmHome} clean verify
  '';

  installPhase = ''
    mkdir -p $out
    cp -r build/jextract/* $out
  '';
}
