{ lib
, stdenv
, fetchurl
, buildFHSUserEnv
, runCommand
, bash
, jdk
, ncurses5
# , llvmPackages
, zlib
}:

let
  version = "1.6.21";

  kotlinNative = stdenv.mkDerivation rec {
    pname = "kotlin-native-unwrapped";
    inherit version;

    src = fetchurl {
      url = "https://github.com/JetBrains/kotlin/releases/download/v${version}/kotlin-native-linux-x86_64-${version}.tar.gz";
      hash = "sha256-r1H2riRLsZl5+65tw6/cp7rkJWjWoz8PozHt1mWmEfo=";
    };

    # nativeBuildInputs = [ autoPatchelfHook ];

    # buildInputs = [
    #   jdk
    #   llvmPackages.libclang
    #   ncurses5
    #   stdenv.cc.cc.lib
    #   zlib
    # ];

    # propagatedBuildInputs = [ jdk ];

    installPhase = ''
      cp -r . $out
    '';

    # passthru = {
    #   inherit llvmPackages;
    # };
  };

  fhsEnv = buildFHSUserEnv {
    name = "kotlin-native-fhs-env-${version}";
    targetPkgs = pkgs: with pkgs; [
      jdk
      ncurses5
      stdenv.cc.cc.lib
      zlib
    ];
  };

in
runCommand "kotlin-native-${version}" {
  passthru = {
    unwrapped = kotlinNative;
  };
  meta = with lib; {
    description = "Kotlin/Native compiler";
    license = licenses.asl20;
    platforms = [ "x86_64-linux" ];
  };
} ''

  mkdir -p $out/bin
  for f in ${kotlinNative}/bin/*; do
    name="$(basename $f)"
    cat <<EOF >$out/bin/$name
  #! ${stdenv.shell}
  ${fhsEnv}/bin/kotlin-native-fhs-env-${version} "$f" "\$@"
  EOF
    chmod +x $out/bin/$name
  done
''
