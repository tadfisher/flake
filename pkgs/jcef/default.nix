{ lib
, stdenv
, fetchFromGitHub
, ant
, clang-tools
, cmake
, coreutils
, git
, gtk3
, jdk
, cefDist
, ninja
, python
, rsync
}:

let
  cmakeProjectArch = if stdenv.hostPlatform.isx86_64 then "x86_64"
    else if stdenv.hostPlatform.isx86_32 then "x86"
    else if stdenv.isAarch64 then "arm64"
    else if stdenv.isAarch32 then "arm/v6"
    else throw "Unsupported platform: ${stdenv.system}";

  cefTarget = if stdenv.hostPlatform.isx86_64 then "linux64"
    else if stdenv.hostPlatform.isx86_32 then "linux32"
    else if stdenv.isAarch64 then "linuxarm64"
    else if stdenv.isAarch32 then "linuxarm"
    else throw "Unsupported platform: ${stdenv.system}";
in
stdenv.mkDerivation {
  pname = "jcef";
  version = "unstable";

  src = fetchFromGitHub {
    owner = "jetbrains";
    repo = "jcef";
    rev = "4ef9139d8abd01b72e29ae33c125d937cd942416";
    sha256 = "yXjI5MlvVPxmojGvRm9bsNPX1FmJidHZkhuTo/s1q+U=";
    leaveDotGit = true;
  };

  patches = [ ./clang-format.patch ];

  nativeBuildInputs = [
    ant
    clang-tools
    cmake
    jdk
    git
    ninja
    python
    rsync
  ];

  buildInputs = [
    jdk
    gtk3
  ];

  cmakeFlags = [
    "-DCEF_DONT_DOWNLOAD=true"
    "-DCEF_VERSION=${cefDist.version}"
    "-DCMAKE_BUILD_TYPE=Release"
    "-DPROJECT_ARCH=${cmakeProjectArch}"
  ];
  dontUseCmakeBuildDir = true;
  cmakeDir = "..";

  ANT_HOME = "${ant}";
  JDK_11 = jdk.home;
  PYTHON_EXECUTABLE = "${python}/bin/python";
  PATCHED_LIBCEF_DIR = "${cefDist}/Release";
  TARGET_ARCH = cmakeProjectArch;

  postUnpack = ''
    ln -sf ${cefDist} $sourceRoot/third_party/cef/cef_binary_${cefDist.version}_${cefTarget}_minimal
  '';

  postPatch = ''
    substituteInPlace tools/clang_util.py \
      --subst-var-by clangFormat ${clang-tools}/bin/clang-format

    patchShebangs .
  '';

  preConfigure = ''
    mkdir -p jcef_build
    cd jcef_build
  '';

  postBuild = ''
    ../jb/tools/linux/build_java.sh
    ../jb/tools/linux/create_bundle.sh
  '';

  installPhase = ''
    mkdir -p $out
    cp -a ../binary_distrib/linux64/* $out
    wrapProgram $out/run.sh \
      --set PATH "${lib.makeBinPath [ jdk coreutils ]}"
  '';

  meta = with lib; {
    description = "Simple framework for embedding Chromium-based browsers into Java-based applications";
    homepage = "https://github.com/JetBrains/jcef";
    maintainers = with maintainers; [ tadfisher ];
    license = licenses.bsd3;
    platforms = cefDist.meta.platforms;
  };
}
