{ lib
, stdenv
, fetchurl
, coreutils
, libarchive
, python3
, file
, which
}:

stdenv.mkDerivation rec {
  pname = "zephyr-toolchain";
  version = "0.11.4";

  src = fetchurl {
    url = "https://github.com/zephyrproject-rtos/sdk-ng/releases/download/v${version}/zephyr-toolchain-arm-${version}-setup.run";
    hash = "sha256-TR6gfIBrAD138H+rmqVCq9jBy2A7JWjkTwaX2W79IbM=";
  };

  nativeBuildInputs = [
    file
    libarchive
    python3
    which
  ];

  unpackCmd = "mkdir src; install $curSrc src/setup.run";

  dontBuild = true;

  installPhase = ''
    tail +499l setup.run | gzip -cd | tar -x
    patchShebangs setup.sh
    ./setup.sh -d $out -y -norc
  '';

  meta = with lib; {
    description = "Zephyr embedded RTOS toolchain";
    homepage = "https://www.zephyrproject.org/";
    license = licenses.asl20;
    maintainers = [ maintainers.tadfisher ];
    platforms = platforms.x86_64;
  };
}
