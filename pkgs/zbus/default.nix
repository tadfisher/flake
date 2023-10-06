{ lib
, fetchFromGitHub
, rustPlatform
}:

rustPlatform.buildRustPackage rec {
  pname = "zbus_xmlgen";
  version = "3.1.1";

  src = fetchFromGitHub {
    owner = "dbus2";
    repo = "zbus";
    rev = "zbus_xmlgen-${version}";
    sha256 = "sha256-L9PELzIP9mPLsvPks//wRQB0fop/3MoRXFH1BsugSN8=";
  };

  cargoLock = {
    lockFile = ./Cargo.lock;
  };

  postPatch = ''
    cp ${./Cargo.lock} Cargo.lock
  '';

  cargoBuildFlags = [ "-p" "zbus_xmlgen" ];
  cargoTestFlags = [ "-p" "zbus_xmlgen" ];

  meta = with lib; {
    executables = [ "zbus_xmlgen" ];
    description = "generate zbus-based Rust code from D-Bus XML interface descriptions";
    longDescription = ''
      A binary crate that provides a developer tool to generate zbus-based Rust code from D-Bus XML
      interface descriptions. It can be used to generate the code directly from a running D-Bus
      system, session or other service, or using a preexisting XML file for input.
    '';
    license = licenses.mit;
    platforms = platforms.unix;
    maintainers = with maintainers; [ tadfisher ];
  };
}
