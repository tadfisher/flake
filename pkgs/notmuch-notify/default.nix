{ lib
, src
, rustPlatform
, notmuch
}:

rustPlatform.buildRustPackage {
  pname = "notmuch-notify";
  version = "0.1.0";

  inherit src;

  buildInputs = [ notmuch ];

  cargoLock = {
    lockFileContents = builtins.readFile ./Cargo.lock;
  };

  postPatch = ''
    cp ${./Cargo.lock} Cargo.lock
  '';

  meta = with lib; {
    description = "Configurable new mail notifications for notmuch mail.";
    homepage = "https://git.celti.name/Celti/notmuch-notify";
    license = with licenses; [ asl20 mit ];
    platforms = platforms.unix;
    maintainers = with maintainers; [ tadfisher ];
  };
}
