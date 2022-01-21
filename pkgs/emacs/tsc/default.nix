{ lib
, rustPlatform
, src
, version
, meta
}:

rustPlatform.buildRustPackage {
  pname = "emacs-tsc-native";
  inherit version src;

  cargoLock = {
    lockFile = "${src}/Cargo.lock";
    outputHashes = {
      "tree-sitter-0.20.0" = "sha256-hGiJZFrQpO+xHXosbEKV2k64e2D8auNGEtdrFk2SsOU=";
    };
  };

  meta = meta // {
    platforms = lib.platforms.unix;
  };
}
