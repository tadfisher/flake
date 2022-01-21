inputs:

final: prev:

with final;

{
  # TODO https://github.com/NixOS/nixpkgs/commit/f0ed9541586dbc2209501b9cdce876c703336e88
  amdvlk = prev.amdvlk.overrideAttrs (attrs: {
    src = fetchRepoProject {
      name = "${attrs.pname}-src";
      manifest = "https://github.com/GPUOpen-Drivers/AMDVLK.git";
      rev = "refs/tags/v-${attrs.version}";
      sha256 = "M+58gJjP33yOuq6RYN73HG7wACPaYRz7WFC/AFFGMzw=";
    };
  });

  # TODO https://github.com/NixOS/nixpkgs/issues/154297
  notmuch = prev.notmuch.overrideAttrs (attrs: {
    patches = [
      (writeText "test-fix-support-for-gpgsm-in-gnupg-2.3.patch" ''
        From a642ad542e3d3f34e949c5c66923ca8a6e6cbbd8 Mon Sep 17 00:00:00 2001
        From: Stig Palmquist <stig@stig.io>
        Date: Tue, 11 Jan 2022 13:23:13 +0100
        Subject: [PATCH] test: fix support for gpgsm in gnupg 2.3

        gpgsm --list-keys output changed the label for fingerprints from
        "fingerprint: " to "sha[12] fpr: " breaking tests with gnupg 2.3. this
        adds support for both.
        ---
         test/test-lib.sh | 2 +-
         1 file changed, 1 insertion(+), 1 deletion(-)

        diff --git a/test/test-lib.sh b/test/test-lib.sh
        index 6bc0b723..3de608f9 100644
        --- a/test/test-lib.sh
        +++ b/test/test-lib.sh
        @@ -145,7 +145,7 @@ add_gpgsm_home () {
             mkdir -p -m 0700 "$GNUPGHOME"
             gpgsm --batch --no-tty --no-common-certs-import --pinentry-mode=loopback --passphrase-fd 3 \
         	  --disable-dirmngr --import  >"$GNUPGHOME"/import.log 2>&1 3<<<''' <$NOTMUCH_SRCDIR/test/smime/0xE0972A47.p12
        -    fpr=$(gpgsm --batch --list-key test_suite@notmuchmail.org | sed -n 's/.*fingerprint: //p')
        +    fpr=$(gpgsm --batch --list-key test_suite@notmuchmail.org | sed -n 's/.*\(fingerprint\|sha1 fpr\): //p')
             echo "$fpr S relax" >> "$GNUPGHOME/trustlist.txt"
             gpgsm --quiet --batch --no-tty --no-common-certs-import --disable-dirmngr --import < $NOTMUCH_SRCDIR/test/smime/ca.crt
             echo "4D:E0:FF:63:C0:E9:EC:01:29:11:C8:7A:EE:DA:3A:9A:7F:6E:C1:0D S" >> "$GNUPGHOME/trustlist.txt"
        --
        2.34.1
      '')
    ];
  });

  emacsPackagesFor = emacs:
    (prev.emacsPackagesFor emacs).overrideScope' (callPackage ./emacs { inherit inputs; });

  nix-direnv = prev.nix-direnv.overrideAttrs (attrs: {
    patches = (attrs.patches or [ ]) ++ [ ./nix-direnv/revert-flakes-dont-run-shellhook.patch ];
  });

  paper-icon-theme = prev.paper-icon-theme.overrideAttrs (attrs: rec {
    pname = "paper-icon-theme-unstable";
    version = "2020-03-12";
    src = fetchFromGitHub {
      owner = "snwh";
      repo = attrs.pname;
      rev = "aa3e8af7a1f0831a51fd7e638a4acb077a1e5188";
      sha256 = "0x6qzch4rrc8firb1dcf926j93gpqxvd7h6dj5wwczxbvxi5bd77";
    };
  });

  plata-theme = prev.plata-theme.override { gtkNextSupport = true; };

  sedutil-fork = prev.sedutil.overrideAttrs (attrs: rec {
    version = "1.15-5ad84d8";

    src = fetchFromGitHub {
      owner = "ChubbyAnt";
      repo = "sedutil";
      rev = version;
      sha256 = "sha256-JvM52KLiKeF8ui85+9PnCmWgBR4qyybEjtgxRLk8PjA=";
    };

    meta = attrs.meta // {
      homepage = "https://sedutil.com";
      maintainers = with lib.maintainers; [ tadfisher ];
    };
  });
}
