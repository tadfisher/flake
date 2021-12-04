final: prev:

with final;

{
  emacsPackagesFor = emacs:
    (prev.emacsPackagesFor emacs).overrideScope' (callPackage ./emacs { });

  # TODO https://github.com/NixOS/nixpkgs/pull/147032
  fwupd = prev.fwupd.overrideAttrs (attrs: {
    patches = attrs.patches ++ [
      (fetchpatch {
        url = "https://raw.githubusercontent.com/NixOS/nixpkgs/47f11b5c01e4b9e3e4f2d8c6b20531d77b46b157/pkgs/os-specific/linux/firmware/fwupd/efi-app-path.patch";
        sha256 = "sha256-RSWkKYhXjIZHHyPx9ri6rRMzoisL0EAEEokVlyG8VWE=";
      })
    ];
  });

  nix-direnv = prev.nix-direnv.overrideAttrs (attrs: {
    patches = (attrs.patches or []) ++ [ ./nix-direnv/revert-flakes-dont-run-shellhook.patch ];
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
