inputs:

final: prev:

with final;

{
  emacsPackagesFor = emacs:
    (prev.emacsPackagesFor emacs).overrideScope' (callPackage ./emacs { inherit inputs; });

  gnome = prev.gnome.overrideScope' (final: prev: {
    mutter = prev.mutter.overrideAttrs (attrs: {
      patches = attrs.patches ++ [
        # Triple-buffer patch
        (fetchpatch {
          url = "https://gitlab.gnome.org/GNOME/mutter/-/merge_requests/1441.diff";
          hash = "sha256-0TABBYRbrrnruNZqTvZVzI7zGNnbB+J/QB58i3hYLIQ=";
        })
      ];
    });
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

  # TODO https://github.com/NixOS/nixpkgs/pull/156305
  spice-gtk = prev.spice-gtk.overrideAttrs (attrs: {
    postPatch = attrs.postPatch + ''
      # https://gitlab.freedesktop.org/spice/spice-common/-/issues/5
      substituteInPlace subprojects/spice-common/meson.build \
        --replace \
        "cmd = run_command(python, '-m', module)" \
        "cmd = run_command(python, '-c', 'import @0@'.format(module))"
    '';
  });
}
