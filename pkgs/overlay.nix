inputs:

final: prev:

with final;

{
  emacsPackagesFor = emacs:
    (prev.emacsPackagesFor emacs).overrideScope' (callPackage ./emacs { inherit inputs; });

  gnome = prev.gnome.overrideScope' (final: prev: {
    mutter = prev.mutter.overrideAttrs (attrs: {
      src = inputs.mutter;
      patches = [];
    });
  });

  mopidy-ytmusic = prev.mopidy-ytmusic.overrideAttrs (attrs: {
    src = inputs.mopidy-ytmusic;

    patches = (prev.patches or []) ++ [
      (fetchpatch {
        url = "https://github.com/OzymandiasTheGreat/mopidy-ytmusic/pull/57.diff";
        hash = "sha256-ysku2UUmzy2ImUnDP904L88UpWsevLri3Y8QdA3HoW8=";
      })
    ];

    postPatch = "";
  });

  openmw = prev.openmw.overrideAttrs (attrs: {
    patches = (attrs.patches or [ ]) ++ [
      (fetchpatch {
        url = "https://gitlab.com/OpenMW/openmw/-/merge_requests/1239.diff";
        hash = "sha256-RhbIGeE6GyqnipisiMTwWjcFnIiR055hUPL8IkjPgZw=";
      })
    ];

    meta = attrs.meta // { broken = false; };
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
    meta = attrs.meta // { broken = false; };
  });

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
