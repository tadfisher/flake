inputs:

final: prev:

with final;

{
  emacsPackagesFor = emacs:
    (inputs.emacs-overlay.lib.${system}.emacsPackagesFor emacs).overrideScope' (callPackage ./emacs { inherit inputs; });

  # jetbrains-mono = stdenv.mkDerivation rec {
  #   pname = "JetBrainsMono";
  #   version = "2.304";

  #   src = fetchFromGitHub {
  #     owner = "JetBrains";
  #     repo = "JetBrainsMono";
  #     rev = "v${version}";
  #     hash = "sha256-SW9d5yVud2BWUJpDOlqYn1E1cqicIHdSZjbXjqOAQGw=";
  #   };

  #   dontConfigure = true;
  #   dontBuild = true;
  #   dontCheck = true;

  #   installPhase = ''
  #     install -m444 -Dt $out/share/fonts/truetype/JetBrainsMono fonts/ttf/*.ttf
  #     # for f in $out/share/fonts/variable/JetBrainsMono/*; do
  #     #  echo "$f -> ''${f//\[*\]/-VF}"
  #     #  mv "$f" "''${f//\[*\]/-VF}"
  #     #done
  #   '';

  #   inherit (prev.jetbrains-mono) meta;
  # };

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

  systemd-patched = prev.systemd.overrideAttrs (attrs: rec {
    patches = attrs.patches ++ [./systemd/query-single-label-on-routing-domain.patch];
  });
}
