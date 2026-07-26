inputs:

final: prev:

with final;

{
  # tree-sitter = prev.tree-sitter.override {
  #   extraGrammars = callPackage ./tree-sitter-grammars { } inputs;
  # };

  emacsPackagesFor =
    emacs:
    (inputs.emacs-overlay.lib.${system}.emacsPackagesFor emacs).overrideScope (
      callPackage ./emacs { inherit inputs; }
    );

  paper-icon-theme = prev.paper-icon-theme.overrideAttrs (attrs: {
    pname = "paper-icon-theme-unstable";
    version = "2020-03-12";
    src = fetchFromGitHub {
      owner = "snwh";
      repo = attrs.pname;
      rev = "aa3e8af7a1f0831a51fd7e638a4acb077a1e5188";
      sha256 = "0x6qzch4rrc8firb1dcf926j93gpqxvd7h6dj5wwczxbvxi5bd77";
    };
    meta = attrs.meta // {
      broken = false;
    };
  });

  pythonPackagesExtensions = prev.pythonPackagesExtensions ++ [
    (python-final: python-prev: {
      # BUG: https://github.com/NixOS/nixpkgs#545346
      # Upstream renamed the cheetah3 PyPI distribution to CT3, so its installed
      # metadata is named "ct3"; pythonMetadataCheckPhase looks up `$pname`
      # ("cheetah3") and fails. Renaming pname to "ct3" resolves the check.
      cheetah3 = python-prev.cheetah3.overrideAttrs (_: {
        pname = "ct3";
      });
    })
  ];

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

  vaultwarden = callPackage ./vaultwarden/package.nix { };
}
