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

  # BUG: https://nixpk.gs/pr-tracker.html?pr=572951
  typescript-language-server = prev.typescript-language-server.overrideAttrs (attrs: {
    pnpmDeps = attrs.pnpmDeps.overrideAttrs (_: {
      outputHash = "sha256-xR/EF60lVBsITKgH7wCM2wHM46PFQevD51KfCENSUH4=";
    });
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
