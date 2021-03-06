final: prev:

with final;

{
  gegl_0_4 = prev.gegl_0_4.overrideAttrs (attrs: rec {
    version = "0.4.28";
    src = fetchurl {
      url = "https://download.gimp.org/pub/gegl/${lib.versions.majorMinor version}/${attrs.pname}-${version}.tar.xz";
      sha256 = "sha256-HRENhXfVTMo7NCOTFb03xXzLJ91DVWVQdKLSs/2JeQA=";
    };
    patches = [];
    buildInputs = attrs.buildInputs ++ [ maxflow ];
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

  # TODO Waiting on https://github.com/NixOS/nixpkgs/pull/101093
  plata-theme = prev.plata-theme.overrideAttrs (attrs: rec {
    version = "0.9.9";
    src = fetchFromGitLab {
      owner = "tista500";
      repo = "plata-theme";
      rev = version;
      sha256 = "1iwvlv9qcrjyfbzab00vjqafmp3vdybz1hi02r6lwbgvwyfyrifk";
    };
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

  steamPackages =
    let
      buildFHSUserEnvBubblewrap = final.callPackage ../builders/build-fhs-userenv-bubblewrap { };
      callPackage = newScope prev.steamPackages;
    in
    rec {
      inherit (prev.steamPackages)
        steamArch
        steam-runtime
        steam-runtime-wrapped
        steam
        steam-fonts
        steamcmd;
      steam-fhsenv = callPackage ./steam/fhsenv.nix {
        inherit (prev.steamPackages) steam-runtime-wrapped;
        glxinfo-i686 = pkgsi686Linux.glxinfo;
        steam-runtime-wrapped-i686 =
          if prev.steamPackages.steamArch == "amd64"
          then prev.pkgsi686Linux.steamPackages.steam-runtime-wrapped
          else null;
        buildFHSUserEnv = buildFHSUserEnvBubblewrap;
      };
    };
}
