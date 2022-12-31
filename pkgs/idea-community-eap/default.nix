{ lib
, stdenv
, nixpkgs
, callPackage
, fetchurl
, jdk
, zlib
, maven
, vmopts ? null
}:

with lib;

let
  platforms = lib.platforms.linux ++ [ "x86_64-darwin" "aarch64-darwin" ];
  ideaPlatforms = [ "x86_64-darwin" "i686-darwin" "i686-linux" "x86_64-linux" "aarch64-darwin" ];

  inherit (stdenv.hostPlatform) system;

  versions = builtins.fromJSON (readFile (./versions.json));
  versionKey = if stdenv.isLinux then "linux" else system;
  products = versions.${versionKey} or (throw "Unsupported system: ${system}");

  package = nixpkgs + "/pkgs/applications/editors/jetbrains/" + (if stdenv.isDarwin then "darwin.nix" else "linux.nix");
  mkJetBrainsProduct = callPackage package { inherit vmopts; };

  buildIdea = { pname, version, src, license, description, wmClass, product, ... }:
    (mkJetBrainsProduct {
      inherit pname version src wmClass jdk product;
      productShort = "IDEA";
      extraLdPath = [ zlib ];
      extraWrapperArgs = [
        ''--set M2_HOME "${maven}/maven"''
        ''--set M2 "${maven}/maven/bin"''
      ];
      meta = with lib; {
        homepage = "https://www.jetbrains.com/idea/";
        inherit description license;
        longDescription = ''
          IDE for Java SE, Groovy & Scala development Powerful
          environment for building Google Android apps Integration
          with JUnit, TestNG, popular SCMs, Ant & Maven. Also known
          as IntelliJ.
        '';
        maintainers = with maintainers; [ edwtjo gytis-ivaskevicius steinybot AnatolyPopov ];
        platforms = ideaPlatforms;
      };
    });

in
{
  idea-community = buildIdea rec {
    pname = "idea-community";
    product = "IntelliJ IDEA CE";
    version = products.idea-community.version;
    description = "Integrated Development Environment (IDE) by Jetbrains, community edition";
    license = lib.licenses.asl20;
    src = fetchurl {
      url = products.idea-community.url;
      sha256 = products.idea-community.sha256;
    };
    wmClass = "jetbrains-idea-ce";
    update-channel = products.idea-community.update-channel;
  };

  idea-community-eap = buildIdea rec {
    pname = "idea-community-eap";
    product = "IntelliJ IDEA CE (EAP)";
    version = products.idea-community-eap.version;
    description = "Integrated Development Environment (IDE) by Jetbrains, community edition (Early-Access Preview)";
    license = lib.licenses.asl20;
    src = fetchurl {
      url = products.idea-community-eap.url;
      sha256 = products.idea-community-eap.sha256;
    };
    wmClass = "jetbrains-idea-ce";
    update-channel = products.idea-community-eap.update-channel;
  };
}
