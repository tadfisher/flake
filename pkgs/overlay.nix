final: prev:

with final;

{
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
}
