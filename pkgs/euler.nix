final: prev:

with final;

{
  haskell-language-server = prev.haskell-language-server.override {
    suppertedGhcVersions = [ "865" "883" "884" "8103" ];
  };
}
