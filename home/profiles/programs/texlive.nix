{
  programs.texlive = {
    enable = true;
    extraPackages = tpkgs: {
      inherit (tpkgs)
        scheme-medium capt-of fontawesome inconsolata moderncv upquote wrapfig;
    };
  };
}
