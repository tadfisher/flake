{
  programs.texlive = {
    enable = true;
    extraPackages = tpkgs: {
      inherit (tpkgs)
        scheme-medium capt-of fontaxes fontawesome inconsolata moderncv roboto upquote wrapfig;
    };
  };
}
