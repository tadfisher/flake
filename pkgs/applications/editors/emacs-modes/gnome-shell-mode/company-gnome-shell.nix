{ lib, trivialBuild, fetchFromGitHub, company, gnome-shell-mode }:
let
  source = import ./source.nix { inherit fetchFromGitHub; };

in
trivialBuild {
  pname = "company-gnome-shell";
  inherit (source) version;

  src = "${source.src}/local/company-gnome-shell";

  packageRequires = [ company gnome-shell-mode ];

  meta = with lib; {
    description = "Gnome Shell runtime js completion";
    homepage = "https://github.com/paperwm/gnome-shell-mode";
    maintainers = [ maintainers.tadfisher ];
    license = licenses.gpl2;
  };
}
