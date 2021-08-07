{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.programs.devhelp;

  devdocPath =
    let
      outputs = flatten (map (pkg: pkg.devdoc or [ ]) cfg.packages);
    in
    concatMapStringsSep ":" (output: "${output}/share") outputs;

  wrapper = pkgs.runCommand "devhelp-wrapped"
    {
      nativeBuildInputs = [ pkgs.makeWrapper ];
    } ''
    mkdir -p $out/bin $out/share/applications

    makeWrapper ${pkgs.gnome.devhelp}/bin/devhelp $out/bin/devhelp \
      --prefix XDG_DATA_DIRS : ${devdocPath}

    cat <<EOF >$out/share/applications/org.gnome.Devhelp.desktop
    ${desktopEntry (placeholder "out")}
    EOF
  '';

  desktopEntry = wrapper: generators.toINI { } {
    "Desktop Entry" = {
      Name = "Devhelp";
      Comment = "A developer tool for browsing and searching API documentation";
      GenericName = "API Documentation Browser";
      Keywords = "documentation;information;manual;developer;api;";
      Exec = "${wrapper}/bin/devhelp";
      Terminal = "false";
      Type = "Application";
      Icon = "org.gnome.Devhelp";
      Categories = "GNOME;GTK;Development;";
      StartupNotify = "true";
    };
  };

in
{
  options.programs.devhelp = {
    enable = mkEnableOption "GNOME Devhelp";

    packages = mkOption {
      type = types.listOf types.package;
      description = ''
        List of packages to include in the Devhelp documentation.
        </para><para>
        Packages with a "devdoc" output will be included; this includes
        most packages in the gnome attrset.
      '';
    };
  };

  config = mkIf cfg.enable {
    home.packages = [ wrapper ];
  };
}
