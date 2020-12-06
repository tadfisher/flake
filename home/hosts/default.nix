{ self
, android-nixpkgs
, emacs-overlay
, home-manager
, nixpkgs
, nur
, ...
}@inputs:
let
  config = hostname: system:
    let
      modules = pkgs:
        let
          global = {
            nixpkgs.overlays = [
              self.overlay
              emacs-overlay.overlay
              nur.overlay
            ];
          };

          local = ./. + "/${hostname}";

          programs = builtins.attrValues self.hmModules.programs;

          services = builtins.attrValues self.hmModules.services;

        in
        [
          android-nixpkgs.hmModules.android-sdk
          # pkgs.nur.repos.rycee.hmModules.emacs-init
          global
          local
        ]
        ++ programs
        ++ services;

      configuration = { pkgs, ... }: { imports = modules pkgs; };

    in
    home-manager.lib.homeManagerConfiguration {
      inherit configuration system;
      username = "tad";
      homeDirectory = "/home/tad";
    };

in
{ dirac = config "dirac" "x86_64-linux"; }
