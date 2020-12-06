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
          local = import "${toString ./.}/${hostname}" inputs;
          programs = builtins.attrValues self.hmModules.programs;
          services = builtins.attrValues self.hmModules.services;
        in
        nixpkgs.lib.flatten [
          android-nixpkgs.hmModules.android-sdk
          pkgs.nur.repos.rycee.hmModules.emacs-init
          programs
          services
        ];

      configuration = { pkgs, ... }: { imports = modules pkgs; };

    in
    home-manager.lib.homeManagerConfiguration {
      inherit configuration system;
      username = "tad";
      homeDirectory = "/home/tad";
    };

in
{ dirac = config "dirac" "x86_64-linux"; }
