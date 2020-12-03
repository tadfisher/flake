{ self
, emacs-overlay
, flake-utils
, home-manager
, nixos-hardware
, nixpkgs
, nur
, ...
}@inputs:

let
  inherit (self) lib packages;

  config = hostname: system:
    lib.nixosSystem {
      inherit system;

      modules =
        let
          home = home-manager.nixosModules.home-manager;

          global = {
            networking.hostname = hostname;

            nix = {
              nixPath = [ "nixpkgs=${inputs.nixpkgs}" ];

              registry = {
                emacs-overlay.flake = inputs.emacs-overlay;
                flake-utils.flake = inputs.flake-utils;
                nixos-hardware.flake = inputs.nixos-hardware;
                nixpkgs.flake = inputs.nixpkgs;
                nur.flake = inputs.nur;
                self.flake = inputs.self;
              };
            };

            system.configurationRevision = lib.mkIf (self ? rev) self.rev;
          };

          local = import "${toString ./.}/${hostname}";

          flake = lib.attrValues self.nixosModules;

        in [
          home
          global
          local
          flake
        ];
    };

in {
  dirac = config "dirac" "x86_64-linux";
}
