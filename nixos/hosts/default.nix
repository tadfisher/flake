{ self, emacs-overlay, home-manager, nixos-hardware, nixpkgs, nur, ... }@inputs:

let
  config = hostname: system:
    nixpkgs.lib.nixosSystem {
      inherit system;

      modules = let
        global = {
          imports = [ ../profiles/users ];

          networking.hostName = hostname;

          nix = {
            nixPath = [ "nixpkgs=${inputs.nixpkgs}" ];

            registry = {
              emacs-overlay.flake = inputs.emacs-overlay;
              nixos-hardware.flake = inputs.nixos-hardware;
              nixpkgs.flake = inputs.nixpkgs;
              nur.flake = inputs.nur;
              self.flake = inputs.self;
            };
          };

          nixpkgs.overlays = [ self.overlay emacs-overlay.overlay nur.overlay ];

          system.configurationRevision = nixpkgs.lib.mkIf (self ? rev) self.rev;
        };

        local = import "${toString ./.}/${hostname}" inputs;

        flake = builtins.attrValues self.nixosModules;

      in nixpkgs.lib.flatten [
        home-manager.nixosModules.home-manager
        global
        local
        flake
      ];
    };

in { dirac = config "dirac" "x86_64-linux"; }
