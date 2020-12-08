{
  description = "Tad's Nix configurations";

  inputs = {
    android-nixpkgs = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:tadfisher/android-nixpkgs/flake";
    };
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    home-manager = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:nix-community/home-manager";
    };
    nixos-hardware.url = "github:NixOS/nixos-hardware";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nur.url = "github:nix-community/NUR";
  };

  outputs = { self, ... }@inputs:
    let
      systems = [ "x86_64-linux" ];

      eachSystem = inputs.nixpkgs.lib.genAttrs systems;

      pkgsBySystem = eachSystem (system:
        import inputs.nixpkgs {
          inherit system;
          config.allowUnfree = true;
          overlays = [
            (self.overlay)
            (inputs.android-nixpkgs.overlay)
            (inputs.emacs-overlay.overlay)
            (inputs.nur.overlay)
          ];
        }
      );

      mkNixosConfiguration = name: { system, config, modules ? [ ] }:
        inputs.nixpkgs.lib.nameValuePair name (inputs.nixpkgs.lib.nixosSystem {
          inherit system;
          modules = modules ++ [
            self.nixosModules.hardware.pulseaudio

            ({ pkgs, ... }: {
              environment.etc.nixpkgs.source = inputs.nixpkgs;
              networking.hostName = name;
              nix = {
                extraOptions = "experimental-features = nix-command flakes";
                nixPath = [ "nixpkgs=/etc/nixpkgs" ];
                package = pkgs.nixFlakes;
                registry = {
                  self.flake = self;
                  nixpkgs = {
                    from = { id = "nixpkgs"; type = "indirect"; };
                    flake = inputs.nixpkgs;
                  };
                };
              };
              nixpkgs.pkgs = pkgsBySystem.${system};
              system.configurationRevision = inputs.nixpkgs.lib.mkIf (self ? rev) self.rev;
            })

            config
          ] ++ (inputs.nixpkgs.lib.optionals (builtins.hasAttr name self.hmConfigurations) [
            inputs.home-manager.nixosModules.home-manager
            {
              home-manager = {
                useGlobalPkgs = true;
                useUserPackages = true;
                users.tad = self.hmConfigurations.${name};
              };
            }
          ]);
        });

      mkHomeConfiguration = name: { system, config }:
        let
          homeConfig = config;
        in
        inputs.nixpkgs.lib.nameValuePair name ({ config, lib, pkgs, ... }: {
          imports = [
            inputs.android-nixpkgs.hmModules.android-sdk
            pkgsBySystem.${system}.nur.repos.rycee.hmModules.emacs-init
            self.hmModules.programs.emacs-lsp
            self.hmModules.programs.firefox
            self.hmModules.programs.gnome-shell
            self.hmModules.programs.pass
            self.hmModules.services.gnirehtet
            self.hmModules.services.mopidy
            homeConfig
          ];

          systemd.user.sessionVariables."NIX_PATH" = inputs.nixpkgs.lib.mkForce
            "nixpkgs=${config.xdg.dataHome}/nixpkgs\${NIX_PATH:+:}$NIX_PATH";

          xdg = {
            dataFile."nixpkgs".source = inputs.nixpkgs;

            configFile."nix/registry.json".text = builtins.toJSON {
              verison = 2;
              flakes =
                let
                  toInput = input: {
                    type = "path";
                    path = input.outPath;
                  } // (
                    inputs.nixpkgs.lib.filterAttrs
                      (n: _: n == "lastModified" || n == "rev" || n == "revCount" || n == "narHash")
                      input
                  );
                in
                [
                  {
                    from = { id = "self"; type = "indirect"; };
                    to = toInput inputs.self;
                  }
                  {
                    from = { id = "nixpkgs"; type = "indirect"; };
                    to = toInput inputs.nixpkgs;
                  }
                ];
            };
          };
        });

    in
    {
      hmConfigurations = inputs.nixpkgs.lib.mapAttrs' mkHomeConfiguration {
        dirac = {
          system = "x86_64-linux";
          config = ./home/hosts/dirac.nix;
        };
      };

      hmModules = {
        programs = {
          emacs-lsp = import ./home/modules/programs/emacs-lsp.nix;
          firefox = import ./home/modules/programs/firefox.nix;
          gnome-shell = import ./home/modules/programs/gnome-shell.nix;
          pass = import ./home/modules/programs/pass.nix;
        };

        services = {
          gnirehtet = import ./home/modules/services/gnirehtet.nix;
          mopidy = import ./home/modules/services/mopidy.nix;
        };
      };

      nixosConfigurations = inputs.nixpkgs.lib.mapAttrs' mkNixosConfiguration {
        dirac = {
          system = "x86_64-linux";
          config = ./nixos/hosts/dirac.nix;
          modules = [
            inputs.nixos-hardware.nixosModules.common-cpu-intel-kaby-lake
            inputs.nixos-hardware.nixosModules.common-pc-laptop-ssd
            inputs.nixos-hardware.nixosModules.lenovo-thinkpad-t480s
          ];
        };
      };

      nixosModules.hardware.pulseaudio = ./nixos/modules/config/pulseaudio.nix;

      overlays = {
        pkgs = (final: prev: import ./pkgs { pkgs = final; });

        # This would be part of `packages' but that doesn’t support nested attrsets
        # (e.g. using `lib.makeScope').
        emacs = (final: prev: {
          emacsPackagesFor = emacs: (prev.emacsPackagesFor emacs).overrideScope'
            (efinal: eprev: import ./pkgs/emacs-packages.nix efinal);
        });
      };

      # There's probably an easier way to merge attributes in `overlays' into a
      # single function.
      overlay = final: prev:
        (self.overlays.pkgs final prev) // (self.overlays.emacs final prev);

      packages = eachSystem (system: import ./pkgs { pkgs = pkgsBySystem.${system}; });
    };
}
