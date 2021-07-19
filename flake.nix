{
  description = "Tad's Nix configurations";

  inputs = {
    android-nixpkgs = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:tadfisher/android-nixpkgs";
    };
    dash-to-panel = {
      url = "github:home-sweet-gnome/dash-to-panel";
      flake = false;
    };
    emacs-flake.url = "github:mjlbach/emacs-overlay";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    instant-workspace-switcher = {
      url = "github:amalantony/gnome-shell-extension-instant-workspace-switcher";
      flake = false;
    };
    home-manager = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:nix-community/home-manager";
    };
    naersk = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:nmattia/naersk";
    };
    nix-dart = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:tadfisher/nix-dart";
    };
    nix-prefetch-github = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:seppeljordan/nix-prefetch-github";
    };
    nixos-hardware.url = "github:NixOS/nixos-hardware";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    paperwm = {
      url = "github:paperwm/PaperWM/next-release";
      flake = false;
    };
    portmod = {
      url = "gitlab:portmod/portmod";
      flake = false;
    };
    rycee = {
      url = "gitlab:rycee/nur-expressions";
      flake = false;
    };
    vertical-overview = {
      url = "github:RensAlthuis/vertical-overview";
      flake = false;
    };
  };

  outputs = { self, ... }@inputs:
    with inputs.nixpkgs.lib;

    let
      systems = [ "x86_64-linux" ];

      eachSystem = genAttrs systems;

      pkgsBySystem = eachSystem (system:
        import inputs.nixpkgs {
          inherit system;

          config =
            let
              profiles = import ./nixpkgs { profiles = [ "games" ]; };
            in
            { allowUnfree = true; } //
            profiles.games;


          overlays = [
            (self.overlay)
            (inputs.android-nixpkgs.overlay)
          ];
        }
      );

      mkNixosConfiguration = name: { system, config, modules ? [ ] }:
        nameValuePair name (nixosSystem {
          inherit system;

          modules = modules ++ [
            {
              disabledModules = [
                "programs/steam.nix"
              ];
            }

            self.nixosModules.boot.opal-unlock
            self.nixosModules.hardware.pulseaudio
            self.nixosModules.services.pia-vpn
            self.nixosModules.programs.steam

            ({ pkgs, ... }: {
              environment.etc.nixpkgs.source = inputs.nixpkgs;
              networking.hostName = name;
              nix = {
                extraOptions = "experimental-features = nix-command flakes";
                nixPath = [ "nixpkgs=${inputs.nixpkgs}" ];
                package = self.packages.${system}.nixUnstable;
                registry = {
                  self.flake = self;
                  nixpkgs = {
                    from = { id = "nixpkgs"; type = "indirect"; };
                    flake = inputs.nixpkgs;
                  };
                };
              };
              nixpkgs.pkgs = pkgsBySystem.${system};
              system.configurationRevision = mkIf (self ? rev) self.rev;
            })

            config
          ] ++ (optionals (self.hmConfigurations ? ${name}) [
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
        nameValuePair name ({ config, lib, pkgs, ... }: {
          imports = [
            inputs.android-nixpkgs.hmModule
            (import inputs.rycee { inherit pkgs; }).hmModules.emacs-init
            self.hmModules.programs.emacs-lsp
            self.hmModules.programs.pass-git-helper
            self.hmModules.services.gnirehtet
            self.hmModules.services.mopidy
            homeConfig
          ];

          systemd.user.sessionVariables."NIX_PATH" =
            mkForce "nixpkgs=${config.xdg.dataHome}/nixpkgs\${NIX_PATH:+:}$NIX_PATH";
        });

    in
    {
      hmConfigurations = mapAttrs' mkHomeConfiguration {
        dirac = {
          system = "x86_64-linux";
          config = ./home/hosts/dirac.nix;
        };
        euler = {
          system = "x86_64-linux";
          config = ./home/hosts/euler.nix;
        };
        kepler = {
          system = "x86_64-linux";
          config = ./home/hosts/kepler.nix;
        };
        tycho = {
          system = "x86_64-linux";
          config = ./home/hosts/tycho.nix;
        };
      };

      hmModules = {
        programs = {
          emacs-init = import ./home/modules/programs/emacs-init.nix;
          emacs-lsp = import ./home/modules/programs/emacs-lsp.nix;
          pass-git-helper = import ./home/modules/programs/pass-git-helper.nix;
        };

        services = {
          gnirehtet = import ./home/modules/services/gnirehtet.nix;
          mopidy = import ./home/modules/services/mopidy.nix;
        };
      };

      nixosConfigurations = mapAttrs' mkNixosConfiguration {
        dirac = {
          system = "x86_64-linux";
          config = ./nixos/hosts/dirac.nix;
          modules = [
            inputs.nixos-hardware.nixosModules.common-cpu-intel-kaby-lake
            inputs.nixos-hardware.nixosModules.lenovo-thinkpad-t480s
          ];
        };
        euler = {
          system = "x86_64-linux";
          config = ./nixos/hosts/euler.nix;
          modules = [
            inputs.nixos-hardware.nixosModules.lenovo-thinkpad-t14-amd-gen1
          ];
        };
        installer = {
          system = "x86_64-linux";
          config = ./nixos/hosts/installer.nix;
          modules = [
            (import "${inputs.nixpkgs}/nixos/modules/installer/cd-dvd/installation-cd-graphical-gnome.nix")
          ];
        };
        kepler = {
          system = "x86_64-linux";
          config = ./nixos/hosts/kepler.nix;
          modules = [
            inputs.nixos-hardware.nixosModules.common-cpu-intel
          ];
        };
        tycho = {
          system = "x86_64-linux";
          config = ./nixos/hosts/tycho.nix;
          modules = [
            inputs.nixos-hardware.nixosModules.common-cpu-amd
          ];
        };
      };

      nixosModules = {
        boot.opal-unlock = ./nixos/modules/opal-unlock.nix;
        hardware.pulseaudio = ./nixos/modules/pulseaudio.nix;
        programs.steam = ./nixos/modules/steam.nix;
        services.pia-vpn = ./nixos/modules/pia-vpn.nix;
      };

      overlays = {
        dart = final: prev: inputs.nix-dart.overlay final prev;

        # This would be part of `packages' but that doesn’t support nested attrsets
        # (e.g. using `lib.makeScope').
        # emacs = final: prev:
        #   let
        #     emacs-overlay = inputs.emacs-overlay.overlay final prev;
        #   in
        #   {
        #     inherit (inputs.emacs-flake.packages.${prev.system}) emacsPgtkGcc;
        #     emacsPackagesFor = emacs:
        #       (emacs-overlay.emacsPackagesFor emacs).overrideScope'
        #         (efinal: eprev: (final.callPackage ./pkgs/emacs {}) efinal eprev);
        #   };

        overlay = final: prev: import ./pkgs/overlay.nix final prev;

        pkgs = final: prev: self.packages.${prev.system} or { };
      };

      # There's probably an easier way to merge attributes in `overlays' into a
      # single function.
      overlay = final: prev:
        (inputs.emacs-overlay.overlay final prev) //
        (self.overlays.dart final prev) //
        (self.overlays.pkgs final prev) //
        (self.overlays.overlay final prev);

      packages = eachSystem (system:
        import ./pkgs { inherit inputs; pkgs = pkgsBySystem.${system}; } //
        inputs.nix-dart.packages.${system} //
        {
          emacsPgtkGcc = inputs.emacs-flake.packages.${system}.emacsPgtkGcc;
          nix-prefetch-github = inputs.nix-prefetch-github.defaultPackage.${system};
          nixos-iso = self.nixosConfigurations.installer.config.system.build.isoImage;
          nixUnstable = inputs.nixpkgs.legacyPackages.${system}.nixUnstable;
          nixos-rebuild = inputs.nixpkgs.legacyPackages.${system}.nixos-rebuild.override {
            nix = self.packages.${system}.nixUnstable;
          };
        }
      );
    };
}
