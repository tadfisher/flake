{
  description = "Tad's Nix configurations";

  inputs = {
    android-nixpkgs = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:tadfisher/android-nixpkgs";
    };
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    home-manager = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:nix-community/home-manager";
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
            self.nixosModules.services.pia-vpn

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
            inputs.android-nixpkgs.hmModule
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
              version = 2;
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
        tycho = {
          system = "x86_64-linux";
          config = ./home/hosts/tycho.nix;
        };
      };

      hmModules = {
        programs = {
          emacs-init = import ./home/modules/programs/emacs-init.nix;
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
        hardware.pulseaudio = ./nixos/modules/pulseaudio.nix;
        services.pia-vpn = ./nixos/modules/pia-vpn.nix;
      };

      overlays = {
        # This would be part of `packages' but that doesn’t support nested attrsets
        # (e.g. using `lib.makeScope').
        emacs = final: prev: {
          emacsPackagesFor = emacs: (prev.emacsPackagesFor emacs).overrideScope'
            (efinal: eprev: import ./pkgs/emacs efinal);
        };

        dart = final: prev: inputs.nix-dart.overlay final prev;

        overlay = final: prev: import ./pkgs/overlay.nix final prev;

        pkgs = final: prev: import ./pkgs { pkgs = final; };
      };

      # There's probably an easier way to merge attributes in `overlays' into a
      # single function.
      overlay = final: prev:
        (self.overlays.dart final prev) //
        (self.overlays.pkgs final prev) //
        (self.overlays.emacs final prev) //
        (self.overlays.overlay final prev);

      packages = eachSystem (system:
        import ./pkgs { pkgs = pkgsBySystem.${system}; } //
        inputs.nix-dart.packages.${system} //
        { nix-prefetch-github = inputs.nix-prefetch-github.defaultPackage.${system}; } //
        { nixos-iso = self.nixosConfigurations.installer.config.system.build.isoImage; }
      );
    };
}
