{
  description = "Tad's Nix configurations";

  inputs = {
    android-nixpkgs = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:tadfisher/android-nixpkgs";
    };
    ath11k-firmware = {
      url = "github:kvalo/ath11k-firmware";
      flake = false;
    };
    emacs-overlay = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:nix-community/emacs-overlay";
    };
    firefox-gnome-theme = {
      url = "github:rafaelmardojai/firefox-gnome-theme";
      flake = false;
    };
    hidviz = {
      url = "github:ondrejbudai/hidviz";
      flake = false;
    };
    home-manager = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:nix-community/home-manager";
    };
    jetbrains-jdk = {
      url = "github:Jetbrains/JetBrainsRuntime/dc8888f2e085fcb387638994627c2e5e8e66bb33";
      flake = false;
    };
    jextract = {
      url = "github:openjdk/jextract";
      flake = false;
    };
    libhidx = {
      url = "github:ondrejbudai/libhidx";
      flake = false;
    };
    ligature-el = {
      url = "github:mickeynp/ligature.el";
      flake = false;
    };
    mutter = {
      url = "git+https://gitlab.gnome.org/Community/Ubuntu/mutter.git?ref=triple-buffering-v4-43";
      flake = false;
    };
    notmuch-notify = {
      url = "git+https://git.celti.name/Celti/notmuch-notify?ref=trunk";
      flake = false;
    };
    nix-dart = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:tadfisher/nix-dart";
    };
    nix-direnv = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:nix-community/nix-direnv";
    };
    nix-prefetch-github = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:seppeljordan/nix-prefetch-github";
    };
    nixos-hardware.url = "github:tadfisher/nixos-hardware/lenovo-p14s-t14-updates";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    paperwm = {
      url = "github:paperwm/PaperWM/develop";
      flake = false;
    };
    pass-audit = {
      url = "github:roddhjav/pass-audit";
      flake = false;
    };
    password-store-otp-el = {
      url = "github:tadfisher/password-store-otp.el/executable-vars";
      flake = false;
    };
    portmod = {
      url = "gitlab:portmod/portmod/v2.5.0";
      flake = false;
    };
    rycee = {
      url = "gitlab:rycee/nur-expressions";
      flake = false;
    };
    sedcli = {
      url = "github:gjoyce-ibm/sedcli/kernel-keyring";
      flake = false;
    };
    tree-sitter-blueprint = {
      url = "github:huanie/tree-sitter-blueprint";
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
            {
              allowAliases = true;
              allowUnfree = true;
              android_sdk.accept_license = true;
              permittedInsecurePackages = [ "figma-linux-0.10.0" ];
            }
            // profiles.games;


          overlays = [
            (inputs.android-nixpkgs.overlays.default)
            (inputs.nix-dart.overlay)
            (inputs.nix-direnv.overlays.default)
            (self.overlays.default)
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
            self.hmModules.programs.devhelp
            self.hmModules.programs.emacs-lsp
            self.hmModules.programs.inkscape
            self.hmModules.programs.notmuch-notify
            self.hmModules.programs.pass-git-helper
            self.hmModules.services.adb
            self.hmModules.services.gnirehtet
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
        imes = {
          system = "x86_64-linux";
          config = ./home/hosts/imes.nix;
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
          devhelp = import ./home/modules/programs/devhelp.nix;
          emacs-init = import ./home/modules/programs/emacs-init.nix;
          emacs-lsp = import ./home/modules/programs/emacs-lsp.nix;
          inkscape = import ./home/modules/programs/inkscape.nix;
          notmuch-notify = import ./home/modules/programs/notmuch-notify.nix;
          pass-git-helper = import ./home/modules/programs/pass-git-helper.nix;
        };

        services = {
          adb = import ./home/modules/services/adb.nix;
          gnirehtet = import ./home/modules/services/gnirehtet.nix;
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
            inputs.nixos-hardware.nixosModules.lenovo-thinkpad-p14s-amd-gen1
          ];
        };
        imes = {
          system = "x86_64-linux";
          config = ./nixos/hosts/imes.nix;
          modules = [
            inputs.nixos-hardware.nixosModules.lenovo-thinkpad-p14s-amd-gen4
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

      overlays = rec {
        overlay = final: prev: import ./pkgs/overlay.nix inputs final prev;

        pkgs = final: prev: import ./pkgs { inherit inputs; pkgs = final; };

        # There's probably an easier way to merge attributes in `overlays' into a
        # single function.
        default = final: prev:
          (self.overlays.pkgs final prev) //
          (self.overlays.overlay final prev);
      };

      packages = eachSystem (system:
        import ./pkgs { inherit inputs; pkgs = pkgsBySystem.${system}; } //
        inputs.nix-dart.packages.${system} //
        {
          nix-prefetch-github = inputs.nix-prefetch-github.packages.${system}.default;
          nixos-iso = self.nixosConfigurations.installer.config.system.build.isoImage;
          nixUnstable = inputs.nixpkgs.legacyPackages.${system}.nixUnstable;
          nixos-rebuild = inputs.nixpkgs.legacyPackages.${system}.nixos-rebuild.override {
            nix = self.packages.${system}.nixUnstable;
          };
        }
      );
    };
}
