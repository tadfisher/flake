{
  description = "Tad's Nix configurations";

  inputs = {
    agent-shell = {
      url = "github:xenodium/agent-shell";
      flake = false;
    };
    android-nixpkgs = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:tadfisher/android-nixpkgs";
    };
    eglot-booster = {
      url = "github:jdtsmith/eglot-booster";
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
    kolide = {
      url = "github:kolide/nix-agent";
      inputs.nixpkgs.follows = "nixpkgs";
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
    nixos-hardware.url = "github:nixos/nixos-hardware";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    openjdk-wakefield = {
      url = "github:openjdk/wakefield/jdk21.0.1-wayland";
      flake = false;
    };
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
    tree-sitter-typespec = {
      url = "github:happenslol/tree-sitter-typespec";
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
              permittedInsecurePackages = [
                "figma-linux-0.10.0"
                # For sonarr.
                # BUG: https://github.com/NixOS/nixpkgs/issues/360592
                "aspnetcore-runtime-6.0.36"
                "aspnetcore-runtime-wrapped-6.0.36"
                "dotnet-sdk-6.0.428"
                "dotnet-sdk-wrapped-6.0.428"
              ];
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
            self.nixosModules.boot.opal-unlock
            self.nixosModules.services.pia-vpn

            ({ pkgs, ... }: {
              environment.etc.nixpkgs.source = inputs.nixpkgs;
              networking.hostName = name;
              nix = {
                extraOptions = "experimental-features = nix-command flakes auto-allocate-uids";
                nixPath = [ "nixpkgs=${inputs.nixpkgs}" ];
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
            inputs.kolide.nixosModules.kolide-launcher
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
        programs.steam = ./nixos/modules/steam.nix;
        services.pia-vpn = ./nixos/modules/pia-vpn.nix;
      };

      overlays = {
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
          inherit (pkgsBySystem.${system}) ccid vaultwarden;
          nixos-iso = self.nixosConfigurations.installer.config.system.build.isoImage;
          nixos-rebuild = inputs.nixpkgs.legacyPackages.${system}.nixos-rebuild;
        }
      );
    };
}
