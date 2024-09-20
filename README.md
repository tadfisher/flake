# Private Internet Access wireguard client on networkd for NixOS

NixOS configuration for connecting to PIA VPN via wireguard. Supports port forwarding. Supports
custom networkd `netdev` and `network` configuration. By default it will select the server with
the lowest ping, but it can be configured to connect to a chosen server (this is faster).

Based on [tadfisher's pia-vpn.nix](https://github.com/tadfisher/flake/blob/f6f9c5a/nixos/modules/pia-vpn.nix). Thanks!

## Usage

```
# flake.nix
{
  inputs = {
    nix-pia-vpn.url = "github:rcambrj/nix-pia-vpn";
    nix-pia-vpn.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = inputs@{ ... }: {
    nixosConfigurations = {
      my-host = inputs.nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          inputs.nia-pia-vpn.nixosModules.default
          {
            services.pia-vpn = {
              enable = true;
              certificateFile = ./ca.rsa.4096.crt;
              environmentFile = # use sops-nix or agenix
            };
          }
        ];
      };
    };
  };
}
```

### Set VPN as default route

```
# configuration.nix
  services.pia-vpn.networkConfig = ''
    [Match]
    Name = ''${interface}

    [Network]
    Description = WireGuard PIA network interface
    Address = ''${peerip}/32

    [RoutingPolicyRule]
    To = ''${wg_ip}/32
    Priority = 1000

    [RoutingPolicyRule]
    To = 0.0.0.0/0
    Priority = 2000
    Table = 42

    [Route]
    Destination = 0.0.0.0/0
    Table = 42
  '';
```

### Configure Transmission with forwarded port

```
# transmission-rpc.env
# put this in sops-nix or agenix!
TR_AUTH=username:password
```

```
# configuration.nix
  services.pia-vpn.portForward = {
    enable = true;
    script = ''
      export $(cat transmission-rpc.env | xargs)
      ${pkgs.transmission_4}/bin/transmission-remote --authenv --port $port || true
    '';
  };
```

### Bring up services when VPN is up (and tear them down when it's not)

```
# configuration.nix
  systemd.services.my-service-which-depends-on-vpn = {
    after = [ "pia-vpn.service" ];
    bindsTo = [ "pia-vpn.service" ];
  };
```