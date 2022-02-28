{ config, lib, ... }:

with lib;

{
  environment.etc."systemd/networkd.conf.d/10-euler.conf".text =
    generators.toINI
      {
        listsAsDuplicateKeys = true;
      }
      {
        Network.RouteTable = [
          "lan:10"
          "mgmt:99"
        ];
      };

  networking.networkmanager.unmanaged = [ "enp2s0f0" ];

  systemd.network = {
    netdevs = {
      "40-enp2s0f0.10" = {
        netdevConfig = {
          Name = "enp2s0f0.10";
          Kind = "vlan";
        };

        vlanConfig.Id = 10;
      };

      "40-enp2s0f0.99" = {
        netdevConfig = {
          Name = "enp2s0f0.99";
          Kind = "vlan";
        };

        vlanConfig.Id = 99;
      };
    };

    networks = {
      "40-enp2s0f0" = {
        name = "enp2s0f0";
        linkConfig.RequiredForOnline = "no";
        vlan = [
          "enp2s0f0.10"
          "enp2s0f0.99"
        ];
      };

      "40-enp2s0f0.10" = {
        name = "enp2s0f0.10";
        linkConfig.RequiredForOnline = "no";
        address = [ "10.0.10.10/24" ];
        routingPolicyRules = [
          {
            routingPolicyRuleConfig = {
              From = "10.0.1.0/24";
              Table = "lan";
            };
          }
        ];
      };

      "40-enp2s0f0.99" = {
        name = "enp2s0f0.99";
        linkConfig.RequiredForOnline = "no";
        address = [ "10.0.99.10/24" ];
        routingPolicyRules = [
          {
            routingPolicyRuleConfig = {
              From = "10.0.99.0/24";
              Table = "mgmt";
            };
          }
        ];
      };
    };
  };
}
