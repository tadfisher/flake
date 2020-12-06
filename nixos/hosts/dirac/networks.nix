{ config, lib, pkgs, ... }:

with lib;

let secrets = import ../../../secrets;

in {
  environment.etc = {
    "NetworkManager/system-connections/simple-vpn.nmconnection" = {
      mode = "0600";

      text = generators.toINI { } {
        connection = {
          id = "simple-vpn";
          uuid = "7c486dc2-a980-4c2b-a476-ef222725cb52";
          type = "vpn";
          permissions = "user:tad:;";
          secondaries = "";
        };

        vpn = {
          inherit (secrets.simple.vpn) ca cert key ta;
          auth = "SHA256";
          cert-pass-flags = 0;
          cipher = "AES-256-CBC";
          connection-type = "password-tls";
          dev-type = "tun";
          dev = "tun";
          password-flags = 1;
          persistent = "yes";
          ping-restart = 3600;
          ping = 10;
          remote = "openvpn.banksimple.com:1194:udp";
          reneg-seconds = 0;
          service-type = "org.freedesktop.NetworkManager.openvpn";
          ta-dir = 1;
          tun-ipv6 = "no";
          username = "tad";
        };

        ipv4 = { method = "auto"; };
      };
    };

    "NetworkManager/system-connections/simple-wifi.nmconnection" = {
      mode = "0600";

      text = generators.toINI { } {
        connection = {
          id = "simple-wifi";
          uuid = "28ab5a0d-fde7-4c0c-8814-9496c302c95f";
          type = "wifi";
          permissions = "user:tad:;";
        };

        wifi = {
          mac-address-blacklist = "";
          mode = "infrastructure";
          ssid = "Simple";
        };

        wifi-security = {
          auth-alg = "open";
          key-mgmt = "wpa-eap";
        };

        "802-1x" = with (secrets.simple.wifi); {
          eap = "ttls";
          identity = user;
          password = pass;
          phase2-auth = "mschapv2";
        };

        ipv4 = {
          dns-search = "";
          method = "auto";
        };

        ipv6 = {
          addr-gen-mode = "stable-privacy";
          dns-search = "";
          method = "auto";
        };
      };
    };
  };
}
