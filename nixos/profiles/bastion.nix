{ config, lib, pkgs, ... }:

with lib;

let
  acmeCerts = config.security.acme.certs."orion.tad.codes".directory;
in
{
  networking = {
    firewall.allowedTCPPorts = [ 22 80 443 ];
  };

  security = {
    acme = {
      acceptTerms = true;
      certs."orion.tad.codes" = {
        extraDomainNames = [ "*.orion.tad.codes" ];
        email = "tadfisher@gmail.com";
        dnsProvider = "cloudflare";
        credentialsFile = "/root/nixos/secrets/cloudflare-dns.env";
      };
    };

    pam = {
      enableSSHAgentAuth = true;
      services.ssh.sshAgentAuth = true;
    };
  };

  services = {
    ddclient = {
      enable = true;
      configFile = "/root/nixos/secrets/ddclient.conf";
    };

    nginx = {
      enable = true;
      recommendedGzipSettings = true;
      recommendedOptimisation = true;
      recommendedProxySettings = true;
      recommendedTlsSettings = true;
      virtualHosts = {
        "orion.tad.codes" = {
          sslCertificate = "${acmeCerts}/fullchain.pem";
          sslCertificateKey = "${acmeCerts}/key.pem";
          sslTrustedCertificate = "${acmeCerts}/chain.pem";
          forceSSL = true;
          default = true;
          extraConfig = ''
            add_header Strict-Transport-Security max-age=2592000;
          '';
          locations = {
            "/" = {
              index = "index.html";
              tryFiles = "$uri $uri/ =404";
            };
          };
        };
        "plex.orion.tad.codes" = {
          sslCertificate = "${acmeCerts}/fullchain.pem";
          sslCertificateKey = "${acmeCerts}/key.pem";
          sslTrustedCertificate = "${acmeCerts}/chain.pem";
          forceSSL = true;
          extraConfig = ''
            client_max_body_size 0;
            proxy_redirect off;
            proxy_buffering off;
          '';
          locations."/" = {
            proxyPass = "http://localhost:32400/";
            extraConfig = ''
              proxy_set_header X-Plex-Client-Identifier $http_x_plex_client_identifier;
              proxy_set_header X-Plex-Device $http_x_plex_device;
              proxy_set_header X-Plex-Device-Name $http_x_plex_device_name;
              proxy_set_header X-Plex-Platform $http_x_plex_platform;
              proxy_set_header X-Plex-Platform-Version $http_x_plex_platform_version;
              proxy_set_header X-Plex-Product $http_x_plex_product;
              proxy_set_header X-Plex-Token $http_x_plex_token;
              proxy_set_header X-Plex-Version $http_x_plex_version;
              proxy_set_header X-Plex-Nocache $http_x_plex_nocache;
              proxy_set_header X-Plex-Provides $http_x_plex_provides;
              proxy_set_header X-Plex-Device-Vendor $http_x_plex_device_vendor;
              proxy_set_header X-Plex-Model $http_x_plex_model;
            '';
          };
        };
      };
    };

    oauth2_proxy = {
      enable = true;
      provider = "google";
      redirectURL = "https://orion.tad.codes/oauth2/callback";

      cookie = {
        domain = ".orion.tad.codes";
        refresh = "24h";
      };

      email.addresses = ''
        tadfisher@gmail.com
        nyoungsma@gmail.com
      '';

      google = {
        adminEmail = "tadfisher@gmail.com";
        serviceAccountJSON = "/root/nixos/secrets/oauth2-proxy-service-account.json";
      };

      upstream = [
        "http://127.0.0.1:8080/"
      ];

      passAccessToken = true;

      reverseProxy = true;

      setXauthrequest = true;

      extraConfig = {
        pass-authorization-header = true;
        whitelist-domain = ".orion.tad.codes";
      };

      keyFile = "/root/nixos/secrets/oauth2-proxy.env";

      nginx.virtualHosts = [
        "orion.tad.codes"
        "plex.orion.tad.codes"
      ];
    };

    sshguard = {
      enable = true;
      blacklist_threshold = 120;
    };
  };

  users.users.nginx.extraGroups = [ "acme" ];
}
