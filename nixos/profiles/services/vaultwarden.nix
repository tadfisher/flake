{ config, lib, pkgs, ... }:

let
  host = "vault.orion.tad.codes";
  sendmail = "${pkgs.msmtp}/bin/sendmail";

in
{
  environment.systemPackages = [ pkgs.vaultwarden ];

  programs.msmtp = {
    enable = true;
    defaults = {
      auth = true;
      tls = true;
      tls_starttls = true;
      tls_trust_file = "/etc/ssl/certs/ca-certificates.crt";
      port = 587;
    };
    accounts.default = {
      host = "smtp.gmail.com";
      from = "tadfisher@gmail.com";
      user = "tadfisher@gmail.com";
      passwordeval = ''cat "$CREDENTIALS_DIRECTORY/gmail-app-password"'';
    };
  };

  systemd.services.vaultwarden.serviceConfig.LoadCredential =
    "gmail-app-password:/root/nixos/secrets/gmail-app-password";

  services = {
    # The service
    vaultwarden = {
      enable = true;
      dbBackend = "sqlite";
      environmentFile = "/root/nixos/secrets/vaultwarden.env";
      config = {
        ROCKET_ADDRESS = "127.0.0.1";
        ROCKET_PORT = 8222;
        DOMAIN = "https://${host}";
        SIGNUPS_ALLOWED = false;
        LOG_FILE = "/var/lib/bitwarden_rs/access.log";
        SSO_ENABLED = true;
        SSO_AUTHORITY = "https://id.orion.tad.codes";
        WEB_VAULT_ENABLED = false;
        USE_SENDMAIL = true;
        SENDMAIL_COMMAND = sendmail;
      };
    };

    # The nginx reverse proxy
    nginx.virtualHosts."${host}" = {
      forceSSL = true;
      useACMEHost = "orion.tad.codes";
      extraConfig = ''
        access_log /var/log/nginx/${host}.access.log;
        error_log /var/log/nginx/${host}.error.log;
      '';
      locations."/" = {
        proxyPass = "http://localhost:${toString config.services.vaultwarden.config.ROCKET_PORT}/";
        proxyWebsockets = true;
        extraConfig = ''
          proxy_set_header Host $host;
          proxy_set_header X-Real-IP $remote_addr;
          proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
          proxy_set_header X-Forwarded-Proto $scheme;
        '';
      };
    };
  };
}
