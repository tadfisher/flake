{ config, lib, pkgs, ... }:

with lib;

{
  environment.systemPackages = [ pkgs.vaultwarden ];

  services = {
    # The service
    vaultwarden = {
      enable = true;
      dbBackend = "sqlite";
      config = {
        ROCKET_ADDRESS = "127.0.0.1";
        ROCKET_PORT = 8222;
        DOMAIN = "https://vault.orion.tad.codes";
        SIGNUPS_ALLOWED = true;
        ADMIN_TOKEN = "$argon2id$v=19$m=65540,t=3,p=4$lD5xiGMasc1vMpixn7jV1GeTAprCxRkwA2nlLpKE6lQ$6lQFJrIB4C89TLX9H3u5uurDaEH33QsqRsh54UTabBg";
        LOG_FILE = "/var/log/vaultwarden/access.log";
      };
    };

    # The nginx reverse proxy
    nginx = let host = "vault.orion.tad.codes"; in {
      enable = true;
      recommendedGzipSettings = true;
      recommendedOptimisation = true;
      recommendedProxySettings = true;
      recommendedTlsSettings = true;

      virtualHosts."${host}" = {
        forceSSL = true;
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
        useACMEHost = "orion.tad.codes";
      };
    };
  };
}
