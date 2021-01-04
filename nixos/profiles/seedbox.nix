{ config, lib, pkgs, ... }:

with lib;
let
  piaIpEnvFile = config.services.pia-vpn.ipEnvFile;

  processTorrent = pkgs.writeScript "processTorrent" ''
    #!${pkgs.stdenv.shell}
    cd "$TR_TORRENT_DIR"
    if [ -d "$TR_TORRENT_NAME" ]; then
      cd "$TR_TORRENT_NAME"
      for dir in $(find . -name '*.rar' -exec dirname {} \; | sort -u); do
        pushd $dir; ${pkgs.unrar}/bin/unrar x *.rar; popd
      done
    in
  '';

in
{
  services = {
    pia-vpn = {
      enable = true;
      certificateFile = "/root/.vpn/ca.rsa.4096.crt";
      environmentFile = "/root/.vpn/pia.env";
      portForward = {
        enable = true;
        script = ''
          ${pkgs.transmission}/bin/transmission-remote --port $port || true
        '';
      };
    };

    transmission = {
      enable = true;
      port = 9091;
      settings = {
        download-queue-enabled = true;
        download-queue-size = 3;
        encryption = 1;
        idle-seeding-limit = 2;
        idle-seeding-limit-enabled = false;
        incomplete-dir-enabled = false;
        peer-limit-global = 1033;
        peer-limit-per-torrent = 310;
        peer-port = 61030;
        peer-port-random-high = 65535;
        peer-port-random-low = 16384;
        peer-port-random-on-start = true;
        peer-socket-tos = "lowcost";
        port-forwarding-enabled = false;
        queue-stalled-enabled = true;
        queue-stalled-minutes = 30;
        ratio-limit = 4;
        ratio-limit-enabled = true;
        rename-partial-files = true;
        rpc-bind-address = "0.0.0.0";
        rpc-enabled = true;
        rpc-password = "{dfed8b5975f9e826885a1bc03d4116b89f4499c3JmkXT62G";
        rpc-port = 9091;
        rpc-url = "/transmission/";
        rpc-username = "";
        rpc-host-whitelist = "*";
        rpc-whitelist = "192.168.*.*,127.0.0.1";
        rpc-whitelist-enabled = false;
        scrape-paused-torrents-enabled = true;
        script-torrent-done-enabled = true;
        script-torrent-done-filename = processTorrent;
        seed-queue-enabled = false;
        speed-limit-up = 550;
        speed-limit-up-enabled = true;
        start-added-torrents = true;
        trash-original-torrent-files = false;
        umask = 2;
        upload-slots-per-torrent = 14;
        utp-enabled = true;
        watch-dir-enabled = true;
      };
    };
  };

  systemd.services.transmission = {
    after = [ "pia-vpn.service" ];
    bindsTo = [ "pia-vpn.service" ];
    requires = [ "network.target" ];
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      BindReadOnlyPaths = [ piaIpEnvFile ];
      EnvironmentFile = [ piaIpEnvFile ];
      ExecStart = mkForce ''
        ${pkgs.transmission}/bin/transmission-daemon -f \
          -g "${config.services.transmission.home}/.config/transmission-daemon" \
          --bind-address-ipv4 $IP \
          --log-debug --logfile $LOGS_DIRECTORY/daemon.log
      '';
      LogsDirectory = [ "transmission" ];
    };
  };

  users.users.transmission.extraGroups = [ "media" ];
}
