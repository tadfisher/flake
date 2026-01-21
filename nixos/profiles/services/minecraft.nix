{ ... }:
{
  networking.firewall.allowedUDPPorts = [
    19132 # Minecraft
  ];

  virtualisation.oci-containers.containers = {
    minecraft = {
      environment = {
        EULA = "TRUE";
        DIFFICULTY = "1";
        SERVER_NAME = "Costco Wholesale";
        TZ = "America/Los_Angeles";
        VERSION = "LATEST";
        ONLINE_MODE = "false";
        OPS = "2535424558211822,2535457855861157,2535425630902140,2535450624199762";
      };
      image = "itzg/minecraft-bedrock-server";
      ports = [ "0.0.0.0:19132:19132/udp" ];
      volumes = [ "/srv/minecraft/:/data" ];
    };
  };
}
