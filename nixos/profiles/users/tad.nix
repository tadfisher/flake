{ inputs, name, ... }:

{
  users.users.tad = {
    createHome = true;
    description = "Tad Fisher";
    extraGroups = [
      "adbusers"
      "backup"
      "btrfs"
      "cdrom"
      "docker"
      "games"
      "libvirtd"
      "media"
      "networkmanager"
      "scanner"
      "tss"
      "vboxusers"
      "video"
      "wheel"
    ];
    hashedPasswordFile = "/root/nixos/secrets/passwd-tad";
    home = "/home/tad";
    isNormalUser = true;
    uid = 1000;
  };
}
