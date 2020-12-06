{ inputs, name, ... }:

{
  users.extraUsers.tad = {
    description = "Tad Fisher";
    extraGroups = [
      "adbusers"
      "btrfs"
      "docker"
      "libvirtd"
      "media"
      "networkmanager"
      "video"
      "wheel"
    ];
    hashedPassword = "$6$99koQN2Mdpu0V$g5FtEPTiZkAGBmLI1M65ZNGZ8VQYIMT3ILX35eCKMYDkPhDfUy2wsMdXNMd4ZaFgz92puExk452b5IPM85qvW.";
    home = "/home/tad";
    isNormalUser = true;
    uid = 1000;
  };
}
