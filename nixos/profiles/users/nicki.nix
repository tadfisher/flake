{ inputs, name, ... }:

{
  users.users.nicki = {
    createHome = true;
    description = "Nicki Youngsma";
    extraGroups = [
      "backup"
      "media"
    ];
    hashedPasswordFile = "/root/nixos/secrets/passwd-nicki";
    home = "/home/nicki";
    isNormalUser = true;
    uid = 1001;
  };
}
