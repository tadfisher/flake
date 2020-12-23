{
  users.users.steam = {
    description = "Steam";
    extraGroups = [
      "cdrom"
      "games"
      "media"
      "networkmanager"
      "video"
    ];
    hashedPassword = "";
    home = "/var/local/steam";
    isNormalUser = true;
    uid = 1001;
  };
}
