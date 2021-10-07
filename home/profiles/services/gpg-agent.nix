let
  socketDir = "/run/user/1000/gnupg";

in
{
  programs.ssh.matchBlocks."kepler" = {
    hostname = "kepler.lan";
    user = "tad";
    remoteForwards = [
      {
        bind.address = "${socketDir}/S.gpg-agent";
        host.address = "${socketDir}/S.gpg-agent.extra";
      }
      {
        bind.address = "${socketDir}/S.gpg-agent.ssh";
        host.address = "${socketDir}/S.gpg-agent.ssh";
      }
    ];
    sendEnv = [
      "SSH_AUTH_SOCK"
    ];
  };

  services.gpg-agent = {
    enable = true;
    defaultCacheTtl = 3600;
    defaultCacheTtlSsh = 3600;
    enableExtraSocket = true;
    enableSshSupport = true;
    grabKeyboardAndMouse = false;
  };
}
