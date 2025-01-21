let
  socketDir = "/run/user/1000/gnupg";

in
{
  programs.ssh.matchBlocks."kepler" = {
    hostname = "kepler.local";
    user = "tad";
    forwardAgent = true;
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
      "SSH_AUTH_SOCK=${socketDir}/S.gpg-agent.ssh"
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
