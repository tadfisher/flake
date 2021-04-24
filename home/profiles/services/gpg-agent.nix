{
  services.gpg-agent = {
    enable = true;
    defaultCacheTtl = 3600;
    defaultCacheTtlSsh = 3600;
    enableExtraSocket = true;
    enableSshSupport = true;
    grabKeyboardAndMouse = false;
  };

  programs.ssh.matchBlocks."kepler" = {
    hostname = "kepler.lan";
    user = "tad";
    extraOptions = {
      RemoteForward = "/run/user/1000/gnupg/S.gpg-agent.ssh /run/user/1000/gnupg/S.gpg-agent.ssh";
    };
  };
}
