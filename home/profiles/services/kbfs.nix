{
  services.kbfs = {
    enable = true;
    extraFlags = [ "-label kbfs" "-mount-type normal" ];
    mountPoint = ".local/keybase";
  };
}
