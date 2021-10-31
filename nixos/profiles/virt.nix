{ pkgs, ... }:

{
  boot.extraModprobeConfig = ''
    options kvm ignore_msrs=1
  '';

  users.users.tad.extraGroups = [ "libvirtd" ];

  virtualisation.libvirtd = {
    enable = true;
    qemu = {
      package = pkgs.qemu_kvm;
      runAsRoot = false;
    };
  };
}
