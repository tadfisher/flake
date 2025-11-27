{ config, pkgs, ... }:

{
  boot.extraModprobeConfig = ''
    options kvm ignore_msrs=1
  '';

  security.polkit.enable = true;

  users.users.tad.extraGroups = [
    config.users.groups.qemu-libvirtd.name
  ];

  virtualisation.libvirtd = {
    enable = true;
    qemu = {
      package = pkgs.qemu_kvm;
      runAsRoot = false;
    };
  };
}
