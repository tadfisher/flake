{ pkgs, ... }:

{
  boot.extraModprobeConfig = ''
    options kvm ignore_msrs=1
  '';

  users.users.tad.extraGroups = [ "libvirtd" ];

  virtualisation.libvirtd = {
    enable = true;
    qemuPackage = pkgs.qemu_kvm;
    qemuRunAsRoot = false;
  };
}
