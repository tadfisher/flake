{ config, pkgs, ... }:

{
  boot.extraModprobeConfig = ''
    options kvm ignore_msrs=1
  '';

  environment.etc = {
    "ovmf/edk2-x86_64-secure-code.fd".source =
      config.virtualisation.libvirtd.qemu.package + "/share/qemu/edk2-x86_64-secure-code.fd";

    "ovmf/edk2-i386-vars.fd" = {
      source = config.virtualisation.libvirtd.qemu.package + "/share/qemu/edk2-i386-vars.fd";
      mode = "0644";
      uid = config.users.users.qemu-libvirtd.uid;
      gid = config.users.groups.qemu-libvirtd.gid;
    };
  };

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
