{
  imports = [ ./virt.nix ];
  boot.extraModprobeConfig = ''
    options kvm_amd nested=1
  '';
}
