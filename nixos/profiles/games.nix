{ ... }:
{
  hardware.steam-hardware.enable = true;

  programs.steam = {
    enable = true;
    gamescopeSession.enable = true;
    extest.enable = true;
  };

  services = {
    udev.extraRules = ''
      ${builtins.readFile ../../data/udev/dualshock3.rules}
    '';
  };
}
