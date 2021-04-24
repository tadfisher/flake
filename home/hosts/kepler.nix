{ config, lib, pkgs, ... }:

{
  imports = [
    ../profiles/core.nix
  ];

  accounts.email.accounts."tadfisher@gmail.com".primary = true;
}
