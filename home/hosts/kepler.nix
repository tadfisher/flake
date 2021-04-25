{ config, lib, pkgs, ... }:

{
  imports = [
    ../profiles/core.nix
    ../profiles/server.nix
  ];

  accounts.email.accounts."tadfisher@gmail.com".primary = true;
}
