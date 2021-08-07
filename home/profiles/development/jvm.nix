{ pkgs, ... }:

{
  home = {
    packages = with pkgs; [ gradle gradle-completion jetbrains.idea-community ];

    sessionVariables = { JAVA_HOME = "${pkgs.openjdk.home}"; };
  };

  xdg.dataFile = {
    "java/openjdk".source = pkgs.openjdk.home;
    "java/openjdk8".source = pkgs.openjdk8.home;
    "java/openjdk11".source = pkgs.openjdk11.home;
    "java/jetbrains".source = pkgs.jetbrains.jdk;
  };
}
