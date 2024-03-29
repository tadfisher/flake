{ pkgs, ... }:

let
  gradleWithToolchains = with pkgs; callPackage gradle-packages.gradle_7 {
    javaToolchains = [
      openjdk8
      jdk11
      jdk17
      jdk19
      jdk21
    ];
  };

in
{
  home = {
    file.".gradle/gradle.properties".text = ''
      org.gradle.java.installations.auto-download=false
      org.gradle.java.installations.paths=${pkgs.jdk8.home},${pkgs.jdk11.home},${pkgs.jdk17.home},${pkgs.jdk19.home},${pkgs.jdk21.home}
    '';

    packages = with pkgs; [
      gradle-completion
      gradleWithToolchains
      jd-gui
      jdk
      jetbrains.idea-community
    ];

    sessionVariables = {
      JAVA_HOME = "${pkgs.jdk.home}";
    };
  };

  xdg.dataFile = {
    "java/openjdk".source = pkgs.openjdk.home;
    "java/openjdk8".source = pkgs.openjdk8.home;
    "java/openjdk11".source = pkgs.openjdk11.home;
    "java/openjdk19".source = pkgs.openjdk19.home;
    "java/openjdk21".source = pkgs.openjdk21.home;
    # "java/jetbrains".source = pkgs.jetbrains.jdk;
  };
}
