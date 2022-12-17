{ pkgs, ... }:

let
  gradleWithToolchains = with pkgs; callPackage gradle-packages.gradle_7 {
    javaToolchains = [
      openjdk8
      openjdk11
      openjdk17
      openjdk19
    ];
  };

in
{
  home = {
    file.".gradle/gradle.properties".text = ''
      org.gradle.java.installations.auto-download=false
      org.gradle.java.installations.fromEnv=JDK8,JDK11,JDK17,JDK19
    '';

    packages = with pkgs; [
      gradleWithToolchains
      gradle-completion
      idea-community
      idea-community-eap
    ];

    sessionVariables = {
      JAVA_HOME = "${pkgs.openjdk.home}";
      JDK8 = "${pkgs.openjdk8}";
      JDK11 = "${pkgs.openjdk11}";
      JDK17 = "${pkgs.openjdk17}";
      JDK19 = "${pkgs.openjdk19}";
    };
  };

  xdg.dataFile = {
    "java/openjdk".source = pkgs.openjdk.home;
    "java/openjdk8".source = pkgs.openjdk8.home;
    "java/openjdk11".source = pkgs.openjdk11.home;
    "java/openjdk19".source = pkgs.openjdk19.home;
    # "java/jetbrains".source = pkgs.jetbrains.jdk;
  };
}
