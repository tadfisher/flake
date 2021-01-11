{ pkgs, ... }:

{
  android-sdk = {
    enable = true;
    packages = sdk:
      with sdk; [
        build-tools-30-0-2
        cmdline-tools-latest
        emulator
        platforms-android-30
        platform-tools
        skiaparser-1
        sources-android-30
        system-images-android-30-google-apis-playstore-x86
      ];
  };

  home = {
    file.".gradle/gradle.properties".text = ''
      org.gradle.jvmargs=-Xmx2048M -Dkotlin.daemon.jvm.options\="-Xmx2048M"
      org.gradle.project.dexJavaMaxHeapSize=2048M
    '';

    packages = with pkgs; [
      androidStudioPackages.canary
    ];
  };
}
