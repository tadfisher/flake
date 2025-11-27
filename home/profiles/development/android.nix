{ lib, pkgs, ... }:

with lib;

{
  android-sdk = {
    enable = true;
    packages = mkDefault (sdk: with sdk; [
      build-tools-36-0-0
      cmake-3-22-1
      cmdline-tools-latest
      emulator
      ndk-25-1-8937393
      platforms-android-35
      platform-tools
      skiaparser-1
      skiaparser-2
      skiaparser-3
      sources-android-35
      system-images-android-34-google-apis-playstore-x86-64
    ]);
  };

  home = {
    # file.".gradle/gradle.properties".text = ''
    #   org.gradle.jvmargs=-Xmx2048M -Dkotlin.daemon.jvm.options\="-Xmx2048M"
    #   org.gradle.project.dexJavaMaxHeapSize=2048M
    # '';

    packages = with pkgs; [
      androidStudioPackages.beta
      androidStudioPackages.canary
    ];
  };

  services.adb = {
    enable = true;
    logLevel = "verbose";
    mdnsBackend = "openscreen";
    port = 5038; # Try not to interfere with the built-in server in Studio
  };
}
