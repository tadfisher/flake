{
  android-sdk = {
    enable = true;
    packages = sdk:
      with sdk; [
        build-tools-30-0-2
        cmdline-tools-latest
        emulator
        platforms.android-30
        platform-tools
        skiaparser-1
        sources.android-30
        system-images.android-30.google-apis-playstore.x86
      ];
  };
}
