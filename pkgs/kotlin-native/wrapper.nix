{ lib
, stdenv
, buildEnv
, callPackage
, runCommand
, writeText
, gccForLibs
, kotlin-native-unwrapped
, libffi
, libgcc
}:

# Goal: Wrap kotlin-native to use prebuilts supplied by us.
# These include:
# - llvm distribution
# - libffi
# - lldb
# - gcc+glibc toolchain

with lib;

let
  toKotlinHost = platform:
    if platform.isLinux && platform.isx86_64 then "linux_x64"
    else if platform.isMacOS then
      if platform.isx86_64 then "macos_x64"
      else if platform.isAarch64 then "macos_arm64"
      else throw "Unsupported build system: ${platform.system}"
    else if platform.isMinGW && platform.isx86_64 then "mingw_x64"
    else throw "Unsupported build system: ${platform.system}";

  # Unpackaged:
  # - ios_simulator_arm64
  # - tvos_x64
  # - tvos_simulator_arm64
  # - tvos_arm64
  # - watchos_arm32
  # - watchos_arm64
  # - watchos_x86
  # - watchos_x64
  # - watchos_simulator_arm64
  # We can probably use the aliases in lib.systems.parse though.
  toKotlinTarget = platform:
    let
      mapping = {
        x86_64-apple-darwin = "macos_x64";
        aarch64-apple-darwin = "macos_arm64";
        armv7a-apple-ios = "ios_arm32";
        aarch64-apple-ios = "ios_arm64";
        x86_64-apple-ios = "ios_x64";
        x86_64-unknown-linux-gnu = "linux_x64";
        armv7l-unknown-linux-gnueabihf = "linux_arm32_hfp";
        aarch64-unknown-linux-gnu = "linux_arm64";
        mips-unknown-linux-gnu = "linux_mips32";
        mipsel-unknown-linux-gnu = "linux_mipsel32";
        armv7a-unknown-linux-androideabi = "android_arm32";
        aarch64-unknown-linux-android = "android_arm64";
        i686-unknown-linux-android = "android_x86";
        x86_64-unknown-linux-android = "android_x64";
        x86_64-w64-minw32 = "mingw_x64";
        i686-pc-mingw32 = "mingw_x86";
        wasm32-unknown-unknown = "wasm32";
      };
    in
      mapping."${platform.config}" or (throw "Unsupported host config: ${platform.config}");

  host = toKotlinHost stdenv.buildPlatform;
  target = toKotlinTarget stdenv.hostPlatform;
  hostTarget = if host != target then "${host}-${target}" else host;

  llvmPrebuilt = callPackage ./llvm-prebuilt.nix {
    inherit (kotlin-native-unwrapped) llvmPackages;
  };

  libffiPrebuilt = buildEnv {
    name = "libffi-prebuilt";
    paths = [ libffi libffi.dev ];
  };

  sysrootPrebuilt = buildEnv {
    name = "sysroot-prebuilt";
    paths = [ gccForLibs.out stdenv.cc.libc.out stdenv.cc.libc.dev ];
  };

  jvminterop = callPackage ./jvminterop.nix { };

  konanProperties = writeText "konan.properties" ''
    dependencies =
    dependenciesUrl = file:///dev/null
    dependencyProfiles = default
    airplaneMode = true
    downloadingAttempts = 0
    downloadingAttemptIntervalMs = 0
    homeDependencyCache = /tmp/konancache
    reducedLlvmAppendix = compact

    # Configurables
    llvmHome.${host} = ${llvmPrebuilt}
    llvmVersion.${host} = ${llvmPrebuilt.version}
    libffiDir.${host} = ${libffiPrebuilt}
    cacheableTargets.${host} =
    quadruple.${target} = ${stdenv.hostPlatform.config}
    targetTriple.${target} = ${stdenv.hostPlatform.config}
    additionalCacheFlags.${target} =
    linkerOptimizationFlags.${target} =
    linkerKonanFlags.${target} = -Bstatic -lstdc++ -Bdynamic -ldl -lm -lpthread \
      --defsym __cxa_demangle=Konan_cxa_demangle --gc-sections
    linkerNoDebugFlags.${target} = -S
    linkerDynamicFlags.${target} = -shared
    targetSysRoot.${target} = ${sysrootPrebuilt}
    targetToolchain.${hostTarget} = ${gccForLibs}/lib/gcc/${stdenv.hostPlatform.config}/${gccForLibs.version}
    targetCpu.${target} = ${stdenv.hostPlatform.parsed.cpu.arch}
    # TODO targetCpuFeatures
    llvmInlineThreshold.${target} = 100
    runtimeDefinitions.${target} = USE_GCC_UNWIND=1 KONAN_LINUX=1 KONAN_X64=1 USE_ELF_SYMBOLS=1 ELFSIZE=64

    # ClangFlags
    clangFlags.${target} = -cc1 -target-cpu $targetCpu.linux_x64 -emit-obj -disable-llvm-optzns -x ir \
      -ffunction-sections -fdata-sections
    clangNooptFlags.${target} = -O1
    clangOptFlags.${target} = -O3
    clangDebugFlags.${target} = -O0

    # GccConfigurables
    gccToolchain.${target} = ${gccForLibs}
    libGcc.${target} = lib/gcc/${stdenv.hostPlatform.config}/${gccForLibs.version}
    dynamicLinker.${target} = ${stdenv.cc.bintools.dynamicLinker}
    abiSpecificLibraries.${target} = lib
    crtFilesLocation.${target} = lib
    linker.${hostTarget} = ${stdenv.cc.bintools}/bin/ld.${stdenv.hostPlatform.linker}
    linkerHostSpecificFlags.${hostTarget} =
    linkerGccFlags.${target} = -lgcc --as-needed -lgcc_s --no-as-needed -lc -lgcc --as-needed -lgcc_s \
      --no-as-needed

    # LldFlags
    # TODO lld.wasm32

    # RelocationModeFlags
    dynamicLibraryRelocationMode.${target} = pic
    staticLibraryRelocationMode.${target} = pic
    # TODO executableRelocationMode
  '';

in
runCommand "kotlin-native-wrapped" { } ''
  mkdir -p $out
  cp -r ${kotlin-native-unwrapped}/* $out/
  chmod +w $out/konan $out/bin
  chmod +w $out/konan/konan.properties
  rm $out/konan/konan.properties
  cp ${konanProperties} $out/konan/konan.properties
  cp ${jvminterop}/bin/jvminterop $out/bin/
''
