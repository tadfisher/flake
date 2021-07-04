{ buildEnv
, llvmPackages
}:

# LLVM prebuilt examples:
# - https://github.com/NixOS/nixpkgs/blob/3b6c3bee9174dfe56fd0e586449457467abe7116/pkgs/development/compilers/intel-graphics-compiler/default.nix
#

buildEnv {
  name = "kotlin-native-llvm-home";
  paths = with llvmPackages; [
    clang
    compiler-rt
    libclang
    libclang.lib
    libcxx
    libcxxabi
    libunwind
    lld
    llvm
  ];

  passthru.version = llvmPackages.llvm.version;
}
