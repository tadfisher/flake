{ lib
, kotlin-native-unwrapped
, writeShellScriptBin
, jdk
}:

writeShellScriptBin "jvminterop" ''
  declare -a java_args
  declare -a java_opts
  declare -a konan_args

  while [ $# -gt 0 ]; do
    case "$1" in
      -D*)
        java_args=("''${java_args[@]}" "$1")
        shift
        ;;
      -J*)
        java_args=("''${java_args[@]}" "''${1:2}")
        shift
        ;;
      --time)
        konan_args=("''${konan_args[@]}" --time)
        java_args=("''${java_args[@]}" -Dkonan.profile=true)
        TIMECMD=time
        shift
        ;;
       *)
        konan_args[''${#konan_args[@]}]=$1
        shift
        ;;
    esac
  done

  java_opts=(-ea \
            -Xmx3G \
            -XX:TieredStopAtLevel=1 \
            -Dfile.encoding=UTF-8 \
            -Dkonan.home=$KONAN_HOME \
            ''${JAVA_OPTS})

  export KONAN_HOME=${kotlin-native-unwrapped}
  KONAN_JAR="$KONAN_HOME/konan/lib/kotlin-native.jar"
  TROVE_JAR="$KONAN_HOME/konan/lib/trove4j.jar"
  KONAN_CLASSPATH="$KONAN_JAR:$TROVE_JAR"
  TOOL_CLASS=org.jetbrains.kotlin.native.interop.gen.jvm.MainKt
  LIBCLANG_DISABLE_CRASH_RECOVERY=1 \
  $TIMECMD "${jdk.home}/bin/java" "''${java_opts[@]}" "''${java_args[@]}" -cp "$KONAN_CLASSPATH" \
    "$TOOL_CLASS" "''${konan_args[@]}"
''
