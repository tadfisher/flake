{ lib
, src
, trivialBuild
, acp
, shell-maker
,
}:

trivialBuild {
  pname = "agent-shell";
  version = "unstable";

  inherit src;

  packageRequires = [ acp shell-maker ];

  meta = with lib; {
    description = "A native Emacs buffer to interact with LLM agents powered by ACP";
    homepage = "https://github.com/xenodium/agent-shell";
    license = licenses.gpl3;
    maintainers = [ maintainers.tadfisher ];
  };
}
