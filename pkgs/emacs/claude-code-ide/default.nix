{ lib
, src
, trivialBuild
, transient
, web-server
, websocket
}:

trivialBuild {
  pname = "claude-code-ide";
  version = "unstable";

  inherit src;

  packageRequires = [ transient web-server websocket ];

  meta = with lib; {
    description = "Claude Code IDE integration for Emacs";
    homepage = "https://github.com/manzaltu/claude-code-ide.el";
    license = licenses.gpl3;
    maintainers = [ maintainers.tadfisher ];
  };
}
