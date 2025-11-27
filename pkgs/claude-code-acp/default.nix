{ lib
, buildNpmPackage
, fetchFromGitHub
,
}:

buildNpmPackage (finalAttrs: {
  pname = "claude-code-acp";
  version = "0.8.0";

  src = fetchFromGitHub {
    owner = "zed-industries";
    repo = "claude-code-acp";
    tag = "v${finalAttrs.version}";
    hash = "sha256-hReGoh4O5ZkUcJ6M0j8CGg0DCV+KLQzFtpats8x9YtU=";
  };

  npmDepsHash = "sha256-/zsBmiczlMoEQdrrgX1qdujoVjtxQoJBPJBytr5KZ5A=";

  meta = {
    description = "ACP-compatible coding agent powered by the Claude Code SDK";
    homepage = "https://github.com/zed-industries/claude-code-acp";
    license = lib.licenses.asl20;
    mainProgram = "claude-code-acp";
  };
})
