{ stdenv, trivialBuild, fetchFromGitLab, org, ox-hugo }:

trivialBuild {
  pname = "org-CV";
  version = "unstable-2020-06-01";

  src = fetchFromGitLab {
    owner = "Titan-C";
    repo = "org-cv";
    rev = "30e93ab10309de2921faf504e1202a9bf188d451";
    sha256 = "1fjk0qa7dzj7p9p0kmbbfhak9xvx88ld2xsp04zbfmnyjdhrzrm5";
  };

  packageRequires = [ org ox-hugo ];

  meta = with stdenv.lib; {
    description = "Collection of export backends for orgmode to generate a CV";
    homepage = "https://gitlab.com/Titan-C/org-cv";
    license = licenses.gpl3;
    maintainers = [ maintainers.tadfisher ];
  };
}
