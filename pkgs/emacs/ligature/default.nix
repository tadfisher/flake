{ stdenv, fetchFromGitHub, trivialBuild }:

trivialBuild {
  pname = "ligature";
  version = "unstable-2020-01-01";

  src = fetchFromGitHub {
    owner = "mickeynp";
    repo = "ligature.el";
    rev = "c830b9d74dcf4ff08e6f19cc631d924ce47e2600";
    sha256 = "sha256-cFaXfL7qy1ocjTsQdWxciojTKNTjc6jVUkdvIN2AiKg";
  };

  meta = with stdenv.lib; {
    description = "Typographic ligatures in Emacs";
    homepage = "https://github.com/mickeynp/ligature.el";
    license = licenses.gpl3;
    maintainers = [ maintainers.tadfisher ];
  };
}
