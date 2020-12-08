{ fetchFromGitHub }:

{
  version = "2020-03-15";
  src = fetchFromGitHub {
    owner = "paperwm";
    repo = "gnome-shell-mode";
    rev = "a780fced2487188759b7b320cbf8884d4dcebe93";
    sha256 = "0gqr9asajwd9fkpfsqr2l7fk124nzjdn5ql8li8h19wack87cc1j";
  };
}
