{ lib
, stdenv
, buildPythonPackage
, fetchPypi
, docopt
}:

buildPythonPackage rec {
  pname = "python-ips";
  version = "2.0";

  src = fetchPypi {
    inherit pname version;
    sha256 = "0r0an1clqrl9sk83jqi2n4y0wcz2p8pzwl0i9nrng1m6mz9mybiw";
  };

  propagatedBuildInputs = [ docopt ];

  meta = with lib; {
    description = "An IPS patching application with api, cli, and WinForms interfaces";
    homepage = "https://pypi.org/project/python-ips/";
    license = licenses.mit;
    maintainers = [ maintainers.tadfisher ];
  };
}
