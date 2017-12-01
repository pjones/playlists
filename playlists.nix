{ mkDerivation, attoparsec, base, bytestring, doctest, filepath
, hspec, optparse-applicative, stdenv, text, word8
}:
mkDerivation {
  pname = "playlists";
  version = "0.5.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    attoparsec base bytestring filepath text word8
  ];
  executableHaskellDepends = [
    base bytestring optparse-applicative text
  ];
  testHaskellDepends = [
    base bytestring doctest filepath hspec text
  ];
  homepage = "https://github.com/pjones/playlists";
  description = "Library and executable for working with playlist files";
  license = stdenv.lib.licenses.bsd3;
}
